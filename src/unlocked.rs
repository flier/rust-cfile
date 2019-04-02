//! Each of these functions has the same behavior as its counterpart without the "_unlocked" suffix,
//! except that they do not use locking (they do not set locks themselves,
//! and do not test for the presence of locks set by others) and hence are thread-unsafe.

use std::fs::Metadata;
use std::io::{self, prelude::*};
use std::mem;
use std::os::unix::io::{AsRawFd, IntoRawFd, RawFd};
use std::path::PathBuf;

use crate::{CFile, Stream, Lines};

impl CFile {
    /// Returns a unlocked `CFile` stream except that they do not use locking.
    pub fn unlocked(self) -> Unlocked {
        Unlocked(self)
    }
}

/// A unlocked `CFile` stream except that they do not use locking.
#[derive(Clone, Debug)]
pub struct Unlocked(CFile);

impl AsRawFd for Unlocked {
    fn as_raw_fd(&self) -> RawFd {
        unsafe { fileno_unlocked(self.0.stream()) }
    }
}

impl IntoRawFd for Unlocked {
    fn into_raw_fd(self) -> RawFd {
        let fd = self.as_raw_fd();

        mem::forget(self);

        fd
    }
}

impl Read for Unlocked {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.read_slice(buf)
    }
}

impl Write for Unlocked {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.write_slice(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        if unsafe { fflush_unlocked(self.0.stream()) } != 0 {
            if let Some(err) = self.last_error() {
                return Err(err);
            }
        }

        Ok(())
    }
}

impl Seek for Unlocked {
    fn seek(&mut self, pos: io::SeekFrom) -> io::Result<u64> {
        self.0.seek(pos)
    }
}

impl Stream for Unlocked {
    #[inline]
    fn position(&self) -> io::Result<u64> {
        self.0.position()
    }

    #[inline]
    fn eof(&self) -> bool {
        unsafe { feof_unlocked(self.0.stream()) != 0 }
    }

    #[inline]
    fn errno(&self) -> i32 {
        unsafe { ferror_unlocked(self.0.stream()) }
    }

    fn last_error(&self) -> Option<io::Error> {
        let errno = self.errno();

        if errno != 0 {
            return Some(io::Error::from_raw_os_error(errno));
        }

        let err = io::Error::last_os_error();

        match err.raw_os_error() {
            Some(errno) if errno != 0 => Some(err),
            _ => None,
        }
    }

    #[inline]
    fn clear_error(&self) {
        unsafe { clearerr_unlocked(self.0.stream()) }
    }

    #[inline]
    fn file_name(&self) -> io::Result<PathBuf> {
        self.0.file_name()
    }

    #[inline]
    fn metadata(&self) -> io::Result<Metadata> {
        self.0.metadata()
    }

    fn read_slice<T: Sized>(&mut self, elements: &mut [T]) -> io::Result<usize> {
        if elements.is_empty() {
            return Ok(0);
        }

        let read = unsafe {
            fread_unlocked(
                elements.as_mut_ptr() as *mut libc::c_void,
                mem::size_of::<T>(),
                elements.len(),
                self.0.stream(),
            )
        };

        if let Some(err) = self.last_error() {
            if read == 0 {
                return Err(err);
            }
        }

        Ok(read)
    }

    fn write_slice<T: Sized>(&mut self, elements: &[T]) -> io::Result<usize> {
        if elements.is_empty() {
            return Ok(0);
        }

        let wrote = unsafe {
            fwrite_unlocked(
                elements.as_ptr() as *const libc::c_void,
                mem::size_of::<T>(),
                elements.len(),
                self.0.stream(),
            )
        };

        if let Some(err) = self.last_error() {
            if wrote == 0 {
                return Err(err);
            }
        }

        Ok(wrote)
    }
}

impl Unlocked {
    /// Unwraps this `Unlocked`, returning the underlying `CFile`.
    pub fn into_inner(self) -> CFile {
        self.0
    }

    /// An iterator over the bytes of a *FILE stream.
    pub fn bytes(&self) -> Bytes {
        Bytes(self)
    }
        /// An iterator over the lines of a *FILE stream.
    pub fn lines(&self) -> Lines<Bytes> {
        Lines(Bytes(self))
    }
}

/// An iterator over the bytes of a *FILE stream.
pub struct Bytes<'a>(&'a Unlocked);

impl<'a> Iterator for Bytes<'a> {
    type Item = io::Result<u8>;

    fn next(&mut self) -> Option<Self::Item> {
        let b = unsafe { fgetc_unlocked((self.0).0.stream()) };

        if b == libc::EOF {
            let err = io::Error::last_os_error();

            if err.raw_os_error() == Some(0) {
                None
            } else {
                Some(Err(err))
            }
        } else {
            Some(Ok(b as u8))
        }
    }
}

impl<'a> IntoIterator for &'a Unlocked {
    type Item = io::Result<u8>;
    type IntoIter = Bytes<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.bytes()
    }
}

extern "C" {
    fn fread_unlocked(
        ptr: *mut libc::c_void,
        size: libc::size_t,
        nobj: libc::size_t,
        stream: *mut libc::FILE,
    ) -> libc::size_t;

    fn fwrite_unlocked(
        ptr: *const libc::c_void,
        size: libc::size_t,
        nobj: libc::size_t,
        stream: *mut libc::FILE,
    ) -> libc::size_t;

    fn fgetc_unlocked(stream: *mut libc::FILE) -> libc::c_int;

    fn feof_unlocked(stream: *mut libc::FILE) -> libc::c_int;

    fn ferror_unlocked(stream: *mut libc::FILE) -> libc::c_int;

    fn fileno_unlocked(stream: *mut libc::FILE) -> libc::c_int;

    fn fflush_unlocked(file: *mut libc::FILE) -> libc::c_int;

    fn clearerr_unlocked(file: *mut libc::FILE);
}

#[cfg(test)]
mod tests {
    use std::io::{prelude::*, Result, SeekFrom};
    use std::os::unix::io::AsRawFd;

    use crate::{tmpfile, Stream};

    #[test]
    fn test_unlocked() {
        let mut f = tmpfile().unwrap().unlocked();

        assert!(f.as_raw_fd() > 2);

        assert_eq!(f.write(b"test").unwrap(), 4);
        assert_eq!(f.seek(SeekFrom::Current(0)).unwrap(), 4);

        let mut buf: [u8; 4] = [0; 4];

        assert_eq!(f.read(&mut buf[..]).unwrap(), 0);

        assert_eq!(f.seek(SeekFrom::Start(0)).unwrap(), 0);

        f.flush().unwrap();

        assert_eq!(f.read(&mut buf[..]).unwrap(), 4);
        assert_eq!(buf, &b"test"[..]);

        let filename = f.file_name().unwrap();

        assert!(filename.is_absolute());
        assert!(filename.parent().unwrap().is_dir());

        assert_eq!(f.seek(SeekFrom::Start(0)).unwrap(), 0);

        assert_eq!(f.into_iter().collect::<Result<Vec<u8>>>().unwrap(), b"test");
    }
}
