use std::io;
use std::str;
use std::fmt;
use std::ffi::{CStr, CString};
use std::fs::Metadata;
use std::path::{Path, PathBuf};
use std::convert::AsRef;
use std::mem::forget;
use std::ops::{Deref, DerefMut, Drop};
use std::os::unix::io::{AsRawFd, IntoRawFd, RawFd};
use std::os::unix::ffi::OsStrExt;

use libc;

/// C *FILE stream
pub trait Stream: io::Read + io::Write + io::Seek {
    /// returns the current position of the stream.
    fn position(&self) -> io::Result<u64>;

    /// tests the end-of-file indicator for the stream
    fn eof(&self) -> bool;

    /// tests the error indicator for the stream
    fn errno(&self) -> i32;

    /// get the last error of the stream
    fn last_error(&self) -> Option<io::Error>;

    /// clears the end-of-file and error indicators for the stream
    fn clear_error(&self);

    /// returns the file name of the stream
    fn file_name(&self) -> io::Result<PathBuf>;

    /// Queries metadata about the underlying file.
    fn metadata(&self) -> io::Result<Metadata>;
}

pub trait ToStream: AsRawFd + Sized {
    fn to_stream(&self, mode: &str) -> io::Result<CFile> {
        open_stream(self, mode)
    }
}

impl<S: AsRawFd + Sized> ToStream for S {}

macro_rules! cstr {
    ($s:expr) => (try!(CString::new($s)).as_ptr() as *const i8)
}

/// Raw C *FILE stream.
pub type RawFilePtr = *mut libc::FILE;

/// Raw file stream.
pub struct RawFile(RawFilePtr);

impl Drop for RawFile {
    fn drop(&mut self) {
        unsafe {
            libc::fclose(self.0);
        }
    }
}

impl Deref for RawFile {
    type Target = libc::FILE;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.0 }
    }
}

impl DerefMut for RawFile {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.0 }
    }
}

impl RawFile {
    /// returns the raw pointer of the stream
    pub fn stream(&self) -> RawFilePtr {
        self.0
    }
}

extern "C" {
    fn clearerr(file: RawFilePtr);
}

/// A reference to an open stream on the filesystem.
///
/// An instance of a `CFile` can be read and/or written depending on what modes it was opened with.
/// `CFile` also implement `Seek` to alter the logical cursor that the file contains internally.
///
pub struct CFile {
    inner: RawFile,
    owned: bool,
}

unsafe impl Sync for CFile {}
unsafe impl Send for CFile {}

impl Deref for CFile {
    type Target = RawFile;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for CFile {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl CFile {
    /// Constructs a `CFile` from a raw pointer.
    pub fn from_raw(f: *mut libc::FILE, owned: bool) -> io::Result<CFile> {
        if f.is_null() {
            Err(io::Error::last_os_error())
        } else {
            Ok(CFile {
                inner: RawFile(f),
                owned: owned,
            })
        }
    }

    fn _open_fd(fd: RawFd, mode: &str, owned: bool) -> io::Result<CFile> {
        Self::from_raw(unsafe { libc::fdopen(fd, cstr!(mode)) }, owned)
    }

    /// opens the file whose name is the string pointed to by `filename`
    /// and associates the stream pointed to by stream with it.
    ///
    /// The original stream (if it exists) is closed.
    /// The mode argument is used just as in the `open()` function.
    ///
    pub fn reopen(&self, filename: &str, mode: &str) -> io::Result<CFile> {
        let f = unsafe { libc::freopen(cstr!(filename), cstr!(mode), self.stream()) };

        Self::from_raw(f, true)
    }
}

/// opens the file whose name is the string pointed to by filename
/// and associates a stream with it.
///
/// The argument mode points to a string beginning with one of the following
/// sequences (Additional characters may follow these sequences.)
///
/// ## Modes
///
/// * `r`   Open text file for reading.  The stream is positioned at the
///         beginning of the file.
///
/// * `r+`  Open for reading and writing.  The stream is positioned at the
///         beginning of the file.
///
/// * `w`     Truncate to zero length or create text file for writing.  The
///         stream is positioned at the beginning of the file.
///
/// * `w+`  Open for reading and writing.  The file is created if it does not
///         exist, otherwise it is truncated.  The stream is positioned at
///         the beginning of the file.
///
/// * `a`   Open for writing.  The file is created if it does not exist.  The
///         stream is positioned at the end of the file.  Subsequent writes
///         to the file will always end up at the then current end of file,
///         irrespective of any intervening fseek(3) or similar.
///
/// * `a+`  Open for reading and writing.  The file is created if it does not
///         exist.  The stream is positioned at the end of the file.  Subse-
///         quent writes to the file will always end up at the then current
///         end of file, irrespective of any intervening fseek(3) or similar.
///
/// The mode string can also include the letter `b` either as last charac-
/// ter or as a character between the characters in any of the two-character
/// strings described above.  This is strictly for compatibility with ISO/IEC
/// 9899:1990 (ISO C90) and has no effect; the `b` is ignored.
///
pub fn open<P: AsRef<Path>>(path: P, mode: &str) -> io::Result<CFile> {
    CFile::from_raw(unsafe {
                        libc::fopen(cstr!(path.as_ref()
                                        .as_os_str()
                                        .as_bytes()),
                                    cstr!(mode))
                    },
                    true)
}

/// open stdin as a read only stream
pub fn stdin() -> io::Result<CFile> {
    CFile::_open_fd(libc::STDIN_FILENO, "r", false)
}

/// open stdout as a write only stream
pub fn stdout() -> io::Result<CFile> {
    CFile::_open_fd(libc::STDOUT_FILENO, "w", false)
}

/// open stderr as a write only stream
pub fn stderr() -> io::Result<CFile> {
    CFile::_open_fd(libc::STDERR_FILENO, "w", false)
}

/// open a temporary file as a read/write stream
pub fn tmpfile() -> io::Result<CFile> {
    CFile::from_raw(unsafe { libc::tmpfile() }, true)
}

/// associates a stream with the existing file descriptor.
///
/// The mode of the stream must be compatible with the mode of the file descriptor.
///
pub fn open_stream<S: AsRawFd>(s: &S, mode: &str) -> io::Result<CFile> {
    CFile::_open_fd(s.as_raw_fd(), mode, true)
}

impl Stream for CFile {
    fn position(&self) -> io::Result<u64> {
        let off = unsafe { libc::ftell(self.stream()) };

        if off < 0 {
            if let Some(err) = self.last_error() {
                return Err(err);
            }
        }

        Ok(off as u64)
    }

    #[inline]
    fn eof(&self) -> bool {
        unsafe { libc::feof(self.stream()) != 0 }
    }

    #[inline]
    fn errno(&self) -> i32 {
        unsafe { libc::ferror(self.stream()) }
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

    fn clear_error(&self) {
        unsafe { clearerr(self.stream()) }
    }

    #[cfg(target_os = "linux")]
    fn file_name(&self) -> io::Result<PathBuf> {
        let s = format!("/proc/self/fd/{}", self.as_raw_fd());
        let p = Path::new(&s);

        if p.exists() {
            p.read_link()
        } else {
            Err(io::Error::new(io::ErrorKind::NotFound, "fd not found"))
        }
    }

    #[cfg(target_os = "macos")]
    fn file_name(&self) -> io::Result<PathBuf> {
        let mut buf = Vec::with_capacity(libc::PATH_MAX as usize);

        let ret = unsafe { libc::fcntl(self.as_raw_fd(), libc::F_GETPATH, buf.as_mut_ptr()) };

        let filename = str::from_utf8(unsafe { CStr::from_ptr(buf.as_ptr()).to_bytes() }).unwrap();

        println!("{}, {}", filename, ret);

        if ret < 0 {
            Err(io::Error::last_os_error())
        } else {
            Ok(PathBuf::from(filename))
        }
    }

    fn metadata(&self) -> io::Result<Metadata> {
        try!(self.file_name()).as_path().metadata()
    }
}

impl AsRawFd for CFile {
    fn as_raw_fd(&self) -> RawFd {
        unsafe { libc::fileno(self.stream()) }
    }
}

impl IntoRawFd for CFile {
    fn into_raw_fd(self) -> RawFd {
        let fd = unsafe { libc::fileno(self.stream()) };

        forget(self);

        fd
    }
}

impl io::Read for CFile {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        if buf.is_empty() {
            return Ok(0);
        }

        let read = unsafe {
            libc::fread(buf.as_ptr() as *mut libc::c_void,
                        1,
                        buf.len(),
                        self.stream())
        };

        if let Some(err) = self.last_error() {
            if read == 0 {
                return Err(err);
            }
        }

        Ok(read)
    }
}

impl io::Write for CFile {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        if buf.is_empty() {
            return Ok(0);
        }

        let wrote = unsafe {
            libc::fwrite(buf.as_ptr() as *const libc::c_void,
                         1,
                         buf.len(),
                         self.stream())
        };

        if let Some(err) = self.last_error() {
            if wrote == 0 {
                return Err(err);
            }
        }

        Ok(wrote)
    }

    fn flush(&mut self) -> io::Result<()> {
        if unsafe { libc::fflush(self.stream()) } != 0 {
            if let Some(err) = self.last_error() {
                return Err(err);
            }
        }

        Ok(())
    }
}

impl io::Seek for CFile {
    fn seek(&mut self, pos: io::SeekFrom) -> io::Result<u64> {
        let ret = unsafe {
            match pos {
                io::SeekFrom::Start(off) => libc::fseek(self.stream(), off as i64, libc::SEEK_SET),
                io::SeekFrom::End(off) => libc::fseek(self.stream(), off, libc::SEEK_END),
                io::SeekFrom::Current(off) => libc::fseek(self.stream(), off, libc::SEEK_CUR),
            }
        };

        if ret != 0 {
            if let Some(err) = self.last_error() {
                return Err(err);
            }
        }

        self.position()
    }
}

impl fmt::Debug for CFile {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "CFile {{f: {:p}, owned: {}}}", self.stream(), self.owned)
    }
}

#[cfg(test)]
mod tests {
    use std::io::{Read, Write, Seek, SeekFrom};
    use std::os::unix::io::AsRawFd;

    use super::*;

    #[test]
    fn test_cfile() {
        let mut f = tmpfile().unwrap();

        assert!(!f.stream().is_null());
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
    }
}
