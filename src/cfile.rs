use std::convert::AsRef;
use std::ffi::CString;
use std::fmt;
use std::fs::Metadata;
use std::io;
use std::mem;
use std::ops::{Deref, DerefMut, Drop};
use std::path::{Path, PathBuf};
use std::ptr::NonNull;
use std::str;
use std::sync::Arc;

cfg_if! {
    if #[cfg(unix)] {
        use std::os::unix::ffi::OsStrExt;
        use std::os::unix::io::{AsRawFd, IntoRawFd, RawFd};
    } else {
        use std::ffi::OsString;
        use std::os::windows::ffi::OsStringExt;
        use std::os::windows::io::{AsRawHandle, IntoRawHandle, RawHandle};
    }
}

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

    /// Reads n elements of data, return the number of items read.
    fn read_slice<T: Sized>(&mut self, elements: &mut [T]) -> io::Result<usize>;

    /// Writes n elements of data, return the number of items written.
    fn write_slice<T: Sized>(&mut self, elements: &[T]) -> io::Result<usize>;
}

cfg_if! {
    if #[cfg(unix)] {
        /// A trait for converting a raw fd to a C *FILE stream.
        pub trait ToStream: AsRawFd + Sized {
            /// Open a raw fd as C *FILE stream
            fn to_stream(&self, mode: &str) -> io::Result<CFile> {
                unsafe { CFile::fdopen(self.as_raw_fd(), mode, false) }
            }
        }

        impl<S: AsRawFd + Sized> ToStream for S {}

        /// A trait to express the ability to consume an object and acquire ownership of its stream.
        pub trait IntoStream: IntoRawFd + Sized {
            /// Consumes this raw fd, returning the raw underlying C *FILE stream.
            fn into_stream(self, mode: &str) -> io::Result<CFile> {
                unsafe { CFile::fdopen(self.into_raw_fd(), mode, true) }
            }
        }

        impl<S: IntoRawFd + Sized> IntoStream for S {}
    } else {
        /// A trait for converting a raw fd to a C *FILE stream.
        pub trait ToStream: AsRawHandle + Sized {
            /// Open a raw fd as C *FILE stream
            fn to_stream(&self, mode: &str) -> io::Result<CFile> {
                unsafe { CFile::fdopen(self.as_raw_handle(), mode, false) }
            }
        }

        impl<S: AsRawHandle + Sized> ToStream for S {}

        /// A trait to express the ability to consume an object and acquire ownership of its stream.
        pub trait IntoStream: IntoRawHandle + Sized {
            /// Consumes this raw fd, returning the raw underlying C *FILE stream.
            fn into_stream(self, mode: &str) -> io::Result<CFile> {
                unsafe { CFile::fdopen(self.into_raw_handle(), mode, true) }
            }
        }

        impl<S: IntoRawHandle + Sized> IntoStream for S {}
    }
}

macro_rules! cstr {
    ($s:expr) => {
        CString::new($s)?.as_ptr() as *const i8
    };
}

/// Raw C *FILE stream.
pub type RawFilePtr = NonNull<libc::FILE>;

#[derive(Debug)]
pub struct Owned(RawFilePtr);

impl Drop for Owned {
    fn drop(&mut self) {
        unsafe {
            libc::fclose(self.0.as_ptr());
        }
    }
}

/// Raw file stream.
#[derive(Clone, Debug)]
pub enum RawFile {
    Owned(Arc<Owned>),
    Borrowed(RawFilePtr),
}

impl Deref for RawFile {
    type Target = libc::FILE;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.stream() }
    }
}

impl DerefMut for RawFile {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.stream() }
    }
}

cfg_if! {
    if #[cfg(unix)] {
        impl AsRawFd for RawFile {
            fn as_raw_fd(&self) -> RawFd {
                unsafe { libc::fileno(self.stream()) }
            }
        }

        impl IntoRawFd for RawFile {
            fn into_raw_fd(self) -> RawFd {
                let fd = self.as_raw_fd();

                mem::forget(self);

                fd
            }
        }
    } else {
        impl AsRawHandle for RawFile {
            fn as_raw_handle(&self) -> RawHandle {
                unsafe { libc::get_osfhandle(libc::fileno(self.stream())) as RawHandle }
            }
        }

        impl IntoRawHandle for RawFile {
            fn into_raw_handle(self) -> RawHandle {
                let handle = self.as_raw_handle();

                mem::forget(self);

                handle
            }
        }
    }
}

impl RawFile {
    /// Extracts the raw *FILE stream.
    pub fn as_raw(&self) -> RawFilePtr {
        match self {
            RawFile::Owned(s) => s.0,
            RawFile::Borrowed(f) => *f,
        }
    }

    /// Consumes this object, returning the raw *FILE stream.
    pub fn into_raw(self) -> RawFilePtr {
        let f = self.as_raw();

        mem::forget(self);

        f
    }

    /// Returns the raw pointer of the stream
    pub fn stream(&self) -> *mut libc::FILE {
        self.as_raw().as_ptr()
    }

    /// Check if it is already owned.
    pub fn owned(&self) -> bool {
        match self {
            RawFile::Owned(_) => true,
            RawFile::Borrowed(_) => false,
        }
    }
}

extern "C" {
    fn clearerr(file: *mut libc::FILE);
}

/// A reference to an open stream on the filesystem.
///
/// An instance of a `CFile` can be read and/or written depending on what modes it was opened with.
/// `CFile` also implement `Seek` to alter the logical cursor that the file contains internally.
///
#[derive(Clone)]
pub struct CFile {
    inner: RawFile,
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
    pub unsafe fn from_raw(f: RawFilePtr, owned: bool) -> CFile {
        CFile {
            inner: if owned {
                RawFile::Owned(Arc::new(Owned(f)))
            } else {
                RawFile::Borrowed(f)
            },
        }
    }

    pub unsafe fn owned(f: RawFilePtr) -> CFile {
        Self::from_raw(f, true)
    }

    pub unsafe fn borrowed(f: RawFilePtr) -> CFile {
        Self::from_raw(f, false)
    }

    /// opens the file whose name is the string pointed to by `filename`
    /// and associates the stream pointed to by stream with it.
    ///
    /// The original stream (if it exists) is closed.
    /// The mode argument is used just as in the `open()` function.
    ///
    pub fn reopen(&self, filename: &str, mode: &str) -> io::Result<CFile> {
        unsafe {
            NonNull::new(libc::freopen(cstr!(filename), cstr!(mode), self.stream()))
                .map(|f| Self::owned(f))
                .ok_or_else(io::Error::last_os_error)
        }
    }
}

cfg_if! {
    if #[cfg(unix)] {
        impl CFile {
            unsafe fn fdopen(fd: RawFd, mode: &str, owned: bool) -> io::Result<CFile> {
                NonNull::new(libc::fdopen(fd, cstr!(mode)))
                    .map(|f| {
                        Self::from_raw(
                            f,
                            match fd {
                                libc::STDIN_FILENO | libc::STDOUT_FILENO | libc::STDERR_FILENO => false,
                                _ => owned,
                            },
                        )
                    })
                    .ok_or_else(io::Error::last_os_error)
            }
        }
    } else {
        impl CFile {
            unsafe fn fdopen(handle: RawHandle, mode: &str, owned: bool) -> io::Result<CFile> {
                let mut flags = 0;
                let fd = libc::open_osfhandle(handle as isize, flags);

                NonNull::new(libc::fdopen(fd, cstr!(mode)))
                    .map(|f| Self::from_raw(f, owned))
                    .ok_or_else(io::Error::last_os_error)
            }
        }
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
cfg_if! {
    if #[cfg(unix)] {
        pub fn open<P: AsRef<Path>>(path: P, mode: &str) -> io::Result<CFile> {
            unsafe {
                NonNull::new(libc::fopen(
                    cstr!(path.as_ref().as_os_str().as_bytes()),
                    cstr!(mode),
                ))
                .map(|f| CFile::owned(f))
                .ok_or_else(io::Error::last_os_error)
            }
        }
    } else {
        pub fn open<P: AsRef<Path>>(path: P, mode: &str) -> io::Result<CFile> {
            let filename = path.as_ref().as_os_str().to_string_lossy().to_string();

            unsafe {
                NonNull::new(libc::fopen(
                    cstr!(filename),
                    cstr!(mode),
                ))
                .map(|f| CFile::owned(f))
                .ok_or_else(io::Error::last_os_error)
            }
        }
    }
}

cfg_if! {
    if #[cfg(unix)] {
        /// open stdin as a read only stream
        ///
        /// ```
        /// use std::os::unix::io::AsRawFd;
        ///
        /// use cfile;
        ///
        /// let stdin = cfile::stdin().unwrap();
        ///
        /// assert!(!stdin.owned());
        /// assert_eq!(stdin.as_raw_fd(), cfile::STDIN_FILENO);
        /// ```
        pub fn stdin() -> io::Result<CFile> {
            unsafe { CFile::fdopen(libc::STDIN_FILENO, "r", false) }
        }

        /// open stdout as a write only stream
        pub fn stdout() -> io::Result<CFile> {
            unsafe { CFile::fdopen(libc::STDOUT_FILENO, "w", false) }
        }

        /// open stderr as a write only stream
        pub fn stderr() -> io::Result<CFile> {
            unsafe { CFile::fdopen(libc::STDERR_FILENO, "w", false) }
        }
    } else {
        use winapi::um::processenv::GetStdHandle;
        use winapi::um::handleapi::INVALID_HANDLE_VALUE;
        use winapi::um::winbase::{STD_INPUT_HANDLE, STD_OUTPUT_HANDLE, STD_ERROR_HANDLE};

        /// open stdin as a read only stream
        pub fn stdin() -> io::Result<CFile> {
            unsafe {
                let h = GetStdHandle(STD_INPUT_HANDLE);

                if h == INVALID_HANDLE_VALUE {
                    Err(io::Error::last_os_error())
                } else {
                    CFile::fdopen(h as *mut _, "r", false)
                }
            }
        }

        /// open stdout as a write only stream
        pub fn stdout() -> io::Result<CFile> {
            unsafe {
                let h = GetStdHandle(STD_OUTPUT_HANDLE);

                if h == INVALID_HANDLE_VALUE {
                    Err(io::Error::last_os_error())
                } else {
                    CFile::fdopen(h as *mut _, "w", false)
                }
            }
        }

        /// open stderr as a write only stream
        pub fn stderr() -> io::Result<CFile> {
            unsafe {
                let h = GetStdHandle(STD_ERROR_HANDLE);

                if h == INVALID_HANDLE_VALUE {
                    Err(io::Error::last_os_error())
                } else {
                    CFile::fdopen(h as *mut _, "w", false)
                }
            }
        }
    }
}

/// open a temporary file as a read/write stream
pub fn tmpfile() -> io::Result<CFile> {
    unsafe {
        NonNull::new(libc::tmpfile())
            .map(|f| CFile::owned(f))
            .ok_or_else(io::Error::last_os_error)
    }
}

cfg_if! {
    if #[cfg(unix)] {
        /// associates a stream with the existing file descriptor.
        ///
        /// The mode of the stream must be compatible with the mode of the file descriptor.
        ///
        pub fn fdopen<S: AsRawFd>(s: &S, mode: &str) -> io::Result<CFile> {
            unsafe { CFile::fdopen(s.as_raw_fd(), mode, true) }
        }
    } else {
        /// associates a stream with the existing file descriptor.
        ///
        /// The mode of the stream must be compatible with the mode of the file descriptor.
        ///
        pub fn fdopen<S: AsRawHandle>(s: &S, mode: &str) -> io::Result<CFile> {
            unsafe { CFile::fdopen(s.as_raw_handle(), mode, true) }
        }
    }
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
        use std::ffi::CStr;

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

    #[cfg(target_os = "windows")]
    fn file_name(&self) -> io::Result<PathBuf> {
        use winapi::shared::minwindef::MAX_PATH;
        use winapi::um::fileapi::FILE_NAME_INFO;
        use winapi::um::minwinbase::FileNameInfo;
        use winapi::um::winbase::GetFileInformationByHandleEx;
        use winapi::um::winnt::WCHAR;

        let wchar_size = mem::size_of::<WCHAR>();
        let bufsize = mem::size_of::<FILE_NAME_INFO>() + MAX_PATH * wchar_size;
        let mut buf = vec![0u8; bufsize];

        unsafe {
            if GetFileInformationByHandleEx(
                self.as_raw_handle() as *mut _,
                FileNameInfo,
                buf.as_mut_ptr() as *mut _,
                buf.len() as u32,
            ) == 0
            {
                Err(io::Error::last_os_error())
            } else {
                let fi = NonNull::new_unchecked(buf.as_mut_ptr()).cast::<FILE_NAME_INFO>();
                let fi = fi.as_ref();
                let filename = std::slice::from_raw_parts(
                    fi.FileName.as_ptr(),
                    fi.FileNameLength as usize / wchar_size,
                );

                Ok(PathBuf::from(OsString::from_wide(filename)))
            }
        }
    }

    fn metadata(&self) -> io::Result<Metadata> {
        self.file_name()?.as_path().metadata()
    }

    fn read_slice<T: Sized>(&mut self, elements: &mut [T]) -> io::Result<usize> {
        if elements.is_empty() {
            return Ok(0);
        }

        let read = unsafe {
            libc::fread(
                elements.as_mut_ptr() as *mut libc::c_void,
                mem::size_of::<T>(),
                elements.len(),
                self.stream(),
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
            libc::fwrite(
                elements.as_ptr() as *const libc::c_void,
                mem::size_of::<T>(),
                elements.len(),
                self.stream(),
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

impl io::Read for CFile {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.read_slice(buf)
    }
}

impl io::Write for CFile {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.write_slice(buf)
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

cfg_if! {
    if #[cfg(unix)] {
        use libc::fseek as fseek64;
    } else {
        extern "C" {
            #[link_name = "_fseeki64"]
            fn fseek64(file: *mut libc::FILE, offset: i64, origin: i32) -> i32;
        }
    }
}

impl io::Seek for CFile {
    fn seek(&mut self, pos: io::SeekFrom) -> io::Result<u64> {
        let ret = unsafe {
            match pos {
                io::SeekFrom::Start(off) => fseek64(self.stream(), off as i64, libc::SEEK_SET),
                io::SeekFrom::End(off) => fseek64(self.stream(), off, libc::SEEK_END),
                io::SeekFrom::Current(off) => fseek64(self.stream(), off, libc::SEEK_CUR),
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
        match self.inner {
            RawFile::Owned(_) => write!(f, "CFile(Owned({:p}))", self.stream()),
            RawFile::Borrowed(_) => write!(f, "CFile(Borrowed({:p})", self.stream()),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::io::{prelude::*, SeekFrom};

    use super::*;

    #[test]
    fn test_cfile() {
        let mut f = tmpfile().unwrap();

        assert_eq!(f.write(b"test").unwrap(), 4);
        assert_eq!(f.seek(SeekFrom::Current(0)).unwrap(), 4);

        let mut buf: [u8; 4] = [0; 4];

        assert_eq!(f.read(&mut buf[..]).unwrap(), 0);

        assert_eq!(f.seek(SeekFrom::Start(0)).unwrap(), 0);

        f.flush().unwrap();

        assert_eq!(f.read(&mut buf[..]).unwrap(), 4);
        assert_eq!(buf, &b"test"[..]);

        let filename = f.file_name().unwrap();

        println!("{:?}", filename);

        assert!(filename.parent().unwrap().is_dir());
    }
}
