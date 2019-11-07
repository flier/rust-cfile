use std::convert::AsRef;
use std::ffi::CString;
use std::io;
use std::mem;
use std::path::Path;
use std::str;

use foreign_types::{foreign_type, ForeignType, ForeignTypeRef};

cfg_if! {
    if #[cfg(unix)] {
        use std::os::unix::ffi::OsStrExt;
        use std::os::unix::io::{AsRawFd, IntoRawFd, RawFd};
    } else {
        use std::os::windows::io::{AsRawHandle, IntoRawHandle, RawHandle};
    }
}

macro_rules! cstr {
    ($s:expr) => {
        CString::new($s)?.as_ptr() as *const i8
    };
}

foreign_type! {
    /// A reference to an open stream on the filesystem.
    ///
    /// An instance of a `CFile` can be read and/or written depending on what modes it was opened with.
    /// `CFile` also implement `Seek` to alter the logical cursor that the file contains internally.
    ///
    pub unsafe type CFile: Send {
        type CType = libc::FILE;

        fn drop = libc::fclose;
    }
}

cfg_if! {
    if #[cfg(unix)] {
        impl AsRawFd for CFileRef {
            fn as_raw_fd(&self) -> RawFd {
                unsafe { libc::fileno(self.as_ptr()) }
            }
        }

        impl IntoRawFd for CFile {
            fn into_raw_fd(self) -> RawFd {
                let fd = self.as_raw_fd();

                mem::forget(self);

                fd
            }
        }
    } else {
        impl AsRawHandle for CFileRef {
            fn as_raw_handle(&self) -> RawHandle {
                unsafe { libc::get_osfhandle(libc::fileno(self.as_ptr())) as RawHandle }
            }
        }

        impl IntoRawHandle for CFile {
            fn into_raw_handle(self) -> RawHandle {
                let handle = self.as_raw_handle();

                mem::forget(self);

                handle
            }
        }
    }
}

impl CFileRef {
    /// opens the file whose name is the string pointed to by `filename`
    /// and associates the stream pointed to by stream with it.
    ///
    /// The original stream (if it exists) is closed.
    /// The mode argument is used just as in the `open()` function.
    ///
    pub fn reopen(&self, filename: &str, mode: &str) -> io::Result<CFile> {
        unsafe {
            let p = libc::freopen(cstr!(filename), cstr!(mode), self.as_ptr());

            if p.is_null() {
                Err(io::Error::last_os_error())
            } else {
                Ok(CFile::from_ptr(p))
            }
        }
    }
}

cfg_if! {
    if #[cfg(unix)] {
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
            unsafe {
                let p = libc::fopen(
                    cstr!(path.as_ref().as_os_str().as_bytes()),
                    cstr!(mode),
                );

                if p.is_null() {
                    Err(io::Error::last_os_error())
                } else {
                    Ok(CFile::from_ptr(p))
                }
            }
        }
    } else {
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
            let filename = path.as_ref().to_string_lossy().to_string();

            unsafe {
                let p = libc::fopen(
                    cstr!(filename),
                    cstr!(mode),
                );

                if p.is_null() {
                    Err(io::Error::last_os_error())
                } else {
                    Ok(CFile::from_ptr(p))
                }
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
        /// let stdin = cfile::stdin().unwrap();
        ///
        /// assert_eq!(stdin.as_raw_fd(), libc::STDIN_FILENO);
        /// ```
        pub fn stdin() -> io::Result<&'static CFileRef> {
            CFileRef::fdopen(libc::STDIN_FILENO, "r")
        }

        /// open stdout as a write only stream
        pub fn stdout() -> io::Result<&'static CFileRef> {
            CFileRef::fdopen(libc::STDOUT_FILENO, "w")
        }

        /// open stderr as a write only stream
        pub fn stderr() -> io::Result<&'static CFileRef> {
            CFileRef::fdopen(libc::STDERR_FILENO, "w")
        }
    } else {
        use winapi::um::processenv::GetStdHandle;
        use winapi::um::handleapi::INVALID_HANDLE_VALUE;
        use winapi::um::winbase::{STD_INPUT_HANDLE, STD_OUTPUT_HANDLE, STD_ERROR_HANDLE};

        /// open stdin as a read only stream
        pub fn stdin() -> io::Result<&'static CFileRef> {
            let h = unsafe { GetStdHandle(STD_INPUT_HANDLE) };

            if h == INVALID_HANDLE_VALUE {
                Err(io::Error::last_os_error())
            } else {
                CFileRef::fdopen(h as *mut _, "r")
            }
        }

        /// open stdout as a write only stream
        pub fn stdout() -> io::Result<&'static CFileRef> {
            let h = unsafe { GetStdHandle(STD_OUTPUT_HANDLE) };

            if h == INVALID_HANDLE_VALUE {
                Err(io::Error::last_os_error())
            } else {
                CFileRef::fdopen(h as *mut _, "w")
            }
        }

        /// open stderr as a write only stream
        pub fn stderr() -> io::Result<&'static CFileRef> {
            let h = unsafe { GetStdHandle(STD_ERROR_HANDLE) };

            if h == INVALID_HANDLE_VALUE {
                Err(io::Error::last_os_error())
            } else {
                CFileRef::fdopen(h as *mut _, "w")
            }
        }
    }
}

/// open a temporary file as a read/write stream
pub fn tmpfile() -> io::Result<CFile> {
    unsafe {
        let p = libc::tmpfile();

        if p.is_null() {
            Err(io::Error::last_os_error())
        } else {
            Ok(CFile::from_ptr(p))
        }
    }
}

cfg_if! {
    if #[cfg(unix)] {
        /// associates a stream with the existing file descriptor.
        ///
        /// The mode of the stream must be compatible with the mode of the file descriptor.
        ///
        pub fn fdopen<S: AsRawFd>(s: &S, mode: &str) -> io::Result<CFile> {
            CFile::fdopen(s.as_raw_fd(), mode)
        }
    } else {
        /// associates a stream with the existing file descriptor.
        ///
        /// The mode of the stream must be compatible with the mode of the file descriptor.
        ///
        pub fn fdopen<S: AsRawHandle>(s: &S, mode: &str) -> io::Result<CFile> {
            CFile::fdopen(s.as_raw_handle(), mode)
        }
    }
}

cfg_if! {
    if #[cfg(unix)] {
        impl CFileRef {
            pub fn fdopen<S: AsRef<str>>(fd: RawFd, mode: S) -> io::Result<&'static CFileRef> {
                unsafe {
                    let p = libc::fdopen(fd, cstr!(mode.as_ref()));

                    if p.is_null() {
                        Err(io::Error::last_os_error())
                    } else {
                        Ok(Self::from_ptr(p))
                    }
                }
            }
        }

        impl CFile {
            pub fn fdopen<S: AsRef<str>>(fd: RawFd, mode: S) -> io::Result<CFile> {
                CFileRef::fdopen(fd, mode).map(|f| unsafe { Self::from_ptr(f.as_ptr()) })
            }
        }
    } else {
        impl CFileRef {
            pub fn fdopen<S: AsRef<str>>(handle: RawHandle, mode: S) -> io::Result<&'static CFileRef> {
                unsafe {
                    let mode = mode.as_ref();
                    let flags = if mode == "r" { libc::O_RDONLY } else {0};
                    let fd = libc::open_osfhandle(handle as isize, flags);

                    if fd == -1 {
                        Err(io::Error::last_os_error())
                    } else {
                        let p = libc::fdopen(fd, cstr!(mode));

                        if p.is_null() {
                            Err(io::Error::last_os_error())
                        } else {
                            Ok(Self::from_ptr(p))
                        }
                    }
                }
            }
        }

        impl CFile {
            pub fn fdopen<S: AsRef<str>>(handle: RawHandle, mode: S) -> io::Result<CFile> {
                CFileRef::fdopen(handle, mode).map(|f| unsafe { Self::from_ptr(f.as_ptr()) })
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::io::{prelude::*, SeekFrom};

    use super::*;
    use crate::stream::Stream;

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
