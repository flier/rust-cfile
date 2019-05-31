use std::convert::AsRef;
use std::fs::Metadata;
use std::io;
use std::mem;
use std::path::PathBuf;
use std::str;

cfg_if! {
    if #[cfg(unix)] {
        use std::os::unix::io::{AsRawFd, IntoRawFd};
    } else {
        use std::ffi::OsString;
        use std::ptr::NonNull;
        use std::os::windows::ffi::OsStringExt;
        use std::os::windows::io::{AsRawHandle, IntoRawHandle};
    }
}

use foreign_types::ForeignTypeRef;

use crate::{CFile, CFileRef};

/// The C *FILE stream
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
            fn to_stream<S: AsRef<str>>(&self, mode: S) -> io::Result<&CFileRef> {
                CFileRef::fdopen(self.as_raw_fd(), mode)
            }
        }

        impl<S: AsRawFd + Sized> ToStream for S {}

        /// A trait to express the ability to consume an object and acquire ownership of its stream.
        pub trait IntoStream: IntoRawFd + Sized {
            /// Consumes this raw fd, returning the raw underlying C *FILE stream.
            fn into_stream<S: AsRef<str>>(self, mode: S) -> io::Result<CFile> {
                CFile::fdopen(self.into_raw_fd(), mode)
            }
        }

        impl<S: IntoRawFd + Sized> IntoStream for S {}
    } else {
        /// A trait for converting a raw fd to a C *FILE stream.
        pub trait ToStream: AsRawHandle + Sized {
            /// Open a raw fd as C *FILE stream
            fn to_stream<S: AsRef<str>>(&self, mode: S) -> io::Result<&CFileRef> {
                CFileRef::fdopen(self.as_raw_handle(), mode)
            }
        }

        impl<S: AsRawHandle + Sized> ToStream for S {}

        /// A trait to express the ability to consume an object and acquire ownership of its stream.
        pub trait IntoStream: IntoRawHandle + Sized {
            /// Consumes this raw fd, returning the raw underlying C *FILE stream.
            fn into_stream<S: AsRef<str>>(self, mode: S) -> io::Result<CFile> {
                CFile::fdopen(self.into_raw_handle(), mode)
            }
        }

        impl<S: IntoRawHandle + Sized> IntoStream for S {}
    }
}

impl io::Read for CFile {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.as_mut().read_slice(buf)
    }
}

impl io::Write for CFile {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.as_mut().write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.as_mut().flush()
    }
}

impl io::Seek for CFile {
    fn seek(&mut self, pos: io::SeekFrom) -> io::Result<u64> {
        self.as_mut().seek(pos)
    }
}

impl io::Read for CFileRef {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.read_slice(buf)
    }
}

impl io::Write for CFileRef {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.write_slice(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        if unsafe { libc::fflush(self.as_ptr()) } != 0 {
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

impl io::Seek for CFileRef {
    fn seek(&mut self, pos: io::SeekFrom) -> io::Result<u64> {
        let ret = unsafe {
            match pos {
                io::SeekFrom::Start(off) => fseek64(self.as_ptr(), off as i64, libc::SEEK_SET),
                io::SeekFrom::End(off) => fseek64(self.as_ptr(), off, libc::SEEK_END),
                io::SeekFrom::Current(off) => fseek64(self.as_ptr(), off, libc::SEEK_CUR),
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

impl Stream for CFileRef {
    fn position(&self) -> io::Result<u64> {
        let off = unsafe { libc::ftell(self.as_ptr()) };

        if off < 0 {
            if let Some(err) = self.last_error() {
                return Err(err);
            }
        }

        Ok(off as u64)
    }

    #[inline]
    fn eof(&self) -> bool {
        unsafe { libc::feof(self.as_ptr()) != 0 }
    }

    #[inline]
    fn errno(&self) -> i32 {
        unsafe { libc::ferror(self.as_ptr()) }
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
        unsafe { clearerr(self.as_ptr()) }
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
                self.as_ptr(),
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
                self.as_ptr(),
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

extern "C" {
    fn clearerr(file: *mut libc::FILE);
}
