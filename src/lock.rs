use std::ops::{Deref, DerefMut};

use crate::cfile::CFile;

/// A locked reference to the `CFile` stream.
pub struct FileLock<'a>(&'a mut CFile);

impl<'a> Drop for FileLock<'a> {
    fn drop(&mut self) {
        unsafe { funlockfile(self.0.stream()) }
    }
}

impl<'a> Deref for FileLock<'a> {
    type Target = CFile;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'a> DerefMut for FileLock<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}

/// Extension methods for `CFile` lock.
pub trait FileLockExt {
    /// acquires an exclusive lock on the specified object.
    ///
    /// If another thread has already locked the object,
    /// will block until the lock is released.
    ///
    /// # Examples
    /// ```
    /// use std::io::Write;
    /// use cfile::{tmpfile, FileLockExt};
    ///
    /// let mut f = tmpfile().unwrap();
    /// let mut l = f.lock();
    ///
    /// assert_eq!(l.write(b"test").unwrap(), 4);
    /// ```
    fn lock(&mut self) -> FileLock;

    /// a non-blocking version of `lock()`;
    ///
    /// if the lock cannot be acquired immediately,
    /// `try_lock()` returns `None` instead of blocking.
    ///
    /// # Examples
    /// ```
    /// use std::io::{Read, Write, BufRead, BufReader, Seek, SeekFrom};
    /// use cfile::{tmpfile, FileLockExt};
    ///
    /// let mut f = tmpfile().unwrap();
    ///
    /// if let Some(mut c) = f.try_lock() {
    ///     assert_eq!(c.write(b"test").unwrap(), 4);
    /// }
    ///
    /// assert_eq!(f.seek(SeekFrom::Start(0)).unwrap(), 0); // seek to the beginning of stream
    ///
    /// let mut r = BufReader::new(f);
    /// let mut s = String::new();
    /// assert_eq!(r.read_line(&mut s).unwrap(), 4); // read back the text
    /// assert_eq!(s, "test");
    /// ```
    fn try_lock(&mut self) -> Option<FileLock>;
}

extern "C" {
    fn flockfile(file: *mut libc::FILE);

    fn ftrylockfile(file: *mut libc::FILE) -> i32;

    fn funlockfile(file: *mut libc::FILE);
}

impl<'a> FileLockExt for CFile {
    fn lock(&mut self) -> FileLock {
        unsafe { flockfile(self.stream()) }

        FileLock(self)
    }

    fn try_lock(&mut self) -> Option<FileLock> {
        if unsafe { ftrylockfile(self.stream()) } == 0 {
            Some(FileLock(self))
        } else {
            None
        }
    }
}
