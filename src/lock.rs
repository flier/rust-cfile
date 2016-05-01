use std::sync::Arc;
use std::cell::RefCell;

use libc;

use cfile::{Stream, CFileRaw, CFile};

/// A locked reference to the CFile stream.
pub struct FileLock(Arc<RefCell<CFileRaw>>);

impl Drop for FileLock {
    fn drop(&mut self) {
        unsafe { funlockfile(**self.0.borrow()) }
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
    /// use cfile::{CFile, FileLockExt};
    ///
    /// let mut f = CFile::open_tmpfile().unwrap();
    /// let l = f.lock();
    ///
    /// assert_eq!(f.write(b"test").unwrap(), 4);
    /// ```
    fn lock(&self) -> FileLock;

    /// a non-blocking version of `lock()`;
    ///
    /// if the lock cannot be acquired immediately,
    /// `try_lock()` returns `None` instead of blocking.
    ///
    /// # Examples
    /// ```
    /// use std::io::Write;
    /// use cfile::{CFile, FileLockExt};
    ///
    /// let mut f = CFile::open_tmpfile().unwrap();
    ///
    /// if let Some(l) = f.try_lock() {
    ///     assert_eq!(f.write(b"test").unwrap(), 4);
    /// }
    /// ```
    fn try_lock(&self) -> Option<FileLock>;

    /// releases the lock on an object acquired
    /// by an earlier call to lock() or try_lock().
    ///
    fn unlock(&self);
}

extern "C" {
    fn flockfile(file: *mut libc::FILE);

    fn ftrylockfile(file: *mut libc::FILE) -> i32;

    fn funlockfile(file: *mut libc::FILE);
}

impl<'a> FileLockExt for CFile {
    fn lock(&self) -> FileLock {
        unsafe { flockfile(self.stream()) }

        FileLock((*self).clone())
    }

    fn try_lock(&self) -> Option<FileLock> {
        if unsafe { ftrylockfile(self.stream()) } == 0 {
            Some(FileLock((*self).clone()))
        } else {
            None
        }
    }

    fn unlock(&self) {
        unsafe { funlockfile(self.stream()) }
    }
}
