//! Rust bindings to C *FILE stream
//!
//! # Examples
//!
//! ```
//! use std::io::prelude::*;
//! use std::io::{BufReader, SeekFrom};
//!
//! use cfile;
//!
//! // open a tempfile
//! let mut f = cfile::tmpfile().unwrap();
//!
//! // write something to the stream
//! assert_eq!(f.write(b"test").unwrap(), 4);
//!
//! // force to flush the stream
//! f.flush().unwrap();
//!
//! // seek to the beginning of stream
//! assert_eq!(f.seek(SeekFrom::Start(0)).unwrap(), 0);
//!
//! let mut r = BufReader::new(f);
//! let mut s = String::new();
//!
//! // read back the text
//! assert_eq!(r.read_line(&mut s).unwrap(), 4);
//! assert_eq!(s, "test");
//! ```
#[macro_use]
extern crate cfg_if;

#[doc(hidden)]
pub extern crate foreign_types;

mod cfile;
mod iter;
mod stream;

cfg_if! {
    if #[cfg(any(target_os = "linux", feature = "doc"))] {
        mod lock;
        pub mod unlocked;

        pub use crate::lock::FileLock;
    }
}

pub use crate::cfile::{fdopen, open, stderr, stdin, stdout, tmpfile, CFile, CFileRef};
pub use crate::iter::{Bytes, Lines};
pub use crate::stream::{AsStream, IntoStream, Stream};
