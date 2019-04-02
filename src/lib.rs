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

extern crate libc;

pub use libc::{STDERR_FILENO, STDIN_FILENO, STDOUT_FILENO};

mod cfile;
mod lock;

pub use cfile::{open, stderr, stdin, stdout, tmpfile, CFile, Stream, ToStream};
pub use lock::{FileLock, FileLockExt};
