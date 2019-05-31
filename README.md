# rust-cfile [![crates.io](http://meritbadge.herokuapp.com/cfile)](https://crates.io/crates/cfile) [![docs](https://docs.rs/fasthash/badge.svg)](https://docs.rs/crate/cfile/) [![travis](https://travis-ci.org/flier/rust-cfile.svg?branch=master)](https://travis-ci.org/flier/rust-cfile) [![Build status](https://ci.appveyor.com/api/projects/status/0lsnxvj7bqeorslw?svg=true)](https://ci.appveyor.com/project/flier/rust-cfile) [![dependency status](https://deps.rs/repo/github/flier/rust-cfile/status.svg)](https://deps.rs/repo/github/flier/rust-cfile)


Rust bindings to C *FILE stream

## Examples

```rust
use std::io::prelude::*;
use std::io::{BufReader, SeekFrom};

use cfile;

// open a tempfile
let mut f = cfile::tmpfile().unwrap();

// write something to the stream
assert_eq!(f.write(b"test").unwrap(), 4);

// force to flush the stream
f.flush().unwrap();

// seek to the beginning of stream
assert_eq!(f.seek(SeekFrom::Start(0)).unwrap(), 0);

let mut r = BufReader::new(f);
let mut s = String::new();

// read back the text
assert_eq!(r.read_line(&mut s).unwrap(), 4);
assert_eq!(s, "test");
```
