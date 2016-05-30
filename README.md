# rust-cfile

Rust bindings to C FILE stream

[![crates.io](http://meritbadge.herokuapp.com/cfile)](https://crates.io/crates/cfile)
[![travis](https://travis-ci.org/flier/rust-cfile.svg?branch=master)](https://travis-ci.org/flier/rust-cfile)

[Document](http://flier.github.io/rust-cfile/doc/cfile)

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
