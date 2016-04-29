# rust-cfile [![travis](https://travis-ci.org/flier/rust-cfile.svg?branch=master)](https://travis-ci.org/flier/rust-cfile)

Rust bindings to C FILE stream

[Document](http://flier.github.io/rust-cfile/doc/cfile)

## Examples

```rust
use std::io::prelude::*;
use std::io::{BufReader, SeekFrom};
use cfile::CFile;

let mut f = CFile::open_tmpfile().unwrap(); // open a tempfile

assert_eq!(f.write(b"test").unwrap(), 4); // write something to the stream
f.flush().unwrap(); // flush stream
assert_eq!(f.seek(SeekFrom::Start(0)).unwrap(), 0); // seek to the beginning of stream

let mut r = BufReader::new(f);
let mut s = String::new();
assert_eq!(r.read_line(&mut s).unwrap(), 4); // read back the text
assert_eq!(s, "test");
```
