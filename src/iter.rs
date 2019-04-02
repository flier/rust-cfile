use std::io;

use crate::cfile::CFile;

/// An iterator over the bytes of a *FILE stream.
pub struct Bytes<'a>(&'a CFile);

impl<'a> Iterator for Bytes<'a> {
    type Item = io::Result<u8>;

    fn next(&mut self) -> Option<Self::Item> {
        let b = unsafe { libc::fgetc(self.0.stream()) };

        if b == libc::EOF {
            let err = io::Error::last_os_error();

            if err.raw_os_error() == Some(0) {
                None
            } else {
                Some(Err(err))
            }
        } else {
            Some(Ok(b as u8))
        }
    }
}

impl<'a> IntoIterator for &'a CFile {
    type Item = io::Result<u8>;
    type IntoIter = Bytes<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.bytes()
    }
}

/// An iterator over the lines of a *FILE stream.
pub struct Lines<T>(pub(crate) T);

impl<T> Iterator for Lines<T>
where
    T: Iterator<Item = io::Result<u8>>,
{
    type Item = io::Result<String>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut v = vec![];

        loop {
            match self.0.next() {
                Some(Ok(b'\r')) => {}
                Some(Ok(b'\n')) => break,
                Some(Ok(b)) => v.push(b),
                Some(Err(err)) => return Some(Err(err)),
                None if v.is_empty() => return None,
                None => break,
            }
        }

        Some(String::from_utf8(v).map_err(|err| io::Error::new(io::ErrorKind::Other, err)))
    }
}

impl CFile {
    /// An iterator over the bytes of a *FILE stream.
    pub fn bytes(&self) -> Bytes {
        Bytes(self)
    }

    /// An iterator over the lines of a *FILE stream.
    pub fn lines(&self) -> Lines<Bytes> {
        Lines(Bytes(self))
    }
}

#[cfg(test)]
mod tests {
    use std::io::{prelude::*, Result, SeekFrom};

    use crate::tmpfile;

    #[test]
    fn test_iter() {
        let mut f = tmpfile().unwrap();

        assert_eq!(f.write(b"hello\r\nworld").unwrap(), 12);

        assert_eq!(f.seek(SeekFrom::Start(0)).unwrap(), 0);

        assert_eq!(
            f.into_iter().collect::<Result<Vec<u8>>>().unwrap(),
            b"hello\r\nworld"
        );

        assert_eq!(f.seek(SeekFrom::Start(0)).unwrap(), 0);

        assert_eq!(
            f.lines().collect::<Result<Vec<String>>>().unwrap(),
            vec!["hello", "world"]
        );
    }
}
