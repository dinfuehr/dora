use std::fs::File;
use std::io::Bytes;
use std::io::{Read, BufReader, Error};

pub trait CodeReader {
    fn next(&mut self) -> ReaderResult;
}

#[derive(PartialEq, Eq, Debug)]
pub enum ReaderResult {
    Char(char),
    Eof,
    Err
}

#[cfg(test)]
pub struct StrReader {
    rest: ::std::str::Chars<'static>,
}

#[cfg(test)]
impl StrReader {
    pub fn new(program: &'static str) -> StrReader {
        StrReader { rest: program.chars() }
    }
}

#[cfg(test)]
impl CodeReader for StrReader {
    fn next(&mut self) -> ReaderResult {
        match self.rest.next() {
            Some(ch) => ReaderResult::Char(ch),
            None => ReaderResult::Eof,
        }
    }
}

pub struct FileReader {
    rest: Bytes<BufReader<File>>,
}

impl FileReader {
    pub fn new(filename: &str) -> Result<FileReader, Error> {
        let file = try!(File::open(filename));
        let reader = BufReader::new(file);

        Ok(FileReader { rest: reader.bytes() })
    }
}

impl CodeReader for FileReader {
    fn next(&mut self) -> ReaderResult {
        match self.rest.next() {
            Some(Ok(ch)) => ReaderResult::Char(ch as char),
            Some(Err(_)) => ReaderResult::Err,
            None => ReaderResult::Eof,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_from_str() {
        let mut reader = StrReader::new("abc");

        assert_eq!(ReaderResult::Char('a'), reader.next());
        assert_eq!(ReaderResult::Char('b'), reader.next());
        assert_eq!(ReaderResult::Char('c'), reader.next());
        assert_eq!(ReaderResult::Eof, reader.next());
    }
}
