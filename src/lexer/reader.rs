use std::fs::File;
use std::io::Chars;
use std::io::{Read, BufReader, Error, CharsError};

pub trait CodeReader {
    fn next(&mut self) -> Option<Result<char, CharsError>>;
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
    fn next(&mut self) -> Option<Result<char, CharsError>> {
        match self.rest.next() {
            Some(ch) => Some(Ok(ch)),
            None => None,
        }
    }
}

pub struct FileReader {
    rest: Chars<BufReader<File>>,
}

impl FileReader {
    pub fn new(filename: &str) -> Result<FileReader, Error> {
        let file = try!(File::open(filename));
        let reader = BufReader::new(file);

        Ok(FileReader { rest: reader.chars() })
    }
}

impl CodeReader for FileReader {
    fn next(&mut self) -> Option<Result<char, CharsError>> {
        self.rest.next()
    }
}

#[cfg(test)]
mod tests {
    use lexer::reader::{CodeReader, StrReader, FileReader};

    #[test]
    fn read_from_str() {
        let mut reader = StrReader::new("abc");

        assert_eq!('a', reader.next().unwrap().ok().unwrap());
        assert_eq!('b', reader.next().unwrap().ok().unwrap());
        assert_eq!('c', reader.next().unwrap().ok().unwrap());
        assert_eq!(true, reader.next().is_none());
    }
}
