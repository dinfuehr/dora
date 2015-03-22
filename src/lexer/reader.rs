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
    pub fn new(filename: String) -> Result<FileReader, Error> {
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

        assert_eq!(Some(Ok('a')), reader.next());
        assert_eq!(Some(Ok('b')), reader.next());
        assert_eq!(Some(Ok('c')), reader.next());
        assert_eq!(None, reader.next());
    }

    #[test]
    fn read_from_file() {
        let mut reader = FileReader::new("tests/abc.txt".to_string()).unwrap();

        assert_eq!(Some(Ok('a')), reader.next());
        assert_eq!(Some(Ok('b')), reader.next());
        assert_eq!(Some(Ok('c')), reader.next());
        assert_eq!(None, reader.next());
    }

    #[test]
    fn read_from_non_existing_file() {
        assert!(FileReader::new("tests/non-existing.txt".to_string()).is_err());
    }
}
