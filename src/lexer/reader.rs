use std::str::Chars;
use std::io::{BufferedReader, File, IoError};

pub trait CodeReader {
    fn read_char(&mut self) -> Option<char>;
    fn filename(&self) -> &str;
}

pub struct StrReader {
  program: &'static str,
  rest: Chars<'static>
}

impl StrReader {
  pub fn new(program: &'static str) -> StrReader {
    StrReader { program: program, rest: program.chars() }
  }
}

impl CodeReader for StrReader {
  fn read_char(&mut self) -> Option<char> {
    self.rest.next()
  }

  fn filename(&self) -> &str {
    "<code>"
  }
}

pub struct FileReader {
    filename: &'static str,
    reader: BufferedReader<Result<File,IoError>>
}

impl FileReader {
  pub fn new(filename: &'static str) -> FileReader {
    let file = File::open(&Path::new(filename));
    let reader = BufferedReader::new(file);

    FileReader { filename: filename, reader: reader }
  }
}

impl CodeReader for FileReader {
  fn read_char(&mut self) -> Option<char> {
      match self.reader.read_char() {
        Ok(ch) => Some(ch),
        _ => None
      }
  }

  fn filename(&self) -> &str {
    self.filename
  }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_from_str() {
        let mut reader = StrReader::new("abc");

        assert_eq!(Some('a'), reader.read_char());
        assert_eq!(Some('b'), reader.read_char());
        assert_eq!(Some('c'), reader.read_char());
        assert_eq!(None, reader.read_char());
    }

    #[test]
    fn read_from_file() {
        let mut reader = FileReader::new("tests/abc.txt");

        assert_eq!(Some('a'), reader.read_char());
        assert_eq!(Some('b'), reader.read_char());
        assert_eq!(Some('c'), reader.read_char());
        assert_eq!(None, reader.read_char());
    }
}
