use std::str::Chars;

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
  filename: &'static str
}

impl FileReader {
  pub fn new(filename: &'static str) -> FileReader {
    FileReader { filename: filename }
  }
}
