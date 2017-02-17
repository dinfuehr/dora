use std::fs::File;
use std::io::{Read, Error};

use lexer::position::Position;

pub struct Reader {
    filename: String,
    src: String,

    pos: usize,
    next_pos: usize,

    cur: Option<char>,
    line: usize,
    col: usize,
    tabwidth: usize,
}

impl Reader {
    pub fn from_file(filename: &str) -> Result<Reader, Error> {
        let mut src = String::new();

        let mut file = File::open(filename)?;
        file.read_to_string(&mut src)?;

        let mut reader = Reader {
            filename: filename.to_string(),
            src: src,
            pos: 0,
            next_pos: 0,

            cur: Some('\n'),
            line: 0,
            col: 0,
            tabwidth: 4,
        };

        reader.advance();

        Ok(reader)
    }

    pub fn from_string(src: &str) -> Reader {
        let mut reader = Reader {
            filename: "<<code>>".into(),
            src: src.into(),
            pos: 0,
            next_pos: 0,

            cur: Some('\n'),
            line: 0,
            col: 0,
            tabwidth: 4,
        };

        reader.advance();

        reader
    }

    pub fn set_tabwidth(&mut self, width: usize) {
        self.tabwidth = width;
    }

    pub fn filename(&self) -> &str {
        &self.filename
    }

    pub fn advance(&mut self) -> Option<char> {
        match self.cur {
            Some('\n') => {
                self.line += 1;
                self.col = 1;
            }

            Some('\t') => {
                let tabdepth = (self.col - 1) / self.tabwidth;
                self.col = 1 + self.tabwidth * (tabdepth + 1);
            }

            Some(_) => {
                self.col += 1;
            }

            None => panic!("advancing from eof")
        }

        self.cur = if self.next_pos < self.src.len() {
            let ch = self.src[self.next_pos..].chars().next().unwrap();
            self.pos = self.next_pos;
            self.next_pos += ch.len_utf8();

            Some(ch)

        } else {
            None
        };

        self.cur
    }

    pub fn cur(&self) -> Option<char> {
        self.cur
    }

    pub fn pos(&self) -> Position {
        Position {
            line: self.line as u32,
            column: self.col as u32,
        }
    }

    pub fn next(&self) -> Option<char> {
        if self.next_pos < self.src.len() {
            let ch = self.src[self.next_pos..].chars().next().unwrap();
            Some(ch)

        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_from_str() {
        let mut reader = Reader::from_string("abc");

        assert_eq!(Some('a'), reader.cur());
        assert_eq!(Some('b'), reader.next());
        reader.advance();

        assert_eq!(Some('b'), reader.cur());
        assert_eq!(Some('c'), reader.next());
        reader.advance();

        assert_eq!(Some('c'), reader.cur());
        assert_eq!(None, reader.next());
        reader.advance();

        assert_eq!(None, reader.cur());
        assert_eq!(None, reader.next());
    }
}
