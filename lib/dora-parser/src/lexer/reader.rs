use std::fs::File;
use std::io::{self, Read, Error};

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
    pub fn from_input() -> Result<Reader, Error> {
        let mut src = String::new();
        io::stdin().read_to_string(&mut src)?;

        Ok(common_init("<<stdin>>".into(), src))
    }

    pub fn from_file(filename: &str) -> Result<Reader, Error> {
        let mut src = String::new();

        let mut file = File::open(filename)?;
        file.read_to_string(&mut src)?;

        Ok(common_init(filename.into(), src))
    }

    pub fn from_string(src: &str) -> Reader {
        common_init("<<code>>".into(), src.into())
    }

    pub fn set_tabwidth(&mut self, width: usize) {
        self.tabwidth = width;
    }

    pub fn path(&self) -> &str {
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

            None => panic!("advancing from eof"),
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

fn common_init(name: String, src: String) -> Reader {
    let mut reader = Reader {
        filename: name,
        src: src,
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
