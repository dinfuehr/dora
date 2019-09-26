use std::fs;
use std::io::{self, Error, Read};

use crate::lexer::position::Position;
use crate::lexer::File;

pub struct Reader {
    name: String,
    content: String,
    line_ends: Vec<u32>,

    idx: usize,
    pos: Position,

    tabwidth: u32,
}

impl Reader {
    pub fn from_input() -> Result<Reader, Error> {
        let mut src = String::new();
        io::stdin().read_to_string(&mut src)?;

        Ok(common_init("<<stdin>>".into(), src))
    }

    pub fn from_file(filename: &str) -> Result<Reader, Error> {
        let mut src = String::new();

        let mut file = fs::File::open(filename)?;
        file.read_to_string(&mut src)?;

        Ok(common_init(filename.into(), src))
    }

    pub fn from_string(src: &str) -> Reader {
        common_init("<<code>>".into(), src.into())
    }

    pub fn set_tabwidth(&mut self, tabwidth: u32) {
        self.tabwidth = tabwidth;
    }

    pub fn path(&self) -> &str {
        &self.name
    }

    pub fn advance(&mut self) -> Option<char> {
        let curr = self.curr();

        match curr {
            Some('\n') => {
                self.pos = Position::new(self.pos.line + 1, 1);
                self.line_ends.push(self.idx as u32);
            }

            Some('\t') => {
                let tabdepth = (self.pos.column - 1) / self.tabwidth;
                let col = 1 + self.tabwidth * (tabdepth + 1);
                self.pos = Position::new(self.pos.line, col);
            }

            Some(_) => {
                self.pos = Position::new(self.pos.line, self.pos.column + 1);
            }

            None => panic!("advancing from eof"),
        }

        if let Some(ch) = curr {
            self.idx += ch.len_utf8();
        }

        self.curr()
    }

    pub fn file(self) -> File {
        File {
            name: self.name,
            content: self.content,
            line_ends: self.line_ends,
        }
    }

    pub fn curr(&self) -> Option<char> {
        self.nth(0)
    }

    pub fn nth(&self, offset: usize) -> Option<char> {
        let pos = self.idx + offset;

        if pos < self.content.len() {
            self.content[pos..].chars().next()
        } else {
            None
        }
    }

    pub fn pos(&self) -> Position {
        self.pos
    }

    pub fn idx(&self) -> u32 {
        self.idx as u32
    }
}

fn common_init(name: String, content: String) -> Reader {
    let reader = Reader {
        name: name,
        content: content,
        line_ends: Vec::new(),

        idx: 0,
        pos: Position::new(1, 1),
        tabwidth: 4,
    };

    reader
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_from_str() {
        let mut reader = Reader::from_string("abc");

        assert_eq!(Some('a'), reader.curr());
        assert_eq!(Some('b'), reader.nth(1));
        reader.advance();

        assert_eq!(Some('b'), reader.curr());
        assert_eq!(Some('c'), reader.nth(1));
        reader.advance();

        assert_eq!(Some('c'), reader.curr());
        assert_eq!(None, reader.nth(1));
        reader.advance();

        assert_eq!(None, reader.curr());
        assert_eq!(None, reader.nth(1));
    }
}
