use std::sync::Arc;

use crate::lexer::position::Position;

pub struct Reader {
    content: Arc<String>,

    idx: usize,
    pos: Position,

    tabwidth: u32,
}

impl Reader {
    pub fn from_string(src: &str) -> Reader {
        Reader::from_shared_string(Arc::new(src.to_string()))
    }

    pub fn from_shared_string(content: Arc<String>) -> Reader {
        let reader = Reader {
            content,

            idx: 0,
            pos: Position::new(1, 1),
            tabwidth: 4,
        };

        reader
    }

    pub fn set_tabwidth(&mut self, tabwidth: u32) {
        self.tabwidth = tabwidth;
    }

    pub fn advance(&mut self) -> Option<char> {
        let curr = self.curr();

        match curr {
            Some('\n') => {
                self.pos = Position::new(self.pos.line + 1, 1);
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

    pub fn set(&mut self, idx: u32, pos: Position) {
        self.idx = idx as usize;
        self.pos = pos;
    }
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
