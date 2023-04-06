use std::sync::Arc;

pub struct Reader {
    content: Arc<String>,
    offset: usize,
    tabwidth: u32,
}

impl Reader {
    pub fn from_string(src: &str) -> Reader {
        Reader::from_shared_string(Arc::new(src.to_string()))
    }

    pub fn from_shared_string(content: Arc<String>) -> Reader {
        let reader = Reader {
            content,

            offset: 0,
            tabwidth: 4,
        };

        reader
    }

    pub fn set_tabwidth(&mut self, tabwidth: u32) {
        self.tabwidth = tabwidth;
    }

    pub fn advance(&mut self) -> Option<char> {
        let curr = self.curr();

        if let Some(ch) = curr {
            self.offset += ch.len_utf8();
        }

        self.curr()
    }

    pub fn curr(&self) -> Option<char> {
        self.nth(0)
    }

    pub fn nth(&self, offset: usize) -> Option<char> {
        let pos = self.offset + offset;

        if pos < self.content.len() {
            self.content[pos..].chars().next()
        } else {
            None
        }
    }

    pub fn offset(&self) -> u32 {
        self.offset as u32
    }

    pub fn content(&self) -> Arc<String> {
        self.content.clone()
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
