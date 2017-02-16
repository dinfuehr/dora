use std::fs::File;
use std::io::{Read, Error};

pub struct FileReader {
    filename: String,
    src: String,
    pos: usize,
}

impl FileReader {
    pub fn from_file(filename: &str) -> Result<FileReader, Error> {
        let mut src = String::new();

        let mut file = File::open(filename)?;
        file.read_to_string(&mut src)?;

        Ok(FileReader {
            filename: filename.to_string(),
            src: src,
            pos: 0,
        })
    }

    pub fn from_string(src: &str) -> FileReader {
        FileReader {
            filename: "<<code>>".into(),
            src: src.into(),
            pos: 0,
        }
    }

    pub fn filename(&self) -> &str {
        &self.filename
    }

    pub fn next(&mut self) -> Option<char> {
        if self.pos < self.src.len() {
            let ch = self.src[self.pos..].chars().next().unwrap();
            self.pos += ch.len_utf8();

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
        let mut reader = FileReader::from_string("abc");

        assert_eq!(Some('a'), reader.next());
        assert_eq!(Some('b'), reader.next());
        assert_eq!(Some('c'), reader.next());
        assert_eq!(None, reader.next());
    }
}
