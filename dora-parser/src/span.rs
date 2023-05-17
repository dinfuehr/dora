use std::fmt::{Display, Error, Formatter};
use std::result::Result;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Span {
    start: u32,
    len: u32,
}

impl Span {
    pub fn new(start: u32, len: u32) -> Span {
        Span { start, len }
    }

    pub fn at(start: u32) -> Span {
        Span {
            start: start,
            len: 0,
        }
    }

    pub fn start(&self) -> u32 {
        self.start
    }

    pub fn len(&self) -> u32 {
        self.len
    }

    pub fn end(&self) -> u32 {
        self.start + self.len
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}-{}", self.start, self.end())
    }
}
