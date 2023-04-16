use std::fmt::{Display, Error, Formatter};
use std::result::Result;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Span {
    start: u32,
    count: u32,
}

impl Span {
    pub fn new(start: u32, count: u32) -> Span {
        Span { start, count }
    }

    pub fn at(start: u32) -> Span {
        Span {
            start: start,
            count: 0,
        }
    }

    pub fn invalid() -> Span {
        Span {
            start: u32::max_value(),
            count: 0,
        }
    }

    pub fn is_valid(&self) -> bool {
        self.start != u32::max_value()
    }

    pub fn start(&self) -> u32 {
        self.start
    }

    pub fn count(&self) -> u32 {
        self.count
    }

    pub fn end(&self) -> u32 {
        self.start + self.count
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}-{}", self.start, self.end())
    }
}
