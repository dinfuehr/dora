use std::fmt::{Display, Error, Formatter};
use std::result::Result;

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub struct Position {
    pub line: u32,
    pub column: u32,
}

impl Position {
    pub fn new(l: u32, c: u32) -> Position {
        assert!(l >= 1);
        assert!(c >= 1);

        Position { line: l, column: c }
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}:{}", self.line, self.column)
    }
}

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
