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

#[derive(Copy, Clone, Debug)]
pub struct Span {
    start: Loc,
    count: u32,
}

impl Span {
    pub fn new(start: u32, count: u32) -> Span {
        Span {
            start: Loc::new(start),
            count: count,
        }
    }

    pub fn at(start: u32) -> Span {
        Span {
            start: Loc::new(start),
            count: 0,
        }
    }

    pub fn invalid() -> Span {
        Span {
            start: Loc::invalid(),
            count: 0,
        }
    }

    pub fn is_valid(&self) -> bool {
        self.start.is_valid()
    }

    pub fn start(&self) -> u32 {
        self.start.idx()
    }

    pub fn count(&self) -> u32 {
        self.count
    }

    pub fn end(&self) -> u32 {
        self.start.idx() + self.count
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Loc(u32);

impl Loc {
    fn new(pos: u32) -> Loc {
        assert!(pos < u32::max_value());
        Loc(pos)
    }

    fn invalid() -> Loc {
        Loc(u32::max_value())
    }

    fn is_valid(&self) -> bool {
        self.0 != u32::max_value()
    }

    fn idx(&self) -> u32 {
        assert!(self.is_valid());
        self.0
    }
}

#[test]
fn test_new() {
    let pos = Position::new(3, 1);

    assert_eq!(pos.line, 3);
    assert_eq!(pos.column, 1);

    assert_eq!(&format!("{}", pos)[..], "3:1");
}
