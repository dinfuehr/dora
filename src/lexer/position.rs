use std::fmt::{Formatter,Display,Error};
use std::result::Result;

#[derive(PartialEq,Eq,Debug,Copy,Clone)]
pub struct Position {
    pub line : u32,
    pub column : u32
}

impl Position {
    pub fn new( l: u32, c: u32 ) -> Position {
        assert!( l >= 1 );
        assert!( c >= 1 );

        Position { line: l, column: c }
    }
}

impl Display for Position {
    fn fmt(&self, f : &mut Formatter) -> Result<(), Error> {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[test]
fn test_new() {
    let pos = Position::new(3, 1);

    assert_eq!(pos.line, 3);
    assert_eq!(pos.column, 1);

    assert_eq!(&format!("{}",pos)[..], "3:1");
}
