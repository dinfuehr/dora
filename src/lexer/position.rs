use std::fmt::{Formatter,Show,Error};
use std::result::Result;

pub struct Position {
    pub line : i32,
    pub column : i32
}

impl Position {
    pub fn new( l: i32, c: i32 ) -> Position {
        assert!( l >= 1 );
        assert!( c >= 1 );

        Position { line: l, column: c }
    }
}

impl Show for Position {
    fn fmt(&self, f : &mut Formatter) -> Result<(), Error> {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[test]
fn test_new() {
    let pos = Position::new(3, 1);

    assert_eq!(pos.line, 3);
    assert_eq!(pos.column, 1);

    assert_eq!(format!("{:?}",pos).as_slice(), "3:1");
}
