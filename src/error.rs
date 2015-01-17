use lexer::position::Position;

pub struct ParseError {
    pub filename: String,
    pub position: Position,
    pub message: String
}

impl ParseError {
    fn print(&self) {
        println!("{} at {}: {}", self.filename, self.position, self.message);
    }
}

