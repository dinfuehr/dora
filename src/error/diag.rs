use error::msg::Msg;
use error::msg::MsgWithPos;

use parser::lexer::position::Position;

pub struct Diagnostic {
    errors: Vec<MsgWithPos>,
}

impl Diagnostic {
    pub fn new() -> Diagnostic {
        Diagnostic {
            errors: Vec::new()
        }
    }

    pub fn report(&mut self, pos: Position, msg: Msg) {
        self.errors.push(MsgWithPos::new(pos, msg));
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
}
