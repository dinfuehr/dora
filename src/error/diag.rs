use error::msg::Msg;
use error::msg::MsgWithPos;

use parser::lexer::position::Position;

struct Diagnostic {
    errors: Vec<MsgWithPos>,
}

impl Diagnostic {
    fn new() -> Diagnostic {
        Diagnostic {
            errors: Vec::new()
        }
    }

    fn report(&mut self, pos: Position, msg: Msg) {
        self.errors.push(MsgWithPos::new(pos, msg));
    }

    fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
}
