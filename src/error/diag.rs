use std::cell::RefCell;

use error::msg::Msg;
use error::msg::MsgWithPos;

use parser::lexer::position::Position;

pub struct Diagnostic {
    errors: RefCell<Vec<MsgWithPos>>,
}

impl Diagnostic {
    pub fn new() -> Diagnostic {
        Diagnostic {
            errors: RefCell::new(Vec::new())
        }
    }

    pub fn report(&self, pos: Position, msg: Msg) {
        self.errors.borrow_mut().push(MsgWithPos::new(pos, msg));
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.borrow().is_empty()
    }
}
