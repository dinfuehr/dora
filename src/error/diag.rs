use error::msg::Msg;
use error::msg::MsgWithPos;

use lexer::position::Position;

pub struct Diagnostic {
    errors: Vec<MsgWithPos>,
}

impl Diagnostic {
    pub fn new() -> Diagnostic {
        Diagnostic { errors: Vec::new() }
    }

    pub fn errors(&self) -> &[MsgWithPos] {
        &self.errors
    }

    pub fn report(&mut self, pos: Position, msg: Msg) {
        self.errors.push(MsgWithPos::new(pos, msg));
    }

    pub fn report_unimplemented(&mut self, pos: Position) {
        self.errors.push(MsgWithPos::new(pos, Msg::Unimplemented));
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn dump(&self) {
        for err in &self.errors {
            println!("{}", &err.message());
        }
    }
}
