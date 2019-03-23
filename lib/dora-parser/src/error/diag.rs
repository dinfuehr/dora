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

    pub fn report_without_path(&mut self, pos: Position, msg: Msg) {
        self.errors.push(MsgWithPos::without_path(pos, msg));
    }

    pub fn report(&mut self, file: String, pos: Position, msg: Msg) {
        self.errors.push(MsgWithPos::new(file, pos, msg));
    }

    pub fn report_unimplemented(&mut self, file: String, pos: Position) {
        self.errors
            .push(MsgWithPos::new(file, pos, Msg::Unimplemented));
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
