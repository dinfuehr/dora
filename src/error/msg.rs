use self::Msg::*;
use parser::lexer::position::Position;

pub enum Msg {
    Unimplemented,
    UnknownType(String)
}

impl Msg {
    pub fn message(&self) -> String {
        match *self {
            Unimplemented => format!("feature not implemented yet."),
            UnknownType(ref name) => format!("no type with name `{}` known.", name)
        }
    }
}

pub struct MsgWithPos {
    msg: Msg,
    pos: Position,
}

impl MsgWithPos {
    pub fn new(pos: Position, msg: Msg) -> MsgWithPos {
        MsgWithPos {
            pos: pos,
            msg: msg
        }
    }

    pub fn message(&self) -> String {
        format!("error at {}: {}", self.pos, self.msg.message())
    }
}
