use self::Msg::*;
use parser::lexer::position::Position;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Msg {
    Unimplemented,
    UnknownType(String),
    UnknownIdentifier(String),
    UnknownFunction(String),
    IdentifierExists(String),
    ShadowFunction(String),
    ShadowParam(String),
    ShadowType(String),
}

impl Msg {
    pub fn message(&self) -> String {
        match *self {
            Unimplemented => format!("feature not implemented yet."),
            UnknownType(ref name) => format!("no type with name `{}` known.", name),
            UnknownIdentifier(ref name) => format!("unknown identifier `{}`.", name),
            UnknownFunction(ref name) => format!("unknown function `{}`", name),
            IdentifierExists(ref name) => format!("can not redefine identifier `{}`.", name),
            ShadowFunction(ref name) => format!("can not shadow function `{}`.", name),
            ShadowParam(ref name) => format!("can not shadow param `{}`.", name),
            ShadowType(ref name) => format!("can not shadow type `{}`.", name),
        }
    }
}

#[derive(Clone)]
pub struct MsgWithPos {
    pub msg: Msg,
    pub pos: Position,
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
