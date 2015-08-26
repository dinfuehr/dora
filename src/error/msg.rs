use parser::lexer::position::Position;

pub enum Msg {
    UnclosedComment,
    UnclosedString,
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
}
