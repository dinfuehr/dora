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
    VarNeedsTypeInfo(String),
    VarTypesIncompatible(String, String, String),
    WhileCondType(String),
    IfCondType(String),
    ReturnType(String, String),
    LvalueExpected,
    AssignType(String, String),
    UnOpType(String, String, String)
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
            VarNeedsTypeInfo(ref name) =>
                format!("variable `{}` needs either type declaration or expression.", name),
            VarTypesIncompatible(ref name, ref def, ref expr) =>
                format!("variable `{}` defined with type `{}` but initialized with type `{}`.",
                        name, def, expr),
            WhileCondType(ref name) =>
                format!("while expects condition of type `bool` but got `{}`.", name),
            IfCondType(ref name) =>
                format!("if expects condition of type `bool` but got `{}`.", name),
            ReturnType(ref def, ref expr) =>
                format!("return expects value of type `{}` but got `{}`.", def, expr),
            LvalueExpected => format!("lvalue expected for assignment"),
            AssignType(ref def, ref expr) =>
                format!("can not assign value of type `{}` to variable of type `{}`", expr, def),
            UnOpType(ref op, ref def, ref expr) =>
                format!("unary `{}` only operates on `{}` but got `{}`.", op, def, expr),
        }
    }
}

#[derive(Clone, Debug)]
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
