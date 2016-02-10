use self::Msg::*;
use lexer::position::Position;
use ty::BuiltinType;

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
    ShadowProp(String),
    VarNeedsTypeInfo(String),
    ParamTypesIncompatible(String, Vec<BuiltinType>, Vec<BuiltinType>),
    WhileCondType(String),
    IfCondType(String),
    ReturnType(String, String),
    LvalueExpected,
    AssignType(String, BuiltinType, BuiltinType),
    UnOpType(String, BuiltinType),
    BinOpType(String, BuiltinType, BuiltinType),
    OutsideLoop,
    NoReturnValue,
    MainNotFound,
    WrongMainDefinition
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
            ShadowProp(ref name) => format!("property with name `{}` already exists.", name),
            VarNeedsTypeInfo(ref name) =>
                format!("variable `{}` needs either type declaration or expression.", name),
            ParamTypesIncompatible(ref name, ref def, ref expr) =>
                format!("function types incompatible"),
            WhileCondType(ref name) =>
                format!("while expects condition of type `bool` but got `{}`.", name),
            IfCondType(ref name) =>
                format!("if expects condition of type `bool` but got `{}`.", name),
            ReturnType(ref def, ref expr) =>
                format!("return expects value of type `{}` but got `{}`.", def, expr),
            LvalueExpected => format!("lvalue expected for assignment"),
            AssignType(ref name, ref def, ref expr) =>
                format!("variable `{}` defined with type `{}` but initialized with type `{}`.",
                        name, &def.to_string(), &expr.to_string()),
            UnOpType(ref op, ref expr) =>
                format!("unary unary `{}` can not handle value of type `{} {}`.", op, op,
                    &expr.to_string()),
            BinOpType(ref op, ref lhs, ref rhs) =>
                format!("binary operator `{}` can not handle expression of type `{} {} {}`",
                    op, &lhs.to_string(), op, &rhs.to_string()),
            OutsideLoop => "statement only allowed inside loops".into(),
            NoReturnValue => "function does not return a value in all code paths".into(),
            MainNotFound => "no main function found in the program".into(),
            WrongMainDefinition => "main function has wrong definition".into(),
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
