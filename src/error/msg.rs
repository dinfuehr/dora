use self::Msg::*;
use ctxt::Context;
use interner::Name;
use lexer::position::Position;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Msg {
    Unimplemented,
    UnknownType(String),
    UnknownIdentifier(String),
    UnknownFunction(String),
    UnknownProp(String, String),
    UnknownMethod(String, String, Vec<String>),
    UnknownCtor(String, Vec<String>),
    MethodExists(String, String, Vec<String>, Position),
    IncompatibleWithNil(String),
    IdentifierExists(String),
    ShadowFunction(String),
    ShadowParam(String),
    ShadowClass(String),
    ShadowProp(String),
    VarNeedsTypeInfo(String),
    ParamTypesIncompatible(String, Vec<String>, Vec<String>),
    WhileCondType(String),
    IfCondType(String),
    ReturnType(String, String),
    LvalueExpected,
    AssignType(String, String, String),
    AssignProp(String, String, String, String),
    UnOpType(String, String),
    BinOpType(String, String, String),
    OutsideLoop,
    NoReturnValue,
    MainNotFound,
    WrongMainDefinition,
    SelfUnavailable,
    MultipleCandidates(String, String, Vec<String>),
    SelfNeeded,
    InvalidUseOfSelf,
    ThrowRefExpected(String),
    ThrowNil,
}

impl Msg {
    pub fn message(&self, ctxt: &Context) -> String {
        match *self {
            Unimplemented => format!("feature not implemented yet."),
            UnknownType(ref name) => format!("type `{}` does not exist.", name),
            UnknownIdentifier(ref name) => format!("unknown identifier `{}`.", name),
            UnknownFunction(ref name) => format!("unknown function `{}`", name),
            UnknownMethod(ref cls, ref name, ref args) => {
                let args = args.connect(", ");
                format!("no method with definition `{}({})` in class `{}`.", name, args, cls)
            },
            UnknownCtor(ref name, ref args) => {
                let args = args.connect(", ");
                format!("no ctor with definition `{}({})`.", name, args)
            }
            MethodExists(ref cls, ref name, ref args, pos) => {
                let args = args.connect(", ");

                format!(
                    "method with definition `{}({})` already exists in class `{}` at line {}.",
                    name, args, cls, pos)
            },
            IncompatibleWithNil(ref ty) => format!("cannot assign `nil` to type `{}`.", ty),
            UnknownProp(ref prop, ref ty) =>
                format!("unknown property `{}` for type `{}`", prop, ty),
            IdentifierExists(ref name) => format!("can not redefine identifier `{}`.", name),
            ShadowFunction(ref name) => format!("can not shadow function `{}`.", name),
            ShadowParam(ref name) => format!("can not shadow param `{}`.", name),
            ShadowClass(ref name) => format!("can not shadow class `{}`.", name),
            ShadowProp(ref name) => format!("property with name `{}` already exists.", name),
            VarNeedsTypeInfo(ref name) =>
                format!("variable `{}` needs either type declaration or expression.", name),
            ParamTypesIncompatible(ref name, ref def, ref expr) => {
                let def = def.connect(", ");
                let expr = expr.connect(", ");

                format!("function `{}({})` cannot be called as `{}({})`",
                    name, def, name, expr)
            },
            WhileCondType(ref ty) =>
                format!("`while` expects condition of type `bool` but got `{}`.", ty),
            IfCondType(ref ty) =>
                format!("`if` expects condition of type `bool` but got `{}`.", ty),
            ReturnType(ref def, ref expr) =>
                format!("`return` expects value of type `{}` but got `{}`.",
                    def, expr),
            LvalueExpected => format!("lvalue expected for assignment"),
            AssignType(ref name, ref def, ref expr) => {
                format!("cannot assign `{}` to variable `{}` of type `{}`.", expr, name, def)
            },
            AssignProp(ref name, ref cls, ref def, ref expr) => {
                format!("cannot assign `{}` to property `{}`.`{}` of type `{}`.",
                        expr, cls, name, def)
            },
            UnOpType(ref op, ref expr) =>
                format!("unary operator `{}` can not handle value of type `{} {}`.", op, op,
                    expr),
            BinOpType(ref op, ref lhs, ref rhs) =>
                format!("binary operator `{}` can not handle expression of type `{} {} {}`",
                    op, lhs, op, rhs),
            OutsideLoop => "statement only allowed inside loops".into(),
            NoReturnValue => "function does not return a value in all code paths".into(),
            MainNotFound => "no `main` function found in the program".into(),
            WrongMainDefinition => "`main` function has wrong definition".into(),
            SelfUnavailable => "`self` can only be used in methods not functions".into(),
            MultipleCandidates(ref cls, ref name, ref call_types) => {
                let call_types = call_types.connect(", ");

                format!("multiple candidates for invocation `{}({})` in class `{}`.",
                    name, call_types, cls)
            },
            SelfNeeded => "`self` parameter needed for methods.".into(),
            InvalidUseOfSelf => "`self` only allowed for first argument of methods.".into(),
            ThrowRefExpected(ref name) => {
                format!("`{}` is not a reference type.", name)
            }
            ThrowNil => "throwing `nil` is not allowed.".into()
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

    pub fn message(&self, ctxt: &Context) -> String {
        format!("error at {}: {}", self.pos, self.msg.message(ctxt))
    }
}
