use self::Msg::*;
use class::ClassId;
use ctxt::Context;
use interner::Name;
use lexer::position::Position;
use ty::BuiltinType;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Msg {
    Unimplemented,
    UnknownType(Name),
    UnknownIdentifier(Name),
    UnknownFunction(String),
    UnknownProp(String, BuiltinType),
    UnknownMethod(BuiltinType, Name, Vec<BuiltinType>),
    UnknownCtor(Name, Vec<BuiltinType>),
    MethodExists(BuiltinType, Name, Vec<BuiltinType>, Position),
    VarNotMutable(Name),
    IncompatibleWithNil(BuiltinType),
    IdentifierExists(String),
    ShadowFunction(String),
    ShadowParam(String),
    ShadowType(String),
    ShadowProp(String),
    VarNeedsTypeInfo(String),
    ParamTypesIncompatible(Name, Vec<BuiltinType>, Vec<BuiltinType>),
    WhileCondType(BuiltinType),
    IfCondType(BuiltinType),
    ReturnType(BuiltinType, BuiltinType),
    LvalueExpected,
    AssignType(Name, BuiltinType, BuiltinType),
    AssignProp(Name, ClassId, BuiltinType, BuiltinType),
    UnOpType(String, BuiltinType),
    BinOpType(String, BuiltinType, BuiltinType),
    OutsideLoop,
    NoReturnValue,
    MainNotFound,
    WrongMainDefinition,
    SelfUnavailable,
    MultipleCandidates(BuiltinType, Name, Vec<BuiltinType>),
}

impl Msg {
    pub fn message(&self, ctxt: &Context) -> String {
        match *self {
            Unimplemented => format!("feature not implemented yet."),
            UnknownType(name) => {
                let name = ctxt.interner.str(name).to_string();
                format!("type `{}` does not exist.", name)
            },
            UnknownIdentifier(name) => {
                let name = ctxt.interner.str(name).to_string();
                format!("unknown identifier `{}`.", name)
            },
            UnknownFunction(ref name) => format!("unknown function `{}`", name),
            UnknownMethod(cls, name, ref args) => {
                let name = ctxt.interner.str(name).to_string();
                let cls = cls.name(ctxt);
                let args = args.iter().map(|a| a.name(ctxt)).collect::<Vec<String>>().connect(", ");

                format!("no method with definition `{}({})` in class `{}`.", name, args, cls)
            },
            UnknownCtor(name, ref args) => {
                let name = ctxt.interner.str(name).to_string();
                let args = args.iter().map(|a| a.name(ctxt)).collect::<Vec<String>>().connect(", ");

                format!("no ctor with definition `{}({})`.", name, args)
            }
            MethodExists(cls, name, ref args, pos) => {
                let name = ctxt.interner.str(name).to_string();
                let cls = cls.name(ctxt);
                let args = args.iter().map(|a| a.name(ctxt)).collect::<Vec<String>>().connect(", ");

                format!(
                    "method with definition `{}({})` already exists in class `{}` at line {}.",
                    name, args, cls, pos)
            },
            VarNotMutable(name) => {
                let name = ctxt.interner.str(name).to_string();

                format!("var `{}` not mutable.", name)
            },
            IncompatibleWithNil(ty) => {
                let name = ty.name(ctxt);

                format!("cannot assign `nil` to type `{}`.", name)
            },
            UnknownProp(ref prop, ref ty) =>
                format!("unknown property `{}` for type `{}`", prop, ty.name(ctxt)),
            IdentifierExists(ref name) => format!("can not redefine identifier `{}`.", name),
            ShadowFunction(ref name) => format!("can not shadow function `{}`.", name),
            ShadowParam(ref name) => format!("can not shadow param `{}`.", name),
            ShadowType(ref name) => format!("can not shadow type `{}`.", name),
            ShadowProp(ref name) => format!("property with name `{}` already exists.", name),
            VarNeedsTypeInfo(ref name) =>
                format!("variable `{}` needs either type declaration or expression.", name),
            ParamTypesIncompatible(name, ref def, ref expr) => {
                let name = ctxt.interner.str(name).to_string();
                let def = def.iter().map(|a| a.name(ctxt)).collect::<Vec<String>>().connect(", ");
                let expr = expr.iter().map(|a| a.name(ctxt)).collect::<Vec<String>>().connect(", ");

                format!("function `{}({})` cannot be called as `{}({})`",
                    name, def, name, expr)
            },
            WhileCondType(ref ty) =>
                format!("`while` expects condition of type `bool` but got `{}`.", ty.name(ctxt)),
            IfCondType(ref ty) =>
                format!("`if` expects condition of type `bool` but got `{}`.", ty.name(ctxt)),
            ReturnType(ref def, ref expr) =>
                format!("`return` expects value of type `{}` but got `{}`.",
                    def.name(ctxt), expr.name(ctxt)),
            LvalueExpected => format!("lvalue expected for assignment"),
            AssignType(name, def, expr) => {
                let name = ctxt.interner.str(name).to_string();
                let def = def.name(ctxt);
                let expr = expr.name(ctxt);

                format!("cannot assign `{}` to variable `{}` of type `{}`.", expr, name, def)
            },
            AssignProp(name, clsid, def, expr) => {
                let cls = ctxt.cls_by_id(clsid);
                let cls_name = ctxt.interner.str(cls.name).to_string();
                let name = ctxt.interner.str(name).to_string();
                let def = def.name(ctxt);
                let expr = expr.name(ctxt);

                format!("cannot assign `{}` to property `{}`.`{}` of type `{}`.",
                        expr, cls_name, name, def)
            },
            UnOpType(ref op, ref expr) =>
                format!("unary operator `{}` can not handle value of type `{} {}`.", op, op,
                    &expr.name(ctxt)),
            BinOpType(ref op, ref lhs, ref rhs) =>
                format!("binary operator `{}` can not handle expression of type `{} {} {}`",
                    op, &lhs.name(ctxt), op, &rhs.name(ctxt)),
            OutsideLoop => "statement only allowed inside loops".into(),
            NoReturnValue => "function does not return a value in all code paths".into(),
            MainNotFound => "no `main` function found in the program".into(),
            WrongMainDefinition => "`main` function has wrong definition".into(),
            SelfUnavailable => "`self` can only be used in methods not functions".into(),
            MultipleCandidates(cls, name, ref call_types) => {
                let cls = cls.name(ctxt);
                let name = ctxt.interner.str(name).to_string();
                let call_types = call_types.iter()
                    .map(|a| a.name(ctxt)).collect::<Vec<String>>().connect(", ");

                format!("multiple candidates for invocation `{}({})` in class `{}`.",
                    name, call_types, cls)
            }
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
