
use std::fmt;

use self::Msg::*;
use lexer::position::Position;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Msg {
    Unimplemented,
    UnknownClass(String),
    UnknownType(String),
    UnknownIdentifier(String),
    UnknownStruct(String),
    UnknownFunction(String),
    UnknownField(String, String),
    UnknownMethod(String, String, Vec<String>),
    UnknownCtor(String, Vec<String>),
    MethodExists(String, String, Vec<String>, Position),
    IncompatibleWithNil(String),
    IdentifierExists(String),
    ShadowFunction(String),
    ShadowParam(String),
    ShadowClass(String),
    ShadowStruct(String),
    ShadowTrait(String),
    ShadowField(String),
    VarNeedsTypeInfo(String),
    ParamTypesIncompatible(String, Vec<String>, Vec<String>),
    WhileCondType(String),
    IfCondType(String),
    ReturnType(String, String),
    LvalueExpected,
    AssignType(String, String, String),
    AssignField(String, String, String, String),
    UnOpType(String, String),
    BinOpType(String, String, String),
    OutsideLoop,
    NoReturnValue,
    MainNotFound,
    WrongMainDefinition,
    ThisUnavailable,
    SuperUnavailable,
    SuperNeedsMethodCall,
    MultipleCandidates(String, String, Vec<String>),
    ReferenceTypeExpected(String),
    ThrowNil,
    CatchOrFinallyExpected,
    LetMissingInitialization,
    LetReassigned,
    UnderivableType(String),
    CycleInHierarchy,
    SuperfluousOverride(String),
    SuperfluousOpen(String),
    MissingOverride(String),
    ThrowsDifference(String),
    MethodNotOverridable(String),
    TypesIncompatible(String, String),
    ReturnTypeMismatch(String, String),
    UnresolvedInternal,
    UnclosedComment,
    UnknownChar(char),
    UnclosedString,
    NumberOverflow(String),
    ExpectedClass(String),
    ExpectedFactor(String),
    ExpectedToken(String, String),
    ExpectedTopLevelElement(String),
    ExpectedTrait(String),
    ExpectedType(String),
    ExpectedIdentifier(String),
    MisplacedElse,
    IoError,
    ExpectedClassElement(String),
    RedundantModifier(String),
    MisplacedModifier(String),
    InvalidEscapeSequence(char),
    MissingFctBody,
    ThisOrSuperExpected(String),
    NoSuperDelegationWithPrimaryCtor(String),
    NoSuperClass(String),
    RecursiveStructure,
    TraitMethodWithBody,
    TryNeedsCall,
    TryCallNonThrowing,
    ThrowingCallWithoutTry,
    MethodNotInTrait(String, String, Vec<String>),
    MethodMissingFromTrait(String, String, Vec<String>),
}

impl Msg {
    pub fn message(&self) -> String {
        match *self {
            Unimplemented => format!("feature not implemented yet."),
            UnknownClass(ref name) => format!("class `{}` does not exist.", name),
            UnknownType(ref name) => format!("type `{}` does not exist.", name),
            UnknownIdentifier(ref name) => format!("unknown identifier `{}`.", name),
            UnknownStruct(ref name) => format!("unknown struct `{}`.", name),
            UnknownFunction(ref name) => format!("unknown function `{}`", name),
            UnknownMethod(ref cls, ref name, ref args) => {
                let args = args.join(", ");
                format!("no method with definition `{}({})` in class `{}`.",
                        name,
                        args,
                        cls)
            }
            UnknownCtor(ref name, ref args) => {
                let args = args.join(", ");
                format!("no ctor with definition `{}({})`.", name, args)
            }
            MethodExists(ref cls, ref name, ref args, pos) => {
                let args = args.join(", ");

                format!("method with definition `{}({})` already exists in class `{}` at line {}.",
                        name,
                        args,
                        cls,
                        pos)
            }
            IncompatibleWithNil(ref ty) => format!("cannot assign `nil` to type `{}`.", ty),
            UnknownField(ref field, ref ty) => {
                format!("unknown field `{}` for type `{}`", field, ty)
            }
            IdentifierExists(ref name) => format!("can not redefine identifier `{}`.", name),
            ShadowFunction(ref name) => format!("can not shadow function `{}`.", name),
            ShadowParam(ref name) => format!("can not shadow param `{}`.", name),
            ShadowClass(ref name) => format!("can not shadow class `{}`.", name),
            ShadowStruct(ref name) => format!("can not shadow struct `{}`.", name),
            ShadowTrait(ref name) => format!("can not shadow trait `{}`.", name),
            ShadowField(ref name) => format!("field with name `{}` already exists.", name),
            VarNeedsTypeInfo(ref name) => {
                format!("variable `{}` needs either type declaration or expression.",
                        name)
            }
            ParamTypesIncompatible(ref name, ref def, ref expr) => {
                let def = def.join(", ");
                let expr = expr.join(", ");

                format!("function `{}({})` cannot be called as `{}({})`",
                        name,
                        def,
                        name,
                        expr)
            }
            WhileCondType(ref ty) => {
                format!("`while` expects condition of type `bool` but got `{}`.", ty)
            }
            IfCondType(ref ty) => {
                format!("`if` expects condition of type `bool` but got `{}`.", ty)
            }
            ReturnType(ref def, ref expr) => {
                format!("`return` expects value of type `{}` but got `{}`.",
                        def,
                        expr)
            }
            LvalueExpected => format!("lvalue expected for assignment"),
            AssignType(ref name, ref def, ref expr) => {
                format!("cannot assign `{}` to variable `{}` of type `{}`.",
                        expr,
                        name,
                        def)
            }
            AssignField(ref name, ref cls, ref def, ref expr) => {
                format!("cannot assign `{}` to field `{}`.`{}` of type `{}`.",
                        expr,
                        cls,
                        name,
                        def)
            }
            UnOpType(ref op, ref expr) => {
                format!("unary operator `{}` can not handle value of type `{} {}`.",
                        op,
                        op,
                        expr)
            }
            BinOpType(ref op, ref lhs, ref rhs) => {
                format!("binary operator `{}` can not handle expression of type `{} {} {}`",
                        op,
                        lhs,
                        op,
                        rhs)
            }
            OutsideLoop => "statement only allowed inside loops".into(),
            NoReturnValue => "function does not return a value in all code paths".into(),
            MainNotFound => "no `main` function found in the program".into(),
            WrongMainDefinition => "`main` function has wrong definition".into(),
            ThisUnavailable => "`self` can only be used in methods not functions".into(),
            SuperUnavailable => {
                "`super` only available in methods of classes with parent class".into()
            }
            SuperNeedsMethodCall => "`super` only allowed in method calls".into(),
            MultipleCandidates(ref cls, ref name, ref call_types) => {
                let call_types = call_types.join(", ");

                format!("multiple candidates for invocation `{}({})` in class `{}`.",
                        name,
                        call_types,
                        cls)
            }
            ReferenceTypeExpected(ref name) => format!("`{}` is not a reference type.", name),
            ThrowNil => "throwing `nil` is not allowed.".into(),
            CatchOrFinallyExpected => "`try` without `catch` or `finally`.".into(),
            LetMissingInitialization => "`let` binding is missing initialization.".into(),
            LetReassigned => "`let` binding cannot be reassigned.".into(),
            UnderivableType(ref name) => format!("type `{}` cannot be used as super class.", name),
            CycleInHierarchy => "cycle in type hierarchy detected.".into(),
            SuperfluousOverride(_) => {
                "method `{}` uses modifier `override` without overriding a function.".into()
            }
            MissingOverride(_) => "method `{}` is missing modifier `override`.".into(),
            SuperfluousOpen(_) => {
                "method `{}` uses modifier `open` but class allows no subclasses.".into()
            }
            ThrowsDifference(_) => {
                "use of `throws` in method `{}`needs to match super class".into()
            }
            MethodNotOverridable(ref name) => {
                format!("method `{}` in super class not overridable.", name)
            }
            TypesIncompatible(ref na, ref nb) => {
                format!("types `{}` and `{}` incompatible.", na, nb)
            }
            ReturnTypeMismatch(ref fct, ref sup) => {
                format!("return types `{}` and `{}` do not match.", fct, sup)
            }
            UnresolvedInternal => "unresolved internal.".into(),
            MisplacedElse => "misplace else.".into(),
            ExpectedToken(ref exp, ref got) => format!("expected {} but got {}.", exp, got),
            NumberOverflow(ref ty) => format!("number does not fit into type {}.", ty),
            ExpectedClass(ref cls) => format!("expected class name but got {}.", cls),
            ExpectedFactor(ref got) => format!("factor expected but got {}.", got),
            ExpectedTrait(ref trt) => format!("expected trait name but got {}.", trt),
            ExpectedType(ref got) => format!("type expected but got {}.", got),
            ExpectedIdentifier(ref tok) => format!("identifier expected but got {}.", tok),
            MisplacedModifier(ref modifier) => format!("misplaced modifier `{}`.", modifier),
            ExpectedTopLevelElement(ref token) => {
                format!("expected function or class but got {}.", token)
            }
            ExpectedClassElement(ref token) => {
                format!("field or method expected but got {}.", token)
            }
            RedundantModifier(ref token) => format!("redundant modifier {}.", token),
            UnknownChar(ch) => format!("unknown character {} (codepoint {}).", ch, ch as usize),
            UnclosedComment => "unclosed comment.".into(),
            InvalidEscapeSequence(ch) => format!("unknown escape sequence `\\{}`.", ch),
            UnclosedString => "unclosed string.".into(),
            IoError => "error reading from file.".into(),
            MissingFctBody => "missing function body.".into(),
            ThisOrSuperExpected(ref val) => format!("`self` or `super` expected but got {}.", val),
            NoSuperDelegationWithPrimaryCtor(ref name) => {
                format!("no `super` delegation allowed for ctor in class {}, because class has \
                         primary ctor.",
                        name)
            }
            NoSuperClass(ref name) => format!("class `{}` does not have super class.", name),
            RecursiveStructure => "recursive structure is not allowed.".into(),
            TraitMethodWithBody => "trait method is not allowed to have definition".into(),
            TryNeedsCall => "`try` expects function or method call.".into(),
            TryCallNonThrowing => "given function or method call for `try` does not throw.".into(),
            ThrowingCallWithoutTry => {
                "function or method call that is able to throw, needs `try`.".into()
            }
            MethodNotInTrait(ref trait_name, ref mtd_name, ref args) => {
                let args = args.join(", ");

                format!("trait `{}` does not define method `{}({})`.",
                        trait_name, mtd_name, args)
            }
            MethodMissingFromTrait(ref trait_name, ref mtd_name, ref args) => {
                let args = args.join(", ");

                format!("trait `{}` defines method `{}({})` but is missing in `impl`.",
                        trait_name, mtd_name, args)
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
            msg: msg,
        }
    }

    pub fn message(&self) -> String {
        format!("error at {}: {}", self.pos, self.msg.message())
    }
}

impl fmt::Display for MsgWithPos {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "error at {}: {}", self.pos, self.msg.message())
    }
}
