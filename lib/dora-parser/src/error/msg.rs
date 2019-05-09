
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
    UnknownStaticMethod(String, String, Vec<String>),
    UnknownCtor(String, Vec<String>),
    MethodExists(String, String, Position),
    IncompatibleWithNil(String),
    IdentifierExists(String),
    ShadowFunction(String),
    ShadowParam(String),
    ShadowClass(String),
    ShadowStruct(String),
    ShadowTrait(String),
    ShadowField(String),
    ShadowGlobal(String),
    ShadowConst(String),
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
    ConstValueExpected,
    OutsideLoop,
    NoReturnValue,
    MainNotFound,
    WrongMainDefinition,
    ThisUnavailable,
    SelfTypeUnavailable,
    SuperUnavailable,
    SuperNeedsMethodCall,
    ReferenceTypeExpected(String),
    ThrowNil,
    CatchOrFinallyExpected,
    LetMissingInitialization,
    LetReassigned,
    FctReassigned,
    FctUsedAsIdentifier,
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
    UnclosedChar,
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
    FctCallExpected,
    ThisOrSuperExpected(String),
    NoSuperDelegationWithPrimaryCtor(String),
    NoSuperClass(String),
    RecursiveStructure,
    TraitMethodWithBody,
    TryNeedsCall,
    TryCallNonThrowing,
    ThrowingCallWithoutTry,
    TypeParamsExpected,
    TypeParamNameNotUnique(String),
    StaticMethodNotInTrait(String, String, Vec<String>),
    MethodNotInTrait(String, String, Vec<String>),
    StaticMethodMissingFromTrait(String, String, Vec<String>),
    MethodMissingFromTrait(String, String, Vec<String>),
    WrongNumberTypeParams(usize, usize),
    ClassExpected(String),
    ClassExpectedAsTypeParam,
    AssignmentToConst,
    BoundExpected,
    NoTypeParamsExpected,
    MultipleClassBounds,
    DuplicateTraitBound,
    ClassBoundNotSatisfied(String, String),
    TraitBoundNotSatisfied(String, String),
    AbstractMethodNotInAbstractClass,
    AbstractMethodWithImplementation,
    NewAbstractClass,
    MissingAbstractOverride(String, String),
    ModifierNotAllowedForStaticMethod(String),
    GlobalInitializerNotSupported,
    MakeIteratorReturnType(String),
    UnknownStructField(String, String),
    StructFieldNotInitialized(String, String),
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
            UnknownStaticMethod(ref cls, ref name, ref args) => {
                let args = args.join(", ");
                format!("no static method `{}::{}({})`.", cls, name, args)
            }
            UnknownCtor(ref name, ref args) => {
                let args = args.join(", ");
                format!("no ctor with definition `{}({})`.", name, args)
            }
            MethodExists(ref cls, ref name, pos) => {
                format!("method with name `{}` already exists in class `{}` at line {}.",
                        name,
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
            ShadowGlobal(ref name) => format!("can not shadow global variable `{}`.", name),
            ShadowConst(ref name) => format!("can not shadow const `{}`", name),
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
            ConstValueExpected => "constant value expected".into(),
            OutsideLoop => "statement only allowed inside loops".into(),
            NoReturnValue => "function does not return a value in all code paths".into(),
            MainNotFound => "no `main` function found in the program".into(),
            WrongMainDefinition => "`main` function has wrong definition".into(),
            ThisUnavailable => "`self` can only be used in methods not functions".into(),
            SelfTypeUnavailable => "`Self` can only be used in traits.".into(),
            SuperUnavailable => {
                "`super` only available in methods of classes with parent class".into()
            }
            SuperNeedsMethodCall => "`super` only allowed in method calls".into(),
            ReferenceTypeExpected(ref name) => format!("`{}` is not a reference type.", name),
            ThrowNil => "throwing `nil` is not allowed.".into(),
            CatchOrFinallyExpected => "`try` without `catch` or `finally`.".into(),
            LetMissingInitialization => "`let` binding is missing initialization.".into(),
            LetReassigned => "`let` binding cannot be reassigned.".into(),
            FctReassigned => "function cannot be reassigned.".into(),
            FctUsedAsIdentifier => "function cannot be used as identifier.".into(),
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
            UnclosedChar => "unclosed char.".into(),
            IoError => "error reading from file.".into(),
            MissingFctBody => "missing function body.".into(),
            FctCallExpected => format!("function call expected"),
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
            TypeParamsExpected => "type params expected.".into(),
            TypeParamNameNotUnique(ref name) => format!("type param `{}` name already used.", name),
            StaticMethodNotInTrait(ref trait_name, ref mtd_name, ref args) => {
                let args = args.join(", ");

                format!("trait `{}` does not define static method `{}({})`.",
                        trait_name,
                        mtd_name,
                        args)
            }
            MethodNotInTrait(ref trait_name, ref mtd_name, ref args) => {
                let args = args.join(", ");

                format!("trait `{}` does not define method `{}({})`.",
                        trait_name,
                        mtd_name,
                        args)
            }
            StaticMethodMissingFromTrait(ref trait_name, ref mtd_name, ref args) => {
                let args = args.join(", ");

                format!("trait `{}` defines static method `{}({})` but is missing in `impl`.",
                        trait_name,
                        mtd_name,
                        args)
            }
            MethodMissingFromTrait(ref trait_name, ref mtd_name, ref args) => {
                let args = args.join(", ");

                format!("trait `{}` defines method `{}({})` but is missing in `impl`.",
                        trait_name,
                        mtd_name,
                        args)
            }
            WrongNumberTypeParams(exp, actual) => {
                format!("expected {} type parameters but got {}.", exp, actual)
            }
            ClassExpected(ref name) => format!("`{}` is not a class.", name),
            ClassExpectedAsTypeParam => "class as type parameter expected.".into(),
            AssignmentToConst => "cannot assign to const variable.".into(),
            BoundExpected => "class or trait bound expected".into(),
            NoTypeParamsExpected => "no type params allowed".into(),
            MultipleClassBounds => "multiple class bounds not allowed".into(),
            DuplicateTraitBound => "duplicate trait bound".into(),
            ClassBoundNotSatisfied(ref name, ref xclass) => {
                format!("type `{}` not a subclass of `{}`.", name, xclass)
            }
            TraitBoundNotSatisfied(ref name, ref xtrait) => {
                format!("type `{}` does not implement trait `{}`.", name, xtrait)
            }
            AbstractMethodWithImplementation => "abstract methods cannot be implemented.".into(),
            AbstractMethodNotInAbstractClass => {
                "abstract methods only allowed in abstract classes.".into()
            }
            NewAbstractClass => "cannot create object of abstract class.".into(),
            MissingAbstractOverride(ref cls, ref name) => {
                format!("missing override of abstract method `{}` in class `{}`.",
                        cls,
                        name)
            }
            ModifierNotAllowedForStaticMethod(ref modifier) => {
                format!("modifier `{}` not allowed for static method.", modifier)
            }
            GlobalInitializerNotSupported => {
                "global variables do no support initial assignment for now.".into()
            }
            MakeIteratorReturnType(ref ty) => {
                format!("makeIterator() returns `{}` which does not implement Iterator.",
                        ty)
            }
            UnknownStructField(ref struc, ref field) => {
                format!("struct `{}` does not have field named `{}`.", struc, field)
            }
            StructFieldNotInitialized(ref struc, ref field) => {
                format!("field `{}` in struct `{}` not initialized.", field, struc)
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct MsgWithPos {
    pub path: String,
    pub pos: Position,
    pub msg: Msg,
}

impl MsgWithPos {
    pub fn new(path: String, pos: Position, msg: Msg,) -> MsgWithPos {
        MsgWithPos { path, pos, msg }
    }

    pub fn message(&self) -> String {
        if self.path.is_empty() {
            format!("error at {}: {}", self.pos, self.msg.message())
        } else {
            format!("error in {} at {}: {}", self.path, self.pos, self.msg.message())
        }
    }

    pub fn without_path(pos: Position, msg: Msg,) -> MsgWithPos {
        MsgWithPos { path: "".to_string(), pos, msg }
    }
}

impl fmt::Display for MsgWithPos {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.message())
    }
}
