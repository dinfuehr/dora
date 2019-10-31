use crate::vm::{FileId, VM};
use dora_parser::lexer::position::Position;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum SemError {
    Unimplemented,
    UnknownClass(String),
    UnknownType(String),
    UnknownIdentifier(String),
    UnknownStruct(String),
    UnknownFunction(String),
    UnknownField(String, String),
    UnknownMethod(String, String, Vec<String>),
    UnknownEnumValue(String),
    MultipleCandidatesForMethod(String, String, Vec<String>),
    UnknownMethodForTypeParam(String, String, Vec<String>),
    MultipleCandidatesForTypeParam(String, String, Vec<String>),
    MultipleCandidatesForStaticMethodWithTypeParam,
    UnknownStaticMethodWithTypeParam,
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
    ShadowEnum(String),
    ShadowEnumValue(String),
    InvalidLhsAssignment,
    NoEnumValue,
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
    ClassReassigned,
    TypeParamReassigned,
    FctUsedAsIdentifier,
    ClsUsedAsIdentifier,
    TypeParamUsedAsIdentifier,
    EnumUsedAsIdentifier,
    TypeParamUsedAsCallee,
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
    ExpectedStringable(String),
    ExpectedSomeIdentifier,
    MisplacedElse,
    IoError,
    ExpectedClassElement(String),
    MisplacedAnnotation(String),
    RedundantAnnotation(String),
    UnknownAnnotation(String),
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
    InvalidLeftSideOfSeparator,
    InvalidUseOfTypeParams,
    NameOfStaticMethodExpected,
    IfBranchTypesIncompatible(String, String),
    NameExpected,
}

impl SemError {
    pub fn message(&self) -> String {
        match *self {
            SemError::Unimplemented => format!("feature not implemented yet."),
            SemError::UnknownClass(ref name) => format!("class `{}` does not exist.", name),
            SemError::UnknownType(ref name) => format!("type `{}` does not exist.", name),
            SemError::UnknownIdentifier(ref name) => format!("unknown identifier `{}`.", name),
            SemError::UnknownStruct(ref name) => format!("unknown struct `{}`.", name),
            SemError::UnknownFunction(ref name) => format!("unknown function `{}`", name),
            SemError::UnknownMethod(ref cls, ref name, ref args) => {
                let args = args.join(", ");
                format!(
                    "no method with definition `{}({})` in class `{}`.",
                    name, args, cls
                )
            }
            SemError::UnknownEnumValue(ref name) => {
                format!("no value with name `{}` in enumeration.", name)
            }
            SemError::MultipleCandidatesForMethod(ref cls, ref name, ref args) => {
                let args = args.join(", ");
                format!(
                    "multiple candidates for definition `{}({})` in class `{}`.",
                    name, args, cls
                )
            }
            SemError::UnknownMethodForTypeParam(ref tp, ref name, ref args) => {
                let args = args.join(", ");
                format!(
                    "no method with definition `{}({})` found for type param `{}`.",
                    name, args, tp
                )
            }
            SemError::MultipleCandidatesForTypeParam(ref tp, ref name, ref args) => {
                let args = args.join(", ");
                format!(
                    "multiple candidates with definition `{}({})` found for type param `{}`.",
                    name, args, tp
                )
            }
            SemError::MultipleCandidatesForStaticMethodWithTypeParam => {
                "multiple candidates for static method call found.".into()
            }
            SemError::UnknownStaticMethodWithTypeParam => {
                "no static method with this name found for type param.".into()
            }
            SemError::UnknownStaticMethod(ref cls, ref name, ref args) => {
                let args = args.join(", ");
                format!("no static method `{}::{}({})`.", cls, name, args)
            }
            SemError::UnknownCtor(ref name, ref args) => {
                let args = args.join(", ");
                format!("no ctor with definition `{}({})`.", name, args)
            }
            SemError::MethodExists(ref cls, ref name, pos) => format!(
                "method with name `{}` already exists in class `{}` at line {}.",
                name, cls, pos
            ),
            SemError::IncompatibleWithNil(ref ty) => {
                format!("cannot assign `nil` to type `{}`.", ty)
            }
            SemError::UnknownField(ref field, ref ty) => {
                format!("unknown field `{}` for type `{}`", field, ty)
            }
            SemError::IdentifierExists(ref name) => {
                format!("can not redefine identifier `{}`.", name)
            }
            SemError::ShadowFunction(ref name) => format!("can not shadow function `{}`.", name),
            SemError::ShadowParam(ref name) => format!("can not shadow param `{}`.", name),
            SemError::ShadowClass(ref name) => format!("can not shadow class `{}`.", name),
            SemError::ShadowStruct(ref name) => format!("can not shadow struct `{}`.", name),
            SemError::ShadowTrait(ref name) => format!("can not shadow trait `{}`.", name),
            SemError::ShadowField(ref name) => {
                format!("field with name `{}` already exists.", name)
            }
            SemError::ShadowGlobal(ref name) => {
                format!("can not shadow global variable `{}`.", name)
            }
            SemError::ShadowConst(ref name) => format!("can not shadow const `{}`.", name),
            SemError::ShadowEnum(ref name) => format!("can not shadow enum `{}`.", name),
            SemError::ShadowEnumValue(ref name) => format!("can not shadow enum value `{}`.", name),
            SemError::NoEnumValue => "enum needs at least one value.".into(),
            SemError::VarNeedsTypeInfo(ref name) => format!(
                "variable `{}` needs either type declaration or expression.",
                name
            ),
            SemError::ParamTypesIncompatible(ref name, ref def, ref expr) => {
                let def = def.join(", ");
                let expr = expr.join(", ");

                format!(
                    "function `{}({})` cannot be called as `{}({})`",
                    name, def, name, expr
                )
            }
            SemError::WhileCondType(ref ty) => {
                format!("`while` expects condition of type `bool` but got `{}`.", ty)
            }
            SemError::IfCondType(ref ty) => {
                format!("`if` expects condition of type `bool` but got `{}`.", ty)
            }
            SemError::ReturnType(ref def, ref expr) => format!(
                "`return` expects value of type `{}` but got `{}`.",
                def, expr
            ),
            SemError::LvalueExpected => format!("lvalue expected for assignment"),
            SemError::AssignType(ref name, ref def, ref expr) => format!(
                "cannot assign `{}` to variable `{}` of type `{}`.",
                expr, name, def
            ),
            SemError::AssignField(ref name, ref cls, ref def, ref expr) => format!(
                "cannot assign `{}` to field `{}`.`{}` of type `{}`.",
                expr, cls, name, def
            ),
            SemError::UnOpType(ref op, ref expr) => format!(
                "unary operator `{}` can not handle value of type `{} {}`.",
                op, op, expr
            ),
            SemError::BinOpType(ref op, ref lhs, ref rhs) => format!(
                "binary operator `{}` can not handle expression of type `{} {} {}`",
                op, lhs, op, rhs
            ),
            SemError::ConstValueExpected => "constant value expected".into(),
            SemError::OutsideLoop => "statement only allowed inside loops".into(),
            SemError::NoReturnValue => "function does not return a value in all code paths".into(),
            SemError::MainNotFound => "no `main` function found in the program".into(),
            SemError::WrongMainDefinition => "`main` function has wrong definition".into(),
            SemError::ThisUnavailable => "`self` can only be used in methods not functions".into(),
            SemError::SelfTypeUnavailable => "`Self` can only be used in traits.".into(),
            SemError::SuperUnavailable => {
                "`super` only available in methods of classes with parent class".into()
            }
            SemError::SuperNeedsMethodCall => "`super` only allowed in method calls".into(),
            SemError::ReferenceTypeExpected(ref name) => {
                format!("`{}` is not a reference type.", name)
            }
            SemError::ThrowNil => "throwing `nil` is not allowed.".into(),
            SemError::CatchOrFinallyExpected => "`try` without `catch` or `finally`.".into(),
            SemError::LetMissingInitialization => "`let` binding is missing initialization.".into(),
            SemError::LetReassigned => "`let` binding cannot be reassigned.".into(),
            SemError::FctReassigned => "function cannot be reassigned.".into(),
            SemError::ClassReassigned => "class cannot be reassigned.".into(),
            SemError::TypeParamReassigned => "type param cannot be reassigned.".into(),
            SemError::FctUsedAsIdentifier => "function cannot be used as identifier.".into(),
            SemError::ClsUsedAsIdentifier => "class cannot be used as identifier.".into(),
            SemError::TypeParamUsedAsIdentifier => {
                "type param cannot be used as identifier.".into()
            }
            SemError::EnumUsedAsIdentifier => "enum cannot be used as identifier.".into(),
            SemError::InvalidLhsAssignment => "invalid left-hand-side of assignment.".into(),
            SemError::TypeParamUsedAsCallee => "type param cannot be used as callee.".into(),
            SemError::UnderivableType(ref name) => {
                format!("type `{}` cannot be used as super class.", name)
            }
            SemError::CycleInHierarchy => "cycle in type hierarchy detected.".into(),
            SemError::SuperfluousOverride(_) => {
                "method `{}` uses modifier `override` without overriding a function.".into()
            }
            SemError::MissingOverride(_) => "method `{}` is missing modifier `override`.".into(),
            SemError::SuperfluousOpen(_) => {
                "method `{}` uses modifier `open` but class allows no subclasses.".into()
            }
            SemError::ThrowsDifference(_) => {
                "use of `throws` in method `{}`needs to match super class".into()
            }
            SemError::MethodNotOverridable(ref name) => {
                format!("method `{}` in super class not overridable.", name)
            }
            SemError::TypesIncompatible(ref na, ref nb) => {
                format!("types `{}` and `{}` incompatible.", na, nb)
            }
            SemError::ReturnTypeMismatch(ref fct, ref sup) => {
                format!("return types `{}` and `{}` do not match.", fct, sup)
            }
            SemError::UnresolvedInternal => "unresolved internal.".into(),
            SemError::MisplacedElse => "misplace else.".into(),
            SemError::ExpectedToken(ref exp, ref got) => {
                format!("expected {} but got {}.", exp, got)
            }
            SemError::NumberOverflow(ref ty) => format!("number does not fit into type {}.", ty),
            SemError::ExpectedClass(ref cls) => format!("expected class name but got {}.", cls),
            SemError::ExpectedFactor(ref got) => format!("factor expected but got {}.", got),
            SemError::ExpectedTrait(ref trt) => format!("expected trait name but got {}.", trt),
            SemError::ExpectedType(ref got) => format!("type expected but got {}.", got),
            SemError::ExpectedIdentifier(ref tok) => {
                format!("identifier expected but got {}.", tok)
            }
            SemError::ExpectedSomeIdentifier => "identifier expected".into(),
            SemError::ExpectedTopLevelElement(ref token) => {
                format!("expected function or class but got {}.", token)
            }
            SemError::ExpectedClassElement(ref token) => {
                format!("field or method expected but got {}.", token)
            }
            SemError::ExpectedStringable(ref ty) => {
                format!("type {} does not implement Stringable.", ty)
            }
            SemError::MisplacedAnnotation(ref modifier) => {
                format!("misplaced annotation `{}`.", modifier)
            }
            SemError::RedundantAnnotation(ref token) => format!("redundant annotation {}.", token),
            SemError::UnknownAnnotation(ref token) => format!("unknown annotation {}.", token),
            SemError::UnknownChar(ch) => {
                format!("unknown character {} (codepoint {}).", ch, ch as usize)
            }
            SemError::UnclosedComment => "unclosed comment.".into(),
            SemError::InvalidEscapeSequence(ch) => format!("unknown escape sequence `\\{}`.", ch),
            SemError::UnclosedString => "unclosed string.".into(),
            SemError::UnclosedChar => "unclosed char.".into(),
            SemError::IoError => "error reading from file.".into(),
            SemError::MissingFctBody => "missing function body.".into(),
            SemError::FctCallExpected => format!("function call expected"),
            SemError::ThisOrSuperExpected(ref val) => {
                format!("`self` or `super` expected but got {}.", val)
            }
            SemError::NoSuperDelegationWithPrimaryCtor(ref name) => format!(
                "no `super` delegation allowed for ctor in class {}, because class has \
                 primary ctor.",
                name
            ),
            SemError::NoSuperClass(ref name) => {
                format!("class `{}` does not have super class.", name)
            }
            SemError::RecursiveStructure => "recursive structure is not allowed.".into(),
            SemError::TraitMethodWithBody => {
                "trait method is not allowed to have definition".into()
            }
            SemError::TryNeedsCall => "`try` expects function or method call.".into(),
            SemError::TryCallNonThrowing => {
                "given function or method call for `try` does not throw.".into()
            }
            SemError::ThrowingCallWithoutTry => {
                "function or method call that is able to throw, needs `try`.".into()
            }
            SemError::TypeParamsExpected => "type params expected.".into(),
            SemError::TypeParamNameNotUnique(ref name) => {
                format!("type param `{}` name already used.", name)
            }
            SemError::StaticMethodNotInTrait(ref trait_name, ref mtd_name, ref args) => {
                let args = args.join(", ");

                format!(
                    "trait `{}` does not define static method `{}({})`.",
                    trait_name, mtd_name, args
                )
            }
            SemError::MethodNotInTrait(ref trait_name, ref mtd_name, ref args) => {
                let args = args.join(", ");

                format!(
                    "trait `{}` does not define method `{}({})`.",
                    trait_name, mtd_name, args
                )
            }
            SemError::StaticMethodMissingFromTrait(ref trait_name, ref mtd_name, ref args) => {
                let args = args.join(", ");

                format!(
                    "trait `{}` defines static method `{}({})` but is missing in `impl`.",
                    trait_name, mtd_name, args
                )
            }
            SemError::MethodMissingFromTrait(ref trait_name, ref mtd_name, ref args) => {
                let args = args.join(", ");

                format!(
                    "trait `{}` defines method `{}({})` but is missing in `impl`.",
                    trait_name, mtd_name, args
                )
            }
            SemError::WrongNumberTypeParams(exp, actual) => {
                format!("expected {} type parameters but got {}.", exp, actual)
            }
            SemError::ClassExpected(ref name) => format!("`{}` is not a class.", name),
            SemError::ClassExpectedAsTypeParam => "class as type parameter expected.".into(),
            SemError::AssignmentToConst => "cannot assign to const variable.".into(),
            SemError::BoundExpected => "class or trait bound expected".into(),
            SemError::NoTypeParamsExpected => "no type params allowed".into(),
            SemError::MultipleClassBounds => "multiple class bounds not allowed".into(),
            SemError::DuplicateTraitBound => "duplicate trait bound".into(),
            SemError::ClassBoundNotSatisfied(ref name, ref xclass) => {
                format!("type `{}` not a subclass of `{}`.", name, xclass)
            }
            SemError::TraitBoundNotSatisfied(ref name, ref xtrait) => {
                format!("type `{}` does not implement trait `{}`.", name, xtrait)
            }
            SemError::AbstractMethodWithImplementation => {
                "abstract methods cannot be implemented.".into()
            }
            SemError::AbstractMethodNotInAbstractClass => {
                "abstract methods only allowed in abstract classes.".into()
            }
            SemError::NewAbstractClass => "cannot create object of abstract class.".into(),
            SemError::MissingAbstractOverride(ref cls, ref name) => format!(
                "missing override of abstract method `{}` in class `{}`.",
                cls, name
            ),
            SemError::ModifierNotAllowedForStaticMethod(ref modifier) => {
                format!("modifier `{}` not allowed for static method.", modifier)
            }
            SemError::GlobalInitializerNotSupported => {
                "global variables do no support initial assignment for now.".into()
            }
            SemError::MakeIteratorReturnType(ref ty) => format!(
                "makeIterator() returns `{}` which does not implement Iterator.",
                ty
            ),
            SemError::UnknownStructField(ref struc, ref field) => {
                format!("struct `{}` does not have field named `{}`.", struc, field)
            }
            SemError::StructFieldNotInitialized(ref struc, ref field) => {
                format!("field `{}` in struct `{}` not initialized.", field, struc)
            }
            SemError::InvalidLeftSideOfSeparator => {
                "left hand side of separator is not a class.".into()
            }
            SemError::InvalidUseOfTypeParams => {
                "type params need to be used on class or function.".into()
            }
            SemError::NameOfStaticMethodExpected => "name of static method expected.".into(),
            SemError::IfBranchTypesIncompatible(ref then_block, ref else_block) => format!(
                "if-branches have incompatible types `{}` and `{}`.",
                then_block, else_block
            ),
            SemError::NameExpected => "name expected for dot-operator.".into(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct SemErrorAndPos {
    pub file: FileId,
    pub pos: Position,
    pub msg: SemError,
}

impl SemErrorAndPos {
    pub fn new(file: FileId, pos: Position, msg: SemError) -> SemErrorAndPos {
        SemErrorAndPos { file, pos, msg }
    }

    pub fn message(&self, vm: &VM) -> String {
        let file = vm.file(self.file);
        format!(
            "error in {} at {}: {}",
            file.name,
            self.pos,
            self.msg.message()
        )
    }
}
