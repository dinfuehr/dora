use std::path::PathBuf;

use crate::language::sem_analysis::{SemAnalysis, SourceFileId};
use dora_parser::lexer::position::Position;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ErrorMessage {
    Unimplemented,
    UnknownClass(String),
    UnknownType(String),
    UnknownIdentifier(String),
    UnknownStruct(String),
    UnknownFunction(String),
    UnknownField(String, String),
    UnknownMethod(String, String, Vec<String>),
    UnknownEnumVariant(String),
    MultipleCandidatesForMethod(String, String, Vec<String>),
    VariadicParameterNeedsToBeLast,
    UnknownMethodForTypeParam(String, String, Vec<String>),
    MultipleCandidatesForTypeParam(String, String, Vec<String>),
    MultipleCandidatesForStaticMethodWithTypeParam,
    UnknownStaticMethodWithTypeParam,
    UnknownStaticMethod(String, String, Vec<String>),
    UnknownCtor,
    MethodExists(String, Position),
    IncompatibleWithNil(String),
    IdentifierExists(String),
    ShadowFunction(String),
    ShadowParam(String),
    ShadowClass(String),
    ShadowClassConstructor(String),
    ShadowStruct(String),
    ShadowStructConstructor(String),
    ShadowTrait(String),
    ShadowField(String),
    ShadowGlobal(String),
    ShadowConst(String),
    ShadowModule(String),
    ShadowEnum(String),
    ShadowEnumVariant(String),
    ShadowTypeParam(String),
    InvalidLhsAssignment,
    NoEnumVariant,
    EnumArgsIncompatible(String, String, Vec<String>, Vec<String>),
    StructArgsIncompatible(String, Vec<String>, Vec<String>),
    EnumArgsNoParens(String, String),
    MatchPatternNoParens,
    MatchPatternWrongNumberOfParams(usize, usize),
    EnumExpected,
    EnumVariantExpected,
    MatchUncoveredVariant,
    MatchUnreachablePattern,
    VarNeedsTypeInfo(String),
    ParamTypesIncompatible(String, Vec<String>, Vec<String>),
    ArgumentNameMismatch(String, Vec<String>, Vec<String>),
    LambdaParamTypesIncompatible(Vec<String>, Vec<String>),
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
    TraitExpected(String),
    NoSuperModule,
    LetMissingInitialization,
    LetReassigned,
    UnderivableType(String),
    CycleInHierarchy,
    SuperfluousOverride(String),
    SuperfluousOpen(String),
    MissingOverride(String),
    MethodNotOverridable(String),
    TypesIncompatible(String, String),
    ReturnTypeMismatch(String, String),
    OverrideMismatch,
    UnresolvedInternal,
    UnclosedComment,
    UnknownChar(char),
    UnclosedChar,
    UnclosedString,
    NumberOverflow(String),
    InvalidSuffix(String),
    ExpectedClass(String),
    ExpectedFactor(String),
    ExpectedToken(String, String),
    ExpectedTopLevelElement(String),
    ExpectedTrait,
    ExpectedType(String),
    ExpectedIdentifier(String),
    ExpectedStringable(String),
    ExpectedSomeIdentifier,
    ExpectedModule,
    ExpectedPath,
    LetPatternExpectedTuple(String),
    LetPatternShouldBeUnit,
    LetPatternExpectedTupleWithLength(String, usize, usize),
    MisplacedElse,
    ValueExpected,
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
    NotAccessible(String),
    StructConstructorNotAccessible(String),
    ClassConstructorNotAccessible(String),
    NotAccessibleInModule(String, String),
    RecursiveStructure,
    TraitMethodWithBody,
    TypeParamsExpected,
    TypeParamNameNotUnique(String),
    StaticMethodNotInTrait(String, String, Vec<String>),
    MethodNotInTrait(String, String, Vec<String>),
    StaticMethodMissingFromTrait(String, String, Vec<String>),
    MethodMissingFromTrait(String, String, Vec<String>),
    WrongNumberTypeParams(usize, usize),
    UnconstrainedTypeParam(String),
    ClassExpected,
    ClassEnumStructExpected,
    ClassExpectedAsTypeParam,
    BoundExpected,
    NoTypeParamsExpected,
    DuplicateTraitBound,
    TypeNotImplementingTrait(String, String),
    AbstractMethodNotInAbstractClass,
    AbstractMethodWithImplementation,
    NewAbstractClass,
    MissingAbstractOverride(String, String),
    ModifierNotAllowedForStaticMethod(String),
    InvalidTestAnnotationUsage,
    GlobalInitializerNotSupported,
    TypeNotUsableInForIn(String),
    UnknownStructField(String, String),
    UnknownIdentifierInModule(String, String),
    StructFieldNotInitialized(String, String),
    InvalidLeftSideOfSeparator,
    InvalidUseOfTypeParams,
    NameOfStaticMethodExpected,
    IfBranchTypesIncompatible(String, String),
    MatchBranchTypesIncompatible(String, String),
    VarAlreadyInPattern,
    NameExpected,
    IndexExpected,
    IllegalTupleIndex(u64, String),
    UninitializedVar,
    DirectoryNotFound(PathBuf),
    FileForModuleNotFound,
    FileNoAccess(PathBuf),
    FileDoesNotExist(PathBuf),
    Custom(String),
    MissingFileArgument,
    PackageAlreadyExists(String),
}

impl ErrorMessage {
    pub fn message(&self) -> String {
        match *self {
            ErrorMessage::Unimplemented => format!("feature not implemented yet."),
            ErrorMessage::UnknownClass(ref name) => format!("class `{}` does not exist.", name),
            ErrorMessage::UnknownType(ref name) => format!("type `{}` does not exist.", name),
            ErrorMessage::UnknownIdentifier(ref name) => format!("unknown identifier `{}`.", name),
            ErrorMessage::UnknownStruct(ref name) => format!("unknown struct `{}`.", name),
            ErrorMessage::UnknownFunction(ref name) => format!("unknown function `{}`", name),
            ErrorMessage::UnknownMethod(ref cls, ref name, ref args) => {
                let args = args.join(", ");
                format!(
                    "no method with definition `{}({})` in type `{}`.",
                    name, args, cls
                )
            }
            ErrorMessage::UnknownEnumVariant(ref name) => {
                format!("no variant with name `{}` in enumeration.", name)
            }
            ErrorMessage::MultipleCandidatesForMethod(ref cls, ref name, ref args) => {
                let args = args.join(", ");
                format!(
                    "multiple candidates for definition `{}({})` in class `{}`.",
                    name, args, cls
                )
            }
            ErrorMessage::VariadicParameterNeedsToBeLast => {
                "variadic parameter needs to be last.".into()
            }
            ErrorMessage::UnknownMethodForTypeParam(ref tp, ref name, ref args) => {
                let args = args.join(", ");
                format!(
                    "no method with definition `{}({})` found for type param `{}`.",
                    name, args, tp
                )
            }
            ErrorMessage::MultipleCandidatesForTypeParam(ref tp, ref name, ref args) => {
                let args = args.join(", ");
                format!(
                    "multiple candidates with definition `{}({})` found for type param `{}`.",
                    name, args, tp
                )
            }
            ErrorMessage::MultipleCandidatesForStaticMethodWithTypeParam => {
                "multiple candidates for static method call found.".into()
            }
            ErrorMessage::UnknownStaticMethodWithTypeParam => {
                "no static method with this name found for type param.".into()
            }
            ErrorMessage::UnknownStaticMethod(ref cls, ref name, ref args) => {
                let args = args.join(", ");
                format!("no static method `{}::{}({})`.", cls, name, args)
            }
            ErrorMessage::UnknownCtor => "class does not have constructor.".into(),
            ErrorMessage::MethodExists(ref name, pos) => format!(
                "method with name `{}` already exists at line {}.",
                name, pos
            ),
            ErrorMessage::IncompatibleWithNil(ref ty) => {
                format!("cannot assign `nil` to type `{}`.", ty)
            }
            ErrorMessage::UnknownField(ref field, ref ty) => {
                format!("unknown field `{}` for type `{}`", field, ty)
            }
            ErrorMessage::IdentifierExists(ref name) => {
                format!("can not redefine identifier `{}`.", name)
            }
            ErrorMessage::ShadowFunction(ref name) => {
                format!("can not shadow function `{}`.", name)
            }
            ErrorMessage::ShadowParam(ref name) => format!("can not shadow param `{}`.", name),
            ErrorMessage::ShadowClass(ref name) => format!("can not shadow class `{}`.", name),
            ErrorMessage::ShadowClassConstructor(ref name) => {
                format!("can not shadow constructor of class `{}`.", name)
            }
            ErrorMessage::ShadowStruct(ref name) => format!("can not shadow struct `{}`.", name),
            ErrorMessage::ShadowStructConstructor(ref name) => {
                format!("can not shadow constructor of struct `{}`.", name)
            }
            ErrorMessage::ShadowTrait(ref name) => format!("can not shadow trait `{}`.", name),
            ErrorMessage::ShadowField(ref name) => {
                format!("field with name `{}` already exists.", name)
            }
            ErrorMessage::ShadowGlobal(ref name) => {
                format!("can not shadow global variable `{}`.", name)
            }
            ErrorMessage::ShadowModule(ref name) => format!("can not shadow mod `{}`.", name),
            ErrorMessage::ShadowConst(ref name) => format!("can not shadow const `{}`.", name),
            ErrorMessage::ShadowEnum(ref name) => format!("can not shadow enum `{}`.", name),
            ErrorMessage::ShadowEnumVariant(ref name) => {
                format!("can not shadow enum variant `{}`.", name)
            }
            ErrorMessage::ShadowTypeParam(ref name) => {
                format!("can not shadow type param `{}`.", name)
            }
            ErrorMessage::NoEnumVariant => "enum needs at least one variant.".into(),
            ErrorMessage::EnumArgsIncompatible(ref enum_, ref name, ref def, ref expr) => {
                let def = def.join(", ");
                let expr = expr.join(", ");

                format!(
                    "enum `{}::{}({})` cannot be called as `{}({})`",
                    enum_, name, def, name, expr
                )
            }
            ErrorMessage::StructArgsIncompatible(ref struct_, ref def, ref expr) => {
                let def = def.join(", ");
                let expr = expr.join(", ");

                format!(
                    "struct `{}({})` cannot be called as `{}({})`",
                    struct_, def, struct_, expr
                )
            }
            ErrorMessage::EnumArgsNoParens(ref name, ref variant) => {
                format!("{}::{} needs to be used without parens.", name, variant)
            }
            ErrorMessage::MatchPatternNoParens => "pattern should be used without parens.".into(),
            ErrorMessage::MatchPatternWrongNumberOfParams(given_params, expected_params) => {
                format!(
                    "pattern expects {} params but got {}.",
                    given_params, expected_params
                )
            }
            ErrorMessage::VarAlreadyInPattern => "var is already used in pattern.".into(),
            ErrorMessage::EnumExpected => format!("enum expected."),
            ErrorMessage::EnumVariantExpected => format!("enum variant expected."),
            ErrorMessage::MatchUncoveredVariant => "not all variants are covered.".into(),
            ErrorMessage::MatchUnreachablePattern => "not all variants are covered.".into(),
            ErrorMessage::VarNeedsTypeInfo(ref name) => format!(
                "variable `{}` needs either type declaration or expression.",
                name
            ),
            ErrorMessage::ParamTypesIncompatible(ref name, ref def, ref expr) => {
                let def = def.join(", ");
                let expr = expr.join(", ");

                format!(
                    "function `{}({})` cannot be called as `{}({})`",
                    name, def, name, expr
                )
            }
            ErrorMessage::ArgumentNameMismatch(ref name, ref params, ref args) => {
                let params = params.join(", ");
                let args = args.join(", ");

                format!(
                    "argument name mismatch `{}({})` cannot be called as `{}({})`",
                    name, params, name, args
                )
            }
            ErrorMessage::LambdaParamTypesIncompatible(ref def, ref expr) => {
                let def = def.join(", ");
                let expr = expr.join(", ");

                format!("lambda `({})` cannot be called with `({})`", def, expr)
            }
            ErrorMessage::WhileCondType(ref ty) => {
                format!("`while` expects condition of type `bool` but got `{}`.", ty)
            }
            ErrorMessage::IfCondType(ref ty) => {
                format!("`if` expects condition of type `bool` but got `{}`.", ty)
            }
            ErrorMessage::ReturnType(ref def, ref expr) => format!(
                "`return` expects value of type `{}` but got `{}`.",
                def, expr
            ),
            ErrorMessage::LvalueExpected => format!("lvalue expected for assignment"),
            ErrorMessage::ValueExpected => format!("value expected"),
            ErrorMessage::AssignType(ref name, ref def, ref expr) => format!(
                "cannot assign `{}` to variable `{}` of type `{}`.",
                expr, name, def
            ),
            ErrorMessage::AssignField(ref name, ref cls, ref def, ref expr) => format!(
                "cannot assign `{}` to field `{}`.`{}` of type `{}`.",
                expr, cls, name, def
            ),
            ErrorMessage::UnOpType(ref op, ref expr) => format!(
                "unary operator `{}` can not handle value of type `{} {}`.",
                op, op, expr
            ),
            ErrorMessage::BinOpType(ref op, ref lhs, ref rhs) => format!(
                "binary operator `{}` can not handle expression of type `{} {} {}`",
                op, lhs, op, rhs
            ),
            ErrorMessage::ConstValueExpected => "constant value expected".into(),
            ErrorMessage::OutsideLoop => "statement only allowed inside loops".into(),
            ErrorMessage::NoReturnValue => {
                "function does not return a value in all code paths".into()
            }
            ErrorMessage::MainNotFound => "no `main` function found in the program".into(),
            ErrorMessage::WrongMainDefinition => "`main` function has wrong definition".into(),
            ErrorMessage::ThisUnavailable => {
                "`self` can only be used in methods not functions".into()
            }
            ErrorMessage::SelfTypeUnavailable => "`Self` can only be used in traits.".into(),
            ErrorMessage::SuperUnavailable => {
                "`super` only available in methods of classes with parent class".into()
            }
            ErrorMessage::SuperNeedsMethodCall => "`super` only allowed in method calls".into(),
            ErrorMessage::TraitExpected(ref name) => {
                format!("`{}` is not a trait.", name)
            }
            ErrorMessage::NoSuperModule => "no super module.".into(),
            ErrorMessage::NotAccessible(ref name) => format!("`{}` is not accessible.", name),
            ErrorMessage::StructConstructorNotAccessible(ref name) => {
                format!("constructor of struct `{}` is not accessible.", name)
            }
            ErrorMessage::ClassConstructorNotAccessible(ref name) => {
                format!("constructor of class `{}` is not accessible.", name)
            }
            ErrorMessage::NotAccessibleInModule(ref module, ref name) => {
                format!("`{}` in module `{}` is not accessible.", name, module)
            }
            ErrorMessage::LetMissingInitialization => {
                "`let` binding is missing initialization.".into()
            }
            ErrorMessage::LetReassigned => "`let` binding cannot be reassigned.".into(),
            ErrorMessage::InvalidLhsAssignment => "invalid left-hand-side of assignment.".into(),
            ErrorMessage::UnderivableType(ref name) => {
                format!("type `{}` cannot be used as super class.", name)
            }
            ErrorMessage::CycleInHierarchy => "cycle in type hierarchy detected.".into(),
            ErrorMessage::SuperfluousOverride(ref name) => format!(
                "method `{}` uses modifier `override` without overriding a function.",
                name
            ),
            ErrorMessage::MissingOverride(ref name) => {
                format!("method `{}` is missing modifier `override`.", name)
            }
            ErrorMessage::SuperfluousOpen(ref name) => format!(
                "method `{}` uses modifier `open` but class allows no subclasses.",
                name
            ),
            ErrorMessage::MethodNotOverridable(ref name) => {
                format!("method `{}` in super class not overridable.", name)
            }
            ErrorMessage::TypesIncompatible(ref na, ref nb) => {
                format!("types `{}` and `{}` incompatible.", na, nb)
            }
            ErrorMessage::ReturnTypeMismatch(ref fct, ref sup) => {
                format!("return types `{}` and `{}` do not match.", fct, sup)
            }
            ErrorMessage::OverrideMismatch => {
                "definition does not match overriden function.".into()
            }
            ErrorMessage::UnresolvedInternal => "unresolved internal.".into(),
            ErrorMessage::MisplacedElse => "misplace else.".into(),
            ErrorMessage::ExpectedToken(ref exp, ref got) => {
                format!("expected {} but got {}.", exp, got)
            }
            ErrorMessage::NumberOverflow(ref ty) => {
                format!("number does not fit into type {}.", ty)
            }
            ErrorMessage::InvalidSuffix(ref suffix) => format!("invalid suffix `{}`.", suffix),
            ErrorMessage::ExpectedClass(ref cls) => format!("expected class name but got {}.", cls),
            ErrorMessage::ExpectedFactor(ref got) => format!("factor expected but got {}.", got),
            ErrorMessage::ExpectedTrait => format!("expected trait."),
            ErrorMessage::ExpectedType(ref got) => format!("type expected but got {}.", got),
            ErrorMessage::ExpectedIdentifier(ref tok) => {
                format!("identifier expected but got {}.", tok)
            }
            ErrorMessage::ExpectedSomeIdentifier => "identifier expected.".into(),
            ErrorMessage::ExpectedModule => "module expected.".into(),
            ErrorMessage::ExpectedPath => "path expected.".into(),
            ErrorMessage::LetPatternExpectedTuple(ref ty) => {
                format!("tuple expected but got type {}.", ty)
            }
            ErrorMessage::LetPatternShouldBeUnit => format!("let pattern should be unit."),
            ErrorMessage::LetPatternExpectedTupleWithLength(ref ty, ty_length, pattern_length) => {
                format!(
                    "tuple {} has {} elements but pattern has {}.",
                    ty, ty_length, pattern_length
                )
            }
            ErrorMessage::ExpectedTopLevelElement(ref token) => {
                format!("expected function or class but got {}.", token)
            }
            ErrorMessage::ExpectedClassElement(ref token) => {
                format!("field or method expected but got {}.", token)
            }
            ErrorMessage::ExpectedStringable(ref ty) => {
                format!("type {} does not implement Stringable.", ty)
            }
            ErrorMessage::MisplacedAnnotation(ref modifier) => {
                format!("misplaced annotation `{}`.", modifier)
            }
            ErrorMessage::RedundantAnnotation(ref token) => {
                format!("redundant annotation {}.", token)
            }
            ErrorMessage::UnknownAnnotation(ref token) => format!("unknown annotation {}.", token),
            ErrorMessage::UnknownChar(ch) => {
                format!("unknown character {} (codepoint {}).", ch, ch as usize)
            }
            ErrorMessage::UnclosedComment => "unclosed comment.".into(),
            ErrorMessage::InvalidEscapeSequence(ch) => {
                format!("unknown escape sequence `\\{}`.", ch)
            }
            ErrorMessage::UnclosedString => "unclosed string.".into(),
            ErrorMessage::UnclosedChar => "unclosed char.".into(),
            ErrorMessage::IoError => "error reading from file.".into(),
            ErrorMessage::MissingFctBody => "missing function body.".into(),
            ErrorMessage::FctCallExpected => format!("function call expected"),
            ErrorMessage::ThisOrSuperExpected(ref val) => {
                format!("`self` or `super` expected but got {}.", val)
            }
            ErrorMessage::NoSuperDelegationWithPrimaryCtor(ref name) => format!(
                "no `super` delegation allowed for ctor in class {}, because class has \
                 primary ctor.",
                name
            ),
            ErrorMessage::NoSuperClass(ref name) => {
                format!("class `{}` does not have super class.", name)
            }
            ErrorMessage::RecursiveStructure => "recursive structure is not allowed.".into(),
            ErrorMessage::TraitMethodWithBody => {
                "trait method is not allowed to have definition".into()
            }
            ErrorMessage::TypeParamsExpected => "type params expected.".into(),
            ErrorMessage::TypeParamNameNotUnique(ref name) => {
                format!("type param `{}` name already used.", name)
            }
            ErrorMessage::StaticMethodNotInTrait(ref trait_name, ref mtd_name, ref args) => {
                let args = args.join(", ");

                format!(
                    "trait `{}` does not define static method `{}({})`.",
                    trait_name, mtd_name, args
                )
            }
            ErrorMessage::MethodNotInTrait(ref trait_name, ref mtd_name, ref args) => {
                let args = args.join(", ");

                format!(
                    "trait `{}` does not define method `{}({})`.",
                    trait_name, mtd_name, args
                )
            }
            ErrorMessage::StaticMethodMissingFromTrait(ref trait_name, ref mtd_name, ref args) => {
                let args = args.join(", ");

                format!(
                    "trait `{}` defines static method `{}({})` but is missing in `impl`.",
                    trait_name, mtd_name, args
                )
            }
            ErrorMessage::MethodMissingFromTrait(ref trait_name, ref mtd_name, ref args) => {
                let args = args.join(", ");

                format!(
                    "trait `{}` defines method `{}({})` but is missing in `impl`.",
                    trait_name, mtd_name, args
                )
            }
            ErrorMessage::WrongNumberTypeParams(exp, actual) => {
                format!("expected {} type parameters but got {}.", exp, actual)
            }
            ErrorMessage::UnconstrainedTypeParam(ref name) => {
                format!("unconstrained type param `{}`.", name)
            }
            ErrorMessage::ClassExpected => "expected class.".into(),
            ErrorMessage::ClassEnumStructExpected => "expected class, struct or enum.".into(),
            ErrorMessage::ClassExpectedAsTypeParam => "class as type parameter expected.".into(),
            ErrorMessage::BoundExpected => "class or trait bound expected".into(),
            ErrorMessage::NoTypeParamsExpected => "no type params allowed".into(),
            ErrorMessage::DuplicateTraitBound => "duplicate trait bound".into(),
            ErrorMessage::TypeNotImplementingTrait(ref name, ref trait_) => {
                format!("type `{}` does not implement trait `{}`.", name, trait_)
            }
            ErrorMessage::AbstractMethodWithImplementation => {
                "abstract methods cannot be implemented.".into()
            }
            ErrorMessage::AbstractMethodNotInAbstractClass => {
                "abstract methods only allowed in abstract classes.".into()
            }
            ErrorMessage::NewAbstractClass => "cannot create object of abstract class.".into(),
            ErrorMessage::MissingAbstractOverride(ref cls, ref name) => format!(
                "missing override of abstract method `{}` in class `{}`.",
                cls, name
            ),
            ErrorMessage::ModifierNotAllowedForStaticMethod(ref modifier) => {
                format!("modifier `{}` not allowed for static method.", modifier)
            }
            ErrorMessage::InvalidTestAnnotationUsage => "invalid usage of @Test annotation.".into(),
            ErrorMessage::GlobalInitializerNotSupported => {
                "global variables do no support initial assignment for now.".into()
            }
            ErrorMessage::TypeNotUsableInForIn(ref ty) => format!(
                "type `{}` doesn't implement iterator() or the iterator protocol.",
                ty
            ),
            ErrorMessage::UnknownStructField(ref struc, ref field) => {
                format!("struct `{}` does not have field named `{}`.", struc, field)
            }
            ErrorMessage::UnknownIdentifierInModule(ref module, ref element) => format!(
                "module `{}` does not contain identifier `{}`.",
                module, element
            ),
            ErrorMessage::StructFieldNotInitialized(ref struc, ref field) => {
                format!("field `{}` in struct `{}` not initialized.", field, struc)
            }
            ErrorMessage::InvalidLeftSideOfSeparator => {
                "left hand side of separator is not a class.".into()
            }
            ErrorMessage::InvalidUseOfTypeParams => {
                "type params need to be used on class or function.".into()
            }
            ErrorMessage::NameOfStaticMethodExpected => "name of static method expected.".into(),
            ErrorMessage::IfBranchTypesIncompatible(ref then_block, ref else_block) => format!(
                "if-branches have incompatible types `{}` and `{}`.",
                then_block, else_block
            ),
            ErrorMessage::MatchBranchTypesIncompatible(ref expected_ty, ref value_ty) => format!(
                "match arms have incompatible types `{}` and `{}`.",
                expected_ty, value_ty
            ),
            ErrorMessage::NameExpected => "name expected for dot-operator.".into(),
            ErrorMessage::IndexExpected => "index expected as right-hand-side for tuple.".into(),
            ErrorMessage::IllegalTupleIndex(idx, ref ty) => {
                format!("illegal index `{}` for type `{}`", idx, ty)
            }
            ErrorMessage::UninitializedVar => "cannot read uninitialized variable.".into(),
            ErrorMessage::DirectoryNotFound(ref path) => {
                format!("directory `{:?}` not found.", path)
            }
            ErrorMessage::FileForModuleNotFound => "file for module not found.".into(),
            ErrorMessage::FileNoAccess(ref path) => {
                format!("could not read file `{}`.", path.display())
            }
            ErrorMessage::FileDoesNotExist(ref path) => {
                format!("file `{}` does not exist.", path.display())
            }
            ErrorMessage::Custom(ref msg) => msg.clone(),
            ErrorMessage::MissingFileArgument => format!("no file argument given."),
            ErrorMessage::PackageAlreadyExists(ref name) => {
                format!("A package with name `{}` already exists.", name)
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct ErrorDescriptor {
    pub file: Option<SourceFileId>,
    pub pos: Option<Position>,
    pub msg: ErrorMessage,
}

impl ErrorDescriptor {
    pub fn new(file: SourceFileId, pos: Position, msg: ErrorMessage) -> ErrorDescriptor {
        ErrorDescriptor {
            file: Some(file),
            pos: Some(pos),
            msg,
        }
    }

    pub fn new_without_location(msg: ErrorMessage) -> ErrorDescriptor {
        ErrorDescriptor {
            file: None,
            pos: None,
            msg,
        }
    }

    pub fn message(&self, sa: &SemAnalysis) -> String {
        if let Some(file) = self.file {
            let pos = self.pos.expect("uninitialized pos");

            let file = sa.source_file(file);
            format!(
                "error in {:?} at {}: {}",
                file.path,
                pos,
                self.msg.message()
            )
        } else {
            assert!(self.pos.is_none());
            format!("error: {}", self.msg.message())
        }
    }
}
