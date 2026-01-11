use std::path::PathBuf;

use crate::sema::{Sema, SourceFileId};
use dora_parser::{Span, compute_line_column};

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ErrorLevel {
    Warn,
    Error,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ErrorMessage {
    Unimplemented,
    UnknownClass(String),
    UnknownIdentifier(String),
    UnknownStruct(String),
    UnknownFunction(String),
    UnknownField(String, String),
    UnknownMethod(String, String),
    UnknownEnumVariant(String),
    UnknownSuffix,
    MultipleCandidatesForMethod(String, String),
    VariadicParameterNeedsToBeLast,
    UnknownMethodForTypeParam,
    MultipleCandidatesForTypeParam,
    MultipleCandidatesForStaticMethodWithTypeParam,
    UnknownStaticMethodWithTypeParam,
    UnknownStaticMethod(String, String),
    UnknownCtor,
    AliasExists(String, Span),
    TypeExists(String, Span),
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
    UnexpectedArgumentsForEnumVariant,
    EnumVariantMissingArguments,
    PatternNoParens,
    PatternWrongNumberOfParams(usize, usize),
    EnumExpected,
    EnumMismatch(String, String),
    EnumVariantExpected,
    NonExhaustiveMatch(Vec<String>),
    UselessPattern,
    VarNeedsTypeOrExpression,
    ParamTypesIncompatible(String, Vec<String>, Vec<String>),
    LambdaParamTypesIncompatible(Vec<String>, Vec<String>),
    WhileCondType(String),
    IfCondType(String),
    ReturnType(String, String),
    LvalueExpected,
    AssignType(String, String),
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
    ExpectedIdentityType(String),
    ReturnTypeMismatch(String, String),
    ImplMethodDefinitionMismatch,
    OverrideMismatch,
    UnresolvedInternal,
    UnclosedComment,
    UnknownChar(char),
    UnclosedChar,
    UnclosedString,
    NumberOverflow(String),
    NumberLimitOverflow,
    InvalidSuffix(String),
    InvalidNumberFormat,
    ExpectedClass(String),
    ExpectedMethod,
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
    PatternTupleExpected(String),
    WrongType(String, String),
    PatternTupleLengthMismatch(String, usize, usize),
    MisplacedElse,
    ValueExpected,
    IoError,
    ExpectedClassElement(String),
    MisplacedAnnotation(String),
    RedundantAnnotation,
    UnknownAnnotation(String),
    InvalidEscapeSequence,
    MissingFctBody,
    FctCallExpected,
    ThisOrSuperExpected(String),
    NoSuperDelegationWithPrimaryCtor(String),
    NoSuperClass(String),
    NotAccessible,
    UnexpectedTypeAliasAssignment,
    StructConstructorNotAccessible(String),
    ImmutableField,
    ClassConstructorNotAccessible(String),
    NotAccessibleInModule(String, String),
    RecursiveStructure,
    TraitMethodWithBody,
    TypeParamsExpected,
    TypeParamNameNotUnique(String),
    StaticMethodNotInTrait(String, String, Vec<String>),
    ElementNotInTrait,
    ElementNotInImpl(String),
    MethodMissingFromTrait(String, String, Vec<String>),
    WrongNumberTypeParams(usize, usize),
    UnconstrainedTypeParam(String),
    StaticMethodCallTargetExpected,
    ExpectedExtensionType,
    BoundExpected,
    NoTypeParamsExpected,
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
    NameExpected,
    IndexExpected,
    IllegalTupleIndex(usize, String),
    UninitializedVar,
    DirectoryNotFound(PathBuf),
    FileForModuleNotFound,
    FileNoAccess(PathBuf),
    FileDoesNotExist(PathBuf),
    Custom(String),
    Descriptor(&'static str, crate::error::DescriptorArgs),
    MissingFileArgument,
    PackageAlreadyExists(String),
    UnknownPackage(String),
    NegativeUnsigned,
    InvalidCharLiteral,
    InvalidReturn,
    PatternAltWithBindingUnsupported,
    UseNotAccessible,
    TypeAliasMissingType,
    AliasCycle,
    UnexpectedTypeBounds,
    PatternTypeMismatch(String),
    PatternDuplicateBinding,
    PatternBindingWrongType(String, String),
    PatternBindingNotDefinedInAllAlternatives(String),
    PatternUnexpectedRest,
    PatternMultipleRest,
    PatternRestShouldBeLast,
    ExtendingTypeDifferentPackage,
    ImplTraitForeignType,
    TraitNotObjectSafe,
    UnexpectedTypeBinding,
    TypeBindingOrder,
    UnknownTypeBinding,
    DuplicateTypeBinding,
    MissingTypeBinding(String),
    ExpectedTypeName,
    UnknownAssoc,
    UnexpectedAssoc,
    UnexpectedWhere,
    UnexpectedPositionalArgument,
    UnexpectedNamedArgument,
    DuplicateNamedArgument,
    MissingNamedArgument(String),
    UseOfUnknownArgument,
    CallRequiresNamedArgument,
    WrongTypeForArgument(String, String),
    SuperfluousArgument,
    MissingArguments(usize, usize),
    FieldShouldBeUnnamed,
    OldClassDefinition,
    ExpectedNamedPattern,
    IndexGetNotImplemented(String),
    IndexSetNotImplemented(String),
    IndexGetAndIndexSetDoNotMatch,
    MissingAssocType(String),
    NameBoundMultipleTimesInParams(String),
    InvalidType,
}

impl ErrorMessage {
    pub fn message(&self, sa: &crate::sema::Sema) -> String {
        match self {
            ErrorMessage::Unimplemented => format!("feature not implemented yet."),
            ErrorMessage::UnknownClass(name) => format!("class `{}` does not exist.", name),
            ErrorMessage::UnknownIdentifier(name) => format!("unknown identifier `{}`.", name),
            ErrorMessage::UnknownStruct(name) => format!("unknown struct `{}`.", name),
            ErrorMessage::UnknownFunction(name) => format!("unknown function `{}`", name),
            ErrorMessage::UnknownMethod(cls, name) => {
                format!("no method with name `{}` in type `{}`.", name, cls)
            }
            ErrorMessage::UnknownEnumVariant(name) => {
                format!("no variant with name `{}` in enumeration.", name)
            }
            ErrorMessage::UnknownSuffix => "unknown integer suffix".into(),
            ErrorMessage::MultipleCandidatesForMethod(cls, name) => {
                format!(
                    "multiple candidates for method named `{}` in type `{}`.",
                    name, cls
                )
            }
            ErrorMessage::VariadicParameterNeedsToBeLast => {
                "variadic parameter needs to be last.".into()
            }
            ErrorMessage::UnknownMethodForTypeParam => {
                "no method with this name found for type param.".into()
            }
            ErrorMessage::MultipleCandidatesForTypeParam => {
                "multiple methods with this name found for type param.".into()
            }
            ErrorMessage::MultipleCandidatesForStaticMethodWithTypeParam => {
                "multiple candidates for static method call found.".into()
            }
            ErrorMessage::UnknownStaticMethodWithTypeParam => {
                "no static method with this name found for type param.".into()
            }
            ErrorMessage::UnknownStaticMethod(cls, name) => {
                format!("no static method of name `{}` for type `{}`.", name, cls,)
            }
            ErrorMessage::UnexpectedTypeAliasAssignment => "no type expected.".into(),
            ErrorMessage::UnknownCtor => "class does not have constructor.".into(),
            ErrorMessage::AliasExists(name, pos) => format!(
                "element with name `{}` already exists at line {}.",
                name, pos
            ),
            ErrorMessage::TypeExists(name, pos) => format!(
                "method with name `{}` already exists at line {}.",
                name, pos
            ),
            ErrorMessage::IncompatibleWithNil(ty) => {
                format!("cannot assign `nil` to type `{}`.", ty)
            }
            ErrorMessage::UnknownField(field, ty) => {
                format!("unknown field `{}` for type `{}`", field, ty)
            }
            ErrorMessage::IdentifierExists(name) => {
                format!("can not redefine identifier `{}`.", name)
            }
            ErrorMessage::ShadowFunction(name) => {
                format!("can not shadow function `{}`.", name)
            }
            ErrorMessage::ShadowParam(name) => format!("can not shadow param `{}`.", name),
            ErrorMessage::ShadowClass(name) => format!("can not shadow class `{}`.", name),
            ErrorMessage::ShadowClassConstructor(name) => {
                format!("can not shadow constructor of class `{}`.", name)
            }
            ErrorMessage::ShadowStruct(name) => format!("can not shadow struct `{}`.", name),
            ErrorMessage::ShadowStructConstructor(name) => {
                format!("can not shadow constructor of struct `{}`.", name)
            }
            ErrorMessage::ShadowTrait(name) => format!("can not shadow trait `{}`.", name),
            ErrorMessage::ShadowField(name) => {
                format!("field with name `{}` already exists.", name)
            }
            ErrorMessage::ShadowGlobal(name) => {
                format!("can not shadow global variable `{}`.", name)
            }
            ErrorMessage::ShadowModule(name) => format!("can not shadow mod `{}`.", name),
            ErrorMessage::ShadowConst(name) => format!("can not shadow const `{}`.", name),
            ErrorMessage::ShadowEnum(name) => format!("can not shadow enum `{}`.", name),
            ErrorMessage::ShadowEnumVariant(name) => {
                format!("can not shadow enum variant `{}`.", name)
            }
            ErrorMessage::ShadowTypeParam(name) => {
                format!("can not shadow type param `{}`.", name)
            }
            ErrorMessage::NoEnumVariant => "enum needs at least one variant.".into(),
            ErrorMessage::EnumArgsIncompatible(enum_, name, def, expr) => {
                let def = def.join(", ");
                let expr = expr.join(", ");

                format!(
                    "enum `{}::{}({})` cannot be called as `{}({})`",
                    enum_, name, def, name, expr
                )
            }
            ErrorMessage::StructArgsIncompatible(struct_, def, expr) => {
                let def = def.join(", ");
                let expr = expr.join(", ");

                format!(
                    "struct `{}({})` cannot be called as `{}({})`",
                    struct_, def, struct_, expr
                )
            }
            ErrorMessage::ImmutableField => {
                format!("Fields of this type are immutable.")
            }
            ErrorMessage::UnexpectedArgumentsForEnumVariant => {
                format!("Enum variant does not have any arguments")
            }
            ErrorMessage::EnumVariantMissingArguments => {
                format!("Fields missing for enum variant.")
            }
            ErrorMessage::PatternNoParens => "pattern should be used without parens.".into(),
            ErrorMessage::PatternWrongNumberOfParams(given_params, expected_params) => {
                format!(
                    "pattern expects {} params but got {}.",
                    given_params, expected_params
                )
            }
            ErrorMessage::EnumExpected => format!("enum expected."),
            ErrorMessage::EnumMismatch(value, pattern) => {
                format!("value of type {} but pattern of type {}.", value, pattern)
            }
            ErrorMessage::EnumVariantExpected => format!("enum variant expected."),
            ErrorMessage::NonExhaustiveMatch(patterns) => {
                let missing = patterns.join(", ");
                format!(
                    "`match` does not cover all possible values. Missing patterns: {}",
                    missing
                )
            }
            ErrorMessage::UselessPattern => "unreachable pattern.".into(),
            ErrorMessage::VarNeedsTypeOrExpression => {
                format!("variable needs either type declaration or expression.")
            }
            ErrorMessage::ParamTypesIncompatible(name, def, expr) => {
                let def = def.join(", ");
                let expr = expr.join(", ");

                format!(
                    "function `{}({})` cannot be called as `{}({})`",
                    name, def, name, expr
                )
            }
            ErrorMessage::LambdaParamTypesIncompatible(def, expr) => {
                let def = def.join(", ");
                let expr = expr.join(", ");

                format!("lambda `({})` cannot be called with `({})`", def, expr)
            }
            ErrorMessage::WhileCondType(ty) => {
                format!("`while` expects condition of type `bool` but got `{}`.", ty)
            }
            ErrorMessage::IfCondType(ty) => {
                format!("`if` expects condition of type `bool` but got `{}`.", ty)
            }
            ErrorMessage::ReturnType(def, expr) => format!(
                "`return` expects value of type `{}` but got `{}`.",
                def, expr
            ),
            ErrorMessage::LvalueExpected => format!("lvalue expected for assignment"),
            ErrorMessage::ValueExpected => format!("value expected"),
            ErrorMessage::AssignType(def, expr) => {
                format!("cannot assign `{}` to variable of type `{}`.", expr, def)
            }
            ErrorMessage::AssignField(name, cls, def, expr) => format!(
                "cannot assign `{}` to field `{}`.`{}` of type `{}`.",
                expr, cls, name, def
            ),
            ErrorMessage::UnOpType(op, expr) => format!(
                "unary operator `{}` can not handle value of type `{} {}`.",
                op, op, expr
            ),
            ErrorMessage::NegativeUnsigned => "cannot use `-` with UInt8".into(),
            ErrorMessage::BinOpType(op, lhs, rhs) => format!(
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
            ErrorMessage::TraitExpected(name) => {
                format!("`{}` is not a trait.", name)
            }
            ErrorMessage::NoSuperModule => "no super module.".into(),
            ErrorMessage::NotAccessible => format!("element is not accessible."),
            ErrorMessage::StructConstructorNotAccessible(name) => {
                format!("constructor of struct `{}` is not accessible.", name)
            }
            ErrorMessage::ClassConstructorNotAccessible(name) => {
                format!("constructor of class `{}` is not accessible.", name)
            }
            ErrorMessage::NotAccessibleInModule(module, name) => {
                format!("`{}` in module `{}` is not accessible.", name, module)
            }
            ErrorMessage::LetMissingInitialization => {
                "`let` binding is missing initialization.".into()
            }
            ErrorMessage::LetReassigned => "`let` binding cannot be reassigned.".into(),
            ErrorMessage::InvalidLhsAssignment => "invalid left-hand-side of assignment.".into(),
            ErrorMessage::UnderivableType(name) => {
                format!("type `{}` cannot be used as super class.", name)
            }
            ErrorMessage::CycleInHierarchy => "cycle in type hierarchy detected.".into(),
            ErrorMessage::SuperfluousOverride(name) => format!(
                "method `{}` uses modifier `override` without overriding a function.",
                name
            ),
            ErrorMessage::MissingOverride(name) => {
                format!("method `{}` is missing modifier `override`.", name)
            }
            ErrorMessage::SuperfluousOpen(name) => format!(
                "method `{}` uses modifier `open` but class allows no subclasses.",
                name
            ),
            ErrorMessage::MethodNotOverridable(name) => {
                format!("method `{}` in super class not overridable.", name)
            }
            ErrorMessage::TypesIncompatible(na, nb) => {
                format!("types `{}` and `{}` incompatible.", na, nb)
            }
            ErrorMessage::ExpectedIdentityType(name) => {
                format!("type `{}` does not have identity.", name)
            }
            ErrorMessage::ReturnTypeMismatch(fct, sup) => {
                format!("return types `{}` and `{}` do not match.", fct, sup)
            }
            ErrorMessage::OverrideMismatch => {
                "definition does not match overriden function.".into()
            }
            ErrorMessage::UnresolvedInternal => "unresolved internal.".into(),
            ErrorMessage::MisplacedElse => "misplace else.".into(),
            ErrorMessage::ExpectedToken(exp, got) => {
                format!("expected {} but got {}.", exp, got)
            }
            ErrorMessage::NumberOverflow(ty) => {
                format!("number does not fit into type {}.", ty)
            }
            ErrorMessage::NumberLimitOverflow => {
                format!("number exceeds maximum value.")
            }
            ErrorMessage::InvalidSuffix(suffix) => format!("invalid suffix `{}`.", suffix),
            ErrorMessage::InvalidNumberFormat => "invalid number format.".into(),
            ErrorMessage::ExpectedClass(cls) => format!("expected class name but got {}.", cls),
            ErrorMessage::ExpectedMethod => format!("expected method."),
            ErrorMessage::ExpectedFactor(got) => format!("factor expected but got {}.", got),
            ErrorMessage::ExpectedTrait => format!("expected trait."),
            ErrorMessage::ExpectedType(got) => format!("type expected but got {}.", got),
            ErrorMessage::ExpectedIdentifier(tok) => {
                format!("identifier expected but got {}.", tok)
            }
            ErrorMessage::ExpectedSomeIdentifier => "identifier expected.".into(),
            ErrorMessage::ExpectedModule => "module expected.".into(),
            ErrorMessage::ExpectedPath => "path expected.".into(),
            ErrorMessage::PatternTupleExpected(ty) => {
                format!("tuple expected but got type {}.", ty)
            }
            ErrorMessage::WrongType(expected, got) => {
                format!("{} expected but got type {}.", expected, got)
            }
            ErrorMessage::PatternTupleLengthMismatch(ty, ty_length, pattern_length) => {
                format!(
                    "tuple {} has {} elements but pattern has {}.",
                    ty, ty_length, pattern_length
                )
            }
            ErrorMessage::ExpectedTopLevelElement(token) => {
                format!("expected function or class but got {}.", token)
            }
            ErrorMessage::ExpectedClassElement(token) => {
                format!("field or method expected but got {}.", token)
            }
            ErrorMessage::ExpectedStringable(ty) => {
                format!("type {} does not implement Stringable.", ty)
            }
            ErrorMessage::MisplacedAnnotation(modifier) => {
                format!("misplaced annotation `{}`.", modifier)
            }
            ErrorMessage::RedundantAnnotation => {
                format!("redundant annotation.")
            }
            ErrorMessage::UnknownAnnotation(token) => format!("unknown annotation {}.", token),
            ErrorMessage::UnknownChar(ch) => {
                format!("unknown character {} (codepoint {}).", ch, *ch as usize)
            }
            ErrorMessage::UnclosedComment => "unclosed comment.".into(),
            ErrorMessage::UnclosedString => "unclosed string.".into(),
            ErrorMessage::UnclosedChar => "unclosed char.".into(),
            ErrorMessage::IoError => "error reading from file.".into(),
            ErrorMessage::MissingFctBody => "missing function body.".into(),
            ErrorMessage::FctCallExpected => format!("function call expected"),
            ErrorMessage::ThisOrSuperExpected(val) => {
                format!("`self` or `super` expected but got {}.", val)
            }
            ErrorMessage::NoSuperDelegationWithPrimaryCtor(name) => format!(
                "no `super` delegation allowed for ctor in class {}, because class has \
                 primary ctor.",
                name
            ),
            ErrorMessage::NoSuperClass(name) => {
                format!("class `{}` does not have super class.", name)
            }
            ErrorMessage::RecursiveStructure => "recursive structure is not allowed.".into(),
            ErrorMessage::TraitMethodWithBody => {
                "trait method is not allowed to have definition".into()
            }
            ErrorMessage::TypeParamsExpected => "type params expected.".into(),
            ErrorMessage::TypeParamNameNotUnique(name) => {
                format!("type param `{}` name already used.", name)
            }
            ErrorMessage::StaticMethodNotInTrait(trait_name, mtd_name, args) => {
                let args = args.join(", ");

                format!(
                    "trait `{}` does not define static method `{}({})`.",
                    trait_name, mtd_name, args
                )
            }
            ErrorMessage::ElementNotInTrait => "element not found in trait.".into(),
            ErrorMessage::ElementNotInImpl(name) => format!("`{}` not found in impl.", name),
            ErrorMessage::MethodMissingFromTrait(trait_name, mtd_name, args) => {
                let args = args.join(", ");

                format!(
                    "trait `{}` defines method `{}({})` but is missing in `impl`.",
                    trait_name, mtd_name, args
                )
            }
            ErrorMessage::WrongNumberTypeParams(exp, actual) => {
                format!("expected {} type parameters but got {}.", exp, actual)
            }
            ErrorMessage::UnconstrainedTypeParam(name) => {
                format!("unconstrained type param `{}`.", name)
            }
            ErrorMessage::StaticMethodCallTargetExpected => {
                "expected static method call target.".into()
            }
            ErrorMessage::ExpectedExtensionType => "cannot extend this type.".into(),
            ErrorMessage::BoundExpected => "class or trait bound expected".into(),
            ErrorMessage::NoTypeParamsExpected => "no type params allowed".into(),
            ErrorMessage::TypeNotImplementingTrait(name, trait_) => {
                format!("type `{}` does not implement trait `{}`.", name, trait_)
            }
            ErrorMessage::AbstractMethodWithImplementation => {
                "abstract methods cannot be implemented.".into()
            }
            ErrorMessage::AbstractMethodNotInAbstractClass => {
                "abstract methods only allowed in abstract classes.".into()
            }
            ErrorMessage::NewAbstractClass => "cannot create object of abstract class.".into(),
            ErrorMessage::MissingAbstractOverride(cls, name) => format!(
                "missing override of abstract method `{}` in class `{}`.",
                cls, name
            ),
            ErrorMessage::ModifierNotAllowedForStaticMethod(modifier) => {
                format!("modifier `{}` not allowed for static method.", modifier)
            }
            ErrorMessage::InvalidTestAnnotationUsage => "invalid usage of @Test annotation.".into(),
            ErrorMessage::GlobalInitializerNotSupported => {
                "global variables do no support initial assignment for now.".into()
            }
            ErrorMessage::TypeNotUsableInForIn(ty) => format!(
                "type `{}` doesn't implement IntoIterator or Iterator trait.",
                ty
            ),
            ErrorMessage::UnknownStructField(struc, field) => {
                format!("struct `{}` does not have field named `{}`.", struc, field)
            }
            ErrorMessage::UnknownIdentifierInModule(module, element) => format!(
                "module `{}` does not contain identifier `{}`.",
                module, element
            ),
            ErrorMessage::StructFieldNotInitialized(struc, field) => {
                format!("field `{}` in struct `{}` not initialized.", field, struc)
            }
            ErrorMessage::InvalidLeftSideOfSeparator => {
                "left hand side of separator is not a class.".into()
            }
            ErrorMessage::InvalidUseOfTypeParams => {
                "type params need to be used on class or function.".into()
            }
            ErrorMessage::NameOfStaticMethodExpected => "name of static method expected.".into(),
            ErrorMessage::IfBranchTypesIncompatible(then_block, else_block) => format!(
                "if-branches have incompatible types `{}` and `{}`.",
                then_block, else_block
            ),
            ErrorMessage::MatchBranchTypesIncompatible(expected_ty, value_ty) => format!(
                "match arms have incompatible types `{}` and `{}`.",
                expected_ty, value_ty
            ),
            ErrorMessage::NameExpected => "name expected for dot-operator.".into(),
            ErrorMessage::IndexExpected => "index expected as right-hand-side for tuple.".into(),
            ErrorMessage::IllegalTupleIndex(idx, ty) => {
                format!("illegal index `{}` for type `{}`", idx, ty)
            }
            ErrorMessage::UninitializedVar => "cannot read uninitialized variable.".into(),
            ErrorMessage::DirectoryNotFound(path) => {
                format!("directory `{:?}` not found.", path)
            }
            ErrorMessage::FileForModuleNotFound => "file for module not found.".into(),
            ErrorMessage::FileNoAccess(path) => {
                format!("could not read file `{}`.", path.display())
            }
            ErrorMessage::FileDoesNotExist(path) => {
                format!("file `{}` does not exist.", path.display())
            }
            ErrorMessage::Custom(msg) => msg.clone(),
            ErrorMessage::Descriptor(msg, args) => {
                crate::error::diagnostics::format_message(msg, args, sa)
            }
            ErrorMessage::MissingFileArgument => format!("no file argument given."),
            ErrorMessage::PackageAlreadyExists(name) => {
                format!("A package with name `{}` already exists.", name)
            }
            ErrorMessage::UnknownPackage(name) => {
                format!("no package with name `{}` was found.", name)
            }
            ErrorMessage::InvalidCharLiteral => {
                format!("invalid char literal.")
            }
            ErrorMessage::InvalidEscapeSequence => {
                format!("invalid escape sequence.")
            }
            ErrorMessage::InvalidReturn => {
                format!("`return` cannot be used in this context.")
            }
            ErrorMessage::PatternAltWithBindingUnsupported => {
                format!(
                    "Multiple patterns with arguments in a `match` arm are currently not supported."
                )
            }
            ErrorMessage::UseNotAccessible => format!("`use` not accessible."),
            ErrorMessage::TypeAliasMissingType => "type alias needs type assignment.".into(),
            ErrorMessage::ImplMethodDefinitionMismatch => {
                "impl method does not match definition in trait.".into()
            }
            ErrorMessage::AliasCycle => "Alias cycle detected.".into(),
            ErrorMessage::UnexpectedTypeBounds => "unexepcted type bounds.".into(),
            ErrorMessage::PatternTypeMismatch(ty) => {
                format!("Pattern does not match type {}", ty)
            }
            ErrorMessage::PatternDuplicateBinding => format!("duplicate binding in pattern."),
            ErrorMessage::PatternBindingWrongType(ty, expected_ty) => {
                format!("binding has type {} but type {} expected.", ty, expected_ty)
            }
            ErrorMessage::PatternBindingNotDefinedInAllAlternatives(name) => {
                format!("binding `{}` not defined in all bindings", name)
            }
            ErrorMessage::PatternUnexpectedRest => format!("Rest pattern is not allowed here."),
            ErrorMessage::PatternMultipleRest => {
                format!("Rest pattern is only allowed once but used multiple times.")
            }
            ErrorMessage::ExtendingTypeDifferentPackage => {
                format!("Can not extend types defined in another package.")
            }
            ErrorMessage::ImplTraitForeignType => {
                format!("Cannot implement foreign trait for a type of another package.")
            }
            ErrorMessage::TraitNotObjectSafe => format!("Trait not object safe"),
            ErrorMessage::UnexpectedTypeBinding => format!("Type binding not allowed here."),
            ErrorMessage::UnknownTypeBinding => format!("Type binding not allowed here."),
            ErrorMessage::TypeBindingOrder => {
                format!("Generic arguments should be ordered before type bindings.")
            }
            ErrorMessage::DuplicateTypeBinding => {
                format!("Type binding for this name already exists.")
            }
            ErrorMessage::MissingTypeBinding(name) => {
                format!("Missing type binding `{}`.", name)
            }
            ErrorMessage::ExpectedTypeName => format!("Type name expected."),
            ErrorMessage::UnknownAssoc => format!("Unknown associated type."),
            ErrorMessage::UnexpectedAssoc => format!("No associated types in this context."),
            ErrorMessage::UnexpectedWhere => format!("Where clauses not allowed here."),
            ErrorMessage::UnexpectedNamedArgument => format!("Named argument not expected here."),
            ErrorMessage::UnexpectedPositionalArgument => {
                format!("Positional argument not allowed anymore after named argument.")
            }
            ErrorMessage::DuplicateNamedArgument => {
                format!("Named argument with that name already exists.")
            }
            ErrorMessage::MissingNamedArgument(name) => {
                format!("Named argument `{}` is missing.", name)
            }
            ErrorMessage::UseOfUnknownArgument => {
                format!("Named argument with this name does not exist.")
            }
            ErrorMessage::CallRequiresNamedArgument => {
                format!("Call requires named arguments.")
            }
            ErrorMessage::WrongTypeForArgument(exp, got) => {
                format!(
                    "Argument expects value of type `{}` but got `{}`.",
                    exp, got
                )
            }
            ErrorMessage::SuperfluousArgument => {
                format!("Superfluous argument.")
            }
            ErrorMessage::MissingArguments(expected, got) => {
                format!(
                    "Call should have {} arguments but got only {}.",
                    expected, got
                )
            }
            ErrorMessage::FieldShouldBeUnnamed => {
                format!("Field access should be unnamed.")
            }
            ErrorMessage::OldClassDefinition => {
                format!("Switch code to new class definition syntax.")
            }
            ErrorMessage::PatternRestShouldBeLast => {
                format!("Rest pattern should be last.")
            }
            ErrorMessage::ExpectedNamedPattern => {
                format!("Expected named pattern field.")
            }
            ErrorMessage::IndexGetNotImplemented(ty) => {
                format!("Type `{}` does not implement trait IndexGet.", ty)
            }
            ErrorMessage::IndexSetNotImplemented(ty) => {
                format!("Type `{}` does not implement trait IndexGet.", ty)
            }
            ErrorMessage::IndexGetAndIndexSetDoNotMatch => {
                format!("`IndexGet` and `IndexSet` do not match for both `Index` and `Item`.")
            }
            ErrorMessage::MissingAssocType(name) => {
                format!("Missing associated type `{}`.", name)
            }
            ErrorMessage::NameBoundMultipleTimesInParams(name) => {
                format!("Name `{}` bound multiple times in parameter list.", name)
            }
            ErrorMessage::InvalidType => format!("Invalid type reference."),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ErrorDescriptor {
    pub file_id: Option<SourceFileId>,
    pub span: Option<Span>,
    pub level: ErrorLevel,
    pub msg: ErrorMessage,
}

impl ErrorDescriptor {
    pub fn new(
        file: SourceFileId,
        span: Span,
        level: ErrorLevel,
        msg: ErrorMessage,
    ) -> ErrorDescriptor {
        ErrorDescriptor {
            file_id: Some(file),
            span: Some(span),
            level,
            msg,
        }
    }

    pub fn new_without_location(msg: ErrorMessage) -> ErrorDescriptor {
        ErrorDescriptor {
            file_id: None,
            span: None,
            level: crate::ErrorLevel::Error,
            msg,
        }
    }

    pub fn line_column(&self, sa: &Sema) -> Option<(u32, u32)> {
        if let Some(file_id) = self.file_id {
            let file = sa.file(file_id);
            let span = self.span.expect("missing location");
            Some(compute_line_column(&file.line_starts, span.start()))
        } else {
            None
        }
    }
}
