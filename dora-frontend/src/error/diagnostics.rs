use super::msg::ErrorLevel;

#[derive(Debug)]
pub struct DiagnosticDescriptor {
    pub message: &'static str,
    pub level: ErrorLevel,
}

pub static UNIMPLEMENTED: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "feature not implemented yet.",
    level: ErrorLevel::Error,
};

pub static UNKNOWN_CLASS: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "class `{0}` does not exist.",
    level: ErrorLevel::Error,
};

pub static UNKNOWN_IDENTIFIER: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "unknown identifier `{0}`.",
    level: ErrorLevel::Error,
};

pub static UNKNOWN_STRUCT: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "unknown struct `{0}`.",
    level: ErrorLevel::Error,
};

pub static UNKNOWN_FUNCTION: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "unknown function `{0}`",
    level: ErrorLevel::Error,
};

pub static UNKNOWN_METHOD: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "no method with name `{1}` in type `{0}`.",
    level: ErrorLevel::Error,
};

pub static UNKNOWN_ENUM_VARIANT: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "no variant with name `{0}` in enumeration.",
    level: ErrorLevel::Error,
};

pub static UNKNOWN_SUFFIX: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "unknown integer suffix",
    level: ErrorLevel::Error,
};

pub static MULTIPLE_CANDIDATES_FOR_METHOD: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "multiple candidates for method named `{1}` in type `{0}`.",
    level: ErrorLevel::Error,
};

pub static VARIADIC_PARAMETER_NEEDS_TO_BE_LAST: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "variadic parameter needs to be last.",
    level: ErrorLevel::Error,
};

pub static UNKNOWN_METHOD_FOR_TYPE_PARAM: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "no method with this name found for type param.",
    level: ErrorLevel::Error,
};

pub static MULTIPLE_CANDIDATES_FOR_TYPE_PARAM: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "multiple methods with this name found for type param.",
    level: ErrorLevel::Error,
};

pub static MULTIPLE_CANDIDATES_FOR_STATIC_METHOD_WITH_TYPE_PARAM: DiagnosticDescriptor =
    DiagnosticDescriptor {
        message: "multiple candidates for static method call found.",
        level: ErrorLevel::Error,
    };

pub static UNKNOWN_STATIC_METHOD_WITH_TYPE_PARAM: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "no static method with this name found for type param.",
    level: ErrorLevel::Error,
};

pub static MULTIPLE_CANDIDATES_FOR_ASSOC_TYPE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "multiple associated types with this name found for type param.",
    level: ErrorLevel::Error,
};

pub static AMBIGUOUS_ASSOC_TYPE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "associated type `{0}` is ambiguous, use explicit syntax instead.",
    level: ErrorLevel::Error,
};

pub static UNKNOWN_STATIC_METHOD: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "no static method of name `{1}` for type `{0}`.",
    level: ErrorLevel::Error,
};

pub static UNEXPECTED_TYPE_ALIAS_ASSIGNMENT: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "no type expected.",
    level: ErrorLevel::Error,
};

pub static UNKNOWN_CTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "class does not have constructor.",
    level: ErrorLevel::Error,
};

pub static ALIAS_EXISTS: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "element with name `{0}` already exists at {1}.",
    level: ErrorLevel::Error,
};

pub static TYPE_EXISTS: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "method with name `{0}` already exists at {1}.",
    level: ErrorLevel::Error,
};

pub static INCOMPATIBLE_WITH_NIL: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "cannot assign `nil` to type `{0}`.",
    level: ErrorLevel::Error,
};

pub static UNKNOWN_FIELD: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "unknown field `{0}` for type `{1}`",
    level: ErrorLevel::Error,
};

pub static IDENTIFIER_EXISTS: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "can not redefine identifier `{0}`.",
    level: ErrorLevel::Error,
};

pub static SHADOW_FUNCTION: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "can not shadow function `{0}`.",
    level: ErrorLevel::Error,
};

pub static SHADOW_PARAM: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "can not shadow param `{0}`.",
    level: ErrorLevel::Error,
};

pub static SHADOW_CLASS: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "can not shadow class `{0}`.",
    level: ErrorLevel::Error,
};

pub static SHADOW_CLASS_CONSTRUCTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "can not shadow constructor of class `{0}`.",
    level: ErrorLevel::Error,
};

pub static SHADOW_STRUCT: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "can not shadow struct `{0}`.",
    level: ErrorLevel::Error,
};

pub static SHADOW_STRUCT_CONSTRUCTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "can not shadow constructor of struct `{0}`.",
    level: ErrorLevel::Error,
};

pub static SHADOW_TRAIT: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "can not shadow trait `{0}`.",
    level: ErrorLevel::Error,
};

pub static SHADOW_FIELD: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "field with name `{0}` already exists.",
    level: ErrorLevel::Error,
};

pub static SHADOW_GLOBAL: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "can not shadow global variable `{0}`.",
    level: ErrorLevel::Error,
};

pub static SHADOW_MODULE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "can not shadow mod `{0}`.",
    level: ErrorLevel::Error,
};

pub static SHADOW_CONST: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "can not shadow const `{0}`.",
    level: ErrorLevel::Error,
};

pub static SHADOW_ENUM: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "can not shadow enum `{0}`.",
    level: ErrorLevel::Error,
};

pub static SHADOW_ENUM_VARIANT: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "can not shadow enum variant `{0}`.",
    level: ErrorLevel::Error,
};

pub static SHADOW_TYPE_PARAM: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "can not shadow type param `{0}`.",
    level: ErrorLevel::Error,
};

pub static NO_ENUM_VARIANT: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "enum needs at least one variant.",
    level: ErrorLevel::Error,
};

pub static ENUM_ARGS_INCOMPATIBLE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "enum `{0}::{1}({2})` cannot be called as `{1}({3})`",
    level: ErrorLevel::Error,
};

pub static STRUCT_ARGS_INCOMPATIBLE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "struct `{0}({1})` cannot be called as `{0}({2})`",
    level: ErrorLevel::Error,
};

pub static IMMUTABLE_FIELD: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Fields of this type are immutable.",
    level: ErrorLevel::Error,
};

pub static UNEXPECTED_ARGUMENTS_FOR_ENUM_VARIANT: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Enum variant does not have any arguments",
    level: ErrorLevel::Error,
};

pub static ENUM_VARIANT_MISSING_ARGUMENTS: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Fields missing for enum variant.",
    level: ErrorLevel::Error,
};

pub static PATTERN_NO_PARENS: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "pattern should be used without parens.",
    level: ErrorLevel::Error,
};

pub static PATTERN_WRONG_NUMBER_OF_PARAMS: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "pattern expects {0} params but got {1}.",
    level: ErrorLevel::Error,
};

pub static ENUM_EXPECTED: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "enum expected.",
    level: ErrorLevel::Error,
};

pub static ENUM_MISMATCH: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "value of type {0} but pattern of type {1}.",
    level: ErrorLevel::Error,
};

pub static ENUM_VARIANT_EXPECTED: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "enum variant expected.",
    level: ErrorLevel::Error,
};

pub static NON_EXHAUSTIVE_MATCH: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "`match` does not cover all possible values. Missing patterns: {0}",
    level: ErrorLevel::Error,
};

pub static USELESS_PATTERN: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "unreachable pattern.",
    level: ErrorLevel::Error,
};

pub static VAR_NEEDS_TYPE_OR_EXPRESSION: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "variable needs either type declaration or expression.",
    level: ErrorLevel::Error,
};

pub static PARAM_TYPES_INCOMPATIBLE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "function `{0}({1})` cannot be called as `{0}({2})`",
    level: ErrorLevel::Error,
};

pub static LAMBDA_PARAM_TYPES_INCOMPATIBLE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "lambda `({0})` cannot be called with `({1})`",
    level: ErrorLevel::Error,
};

pub static LAMBDA_PARAM_MISSING_TYPE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "cannot infer type for lambda parameter.",
    level: ErrorLevel::Error,
};

pub static LAMBDA_PARAM_COUNT_MISMATCH: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "lambda has {0} parameters but {1} were expected.",
    level: ErrorLevel::Error,
};

pub static WHILE_COND_TYPE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "`while` expects condition of type `bool` but got `{0}`.",
    level: ErrorLevel::Error,
};

pub static IF_COND_TYPE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "`if` expects condition of type `bool` but got `{0}`.",
    level: ErrorLevel::Error,
};

pub static RETURN_TYPE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "`return` expects value of type `{0}` but got `{1}`.",
    level: ErrorLevel::Error,
};

pub static LVALUE_EXPECTED: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "lvalue expected for assignment",
    level: ErrorLevel::Error,
};

pub static VALUE_EXPECTED: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "value expected",
    level: ErrorLevel::Error,
};

pub static ASSIGN_TYPE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "cannot assign `{1}` to variable of type `{0}`.",
    level: ErrorLevel::Error,
};

pub static ASSIGN_FIELD: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "cannot assign `{3}` to field `{1}`.`{0}` of type `{2}`.",
    level: ErrorLevel::Error,
};

pub static UN_OP_TYPE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "unary operator `{0}` can not handle value of type `{0} {1}`.",
    level: ErrorLevel::Error,
};

pub static NEGATIVE_UNSIGNED: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "cannot use `-` with UInt8",
    level: ErrorLevel::Error,
};

pub static BIN_OP_TYPE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "binary operator `{0}` can not handle expression of type `{1} {0} {2}`",
    level: ErrorLevel::Error,
};

pub static CONST_VALUE_EXPECTED: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "constant value expected",
    level: ErrorLevel::Error,
};

pub static OUTSIDE_LOOP: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "statement only allowed inside loops",
    level: ErrorLevel::Error,
};

pub static NO_RETURN_VALUE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "function does not return a value in all code paths",
    level: ErrorLevel::Error,
};

pub static MAIN_NOT_FOUND: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "no `main` function found in the program",
    level: ErrorLevel::Error,
};

pub static WRONG_MAIN_DEFINITION: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "`main` function has wrong definition",
    level: ErrorLevel::Error,
};

pub static THIS_UNAVAILABLE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "`self` can only be used in methods not functions",
    level: ErrorLevel::Error,
};

pub static SELF_TYPE_UNAVAILABLE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "`Self` can only be used in traits.",
    level: ErrorLevel::Error,
};

pub static SUPER_UNAVAILABLE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "`super` only available in methods of classes with parent class",
    level: ErrorLevel::Error,
};

pub static SUPER_NEEDS_METHOD_CALL: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "`super` only allowed in method calls",
    level: ErrorLevel::Error,
};

pub static TRAIT_EXPECTED: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "`{0}` is not a trait.",
    level: ErrorLevel::Error,
};

pub static NO_SUPER_MODULE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "no super module.",
    level: ErrorLevel::Error,
};

pub static SUPER_AS_VALUE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "`super` cannot be used as a value.",
    level: ErrorLevel::Error,
};

pub static PACKAGE_AS_VALUE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "`package` cannot be used as a value.",
    level: ErrorLevel::Error,
};

pub static NOT_ACCESSIBLE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "element is not accessible.",
    level: ErrorLevel::Error,
};

pub static STRUCT_CONSTRUCTOR_NOT_ACCESSIBLE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "constructor of struct `{0}` is not accessible.",
    level: ErrorLevel::Error,
};

pub static CLASS_CONSTRUCTOR_NOT_ACCESSIBLE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "constructor of class `{0}` is not accessible.",
    level: ErrorLevel::Error,
};

pub static NOT_ACCESSIBLE_IN_MODULE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "`{1}` in module `{0}` is not accessible.",
    level: ErrorLevel::Error,
};

pub static LET_MISSING_INITIALIZATION: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "`let` binding is missing initialization.",
    level: ErrorLevel::Error,
};

pub static LET_REASSIGNED: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "`let` binding cannot be reassigned.",
    level: ErrorLevel::Error,
};

pub static INVALID_LHS_ASSIGNMENT: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "invalid left-hand-side of assignment.",
    level: ErrorLevel::Error,
};

pub static UNDERIVABLE_TYPE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "type `{0}` cannot be used as super class.",
    level: ErrorLevel::Error,
};

pub static CYCLE_IN_HIERARCHY: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "cycle in type hierarchy detected.",
    level: ErrorLevel::Error,
};

pub static SUPERFLUOUS_OVERRIDE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "method `{0}` uses modifier `override` without overriding a function.",
    level: ErrorLevel::Error,
};

pub static MISSING_OVERRIDE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "method `{0}` is missing modifier `override`.",
    level: ErrorLevel::Error,
};

pub static SUPERFLUOUS_OPEN: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "method `{0}` uses modifier `open` but class allows no subclasses.",
    level: ErrorLevel::Error,
};

pub static METHOD_NOT_OVERRIDABLE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "method `{0}` in super class not overridable.",
    level: ErrorLevel::Error,
};

pub static TYPES_INCOMPATIBLE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "types `{0}` and `{1}` incompatible.",
    level: ErrorLevel::Error,
};

pub static EXPECTED_IDENTITY_TYPE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "type `{0}` does not have identity.",
    level: ErrorLevel::Error,
};

pub static RETURN_TYPE_MISMATCH: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "return types `{0}` and `{1}` do not match.",
    level: ErrorLevel::Error,
};

pub static OVERRIDE_MISMATCH: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "definition does not match overriden function.",
    level: ErrorLevel::Error,
};

pub static UNRESOLVED_INTERNAL: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "unresolved internal.",
    level: ErrorLevel::Error,
};

pub static MISPLACED_ELSE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "misplace else.",
    level: ErrorLevel::Error,
};

pub static EXPECTED_TOKEN: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "expected {0} but got {1}.",
    level: ErrorLevel::Error,
};

pub static NUMBER_OVERFLOW: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "number does not fit into type {0}.",
    level: ErrorLevel::Error,
};

pub static NUMBER_LIMIT_OVERFLOW: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "number exceeds maximum value.",
    level: ErrorLevel::Error,
};

pub static INVALID_SUFFIX: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "invalid suffix `{0}`.",
    level: ErrorLevel::Error,
};

pub static INVALID_NUMBER_FORMAT: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "invalid number format.",
    level: ErrorLevel::Error,
};

pub static EXPECTED_CLASS: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "expected class name but got {0}.",
    level: ErrorLevel::Error,
};

pub static EXPECTED_METHOD: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "expected method.",
    level: ErrorLevel::Error,
};

pub static EXPECTED_FACTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "factor expected but got {0}.",
    level: ErrorLevel::Error,
};

pub static EXPECTED_TRAIT: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "expected trait.",
    level: ErrorLevel::Error,
};

pub static EXPECTED_TYPE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "type expected but got {0}.",
    level: ErrorLevel::Error,
};

pub static EXPECTED_IDENTIFIER: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "identifier expected but got {0}.",
    level: ErrorLevel::Error,
};

pub static EXPECTED_SOME_IDENTIFIER: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "identifier expected.",
    level: ErrorLevel::Error,
};

pub static EXPECTED_MODULE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "module expected.",
    level: ErrorLevel::Error,
};

pub static EXPECTED_PATH: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "path expected.",
    level: ErrorLevel::Error,
};

pub static PATTERN_TUPLE_EXPECTED: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "tuple expected but got type {0}.",
    level: ErrorLevel::Error,
};

pub static WRONG_TYPE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "{0} expected but got type {1}.",
    level: ErrorLevel::Error,
};

pub static PATTERN_TUPLE_LENGTH_MISMATCH: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "tuple {0} has {1} elements but pattern has {2}.",
    level: ErrorLevel::Error,
};

pub static EXPECTED_TOP_LEVEL_ELEMENT: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "expected function or class but got {0}.",
    level: ErrorLevel::Error,
};

pub static EXPECTED_CLASS_ELEMENT: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "field or method expected but got {0}.",
    level: ErrorLevel::Error,
};

pub static EXPECTED_STRINGABLE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "type {0} does not implement Stringable.",
    level: ErrorLevel::Error,
};

pub static MISPLACED_ANNOTATION: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "misplaced annotation `{0}`.",
    level: ErrorLevel::Error,
};

pub static REDUNDANT_ANNOTATION: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "redundant annotation.",
    level: ErrorLevel::Error,
};

pub static UNKNOWN_ANNOTATION: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "unknown annotation {0}.",
    level: ErrorLevel::Error,
};

pub static UNKNOWN_CHAR: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "unknown character {0} (codepoint {1}).",
    level: ErrorLevel::Error,
};

pub static UNCLOSED_COMMENT: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "unclosed comment.",
    level: ErrorLevel::Error,
};

pub static UNCLOSED_STRING: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "unclosed string.",
    level: ErrorLevel::Error,
};

pub static UNCLOSED_CHAR: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "unclosed char.",
    level: ErrorLevel::Error,
};

pub static IO_ERROR: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "error reading from file.",
    level: ErrorLevel::Error,
};

pub static MISSING_FCT_BODY: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "missing function body.",
    level: ErrorLevel::Error,
};

pub static FCT_CALL_EXPECTED: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "function call expected",
    level: ErrorLevel::Error,
};

pub static THIS_OR_SUPER_EXPECTED: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "`self` or `super` expected but got {0}.",
    level: ErrorLevel::Error,
};

pub static NO_SUPER_DELEGATION_WITH_PRIMARY_CTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "no `super` delegation allowed for ctor in class {0}, because class has primary ctor.",
    level: ErrorLevel::Error,
};

pub static NO_SUPER_CLASS: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "class `{0}` does not have super class.",
    level: ErrorLevel::Error,
};

pub static RECURSIVE_STRUCTURE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "recursive structure is not allowed.",
    level: ErrorLevel::Error,
};

pub static TRAIT_METHOD_WITH_BODY: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "trait method is not allowed to have definition",
    level: ErrorLevel::Error,
};

pub static TYPE_PARAMS_EXPECTED: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "type params expected.",
    level: ErrorLevel::Error,
};

pub static TYPE_PARAM_NAME_NOT_UNIQUE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "type param `{0}` name already used.",
    level: ErrorLevel::Error,
};

pub static STATIC_METHOD_NOT_IN_TRAIT: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "trait `{0}` does not define static method `{1}({2})`.",
    level: ErrorLevel::Error,
};

pub static ELEMENT_NOT_IN_TRAIT: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "element not found in trait.",
    level: ErrorLevel::Error,
};

pub static ELEMENT_NOT_IN_IMPL: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "`{0}` not found in impl.",
    level: ErrorLevel::Error,
};

pub static METHOD_MISSING_FROM_TRAIT: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "trait `{0}` defines method `{1}({2})` but is missing in `impl`.",
    level: ErrorLevel::Error,
};

pub static WRONG_NUMBER_TYPE_PARAMS: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "expected {0} type parameters but got {1}.",
    level: ErrorLevel::Error,
};

pub static UNCONSTRAINED_TYPE_PARAM: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "unconstrained type param `{0}`.",
    level: ErrorLevel::Error,
};

pub static STATIC_METHOD_CALL_TARGET_EXPECTED: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "expected static method call target.",
    level: ErrorLevel::Error,
};

pub static EXPECTED_EXTENSION_TYPE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "cannot extend this type.",
    level: ErrorLevel::Error,
};

pub static BOUND_EXPECTED: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "class or trait bound expected",
    level: ErrorLevel::Error,
};

pub static NO_TYPE_PARAMS_EXPECTED: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "no type params allowed",
    level: ErrorLevel::Error,
};

pub static TYPE_NOT_IMPLEMENTING_TRAIT: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "type `{0}` does not implement trait `{1}`.",
    level: ErrorLevel::Error,
};

pub static MISSING_TYPE_PARAM_BOUND: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "missing bound `{0}` on type parameter `{1}`.",
    level: ErrorLevel::Error,
};

pub static EXTRA_TYPE_PARAM_BOUND: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "extra bound `{0}` on type parameter `{1}` not in trait.",
    level: ErrorLevel::Error,
};

pub static ABSTRACT_METHOD_WITH_IMPLEMENTATION: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "abstract methods cannot be implemented.",
    level: ErrorLevel::Error,
};

pub static ABSTRACT_METHOD_NOT_IN_ABSTRACT_CLASS: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "abstract methods only allowed in abstract classes.",
    level: ErrorLevel::Error,
};

pub static NEW_ABSTRACT_CLASS: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "cannot create object of abstract class.",
    level: ErrorLevel::Error,
};

pub static MISSING_ABSTRACT_OVERRIDE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "missing override of abstract method `{0}` in class `{1}`.",
    level: ErrorLevel::Error,
};

pub static MODIFIER_NOT_ALLOWED_FOR_STATIC_METHOD: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "modifier `{0}` not allowed for static method.",
    level: ErrorLevel::Error,
};

pub static INVALID_TEST_ANNOTATION_USAGE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "invalid usage of @Test annotation.",
    level: ErrorLevel::Error,
};

pub static GLOBAL_INITIALIZER_NOT_SUPPORTED: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "global variables do no support initial assignment for now.",
    level: ErrorLevel::Error,
};

pub static TYPE_NOT_USABLE_IN_FOR_IN: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "type `{0}` doesn't implement IntoIterator or Iterator trait.",
    level: ErrorLevel::Error,
};

pub static UNKNOWN_STRUCT_FIELD: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "struct `{0}` does not have field named `{1}`.",
    level: ErrorLevel::Error,
};

pub static UNKNOWN_IDENTIFIER_IN_MODULE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "module `{0}` does not contain identifier `{1}`.",
    level: ErrorLevel::Error,
};

pub static STRUCT_FIELD_NOT_INITIALIZED: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "field `{1}` in struct `{0}` not initialized.",
    level: ErrorLevel::Error,
};

pub static INVALID_LEFT_SIDE_OF_SEPARATOR: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "left hand side of separator is not a class.",
    level: ErrorLevel::Error,
};

pub static INVALID_USE_OF_TYPE_PARAMS: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "type params need to be used on class or function.",
    level: ErrorLevel::Error,
};

pub static NAME_OF_STATIC_METHOD_EXPECTED: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "name of static method expected.",
    level: ErrorLevel::Error,
};

pub static IF_BRANCH_TYPES_INCOMPATIBLE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "if-branches have incompatible types `{0}` and `{1}`.",
    level: ErrorLevel::Error,
};

pub static MATCH_BRANCH_TYPES_INCOMPATIBLE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "match arms have incompatible types `{0}` and `{1}`.",
    level: ErrorLevel::Error,
};

pub static NAME_EXPECTED: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "name expected for dot-operator.",
    level: ErrorLevel::Error,
};

pub static INDEX_EXPECTED: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "index expected as right-hand-side for tuple.",
    level: ErrorLevel::Error,
};

pub static ILLEGAL_TUPLE_INDEX: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "illegal index `{0}` for type `{1}`",
    level: ErrorLevel::Error,
};

pub static UNINITIALIZED_VAR: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "cannot read uninitialized variable.",
    level: ErrorLevel::Error,
};

pub static DIRECTORY_NOT_FOUND: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "directory `{0}` not found.",
    level: ErrorLevel::Error,
};

pub static FILE_FOR_MODULE_NOT_FOUND: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "file for module not found.",
    level: ErrorLevel::Error,
};

pub static FILE_NO_ACCESS: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "could not read file `{0}`.",
    level: ErrorLevel::Error,
};

pub static FILE_DOES_NOT_EXIST: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "file `{0}` does not exist.",
    level: ErrorLevel::Error,
};

pub static CUSTOM: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "{0}",
    level: ErrorLevel::Error,
};

pub static MISSING_FILE_ARGUMENT: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "no file argument given.",
    level: ErrorLevel::Error,
};

pub static PACKAGE_ALREADY_EXISTS: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "A package with name `{0}` already exists.",
    level: ErrorLevel::Error,
};

pub static UNKNOWN_PACKAGE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "no package with name `{0}` was found.",
    level: ErrorLevel::Error,
};

pub static INVALID_CHAR_LITERAL: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "invalid char literal.",
    level: ErrorLevel::Error,
};

pub static INVALID_ESCAPE_SEQUENCE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "invalid escape sequence.",
    level: ErrorLevel::Error,
};

pub static INVALID_RETURN: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "`return` cannot be used in this context.",
    level: ErrorLevel::Error,
};

pub static PATTERN_ALT_WITH_BINDING_UNSUPPORTED: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Multiple patterns with arguments in a `match` arm are currently not supported.",
    level: ErrorLevel::Error,
};

pub static USE_NOT_ACCESSIBLE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "`use` not accessible.",
    level: ErrorLevel::Error,
};

pub static TYPE_ALIAS_MISSING_TYPE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "type alias needs type assignment.",
    level: ErrorLevel::Error,
};

pub static IMPL_METHOD_DEFINITION_MISMATCH: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "impl method does not match definition in trait.",
    level: ErrorLevel::Error,
};

pub static ALIAS_CYCLE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Alias cycle detected.",
    level: ErrorLevel::Error,
};

pub static UNEXPECTED_TYPE_BOUNDS: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "unexepcted type bounds.",
    level: ErrorLevel::Error,
};

pub static PATTERN_TYPE_MISMATCH: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Pattern does not match type {0}",
    level: ErrorLevel::Error,
};

pub static PATTERN_DUPLICATE_BINDING: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "duplicate binding in pattern.",
    level: ErrorLevel::Error,
};

pub static PATTERN_BINDING_WRONG_TYPE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "binding has type {0} but type {1} expected.",
    level: ErrorLevel::Error,
};

pub static PATTERN_BINDING_NOT_DEFINED_IN_ALL_ALTERNATIVES: DiagnosticDescriptor =
    DiagnosticDescriptor {
        message: "binding `{0}` not defined in all bindings",
        level: ErrorLevel::Error,
    };

pub static PATTERN_UNEXPECTED_REST: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Rest pattern is not allowed here.",
    level: ErrorLevel::Error,
};

pub static PATTERN_MULTIPLE_REST: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Rest pattern is only allowed once but used multiple times.",
    level: ErrorLevel::Error,
};

pub static EXTENDING_TYPE_DIFFERENT_PACKAGE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Can not extend types defined in another package.",
    level: ErrorLevel::Error,
};

pub static IMPL_TRAIT_FOREIGN_TYPE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Cannot implement foreign trait for a type of another package.",
    level: ErrorLevel::Error,
};

pub static TRAIT_NOT_OBJECT_SAFE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Trait not object safe",
    level: ErrorLevel::Error,
};

pub static UNEXPECTED_TYPE_BINDING: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Type binding not allowed here.",
    level: ErrorLevel::Error,
};

pub static UNKNOWN_TYPE_BINDING: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Type binding not allowed here.",
    level: ErrorLevel::Error,
};

pub static TYPE_BINDING_ORDER: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Generic arguments should be ordered before type bindings.",
    level: ErrorLevel::Error,
};

pub static DUPLICATE_TYPE_BINDING: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Type binding for this name already exists.",
    level: ErrorLevel::Error,
};

pub static MISSING_TYPE_BINDING: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Missing type binding `{0}`.",
    level: ErrorLevel::Error,
};

pub static EXPECTED_TYPE_NAME: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Type name expected.",
    level: ErrorLevel::Error,
};

pub static UNKNOWN_ASSOC: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Unknown associated type.",
    level: ErrorLevel::Error,
};

pub static UNEXPECTED_ASSOC: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "No associated types in this context.",
    level: ErrorLevel::Error,
};

pub static UNEXPECTED_WHERE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Where clauses not allowed here.",
    level: ErrorLevel::Error,
};

pub static UNEXPECTED_NAMED_ARGUMENT: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Named argument not expected here.",
    level: ErrorLevel::Error,
};

pub static UNEXPECTED_POSITIONAL_ARGUMENT: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Positional argument not allowed anymore after named argument.",
    level: ErrorLevel::Error,
};

pub static UNEXPECTED_PATH_SEGMENT: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "unexpected path segment.",
    level: ErrorLevel::Error,
};

pub static DUPLICATE_NAMED_ARGUMENT: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Named argument with that name already exists.",
    level: ErrorLevel::Error,
};

pub static MISSING_NAMED_ARGUMENT: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Named argument `{0}` is missing.",
    level: ErrorLevel::Error,
};

pub static USE_OF_UNKNOWN_ARGUMENT: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Named argument with this name does not exist.",
    level: ErrorLevel::Error,
};

pub static CALL_REQUIRES_NAMED_ARGUMENT: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Call requires named arguments.",
    level: ErrorLevel::Error,
};

pub static WRONG_TYPE_FOR_ARGUMENT: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Argument expects value of type `{0}` but got `{1}`.",
    level: ErrorLevel::Error,
};

pub static SUPERFLUOUS_ARGUMENT: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Superfluous argument.",
    level: ErrorLevel::Error,
};

pub static MISSING_ARGUMENTS: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Call should have {0} arguments but got only {1}.",
    level: ErrorLevel::Error,
};

pub static FIELD_SHOULD_BE_UNNAMED: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Field access should be unnamed.",
    level: ErrorLevel::Error,
};

pub static OLD_CLASS_DEFINITION: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Switch code to new class definition syntax.",
    level: ErrorLevel::Error,
};

pub static PATTERN_REST_SHOULD_BE_LAST: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Rest pattern should be last.",
    level: ErrorLevel::Error,
};

pub static EXPECTED_NAMED_PATTERN: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Expected named pattern field.",
    level: ErrorLevel::Error,
};

pub static INDEX_GET_NOT_IMPLEMENTED: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Type `{0}` does not implement trait IndexGet.",
    level: ErrorLevel::Error,
};

pub static INDEX_SET_NOT_IMPLEMENTED: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Type `{0}` does not implement trait IndexGet.",
    level: ErrorLevel::Error,
};

pub static INDEX_GET_AND_INDEX_SET_DO_NOT_MATCH: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "`IndexGet` and `IndexSet` do not match for both `Index` and `Item`.",
    level: ErrorLevel::Error,
};

pub static MISSING_ASSOC_TYPE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Missing associated type `{0}`.",
    level: ErrorLevel::Error,
};

pub static NAME_BOUND_MULTIPLE_TIMES_IN_PARAMS: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Name `{0}` bound multiple times in parameter list.",
    level: ErrorLevel::Error,
};

pub static INVALID_TYPE: DiagnosticDescriptor = DiagnosticDescriptor {
    message: "Invalid type reference.",
    level: ErrorLevel::Error,
};

use crate::error::DescriptorArgs;
use crate::sema::Sema;

pub fn format_message(message: &str, args: &DescriptorArgs, sa: &Sema) -> String {
    let string_args: Vec<String> = args.iter().map(|arg| arg.to_string(sa)).collect();
    format_message_strings(message, &string_args)
}

pub fn format_message_strings(message: &str, args: &[String]) -> String {
    let mut result = message.to_string();

    for (index, arg) in args.iter().enumerate() {
        let placeholder = format!("{{{}}}", index);
        result = result.replace(&placeholder, arg);
    }

    result
}
