use crate::args;
use crate::error::diagnostics::{
    ALIAS_EXISTS, ASSIGN_FIELD, ASSIGN_TYPE, BIN_OP_TYPE, CLASS_CONSTRUCTOR_NOT_ACCESSIBLE,
    DUPLICATE_NAMED_ARGUMENT, ELEMENT_NOT_IN_IMPL, ELEMENT_NOT_IN_TRAIT,
    ENUM_VARIANT_MISSING_ARGUMENTS, EXPECTED_MODULE, EXPECTED_NAMED_PATTERN, EXPECTED_PATH,
    EXPECTED_STRINGABLE, IF_COND_TYPE, IMMUTABLE_FIELD, IMPL_TRAIT_FOREIGN_TYPE,
    INDEX_GET_AND_INDEX_SET_DO_NOT_MATCH, INDEX_GET_NOT_IMPLEMENTED, INDEX_SET_NOT_IMPLEMENTED,
    INVALID_CHAR_LITERAL, INVALID_ESCAPE_SEQUENCE, INVALID_NUMBER_FORMAT,
    LAMBDA_PARAM_COUNT_MISMATCH, LAMBDA_PARAM_MISSING_TYPE, LET_MISSING_INITIALIZATION,
    LET_REASSIGNED, LVALUE_EXPECTED, MATCH_BRANCH_TYPES_INCOMPATIBLE, MISSING_ARGUMENTS,
    MISSING_ASSOC_TYPE, MISSING_NAMED_ARGUMENT, MULTIPLE_CANDIDATES_FOR_METHOD,
    MULTIPLE_CANDIDATES_FOR_STATIC_METHOD_WITH_TYPE_PARAM, MULTIPLE_CANDIDATES_FOR_TYPE_PARAM,
    NAME_BOUND_MULTIPLE_TIMES_IN_PARAMS, NEGATIVE_UNSIGNED, NO_SUPER_MODULE,
    NO_TYPE_PARAMS_EXPECTED, NOT_ACCESSIBLE, NUMBER_LIMIT_OVERFLOW, NUMBER_OVERFLOW,
    PATTERN_BINDING_NOT_DEFINED_IN_ALL_ALTERNATIVES, PATTERN_BINDING_WRONG_TYPE,
    PATTERN_DUPLICATE_BINDING, PATTERN_MULTIPLE_REST, PATTERN_NO_PARENS,
    PATTERN_REST_SHOULD_BE_LAST, PATTERN_TUPLE_EXPECTED, PATTERN_TYPE_MISMATCH,
    PATTERN_UNEXPECTED_REST, PATTERN_WRONG_NUMBER_OF_PARAMS, RETURN_TYPE, SHADOW_CLASS,
    SHADOW_FUNCTION, SHADOW_TYPE_PARAM, STRUCT_CONSTRUCTOR_NOT_ACCESSIBLE, SUPERFLUOUS_ARGUMENT,
    THIS_UNAVAILABLE, TYPE_NOT_IMPLEMENTING_TRAIT, TYPE_NOT_USABLE_IN_FOR_IN, TYPES_INCOMPATIBLE,
    UN_OP_TYPE, UNEXPECTED_ARGUMENTS_FOR_ENUM_VARIANT, UNEXPECTED_NAMED_ARGUMENT,
    UNEXPECTED_POSITIONAL_ARGUMENT, UNKNOWN_ENUM_VARIANT, UNKNOWN_FIELD, UNKNOWN_IDENTIFIER,
    UNKNOWN_IDENTIFIER_IN_MODULE, UNKNOWN_METHOD, UNKNOWN_METHOD_FOR_TYPE_PARAM,
    UNKNOWN_STATIC_METHOD, UNKNOWN_STATIC_METHOD_WITH_TYPE_PARAM, USE_NOT_ACCESSIBLE,
    USE_OF_UNKNOWN_ARGUMENT, VALUE_EXPECTED, VARIADIC_PARAMETER_NEEDS_TO_BE_LAST, WHILE_COND_TYPE,
    WRONG_NUMBER_TYPE_PARAMS, WRONG_TYPE, WRONG_TYPE_FOR_ARGUMENT,
};
use crate::sema::ConstValue;
use crate::stdlib_lookup::resolve_path;
use crate::tests::*;

#[test]
fn type_method_len() {
    ok("fn f(a: String): Int64 { return a.size(); }");
    ok("fn f(a: String): Int64 { return \"abc\".size(); }");
}

#[test]
fn type_object_field() {
    ok("class Foo{a:Int32} fn f(x: Foo): Int32 { return x.a; }");
    ok("class Foo{a:String} fn f(x: Foo): String { return x.a; }");
    err(
        "class Foo{a:Int32} fn f(x: Foo): Bool { return x.a; }",
        (1, 41),
        10,
        crate::ErrorLevel::Error,
        &RETURN_TYPE,
        args!("Bool", "Int32"),
    );
    err(
        "class Foo{a:Int32} fn f(x: Foo): Int32 { return x.b; }",
        (1, 49),
        3,
        crate::ErrorLevel::Error,
        &UNKNOWN_FIELD,
        args!("b", "Foo"),
    );
}

#[test]
fn type_object_set_field() {
    ok("class Foo{a: Int32} fn f(x: Foo) { x.a = 1; }");
    err(
        "class Foo{a: Int32} fn f(x: Foo) { x.a = false; }",
        (1, 36),
        11,
        crate::ErrorLevel::Error,
        &ASSIGN_TYPE,
        args!("Int32", "Bool"),
    );
}

#[test]
fn type_object_field_without_self() {
    err(
        "class Foo{a: Int32} impl Foo { fn f(): Int32 { return a; } }",
        (1, 55),
        1,
        crate::ErrorLevel::Error,
        &UNKNOWN_IDENTIFIER,
        args!("a"),
    );
    err(
        "class Foo{a: Int32} impl Foo { fn set(x: Int32) { a = x; } }",
        (1, 51),
        1,
        crate::ErrorLevel::Error,
        &UNKNOWN_IDENTIFIER,
        args!("a"),
    );
}

#[test]
fn type_class_method_call() {
    ok("
        class Foo
        impl Foo {
            fn bar() {}
            fn baz(): Int32 { return 1; }
        }

        fn f(x: Foo) { x.bar(); }
        fn g(x: Foo): Int32 { return x.baz(); }");

    err(
        "
        class Foo
        impl Foo {
            fn bar(): Int32 { return 0; }
        }

        fn f(x: Foo): String { return x.bar(); }",
        (7, 32),
        14,
        crate::ErrorLevel::Error,
        &RETURN_TYPE,
        args!("String", "Int32"),
    );
}

#[test]
fn return_type() {
    err(
        "
        class Foo[T]
        fn f(): Foo[Int32] { Foo[Int64]() }
    ",
        (3, 28),
        16,
        crate::ErrorLevel::Error,
        &RETURN_TYPE,
        args!("Foo[Int32]", "Foo[Int64]"),
    );
}

#[test]
fn type_method_defined_twice() {
    err(
        "class Foo
        impl Foo {
                 fn bar() {}
                 fn bar() {}
             }",
        (4, 18),
        11,
        crate::ErrorLevel::Error,
        &ALIAS_EXISTS,
        args!("bar", "main.dora:3:18"),
    );

    err(
        "class Foo
        impl Foo{
                 fn bar() {}
                 fn bar(): Int32 { 0 }
             }",
        (4, 18),
        21,
        crate::ErrorLevel::Error,
        &ALIAS_EXISTS,
        args!("bar", "main.dora:3:18"),
    );

    err(
        "class Foo
        impl Foo {
                 fn bar(a: Int32) {}
                 fn bar(a: Int32): Int32 { 0 }
             }",
        (4, 18),
        29,
        crate::ErrorLevel::Error,
        &ALIAS_EXISTS,
        args!("bar", "main.dora:3:18"),
    );

    err(
        "class Foo
        impl Foo {
                fn bar(a: Int32) {}
                fn bar(a: String) {}
            }",
        (4, 17),
        20,
        crate::ErrorLevel::Error,
        &ALIAS_EXISTS,
        args!("bar", "main.dora:3:17"),
    );
}

#[test]
fn type_self() {
    ok("class Foo impl Foo { fn me(): Foo { return self; } }");
    err(
        "class Foo fn me() { return self; }",
        (1, 28),
        4,
        crate::ErrorLevel::Error,
        &THIS_UNAVAILABLE,
        args!(),
    );

    ok("class Foo{a: Int32, b: Int32}
        impl Foo {
            fn bar(): Int32 { return self.a + self.b; }
        }");

    ok("class Foo{a: Int32}
        impl Foo {
            fn setA(a: Int32) { self.a = a; }
        }");

    ok("class Foo
        impl Foo {
            fn zero(): Int32 { return 0i32; }
            fn other(): Int32 { return self.zero(); }
        }");

    ok("class Foo
        impl Foo {
            fn bar() { self.bar(); }
        }");
}

#[test]
fn type_unknown_method() {
    err(
        "class Foo
            impl Foo {
                 fn bar(a: Int32) { }
            }

            fn f(x: Foo) { x.bar(); }",
        (6, 28),
        7,
        crate::ErrorLevel::Error,
        &MISSING_ARGUMENTS,
        args!(1, 0),
    );

    err(
        "class Foo
              fn f(x: Foo) { x.bar(1i32); }",
        (2, 30),
        11,
        crate::ErrorLevel::Error,
        &UNKNOWN_METHOD,
        args!("Foo", "bar"),
    );
}

#[test]
fn type_ctor() {
    ok("class Foo fn f(): Foo { return Foo(); }");
    ok("class Foo(Int32) fn f(): Foo { return Foo(1i32); }");
    err(
        "class Foo fn f(): Foo { return 1i32; }",
        (1, 25),
        11,
        crate::ErrorLevel::Error,
        &RETURN_TYPE,
        args!("Foo", "Int32"),
    );
}

#[test]
fn type_def_for_return_type() {
    ok("fn a(): Int32 { return 1i32; }");
    err(
        "fn a(): unknown {}",
        (1, 9),
        7,
        crate::ErrorLevel::Error,
        &UNKNOWN_IDENTIFIER,
        args!("unknown"),
    );
}

#[test]
fn type_def_for_param() {
    ok("fn a(b: Int32) {}");
    err(
        "fn a(b: foo) {}",
        (1, 9),
        3,
        crate::ErrorLevel::Error,
        &UNKNOWN_IDENTIFIER,
        args!("foo"),
    );
}

#[test]
fn type_def_for_var() {
    ok("fn a() { let a : Int32 = 1i32; }");
    err(
        "fn a() { let a : test = 1; }",
        (1, 18),
        4,
        crate::ErrorLevel::Error,
        &UNKNOWN_IDENTIFIER,
        args!("test"),
    );
}

#[test]
fn type_var_wrong_type_defined() {
    ok("fn f() { let a : Int32 = 1i32; }");
    ok("fn f() { let a : Bool = false; }");
    ok("fn f() { let a : String = \"f\"; }");

    err(
        "fn f() { let a : Int32 = true; }",
        (1, 10),
        21,
        crate::ErrorLevel::Error,
        &ASSIGN_TYPE,
        args!("Int32", "Bool"),
    );
    err(
        "fn f() { let b : Bool = 2i32; }",
        (1, 10),
        20,
        crate::ErrorLevel::Error,
        &ASSIGN_TYPE,
        args!("Bool", "Int32"),
    );
}

#[test]
fn type_while() {
    ok("fn x() { while true { } }");
    ok("fn x() { while false { } }");
    err(
        "fn x() { while 2i32 { } }",
        (1, 16),
        4,
        crate::ErrorLevel::Error,
        &WHILE_COND_TYPE,
        args!("Int32"),
    );
}

#[test]
fn type_if() {
    ok("fn x() { if true { } }");
    ok("fn x() { if false { } }");
    err(
        "fn x() { if 4i32 { } }",
        (1, 13),
        4,
        crate::ErrorLevel::Error,
        &IF_COND_TYPE,
        args!("Int32"),
    );
}

#[test]
fn type_return_unit() {
    ok("fn f() { return; }");
    err(
        "fn f() { return 1i32; }",
        (1, 10),
        11,
        crate::ErrorLevel::Error,
        &RETURN_TYPE,
        args!("()", "Int32"),
    );
}

#[test]
fn type_return() {
    ok("fn f(): Int32 { let a = 1i32; return a; }");
    ok("fn f(): Int32 { return 1i32; }");
    err(
        "fn f(): Int32 { return; }",
        (1, 17),
        6,
        crate::ErrorLevel::Error,
        &RETURN_TYPE,
        args!("Int32", "()"),
    );

    ok("fn f(): Int32 { return 0i32; }
            fn g(): Int32 { return f(); }");
    err(
        "fn f() { }
             fn g(): Int32 { return f(); }",
        (2, 30),
        10,
        crate::ErrorLevel::Error,
        &RETURN_TYPE,
        args!("Int32", "()"),
    );
}

#[test]
fn type_variable() {
    ok("fn f(a: Int32) { let b: Int32 = a; }");
}

#[test]
fn type_let_pattern_tuple() {
    ok("fn f(value: (Int32, Int32)): Int32 { let (a, b) = value; a+b }");
    err(
        "fn f() { let (a, b) = true; }",
        (1, 14),
        6,
        crate::ErrorLevel::Error,
        &PATTERN_TUPLE_EXPECTED,
        args!("Bool"),
    );

    ok("fn f(value: ()) { let () = value; }");
    err(
        "fn f() { let () = true; }",
        (1, 14),
        2,
        crate::ErrorLevel::Error,
        &PATTERN_TUPLE_EXPECTED,
        args!("Bool"),
    );
    err(
        "fn f() { let (a, b) = (); }",
        (1, 14),
        6,
        crate::ErrorLevel::Error,
        &PATTERN_WRONG_NUMBER_OF_PARAMS,
        args!(2, 0),
    );

    err(
        "fn f() { let (a, b) = (true,); }",
        (1, 14),
        6,
        crate::ErrorLevel::Error,
        &PATTERN_WRONG_NUMBER_OF_PARAMS,
        args!(2, 1),
    );
    err(
        "fn f() { let () = (true,); }",
        (1, 14),
        2,
        crate::ErrorLevel::Error,
        &PATTERN_WRONG_NUMBER_OF_PARAMS,
        args!(0, 1),
    );

    ok("fn f(value: (Int32, (Int32, Int32))): Int32 { let (a, (b, c)) = value; a+b+c }");
}

#[test]
fn type_assign_lvalue() {
    err(
        "fn f() { 1 = 3; }",
        (1, 10),
        5,
        crate::ErrorLevel::Error,
        &LVALUE_EXPECTED,
        args!(),
    );
}

#[test]
fn type_un_op() {
    ok("fn f(a: Int32) { !a; -a; }");
    err(
        "fn f(a: Bool) { -a; }",
        (1, 17),
        2,
        crate::ErrorLevel::Error,
        &UN_OP_TYPE,
        args!("-", "Bool"),
    );
}

#[test]
fn type_bin_op() {
    ok("fn f(a: Int32) { a+a; a-a; a*a; a/a; a%a; }");
    ok("fn f(a: Int32) { a<a; a<=a; a==a; a!=a; a>a; a>=a; }");
    ok("fn f(a: String) { a<a; a<=a; a==a; a!=a; a>a; a>=a; }");
    ok("fn f(a: String) { a===a; a!==a; a+a; }");
    ok("class Foo fn f(a: Foo) { a===a; a!==a; }");
    ok("fn f(a: Int32) { a|a; a&a; a^a; }");
    ok("fn f(a: Bool) { a||a; a&&a; }");

    err(
        "class A class B fn f(a: A, b: B) { a === b; }",
        (1, 36),
        7,
        crate::ErrorLevel::Error,
        &TYPES_INCOMPATIBLE,
        args!("A", "B"),
    );
    err(
        "class A class B fn f(a: A, b: B) { b !== a; }",
        (1, 36),
        7,
        crate::ErrorLevel::Error,
        &TYPES_INCOMPATIBLE,
        args!("B", "A"),
    );
    err(
        "fn f(a: Bool) { a+a; }",
        (1, 17),
        3,
        crate::ErrorLevel::Error,
        &BIN_OP_TYPE,
        args!("+", "Bool", "Bool"),
    );
    err(
        "fn f(a: Bool) { a^a; }",
        (1, 17),
        3,
        crate::ErrorLevel::Error,
        &BIN_OP_TYPE,
        args!("^", "Bool", "Bool"),
    );
    err(
        "fn f(a: Int32) { a||a; }",
        (1, 18),
        4,
        crate::ErrorLevel::Error,
        &BIN_OP_TYPE,
        args!("||", "Int32", "Int32"),
    );
    errors(
        "fn f(a: Int32) { a&&a; }",
        vec![
            (
                (1, 18),
                1,
                crate::ErrorLevel::Error,
                &WRONG_TYPE,
                args!("Bool", "Int32"),
            ),
            (
                (1, 21),
                1,
                crate::ErrorLevel::Error,
                &WRONG_TYPE,
                args!("Bool", "Int32"),
            ),
        ],
    );
    err(
        "fn f(a: String) { a-a; }",
        (1, 19),
        3,
        crate::ErrorLevel::Error,
        &BIN_OP_TYPE,
        args!("-", "String", "String"),
    );
    err(
        "fn f(a: String) { a*a; }",
        (1, 19),
        3,
        crate::ErrorLevel::Error,
        &BIN_OP_TYPE,
        args!("*", "String", "String"),
    );
    err(
        "fn f(a: String) { a%a; }",
        (1, 19),
        3,
        crate::ErrorLevel::Error,
        &BIN_OP_TYPE,
        args!("%", "String", "String"),
    );
}

#[test]
fn type_function_return_type() {
    ok("fn foo(): Int32 { return 1i32; } fn f() { let i: Int32 = foo(); }");
    err(
        "
        fn foo(): Int32 { return 1i32; }
        fn f() { let i: Bool = foo(); }",
        (3, 18),
        20,
        crate::ErrorLevel::Error,
        &ASSIGN_TYPE,
        args!("Bool", "Int32"),
    );
}

#[test]
fn type_ident_in_function_params() {
    ok("
    fn f(a: Int32) {}
    fn g() { let a = 1i32; f(a); }");
}

#[test]
fn type_recursive_function_call() {
    ok("fn f(a: Int32) { f(a); }");
}

#[test]
fn type_function_params() {
    ok("fn foo() {} fn f() { foo(); }");
    ok("fn foo(a: Int32) {} fn f() { foo(1i32); }");
    ok("fn foo(a: Int32, b: Bool) {} fn f() { foo(1i32, true); }");

    err(
        "
        fn foo() {}
        fn f() { foo(1i32); }",
        (3, 22),
        4,
        crate::ErrorLevel::Error,
        &SUPERFLUOUS_ARGUMENT,
        args!(),
    );
    err(
        "
        fn foo(a: Int32) {}
        fn f() { foo(true); }",
        (3, 22),
        4,
        crate::ErrorLevel::Error,
        &WRONG_TYPE_FOR_ARGUMENT,
        args!("Int32", "Bool"),
    );
    err(
        "
        fn foo(a: Int32, b: Bool) {}
        fn f() { foo(1i32, 2i32); }",
        (3, 28),
        4,
        crate::ErrorLevel::Error,
        &WRONG_TYPE_FOR_ARGUMENT,
        args!("Bool", "Int32"),
    );
}

#[test]
fn type_array() {
    ok("fn f(a: Array[Int32]): Int32 { return a(1i64); }");
    err(
        "fn f(a: Array[Int32]): String { return a(1i64); }",
        (1, 33),
        14,
        crate::ErrorLevel::Error,
        &RETURN_TYPE,
        args!("String", "Int32"),
    );
}

#[test]
fn type_array_assign() {
    err(
        "fn f(a: Array[Int32]): Int32 { return a(3) = 4i32; }",
        (1, 32),
        18,
        crate::ErrorLevel::Error,
        &RETURN_TYPE,
        args!("Int32", "()"),
    );
    errors(
        "fn f(a: Array[Int32]) { a(3) = \"b\"; }",
        vec![(
            (1, 32),
            3,
            crate::ErrorLevel::Error,
            &WRONG_TYPE_FOR_ARGUMENT,
            args!("Int32", "String"),
        )],
    );
}

#[test]
fn type_array_field() {
    ok("
        class Foo { x: Array[Int32] }
        fn f(a: Foo): Int32 { return a.x(1i64); }
    ");
}

#[test]
fn wrong_type_params_for_primitive() {
    err(
        "
        fn f() { let a: Int32[Bool, Char] = 10; }
    ",
        (2, 25),
        17,
        crate::ErrorLevel::Error,
        &WRONG_NUMBER_TYPE_PARAMS,
        args!(0, 2),
    );
}

#[test]
fn let_without_initialization() {
    err(
        "fn f() { let x: Int32; }",
        (1, 10),
        13,
        crate::ErrorLevel::Error,
        &LET_MISSING_INITIALIZATION,
        args!(),
    );
}

#[test]
fn reassign_param() {
    err(
        "fn f(a: Int32) { a = 1; }",
        (1, 18),
        5,
        crate::ErrorLevel::Error,
        &LET_REASSIGNED,
        args!(),
    );
}

#[test]
fn reassign_field() {
    ok("class Foo {x: Int32} fn foo(f: Foo) { f.x = 1; }");
}

#[test]
fn reassign_var() {
    ok("fn f() { let mut a=1; a=2; }");
}

#[test]
fn reassign_let() {
    err(
        "fn f() { let a=1; a=2; }",
        (1, 19),
        3,
        crate::ErrorLevel::Error,
        &LET_REASSIGNED,
        args!(),
    );
}

#[test]
fn reassign_self() {
    err(
        "class Foo
        impl Foo {
            fn f() { self = Foo(); }
        }",
        (3, 22),
        12,
        crate::ErrorLevel::Error,
        &LVALUE_EXPECTED,
        args!(),
    );
}

#[test]
fn same_names() {
    ok("class Foo { Foo: Foo }");
}

#[test]
fn lit_int64() {
    ok("fn f(): Int64 { return 1i64; }");
    ok("fn f(): Int32 { return 1i32; }");

    err(
        "fn f(): Int32 { return 1i64; }",
        (1, 17),
        11,
        crate::ErrorLevel::Error,
        &RETURN_TYPE,
        args!("Int32", "Int64"),
    );

    ok("fn f(): Int64 { return 1; }");
}

#[test]
fn lit_int64_as_default() {
    ok("fn f(): Int64 { return 1; }");
    ok("fn f(): Int64 {
        let x = 1;
        return x;
    }");
}

#[test]
fn overload_equals() {
    ok("
        class A
        impl std::traits::Equals for A {
            fn equals(rhs: A): Bool { return true; }
        }
        fn f1(): Bool { return A() == A(); }
        fn f2(): Bool { return A() != A(); }
    ");
}

#[test]
fn overload_compare_to() {
    ok("class A
            use std::traits::{Comparable, Ordering};
            impl Comparable for A {
                fn cmp(rhs: A): Ordering { Ordering::Less }
            }
            fn f1(): Bool { return A() < A(); }
            fn f2(): Bool { return A() <= A(); }
            fn f3(): Bool { return A() > A(); }
            fn f4(): Bool { return A() >= A(); }");
}

#[test]
fn int64_operations() {
    ok("fn f(a: Int64, b: Int64): Int64 { return a + b; }");
    ok("fn f(a: Int64, b: Int64): Int64 { return a - b; }");
    ok("fn f(a: Int64, b: Int64): Int64 { return a * b; }");
    ok("fn f(a: Int64, b: Int64): Int64 { return a / b; }");
    ok("fn f(a: Int64, b: Int64): Int64 { return a % b; }");
    ok("fn f(a: Int64, b: Int64): Int64 { return a | b; }");
    ok("fn f(a: Int64, b: Int64): Int64 { return a & b; }");
    ok("fn f(a: Int64, b: Int64): Int64 { return a ^ b; }");
    ok("fn f(a: Int64, b: Int32): Int64 { return a << b; }");
    ok("fn f(a: Int64, b: Int32): Int64 { return a >> b; }");
    ok("fn f(a: Int64, b: Int32): Int64 { return a >>> b; }");
    ok("fn f(a: Int64, b: Int64): Bool { return a == b; }");
    ok("fn f(a: Int64, b: Int64): Bool { return a != b; }");
    ok("fn f(a: Int64, b: Int64): Bool { return a < b; }");
    ok("fn f(a: Int64, b: Int64): Bool { return a <= b; }");
    ok("fn f(a: Int64, b: Int64): Bool { return a > b; }");
    ok("fn f(a: Int64, b: Int64): Bool { return a >= b; }");
    ok("fn f(a: Int64): Int64 { return !a; }");
    ok("fn f(a: Int64): Int64 { return -a; }");
}

#[test]
fn test_literal_int_overflow() {
    err(
        "fn f() { let x = 2147483648i32; }",
        (1, 18),
        13,
        crate::ErrorLevel::Error,
        &NUMBER_OVERFLOW,
        args!("Int32"),
    );
    ok("fn f() { let x = 2147483647i32; }");
    err(
        "fn f() { let x = -2147483649i32; }",
        (1, 19),
        13,
        crate::ErrorLevel::Error,
        &NUMBER_OVERFLOW,
        args!("Int32"),
    );
    ok("fn f() { let x = -2147483648i32; }");
}

#[test]
fn test_literal_hex_int_overflow() {
    err(
        "fn f() { let x = 0x1_FF_FF_FF_FFi32; }",
        (1, 18),
        18,
        crate::ErrorLevel::Error,
        &NUMBER_OVERFLOW,
        args!("Int32"),
    );
    ok("fn f() { let x: Int32 = 0xFF_FF_FF_FFi32; }");
}

#[test]
fn test_literal_bin_int_overflow() {
    err(
        "fn f() { let x = 0b1_11111111_11111111_11111111_11111111i32; }",
        (1, 18),
        42,
        crate::ErrorLevel::Error,
        &NUMBER_OVERFLOW,
        args!("Int32"),
    );
    ok("fn f() { let x: Int32 = 0b11111111_11111111_11111111_11111111i32; }");
}

#[test]
fn test_literal_int64_overflow() {
    err(
        "fn f() { let x = 922337203685477580800000i64; }",
        (1, 18),
        27,
        crate::ErrorLevel::Error,
        &NUMBER_LIMIT_OVERFLOW,
        args!(),
    );
    err(
        "fn f() { let x = 9223372036854775808i64; }",
        (1, 18),
        22,
        crate::ErrorLevel::Error,
        &NUMBER_OVERFLOW,
        args!("Int64"),
    );
    ok("fn f() { let x = 9223372036854775807i64; }");
    err(
        "fn f() { let x = -9223372036854775809i64; }",
        (1, 19),
        22,
        crate::ErrorLevel::Error,
        &NUMBER_OVERFLOW,
        args!("Int64"),
    );
    ok("fn f() { let x = -9223372036854775808i64; }");
}

#[test]
fn test_literal_float_format() {
    ok("fn f() { let x = -340282350000000000000000000000000000000f32; }");
    err(
        "fn f() { let x = 0b1001f32; }",
        (1, 18),
        9,
        crate::ErrorLevel::Error,
        &INVALID_NUMBER_FORMAT,
        args!(),
    );
    ok("fn f() { let x = 0x1001f32; }");
}

#[test]
fn test_char() {
    ok("fn foo(): Char { return 'c'; }");
    ok("fn foo(a: Char): Char { return a; }");
    err(
        "fn foo(): Char { return false; }",
        (1, 18),
        12,
        crate::ErrorLevel::Error,
        &RETURN_TYPE,
        args!("Char", "Bool"),
    );
    err(
        "fn foo(): Char { return 10i32; }",
        (1, 18),
        12,
        crate::ErrorLevel::Error,
        &RETURN_TYPE,
        args!("Char", "Int32"),
    );
}

#[test]
fn test_generic_arguments_mismatch() {
    err(
        "class A[T]
            fn foo() {
                let a = A[Int32, Int32]();
            }",
        (3, 25),
        17,
        crate::ErrorLevel::Error,
        &WRONG_NUMBER_TYPE_PARAMS,
        args!(1, 2),
    );

    err(
        "class A[T]
            fn foo() {
                let a = A();
            }",
        (3, 25),
        3,
        crate::ErrorLevel::Error,
        &WRONG_NUMBER_TYPE_PARAMS,
        args!(1, 0),
    );

    err(
        "class A
            fn foo() {
                let a = A[Int32]();
            }",
        (3, 25),
        10,
        crate::ErrorLevel::Error,
        &WRONG_NUMBER_TYPE_PARAMS,
        args!(0, 1),
    );
}

#[test]
fn test_invoke_static_method_as_instance_method() {
    err(
        "class A
        impl A {
            static fn foo() {}
            fn test() { self.foo(); }
        }",
        (4, 25),
        10,
        crate::ErrorLevel::Error,
        &UNKNOWN_METHOD,
        args!("A", "foo"),
    );
}
#[test]

fn test_invoke_method_as_static() {
    err(
        "class A
        impl A {
            fn foo() {}
            static fn test() { A::foo(); }
        }",
        (4, 32),
        8,
        crate::ErrorLevel::Error,
        &UNKNOWN_STATIC_METHOD,
        args!("A", "foo"),
    );
}

#[test]
fn test_fct_with_type_params() {
    err(
        "fn f() {} fn g() { f[Int32](); }",
        (1, 20),
        10,
        crate::ErrorLevel::Error,
        &WRONG_NUMBER_TYPE_PARAMS,
        args!(0, 1),
    );
    err(
        "fn f[T]() {} fn g() { f(); }",
        (1, 23),
        3,
        crate::ErrorLevel::Error,
        &WRONG_NUMBER_TYPE_PARAMS,
        args!(1, 0),
    );
    ok("fn f[T]() {} fn g() { f[Int32](); }");
    ok("fn f[T1, T2]() {} fn g() { f[Int32, String](); }");
}

#[test]
fn test_type_param_bounds_in_definition() {
    err(
        "
            trait MyTrait {}
            class Foo[T: MyTrait]
            fn bar[T](arg: Foo[T]) {}
        ",
        (4, 28),
        6,
        crate::ErrorLevel::Error,
        &TYPE_NOT_IMPLEMENTING_TRAIT,
        args!("T", "MyTrait"),
    );

    err(
        "
            trait MyTraitA {}
            trait MyTraitB {}
            class Foo[T: MyTraitA + MyTraitB]
            fn bar[T: MyTraitA](arg: Foo[T]) {}
        ",
        (5, 38),
        6,
        crate::ErrorLevel::Error,
        &TYPE_NOT_IMPLEMENTING_TRAIT,
        args!("T", "MyTraitB"),
    );

    err(
        "
            trait MyTraitA {}
            trait MyTraitB {}
            class Foo[T: MyTraitA + MyTraitB]
            class Baz[X]
            impl[X] Baz[X] {
                fn bar[T: MyTraitA](arg: Foo[T]) {}
            }
        ",
        (7, 42),
        6,
        crate::ErrorLevel::Error,
        &TYPE_NOT_IMPLEMENTING_TRAIT,
        args!("T", "MyTraitB"),
    );
}

#[test]
fn test_const_check() {
    err(
        "const one: Int32 = 1i32;
            fn f(): Int64 { return one; }",
        (2, 29),
        10,
        crate::ErrorLevel::Error,
        &RETURN_TYPE,
        args!("Int64", "Int32"),
    );

    err(
        "const one: Int32 = 1i32;
            fn f() { let x: String = one; }",
        (2, 22),
        20,
        crate::ErrorLevel::Error,
        &ASSIGN_TYPE,
        args!("String", "Int32"),
    );
}

#[test]
fn test_const_values() {
    let sa = ok("  const yes: Bool = true;
                        const x: UInt8 = 255u8;
                        const a: Int32 = 100i32;
                        const b: Int64 = 200i64;
                        const c: Char = 'A';
                        const d: Float32 = 3.0f32;
                        const e: Float64 = 6.0;");
    {
        let id = resolve_path(&sa, "<prog>::yes")
            .to_const()
            .expect("const expected");
        assert_eq!(&ConstValue::Bool(true), sa.const_(id).value());
    }

    {
        let id = resolve_path(&sa, "<prog>::x")
            .to_const()
            .expect("const expected");
        assert_eq!(&ConstValue::Int(255), sa.const_(id).value());
    }

    {
        let id = resolve_path(&sa, "<prog>::a")
            .to_const()
            .expect("const expected");
        assert_eq!(&ConstValue::Int(100), sa.const_(id).value());
    }

    {
        let id = resolve_path(&sa, "<prog>::b")
            .to_const()
            .expect("const expected");
        assert_eq!(&ConstValue::Int(200), sa.const_(id).value());
    }

    {
        let id = resolve_path(&sa, "<prog>::c")
            .to_const()
            .expect("const expected");
        assert_eq!(&ConstValue::Char('A'), sa.const_(id).value());
    }

    {
        let id = resolve_path(&sa, "<prog>::d")
            .to_const()
            .expect("const expected");
        assert_eq!(&ConstValue::Float(3.0), sa.const_(id).value());
    }

    {
        let id = resolve_path(&sa, "<prog>::e")
            .to_const()
            .expect("const expected");
        assert_eq!(&ConstValue::Float(6.0), sa.const_(id).value());
    }
}

#[test]
fn test_assignment_to_const() {
    err(
        "const one: Int32 = 1i32;
            fn f() { one = 2i32; }",
        (2, 22),
        3,
        crate::ErrorLevel::Error,
        &LVALUE_EXPECTED,
        args!(),
    );
}

#[test]
fn test_unary_minus_byte() {
    err(
        "const m1: UInt8 = -1u8;",
        (1, 20),
        3,
        crate::ErrorLevel::Error,
        &NEGATIVE_UNSIGNED,
        args!(),
    );
    err(
        "const m1: UInt8 = -1;",
        (1, 19),
        2,
        crate::ErrorLevel::Error,
        &ASSIGN_TYPE,
        args!("UInt8", "Int64"),
    );
    err(
        "const m1: UInt8 = -1i32;",
        (1, 19),
        5,
        crate::ErrorLevel::Error,
        &ASSIGN_TYPE,
        args!("UInt8", "Int32"),
    );
    err(
        "fn main() { let m1: UInt8 = -1u8; }",
        (1, 30),
        3,
        crate::ErrorLevel::Error,
        &NEGATIVE_UNSIGNED,
        args!(),
    );
    ok("const m1: Int32 = -1i32;");
    ok("const m1: Int64 = -1i64;");
}

#[test]
fn test_generic_trait_bounds() {
    ok("trait Foo {}
            class X
            impl Foo for X {}
            class A[T: Foo]
            fn f(): A[X] { A[X]() }");

    err(
        "
            trait Foo {}
            class X
            class A[T: Foo]
            fn f(x: A[X]) {}
        ",
        (5, 21),
        4,
        crate::ErrorLevel::Error,
        &TYPE_NOT_IMPLEMENTING_TRAIT,
        args!("X", "Foo"),
    );

    err(
        "trait Foo {}
            fn f[T: Foo]() {}
            fn t() { f[Int32](); }",
        (3, 22),
        10,
        crate::ErrorLevel::Error,
        &TYPE_NOT_IMPLEMENTING_TRAIT,
        args!("Int32", "Foo"),
    );
}

#[test]
fn test_operator_on_generic_type() {
    err(
        "fn f[T](a: T, b: T) { a + b; }",
        (1, 23),
        5,
        crate::ErrorLevel::Error,
        &BIN_OP_TYPE,
        args!("+", "T", "T"),
    );
}

#[test]
fn test_find_class_method_precedence() {
    // finding class method should have precedence over
    // trait methods
    ok("class A
            impl A { fn foo() {} }
            trait Foo { fn foo(); }
            impl Foo for A { fn foo() {} }
            fn test(a: A) { a.foo(); }");

    err(
        "class A
            impl A { fn foo() {} }
            trait Foo { fn foo(a: Int32); }
            impl Foo for A { fn foo(a: Int32) {} }
            fn test(a: A) { a.foo(1i32); }",
        (5, 35),
        4,
        crate::ErrorLevel::Error,
        &SUPERFLUOUS_ARGUMENT,
        args!(),
    );

    ok("class A
            impl A { static fn foo() {} }
            trait Foo { fn foo(a: Int32); }
            impl Foo for A { fn foo(a:  Int32) {} }
            fn test(a: A) { a.foo(1i32); }");
}

#[test]
fn test_global_get() {
    ok("let mut x: Int32 = 0i32; fn foo(): Int32 { return x; }");
}

#[test]
fn test_global_set() {
    ok("let mut x: Int32 = 0i32; fn foo(a: Int32) { x = a; }");
    err(
        "let x: Int32 = 0i32; fn foo(a: Int32) { x = a; }",
        (1, 41),
        5,
        crate::ErrorLevel::Error,
        &LET_REASSIGNED,
        args!(),
    );
    err(
        "let x: Int32 = true;",
        (1, 1),
        20,
        crate::ErrorLevel::Error,
        &ASSIGN_TYPE,
        args!("Int32", "Bool"),
    );
}

#[test]
fn lambda_assignment() {
    ok("fn f() { let x = || {}; }");
    ok("fn f() { let x = ||: Int32 { return 2i32; }; }");
    ok("fn f() { let x: (): () = || {}; }");
    ok("fn f() { let x: (): () = ||: () {}; }");
    ok("fn f() { let x: (): Int32 = ||: Int32 { return 2i32; }; }");
    // Lambda body returns () but expected return type from context is Int32
    err(
        "fn f() { let x: (): Int32 = || {}; }",
        (1, 32),
        2,
        crate::ErrorLevel::Error,
        &RETURN_TYPE,
        args!("Int32", "()"),
    );
}

#[test]
fn method_call_with_multiple_matching_traits() {
    err(
        "class A
            trait X { fn f(); }
            trait Y { fn f(); }

            impl X for A { fn f() {} }
            impl Y for A { fn f() {} }

            fn g(a: A) { a.f(); }",
        (8, 26),
        5,
        crate::ErrorLevel::Error,
        &MULTIPLE_CANDIDATES_FOR_METHOD,
        args!("A", "f"),
    );
}

#[test]
fn generic_trait_method_call() {
    ok("trait Foo { fn bar(); }
            fn f[T: Foo](t: T) { t.bar(); }");
    ok("trait Foo { fn bar(); }
            class A[T: Foo] { t: T }
            impl[T: Foo] A[T] {
                fn baz() { self.t.bar(); }
            }");
}

#[test]
fn trait_method_call_with_function_type_params() {
    ok("
        trait Foo {
            fn bar[T](x: T);
        }

        fn f[T: Foo](x: T) {
            x.bar[Int](1);
        }
    ");
}

#[test]
fn trait_method_call_with_function_type_params_and_missing_params() {
    err(
        "
        trait Foo {
            fn bar[T](x: T);
        }

        fn f[T: Foo](x: T) {
            x.bar(1);
        }
    ",
        (7, 13),
        8,
        crate::ErrorLevel::Error,
        &WRONG_NUMBER_TYPE_PARAMS,
        args!(1, 0),
    );

    err(
        "
        trait Foo[X] {
            fn bar[T](x: T);
        }

        fn f[T: Foo[Int]](x: T) {
            x.bar(1);
        }
    ",
        (7, 13),
        8,
        crate::ErrorLevel::Error,
        &WRONG_NUMBER_TYPE_PARAMS,
        args!(1, 0),
    );
}

#[test]
fn trait_method_call_with_function_type_params_invalid_param() {
    err(
        "
        trait Foo {
            fn bar[T: Bar](x: T);
        }

        trait Bar {}

        fn f[T: Foo](x: T) {
            x.bar[Int](1);
        }
    ",
        (9, 13),
        13,
        crate::ErrorLevel::Error,
        &TYPE_NOT_IMPLEMENTING_TRAIT,
        args!("Int64", "Bar"),
    );
}

#[test]
fn static_trait_method_call_with_function_type_params() {
    ok("
        trait Foo {
            static fn bar[T](x: T);
        }

        fn f[T: Foo](x: T) {
            T::bar[Int](1);
        }
    ");

    ok("
        trait Foo {
            static fn bar[T: Bar[X=String]](x: T);
        }

        trait Bar {
            type X;
        }

        impl Bar for Int {
            type X = String;
        }

        fn f[T: Foo](x: T) {
            T::bar[Int](1);
        }
    ");
}

#[test]
fn static_trait_method_call_with_function_type_params_and_missing_params() {
    err(
        "
        trait Foo {
            static fn bar[T](x: T);
        }

        fn f[T: Foo](x: T) {
            T::bar(1);
        }
    ",
        (7, 13),
        9,
        crate::ErrorLevel::Error,
        &WRONG_NUMBER_TYPE_PARAMS,
        args!(1, 0),
    );

    err(
        "
        trait Foo[X] {
            static fn bar[T](x: T);
        }

        fn f[T: Foo[Int]](x: T) {
            T::bar(1);
        }
    ",
        (7, 13),
        9,
        crate::ErrorLevel::Error,
        &WRONG_NUMBER_TYPE_PARAMS,
        args!(1, 0),
    );
}

#[test]
fn static_trait_method_call_with_function_type_params_invalid_param() {
    err(
        "
        trait Foo {
            static fn bar[T: Bar](x: T);
        }

        trait Bar {}

        fn f[T: Foo](x: T) {
            T::bar[Int](1);
        }
    ",
        (9, 13),
        14,
        crate::ErrorLevel::Error,
        &TYPE_NOT_IMPLEMENTING_TRAIT,
        args!("Int64", "Bar"),
    );

    err(
        "
        trait Foo {
            static fn bar[T: Bar[X=Int]](x: T);
        }

        trait Bar {
            type X;
        }

        impl Bar for Int {
            type X = String;
        }

        fn f[T: Foo](x: T) {
            T::bar[Int](1);
        }
    ",
        (15, 13),
        14,
        crate::ErrorLevel::Error,
        &TYPE_NOT_IMPLEMENTING_TRAIT,
        args!("Int64", "Bar"),
    );
}

#[test]
fn test_generic_ctor_without_type_params() {
    err(
        "class Foo[A, B]
            fn test() { Foo(); }",
        (2, 25),
        5,
        crate::ErrorLevel::Error,
        &WRONG_NUMBER_TYPE_PARAMS,
        args!(2, 0),
    );
}

#[test]
fn test_generic_argument_with_trait_bound() {
    err(
        "fn f[X: std::Comparable](x: X) {}
            fn g[T](t: T) { f[T](t); }",
        (2, 29),
        7,
        crate::ErrorLevel::Error,
        &TYPE_NOT_IMPLEMENTING_TRAIT,
        args!("T", "Comparable"),
    );
}

#[test]
fn test_for_supports_into_iterator() {
    err(
        "fn f() { for i in 1i32 {} }",
        (1, 19),
        4,
        crate::ErrorLevel::Error,
        &TYPE_NOT_USABLE_IN_FOR_IN,
        args!("Int32"),
    );

    err(
        "
            class Foo
            fn bar(x: Foo) {
                for i in x {
                    let x: Foo = i;
                }
            }
        ",
        (4, 26),
        1,
        crate::ErrorLevel::Error,
        &TYPE_NOT_USABLE_IN_FOR_IN,
        args!("Foo"),
    );

    ok("
            class Foo
            impl std::traits::IntoIterator for Foo {
                type IteratorType = FooIter;
                fn iter(): FooIter { return FooIter(); }
            }

            class FooIter

            impl std::traits::Iterator for FooIter {
                type Item = Int32;
                fn next(): Option[Int32] { Some[Int32](0i32) }
            }

            fn f(): Int32 {
                for i in Foo() {
                    return i;
                }
                return 0i32;
            }
    ");
}

#[test]
fn test_for_supports_into_iterator_with_missing_assoc_type() {
    errors(
        "
            class Foo
            impl std::traits::IntoIterator for Foo {
                fn iter(): FooIter { return FooIter(); }
            }

            class FooIter

            impl std::traits::Iterator for FooIter {
                type Item = Int32;
                fn next(): Option[Int32] { Some[Int32](0i32) }
            }

            fn f(): Int32 {
                for i in Foo() {
                    return i;
                }
                return 0i32;
            }
    ",
        vec![(
            (3, 13),
            111,
            crate::ErrorLevel::Error,
            &MISSING_ASSOC_TYPE,
            args!("IteratorType"),
        )],
    );
}

#[test]
fn test_for_supports_into_iterator_with_missing_method() {
    errors(
        "
            class Foo
            impl std::traits::IntoIterator for Foo {
                type IteratorType = FooIter;
            }

            class FooIter

            impl std::traits::Iterator for FooIter {
                type Item = Int32;
                fn next(): Option[Int32] { Some[Int32](0i32) }
            }

            fn f(): Int32 {
                for i in Foo() {
                    return i;
                }
                return 0i32;
            }
    ",
        vec![(
            (3, 13),
            134,
            crate::ErrorLevel::Error,
            &ELEMENT_NOT_IN_IMPL,
            args!("iter"),
        )],
    );
}

#[test]
fn test_for_supports_into_iterator_with_invalid_type() {
    errors(
        "
            class Foo
            impl std::traits::IntoIterator for Foo {
                type IteratorType = FooIter;
                fn iter(): FooIter { return FooIter(); }
            }

            class FooIter

            fn f(): Int32 {
                for i in Foo() {
                    return i;
                }
                return 0i32;
            }
    ",
        vec![(
            (4, 17),
            28,
            crate::ErrorLevel::Error,
            &TYPE_NOT_IMPLEMENTING_TRAIT,
            args!("FooIter", "Iterator"),
        )],
    );
}

#[test]
fn test_for_supports_iterator_with_missing_item() {
    errors(
        "
            class Foo
            impl std::traits::Iterator for Foo {
                fn next(): Option[Int] { None }
            }

            fn f(x: Foo): Int {
                for i in x {
                    return i;
                }
                0
            }
    ",
        vec![(
            (3, 13),
            98,
            crate::ErrorLevel::Error,
            &MISSING_ASSOC_TYPE,
            args!("Item"),
        )],
    );
}

#[test]
fn test_for_supports_iterator_with_missing_next() {
    errors(
        "
            class Foo
            impl std::traits::Iterator for Foo {
                type Item = Int;
            }

            fn f(x: Foo): Int {
                for i in x {
                    return i;
                }
                0
            }
    ",
        vec![(
            (3, 13),
            118,
            crate::ErrorLevel::Error,
            &ELEMENT_NOT_IN_IMPL,
            args!("next"),
        )],
    );
}

#[test]
fn test_ctor_with_type_param() {
    err(
        "
            class Foo[T]
            impl[T] Foo[T] {
                fn foo(a: Int32) {
                    Bar[T](a);
                }
            }

            class Bar[T](T)
            ",
        (5, 28),
        1,
        crate::ErrorLevel::Error,
        &WRONG_TYPE_FOR_ARGUMENT,
        args!("T", "Int32"),
    );
}

#[test]
fn test_fct_used_as_identifier() {
    err(
        "fn foo() {} fn bar() { foo; }",
        (1, 24),
        3,
        crate::ErrorLevel::Error,
        &VALUE_EXPECTED,
        args!(),
    );
}

#[test]
fn test_cls_used_as_identifier() {
    err(
        "class X fn f() { X; }",
        (1, 18),
        1,
        crate::ErrorLevel::Error,
        &VALUE_EXPECTED,
        args!(),
    );
}

#[test]
fn test_assign_fct() {
    err(
        "fn foo() {} fn bar() { foo = 1i32; }",
        (1, 24),
        3,
        crate::ErrorLevel::Error,
        &LVALUE_EXPECTED,
        args!(),
    );
}

#[test]
fn test_assign_class() {
    err(
        "
            class X
            fn foo() { X = 2i32; }
        ",
        (3, 24),
        1,
        crate::ErrorLevel::Error,
        &LVALUE_EXPECTED,
        args!(),
    );
}

#[test]
fn test_new_call_fct() {
    ok("fn g() {} fn f() { g(); }");
}

#[test]
fn test_new_call_fct_wrong_params() {
    err(
        "fn g() {} fn f() { g(1i32); }",
        (1, 22),
        4,
        crate::ErrorLevel::Error,
        &SUPERFLUOUS_ARGUMENT,
        args!(),
    );
}

#[test]
fn test_new_call_fct_with_type_params() {
    ok("fn g[T]() {} fn f() { g[Int32](); }");
}

#[test]
fn test_new_call_fct_with_wrong_type_params() {
    err(
        "fn g() {} fn f() { g[Int32](); }",
        (1, 20),
        10,
        crate::ErrorLevel::Error,
        &WRONG_NUMBER_TYPE_PARAMS,
        args!(0, 1),
    );
}

#[test]
fn test_new_call_static_method() {
    ok("class Foo impl Foo { static fn bar() {} }
            fn f() { Foo::bar(); }");
}

#[test]
fn test_new_call_static_method_wrong_params() {
    err(
        "class Foo impl Foo { static fn bar() {} }
            fn f() { Foo::bar(1i32); }",
        (2, 31),
        4,
        crate::ErrorLevel::Error,
        &SUPERFLUOUS_ARGUMENT,
        args!(),
    );
}

#[test]
fn test_new_call_static_method_type_params() {
    ok("class Foo impl Foo { static fn bar[T]() {} }
            fn f() { Foo::bar[Int32](); }");
}

#[test]
fn test_new_call_class() {
    ok("
        class X
        fn f() { X(); }
    ");
}

#[test]
fn test_new_call_class_wrong_params() {
    err(
        "
        class X
        fn f() { X(1i32); }
    ",
        (3, 20),
        4,
        crate::ErrorLevel::Error,
        &SUPERFLUOUS_ARGUMENT,
        args!(),
    );
}

#[test]
fn test_new_call_class_with_type_params() {
    ok("
        class X[T]
        fn f() { X[Int32](); }
    ");
}

#[test]
fn test_new_call_class_with_wrong_type_params() {
    err(
        "
            class X
            fn f() { X[Int32](); }
        ",
        (3, 22),
        10,
        crate::ErrorLevel::Error,
        &WRONG_NUMBER_TYPE_PARAMS,
        args!(0, 1),
    );
}

#[test]
fn test_new_call_method() {
    ok("
        class X
        impl X { fn f() {} }
        fn f(x: X) { x.f(); }
    ");
}

#[test]
fn test_new_call_method_type_param() {
    ok("
        class X
        impl X { fn f[T]() {} }
        fn f(x: X) { x.f[Int32](); }
    ");
}

#[test]
fn test_new_call_method_wrong_params() {
    err(
        "
        class X
        impl X { fn f() {} }
        fn f(x: X) { x.f(1i32); }",
        (4, 26),
        4,
        crate::ErrorLevel::Error,
        &SUPERFLUOUS_ARGUMENT,
        args!(),
    );
}

#[test]
fn test_new_call_method_generic() {
    ok("fn f[T: std::Hash](t: T) { t.hash(); }");
}

#[test]
fn test_new_call_method_generic_error() {
    err(
        "fn f[T](t: T) { t.hash(); }",
        (1, 17),
        8,
        crate::ErrorLevel::Error,
        &UNKNOWN_METHOD_FOR_TYPE_PARAM,
        args!(),
    );
}

#[test]
fn test_new_call_method_generic_error_multiple() {
    err(
        "
            trait TraitA { fn id(); }
            trait TraitB { fn id(); }
            fn f[T: TraitA + TraitB](t: T) { t.id(); }",
        (4, 46),
        6,
        crate::ErrorLevel::Error,
        &MULTIPLE_CANDIDATES_FOR_TYPE_PARAM,
        args!(),
    );
}

#[test]
fn test_array_syntax_get() {
    ok("fn f(t: Array[Int32]): Int32 { return t(0); }");
}

#[test]
fn test_array_syntax_set() {
    ok("fn f(t: Array[Int32]){ t(0) = 10i32; }");
}

#[test]
fn test_array_syntax_set_wrong_value() {
    errors(
        "
            fn f(t: Array[Int32]) { t(0) = true; }
        ",
        vec![(
            (2, 44),
            4,
            crate::ErrorLevel::Error,
            &WRONG_TYPE_FOR_ARGUMENT,
            args!("Int32", "Bool"),
        )],
    );
}

#[test]
fn test_array_syntax_set_wrong_index() {
    errors(
        "fn f(t: Array[Int32]){ t(\"bla\") = 9i32; }",
        vec![(
            (1, 26),
            5,
            crate::ErrorLevel::Error,
            &WRONG_TYPE_FOR_ARGUMENT,
            args!("Int64", "String"),
        )],
    );
}

#[test]
fn test_template() {
    ok("fn f(x: Int32): String { return \"x = ${x}\"; }");
    err(
        "
            class Foo
            fn f(x: Foo): String { return \"x = ${x}\"; }
        ",
        (3, 50),
        1,
        crate::ErrorLevel::Error,
        &EXPECTED_STRINGABLE,
        args!("Foo"),
    );
    ok("fn f[T: std::Stringable](x: T): String { return \"${x}\"; }");

    ok("
        class Foo
        impl std::Stringable for Foo {
            fn toString(): String { \"abc\" }
        }
        fn bar(x: Option[Foo]): String {
            \"${x}\"
        }
    ");

    err(
        "
        class Foo
        fn bar(x: Option[Foo]): String {
            \"${x}\"
        }
    ",
        (4, 16),
        1,
        crate::ErrorLevel::Error,
        &EXPECTED_STRINGABLE,
        args!("Option[Foo]"),
    );
}

#[test]
fn test_trait_object_as_argument() {
    ok("trait Foo { fn bar(): Int32; }
        fn f(x: Foo): Int32 { return x.bar(); }");
    err(
        "trait Foo { fn baz(); }
        fn f(x: Foo): String { return x.baz(); }",
        (2, 32),
        14,
        crate::ErrorLevel::Error,
        &RETURN_TYPE,
        args!("String", "()"),
    );
}

#[test]
fn test_type_param_used_as_value() {
    err(
        "fn f[T](): Int32 { return T; }",
        (1, 27),
        1,
        crate::ErrorLevel::Error,
        &VALUE_EXPECTED,
        args!(),
    );

    err(
        "class SomeClass[T]
        impl[T] SomeClass[T] {
            fn f(): Int32 { return T; }
        }",
        (3, 36),
        1,
        crate::ErrorLevel::Error,
        &VALUE_EXPECTED,
        args!(),
    );
}

#[test]
fn test_assign_to_type_param() {
    err(
        "fn f[T]() { T = 10; }",
        (1, 13),
        1,
        crate::ErrorLevel::Error,
        &LVALUE_EXPECTED,
        args!(),
    );

    err(
        "
        class SomeClass[T]
        impl[T] SomeClass[T] {
            fn f() { T = 10; }
        }",
        (4, 22),
        1,
        crate::ErrorLevel::Error,
        &LVALUE_EXPECTED,
        args!(),
    );
}

#[test]
fn test_type_param_with_name_but_no_call() {
    err(
        "trait X { fn foo(): Int32; }
        fn f[T: X]() { T::foo; }",
        (2, 24),
        6,
        crate::ErrorLevel::Error,
        &EXPECTED_MODULE,
        args!(),
    );

    err(
        "trait X { fn foo(): Int32; }
        class SomeClass[T: X]
        impl[T: X] SomeClass[T] {
            fn f() { T::foo; }
        }",
        (4, 22),
        6,
        crate::ErrorLevel::Error,
        &EXPECTED_MODULE,
        args!(),
    );
}

#[test]
fn test_type_param_call() {
    err(
        "trait X { fn foo(): Int32; }
        fn f[T: X]() { T(); }",
        (2, 24),
        1,
        crate::ErrorLevel::Error,
        &VALUE_EXPECTED,
        args!(),
    );

    err(
        "trait X { fn foo(): Int32; }
        class SomeClass[T: X]
        impl[T: X] SomeClass[T] {
            fn f() { T(); }
        }",
        (4, 22),
        1,
        crate::ErrorLevel::Error,
        &VALUE_EXPECTED,
        args!(),
    );
}

#[test]
fn test_static_method_call_with_type_param() {
    err(
        "trait X { static fn bar(): Int32; }
        fn f[T: X]() { T::foo(); }",
        (2, 24),
        8,
        crate::ErrorLevel::Error,
        &UNKNOWN_STATIC_METHOD_WITH_TYPE_PARAM,
        args!(),
    );

    err(
        "trait X { static fn foo(): Int32; }
        trait Y { static fn foo(): String; }
        fn f[T: X + Y]() { T::foo(); }",
        (3, 28),
        8,
        crate::ErrorLevel::Error,
        &MULTIPLE_CANDIDATES_FOR_STATIC_METHOD_WITH_TYPE_PARAM,
        args!(),
    );

    err(
        "trait X { static fn foo(): Int32; }
        fn f[T: X](): Int32 { return T::foo(1i32); }",
        (2, 45),
        4,
        crate::ErrorLevel::Error,
        &SUPERFLUOUS_ARGUMENT,
        args!(),
    );

    ok("trait X { static fn foo(): Int32; }
        fn f[T: X](): Int32 { return T::foo(); }");
}

#[test]
fn test_type_param_with_let() {
    ok("fn myid[T](val: T): T {
        let tmp: T = val;
        return tmp;
    }");
}

#[test]
fn test_fct_and_class_type_params() {
    ok("
    class A[X]
    impl[X] A[X] {
        fn test[Y]() {}
    }");

    ok("
    class A[X]
    impl[X] A[X] {
        fn t1[Y](x: X, y: Y): Y { return y; }
        fn t2[Y](x: X, y: Y): X { return x; }
    }

    fn t1(a: A[Int32]): String {
        return a.t1[String](1i32, \"bla\");
    }

    fn t2(a: A[Int32]): Int32 {
        return a.t2[String](1i32, \"bla\");
    }
    ");
}

#[test]
fn test_struct() {
    ok("
        struct Foo(Int32)
        fn f(): Foo { Foo(1i32) }
    ");
    err(
        "
        struct Foo(Int32)
        fn f(): Foo { Foo() }",
        (3, 23),
        5,
        crate::ErrorLevel::Error,
        &MISSING_ARGUMENTS,
        args!(1, 0),
    );
    err(
        "
        struct Foo(Int32)
        fn f(): Foo { Foo(true) }",
        (3, 27),
        4,
        crate::ErrorLevel::Error,
        &WRONG_TYPE_FOR_ARGUMENT,
        args!("Int32", "Bool"),
    );
}

#[test]
fn test_struct_field() {
    ok("
        struct Foo(Int32)
        fn f(x: Foo): Int32 { x.0 }
    ");

    err(
        "
        struct Foo(Bool)
        fn f(x: Foo): Int32 { x.0 }
    ",
        (3, 29),
        7,
        crate::ErrorLevel::Error,
        &RETURN_TYPE,
        args!("Int32", "Bool"),
    );

    err(
        "
        struct Foo(Bool)
        fn f(x: Foo): Int32 { x.unknown }
    ",
        (3, 31),
        9,
        crate::ErrorLevel::Error,
        &UNKNOWN_FIELD,
        args!("unknown", "Foo"),
    );
}

#[test]
fn test_struct_field_array() {
    ok("
        struct Foo { f1: Array[Int32] }
        fn f(x: Foo): Int32 { x.f1(0) }
    ");
}

#[test]
fn test_struct_with_type_params() {
    ok("
        struct Foo[T](Int32)
        fn f(): Foo[Int32] { Foo[Int32](1i32) }
    ");
    err(
        "
        struct Foo[T](Int32)
        fn f(): Foo[Int32] { Foo(1i32) }
    ",
        (3, 30),
        9,
        crate::ErrorLevel::Error,
        &WRONG_NUMBER_TYPE_PARAMS,
        args!(1, 0),
    );
    err(
        "
        struct Foo[T](Int32)
        fn f(): Foo[Int32] { Foo[Int32, Bool](1i32) }
    ",
        (3, 30),
        22,
        crate::ErrorLevel::Error,
        &WRONG_NUMBER_TYPE_PARAMS,
        args!(1, 2),
    );
    err(
        "
        trait MyTrait {}
        struct Foo[T: MyTrait](Int32)
        fn f(x: Foo[Int32]) {}
    ",
        (4, 17),
        10,
        crate::ErrorLevel::Error,
        &TYPE_NOT_IMPLEMENTING_TRAIT,
        args!("Int32", "MyTrait"),
    );
    ok("
        trait MyTrait {}
        class Bar
        impl MyTrait for Bar {}
        struct Foo[T: MyTrait](Int32)
        fn f(): Foo[Bar] { Foo[Bar](1i32) }
    ");
    err(
        "
        struct Foo[T](Int32)
        fn f(): Foo[Int32] { Foo[Bool](1i32) }
    ",
        (3, 28),
        19,
        crate::ErrorLevel::Error,
        &RETURN_TYPE,
        args!("Foo[Int32]", "Foo[Bool]"),
    );
    err(
        "
        struct Foo[T](T, Bool)
        fn f[T](val: T): Foo[T] { Foo(val, false) }",
        (3, 35),
        15,
        crate::ErrorLevel::Error,
        &WRONG_NUMBER_TYPE_PARAMS,
        args!(1, 0),
    );
}

#[test]
fn test_struct_mod() {
    err(
        "
        fn f() { foo::Foo(1i32); }
        mod foo { struct Foo(Int32) }
        ",
        (2, 18),
        14,
        crate::ErrorLevel::Error,
        &NOT_ACCESSIBLE,
        args!(),
    );
}

#[test]
fn test_struct_with_static_method() {
    ok("
        struct Foo(Int32)
        impl Foo {
            static fn bar() {}
        }
        fn f() {
            Foo::bar();
        }
        ");

    ok("
        struct Foo[T](Int32)
        impl[T] Foo[T] {
            static fn bar() {}
        }
        fn f() {
            Foo[Int32]::bar();
        }
        ");

    err(
        "
            struct Foo(Int32)
            fn f() {
                Foo[Int32]::bar();
            }
            ",
        (4, 17),
        17,
        crate::ErrorLevel::Error,
        &WRONG_NUMBER_TYPE_PARAMS,
        args!(0, 1),
    );
}

#[test]
fn test_enum_with_static_method() {
    ok("
        enum Foo { A, B }
        impl Foo {
            static fn bar() {}
        }
        fn f() {
            Foo::bar();
        }
        ");

    err(
        "
        enum Foo { A, B }
        fn f() {
            Foo[Int32]::bar();
        }
        ",
        (4, 13),
        17,
        crate::ErrorLevel::Error,
        &WRONG_NUMBER_TYPE_PARAMS,
        args!(0, 1),
    );
}

#[test]
fn test_enum() {
    ok("enum A { V1, V2 }");
    ok("enum A { V1, V2 } fn f(a: A): A { return a; }");
    ok("enum A { V1, V2 } fn f(): A { return A::V1; }");

    ok("enum A { V1, V2 } fn f(): Bool { return A::V1 == A::V2; }");
    ok("enum A { V1, V2 } fn f(): Bool { return A::V1 != A::V2; }");

    err(
        "enum A { V1 } fn f(): A { A }",
        (1, 27),
        1,
        crate::ErrorLevel::Error,
        &VALUE_EXPECTED,
        args!(),
    );

    err(
        "enum A { V1 } fn f() { A = 1; }",
        (1, 24),
        1,
        crate::ErrorLevel::Error,
        &LVALUE_EXPECTED,
        args!(),
    );

    err(
        "enum A { V1, V2 } fn f(): A { A::V3 }",
        (1, 31),
        5,
        crate::ErrorLevel::Error,
        &UNKNOWN_ENUM_VARIANT,
        args!("V3"),
    );

    // Type params are inferred from expected type
    ok("enum A[T] { V1, V2 } fn f(): A[Int32] { A::V1 }");

    err(
        "enum A[T] { V1(T), V2 } fn f(): A[Int32] { A[Int32]::V1 }",
        (1, 44),
        12,
        crate::ErrorLevel::Error,
        &ENUM_VARIANT_MISSING_ARGUMENTS,
        args!(),
    );

    err(
        "
        enum Foo[T] { A(T, Bool), B }
        fn f[T](val: T): Foo[T] { Foo::A[T, String](val, false) }",
        (3, 35),
        29,
        crate::ErrorLevel::Error,
        &WRONG_NUMBER_TYPE_PARAMS,
        args!(1, 2),
    );

    ok("
        enum Foo[T] { A(T, Bool), B }
        fn f[T](val: T): Foo[T] { Foo::A(val, false) }");
}

#[test]
fn test_enum_match() {
    ok("
        enum A { V1, V2 }
        fn f(x: A): Int32 {
            match x {
                A::V1 => 0i32,
                A::V2 => 1i32
            }
        }
    ");

    err(
        "
        enum A { V1, V2 }
        fn f(x: A): Int32 {
            match x {
                A::V1 => 0i32,
                A::V2 => \"foo\"
            }
        }
    ",
        (6, 26),
        5,
        crate::ErrorLevel::Error,
        &MATCH_BRANCH_TYPES_INCOMPATIBLE,
        args!("Int32", "String"),
    );
}

#[test]
fn test_enum_match_with_guard() {
    ok("
        enum A { V1, V2 }
        fn f(x: A, y: Bool): Int64 {
            match x {
                A::V1 if y => 0,
                A::V1 => 1,
                A::V2 => 2
            }
        }
    ");

    ok("
        enum A { V1(Int64), V2 }
        fn f(x: A, y: Bool): Int64 {
            match x {
                A::V1(a) if a > 10 => a,
                A::V1(..) => 1,
                A::V2 => 2
            }
        }
    ");

    err(
        "
        enum A { V1, V2 }
        fn f(x: A, y: Float64): Int64 {
            match x {
                A::V1 if y => 0,
                A::V1 => 1,
                A::V2 => 2
            }
        }
    ",
        (5, 26),
        1,
        crate::ErrorLevel::Error,
        &IF_COND_TYPE,
        args!("Float64"),
    );
}

#[test]
fn test_enum_match_multiple_patterns() {
    ok("
        enum Foo { A, B, C }
        fn f(x: Foo): Int64 {
            match x {
                Foo::A | Foo::B => 0,
                Foo::C => 1
            }
        }
    ");

    ok("
        enum Foo { A(Int64), B(Int64), C }
        fn f(x: Foo): Int64 {
            match x {
                Foo::A(x) | Foo::B(x) => x,
                Foo::C => 1
            }
        }
    ");
}

#[test]
fn test_enum_match_with_parens() {
    err(
        "
        enum A { V1, V2 }
        fn f(x: A): Int32 {
            match x {
                A::V1() => 0i32,
                A::V2 => 1i32
            }
        }
    ",
        (5, 17),
        7,
        crate::ErrorLevel::Error,
        &PATTERN_NO_PARENS,
        args!(),
    );
}

#[test]
fn test_enum_match_wrong_number_params() {
    err(
        "
        enum A { V1(Int32), V2 }
        fn f(x: A): Int32 {
            match x {
                A::V1 => 0i32,
                A::V2 => 1i32
            }
        }
    ",
        (5, 17),
        5,
        crate::ErrorLevel::Error,
        &PATTERN_WRONG_NUMBER_OF_PARAMS,
        args!(0, 1),
    );

    err(
        "
        enum A { V1(Int32, Float32, Bool), V2 }
        fn f(x: A): Int32 {
            match x {
                A::V1(a, b, c, d) => 0i32,
                A::V2 => 1i32
            }
        }
    ",
        (5, 17),
        17,
        crate::ErrorLevel::Error,
        &PATTERN_WRONG_NUMBER_OF_PARAMS,
        args!(4, 3),
    );
}

#[test]
fn test_enum_match_params() {
    ok("
        enum A { V1(Int32, Int32, Int32), V2 }
        fn f(x: A): Int32 {
            match x {
                A::V1(a, _, c) => a + c,
                A::V2 => 1i32
            }
        }
    ");

    err(
        "
        enum A { V1(Int32, Int32, Int32), V2 }
        fn f(x: A): Int32 {
            match x {
                A::V1(a, _, c) => a + c,
                A::V2 => a
            }
        }
    ",
        (6, 26),
        1,
        crate::ErrorLevel::Error,
        &UNKNOWN_IDENTIFIER,
        args!("a"),
    );

    err(
        "
        enum A { V1(Int32, Int32), V2 }
        fn f(x: A): Int32 {
            match x {
                A::V1(a, a) => a + a,
                A::V2 => 1i32
            }
        }
    ",
        (5, 26),
        1,
        crate::ErrorLevel::Error,
        &PATTERN_DUPLICATE_BINDING,
        args!(),
    );
}

#[test]
fn test_enum_equals() {
    ok("
        enum A { V1, V2 }
        fn f(x: A, y: A): Bool {
            x == y
        }
    ");

    err(
        "
        enum A { V1(Int32), V2 }
        fn f(x: A, y: A): Bool {
            x == y
        }
    ",
        (4, 13),
        6,
        crate::ErrorLevel::Error,
        &BIN_OP_TYPE,
        args!("==", "A", "A"),
    );
}

#[test]
fn test_use_enum_value() {
    ok("enum A { V1(Int32), V2 } use self::A::V1; fn f(): A { V1(1i32) }");
    ok("enum A[T] { V1(Int32), V2 } use self::A::V1; fn f(): A[Int32] { V1[Int32](1i32) }");
    ok("enum A[T] { V1(Int32), V2 } use self::A::V1; fn f(): A[Int32] { V1(1i32) }");

    ok("enum A { V1, V2 } use self::A::V2; fn f(): A { V2 }");

    err(
        "enum A { V1(Int32), V2 } use self::A::V1; fn f(): A { V1 }",
        (1, 55),
        2,
        crate::ErrorLevel::Error,
        &ENUM_VARIANT_MISSING_ARGUMENTS,
        args!(),
    );

    err(
        "enum A { V1(Int32), V2 } use self::A::V2; fn f(): A { V2(0i32) }",
        (1, 55),
        8,
        crate::ErrorLevel::Error,
        &UNEXPECTED_ARGUMENTS_FOR_ENUM_VARIANT,
        args!(),
    );

    ok("enum A[T] { V1(Int32), V2 } use self::A::V2; fn f(): A[Int32] { V2 }");

    ok("enum A[T] { V1, V2 } use self::A::V2; fn f(): A[Int32] { V2[Int32] }");

    err(
        "enum A[T] { V1, V2 } use self::A::V2; fn f(): A[Int32] { V2[Int32, Float32] }",
        (1, 58),
        18,
        crate::ErrorLevel::Error,
        &WRONG_NUMBER_TYPE_PARAMS,
        args!(1, 2),
    );
}

#[test]
fn test_enum_value_with_type_param() {
    ok("enum A[T] { V1, V2 } fn f(): A[Int32] { A::V2[Int32] }");
    ok("enum A[T] { V1, V2 } fn f(): A[Int32] { A[Int32]::V2 }");
    err(
        "enum A[T] { V1, V2 } fn f(): A[Int32] { A[Int32]::V2[Int32] }",
        (1, 53),
        7,
        crate::ErrorLevel::Error,
        &NO_TYPE_PARAMS_EXPECTED,
        args!(),
    );
}

#[test]
fn test_block_value() {
    ok("fn f(): Int32 { 1i32 }");
    ok("fn f() { let x = { 1i32 }; }");
    ok("fn g(): Int32 { return 1i32; } fn f() { let x: Int32 = { g() }; }");
    ok("fn g(): Int32 { return 1i32; } fn f() { let x: Int32 = { g(); 1i32 }; }");
}

#[test]
fn test_if_expression() {
    ok("fn f(): Int32 { if true { 1i32 } else { 2i32 } }");
    ok("fn f(): Float32 { if true { 1.0f32 } else { 2.0f32 } }");
    ok("fn f(): Float64 { if true { 1.0 } else { 2.0 } }");

    ok("fn f(): Int32 { 4i32 * if true { 1i32 } else { 2i32 } }");
}

#[test]
fn test_tuple() {
    ok("fn f(a: (Int32, Bool)) {}");
    ok("fn f(a: (Int32, Bool)): (Int32, Bool) { return a; }");
    ok("fn f(a: (Int32, Bool)): (Int32, Bool) {
            let tmp = a;
            return tmp;
        }");
    err(
        "fn f(a: (Int32, Bool)): (Int32) { return a; }",
        (1, 35),
        8,
        crate::ErrorLevel::Error,
        &RETURN_TYPE,
        args!("(Int32)", "(Int32, Bool)"),
    );
    err(
        "fn f(a: (Int32, Bool)): (Int32, Float32) { return a; }",
        (1, 44),
        8,
        crate::ErrorLevel::Error,
        &RETURN_TYPE,
        args!("(Int32, Float32)", "(Int32, Bool)"),
    );
}

#[test]
fn test_tuple_literal() {
    ok("fn f(): (Int32, Bool) {
        return (1i32, false);
    }");

    err(
        "fn f(): (Int32) {
        return (1i32);
    }",
        (2, 9),
        13,
        crate::ErrorLevel::Error,
        &RETURN_TYPE,
        args!("(Int32)", "Int32"),
    );

    err(
        "fn f(): (Int32, Int32) {
        return (1i32, false);
    }",
        (2, 9),
        20,
        crate::ErrorLevel::Error,
        &RETURN_TYPE,
        args!("(Int32, Int32)", "(Int32, Bool)"),
    );
}

#[test]
fn test_tuple_in_call() {
    ok("
        fn f(a: (Int32, Bool)) {}
        fn g() {
            f((1i32, true));
        }
    ");
}

#[test]
fn test_tuple_element() {
    ok("
        fn f(a: (Int32, Bool)): Int32 {
            return a.0;
        }
    ");

    ok("
        fn f(a: (Int32, Bool)): Bool {
            return a.1;
        }
    ");

    err(
        "
        fn f(a: (Int32, Bool)): String {
            return a.1;
        }
    ",
        (3, 13),
        10,
        crate::ErrorLevel::Error,
        &RETURN_TYPE,
        args!("String", "Bool"),
    );
}

#[test]
fn zero_trait_ok() {
    ok("fn f() { Array[Int32]::zero(12i64); }");
}

#[test]
fn zero_trait_err() {
    err(
        "fn f() { Array[String]::zero(12i64); }",
        (1, 10),
        26,
        crate::ErrorLevel::Error,
        &UNKNOWN_STATIC_METHOD,
        args!("Array[String]", "zero"),
    );
}

#[test]
fn extension_method_call() {
    ok("
        class Foo { value: Int32 }
        impl Foo { fn foo(): Int32 { self.value } }
        fn bar(x: Foo): Int32 { x.foo() }
    ");
}

#[test]
fn extension_class_with_type_param() {
    ok("
        class Foo[T](T)
        trait MyTrait {}
        impl[T: MyTrait] Foo[T] { fn foo(): Int32 { 12i32 } }
        impl MyTrait for Int32 {}
        fn bar(x: Foo[Int32]): Int32 { x.foo() }
    ");

    ok("
        class Foo[T](T)
        impl Foo[Int32] { fn foo() {} }
        impl Foo[Float32] { fn bar() {} }
        fn f(x: Foo[Int32]) { x.foo() }
        fn g(x: Foo[Float32]) { x.bar() }
    ");

    err(
        "
        class Foo[T](T)
        impl Foo[Float32] { fn bar() {} }
        fn f(x: Foo[Int32]) { x.bar() }
    ",
        (4, 31),
        7,
        crate::ErrorLevel::Error,
        &UNKNOWN_METHOD,
        args!("Foo[Int32]", "bar"),
    );
}

#[test]
fn extension_class_tuple() {
    ok("
        class Foo[T](T)
        impl Foo[(Int32, Int32)] {
            fn bar() {}
        }
        fn f(x: Foo[(Int32, Int32)]) {
            x.bar();
        }
    ");

    ok("
        class Foo[T]
        impl[T] Foo[(T, Int32)] {
            fn bar() {}
        }
        fn f() {
            Foo[(Int32, Int32)]().bar();
            Foo[(Float32, Int32)]().bar();
        }
    ");

    err(
        "
        class Foo[T]
        impl Foo[(Int32, Float32)] {
            fn bar() {}
        }
        fn f(x: Foo[(Int32, Int32)]) {
            x.bar();
        }
    ",
        (7, 13),
        7,
        crate::ErrorLevel::Error,
        &UNKNOWN_METHOD,
        args!("Foo[(Int32, Int32)]", "bar"),
    );
}

#[test]
fn extension_nested() {
    err(
        "
        class Foo[T]
        impl Foo[Foo[Foo[Int32]]] {
            fn bar() {}
        }
        fn f(value: Foo[Foo[Foo[Int32]]]) {
            value.bar();
        }
        fn g(value: Foo[Foo[Int32]]) {
            value.bar();
        }
    ",
        (10, 13),
        11,
        crate::ErrorLevel::Error,
        &UNKNOWN_METHOD,
        args!("Foo[Foo[Int32]]", "bar"),
    );
}

#[test]
fn extension_bind_type_param_twice() {
    ok("
        class Foo[T]
        impl[T] Foo[(T, T)] {
            fn bar() {}
        }
        fn f(x: Foo[(Int32, Int32)]) {
            x.bar();
        }
    ");

    ok("
        class Foo[T]
        impl[T] Foo[(T, T)] {
            fn bar() {}
        }
        fn f[T](x: Foo[(T, T)]) {
            x.bar();
        }
    ");

    err(
        "
        class Foo[T]
        impl[T] Foo[(T, T)] {
            fn bar() {}
        }
        fn f(x: Foo[(Int32, Float32)]) {
            x.bar();
        }
    ",
        (7, 13),
        7,
        crate::ErrorLevel::Error,
        &UNKNOWN_METHOD,
        args!("Foo[(Int32, Float32)]", "bar"),
    );

    err(
        "
        class Foo[T]
        impl[T] Foo[(T, T)] {
            fn bar() {}
        }
        fn f[T](x: Foo[(T, Float32)]) {
            x.bar();
        }
    ",
        (7, 13),
        7,
        crate::ErrorLevel::Error,
        &UNKNOWN_METHOD,
        args!("Foo[(T, Float32)]", "bar"),
    );
}

#[test]
fn extension_struct_with_type_param() {
    ok("
        struct Foo[T](T)
        trait MyTrait {}
        impl[T: MyTrait] Foo[T] { fn foo(): Int32 { 12i32 } }
        impl MyTrait for Int32 {}
        fn bar(x: Foo[Int32]): Int32 { x.foo() }
    ");

    ok("
        struct Foo[T](T)
        impl Foo[Int32] { fn foo() {} }
        impl Foo[Float32] { fn bar() {} }
        fn f(x: Foo[Int32]) { x.foo() }
        fn g(x: Foo[Float32]) { x.bar() }
    ");

    err(
        "
        struct Foo[T](T)
        impl Foo[Float32] { fn bar() {} }
        fn f(x: Foo[Int32]) { x.bar() }
    ",
        (4, 31),
        7,
        crate::ErrorLevel::Error,
        &UNKNOWN_METHOD,
        args!("Foo[Int32]", "bar"),
    );
}

#[test]
fn extension_enum_with_type_param() {
    ok("
        enum Foo[T] { A(T), B }
        trait MyTrait {}
        impl[T: MyTrait] Foo[T] { fn foo(): Int32 { 12i32 } }
        impl MyTrait for Int32 {}
        fn bar(x: Foo[Int32]): Int32 { x.foo() }
    ");

    ok("
        enum Foo[T] { A(T), B }
        impl Foo[Int32] { fn foo() {} }
        impl Foo[Float32] { fn bar() {} }
        fn f(x: Foo[Int32]) { x.foo() }
        fn g(x: Foo[Float32]) { x.bar() }
    ");

    err(
        "
        enum Foo[T] { A(T), B }
        impl Foo[Float32] { fn bar() {} }
        fn f(x: Foo[Int32]) { x.bar() }
    ",
        (4, 31),
        7,
        crate::ErrorLevel::Error,
        &UNKNOWN_METHOD,
        args!("Foo[Int32]", "bar"),
    );
}

#[test]
fn impl_class_type_params() {
    err(
        "
        trait MyTrait { fn bar(); }
        class Foo[T]
        impl MyTrait for Foo[String] { fn bar() {} }
        fn bar(x: Foo[Int32]) { x.bar(); }
    ",
        (5, 33),
        7,
        crate::ErrorLevel::Error,
        &UNKNOWN_METHOD,
        args!("Foo[Int32]", "bar"),
    );

    ok("
        trait MyTrait { fn bar(); }
        class Foo[T]
        impl MyTrait for Foo[Int32] { fn bar() {} }
        fn bar(x: Foo[Int32]) { x.bar(); }
    ");
}

#[test]
fn extension_with_fct_type_param() {
    ok("
        class MyClass[T]
        class Foo
        impl MyClass[Foo] {
            fn do[T](another: MyClass[T]) {}
        }
        fn f() {
            MyClass[Foo]().do[Int32](MyClass[Int32]());
            MyClass[Foo]().do[Float32](MyClass[Float32]());
        }
    ");
}

#[test]
fn impl_struct_type_params() {
    err(
        "
        trait MyTrait { fn bar(); }
        struct Foo[T](T)
        impl MyTrait for Foo[String] { fn bar() {} }
        fn bar(x: Foo[Int32]) { x.bar(); }
    ",
        (5, 33),
        7,
        crate::ErrorLevel::Error,
        &UNKNOWN_METHOD,
        args!("Foo[Int32]", "bar"),
    );

    ok("
        trait MyTrait { fn bar(); }
        struct Foo[T](T)
        impl MyTrait for Foo[Int32] { fn bar() {} }
        fn bar(x: Foo[Int32]) { x.bar(); }
    ");
}

#[test]
fn impl_struct_method_with_self() {
    ok("
        struct Foo(Int32)
        trait AsInt32 { fn value(): Int32; }
        impl AsInt32 for Foo { fn value(): Int32 { self.0 } }
    ");
}

#[test]
fn impl_enum_type_params() {
    err(
        "
        trait MyTrait { fn bar(); }
        enum Foo[T] { A(T), B }
        impl MyTrait for Foo[String] { fn bar() {} }
        fn bar(x: Foo[Int32]) { x.bar(); }
    ",
        (5, 33),
        7,
        crate::ErrorLevel::Error,
        &UNKNOWN_METHOD,
        args!("Foo[Int32]", "bar"),
    );

    ok("
        trait MyTrait { fn bar(); }
        enum Foo[T] { A(T), B }
        impl MyTrait for Foo[Int32] { fn bar() {} }
        fn bar(x: Foo[Int32]) { x.bar(); }
    ");
}

#[test]
fn method_call_on_unit() {
    err(
        "fn foo(a: ()) { a.foo(); }",
        (1, 17),
        7,
        crate::ErrorLevel::Error,
        &UNKNOWN_METHOD,
        args!("()", "foo"),
    );
}

#[test]
fn method_on_enum() {
    ok("
        enum MyEnum { A, B }
        impl MyEnum { fn foo() {} }
        fn f(x: MyEnum) { x.foo(); }
    ");
}

#[test]
fn literal_without_suffix_byte() {
    ok("fn f(): UInt8 { 1 }");
    err(
        "fn f(): UInt8 { 256 }",
        (1, 17),
        3,
        crate::ErrorLevel::Error,
        &NUMBER_OVERFLOW,
        args!("UInt8"),
    );
    ok("fn f() { let x: UInt8 = 1; }");
}

#[test]
fn literal_without_suffix_long() {
    ok("fn f(): Int64 { 1 }");
    ok("fn f() { let x: Int64 = 1; }");
}

#[test]
fn variadic_parameter() {
    ok("
        fn f(x: Int32...): Int64 {
            x.size()
        }
        fn g() {
            f(1i32, 2i32, 3i32, 4i32);
            f();
            f(1i32);
        }
    ");
    err(
        "
        fn f(x: Int32...) {}
        fn g() {
            f(true);
        }
    ",
        (4, 15),
        4,
        crate::ErrorLevel::Error,
        &WRONG_TYPE_FOR_ARGUMENT,
        args!("Int32", "Bool"),
    );
    ok("
        fn f(x: Int32, y: Int32...) {}
        fn g() {
            f(1i32, 2i32, 3i32, 4i32);
            f(1i32, 2i32);
            f(1i32);
        }
    ");
    err(
        "
        fn f(x: Int32, y: Int32...) {}
        fn g() {
            f();
        }
    ",
        (4, 13),
        3,
        crate::ErrorLevel::Error,
        &MISSING_ARGUMENTS,
        args!(1, 0),
    );
    err(
        "fn f(x: Int32..., y: Int32) {}",
        (1, 6),
        11,
        crate::ErrorLevel::Error,
        &VARIADIC_PARAMETER_NEEDS_TO_BE_LAST,
        args!(),
    );
}

#[test]
fn for_with_array() {
    ok("fn f(x: Array[Int32]): Int32 {
        let mut result = 0i32;
        for i in x {
            result = result + i;
        }
        result
    }");

    ok("fn f(x: Array[Float32]): Float32 {
        let mut result = 0.0f32;
        for i in x {
            result = result + i;
        }
        result
    }");
}

#[test]
fn for_with_vec() {
    ok("
    use std::traits::IntoIterator;
    fn f(x: Vec[Int32]): Int32 {
        let mut result = 0i32;
        for i in x.iter() {
            result = result + i;
        }
        result
    }");

    ok("fn f(x: Vec[Int32]): Int32 {
        let mut result = 0i32;
        for i in x {
            result = result + i;
        }
        result
    }");

    ok("fn f(x: Vec[Float32]): Float32 {
        let mut result = 0.0f32;
        for i in x.makeReverseIterator() {
            result = result + i;
        }
        result
    }");

    ok("fn f(x: Vec[Float32]): Float32 {
        let mut result = 0.0f32;
        for i in x {
            result = result + i;
        }
        result
    }");
}

#[test]
fn check_no_type_params_with_generic_type() {
    err(
        "
            class Bar[T]
            fn f(x: Bar) {}
        ",
        (3, 21),
        3,
        crate::ErrorLevel::Error,
        &WRONG_NUMBER_TYPE_PARAMS,
        args!(1, 0),
    );
}

#[test]
fn check_wrong_number_type_params() {
    err(
        "
            fn foo() { bar[Int32](false); }
            fn bar[T](x: T) {}
        ",
        (2, 35),
        5,
        crate::ErrorLevel::Error,
        &WRONG_TYPE_FOR_ARGUMENT,
        args!("Int32", "Bool"),
    );
}

#[test]
fn multiple_functions() {
    ok("fn f() {} fn g() {}");
}

#[test]
fn redefine_function() {
    err(
        "
        fn f() {}
        fn f() {}",
        (3, 9),
        9,
        crate::ErrorLevel::Error,
        &SHADOW_FUNCTION,
        args!("f"),
    );
}

#[test]
fn shadow_type_with_function() {
    err(
        "
        class FooBar
        fn FooBar() {}
        ",
        (3, 9),
        14,
        crate::ErrorLevel::Error,
        &SHADOW_CLASS,
        args!("FooBar"),
    );
}

#[test]
fn define_param_name_twice() {
    err(
        "fn test(x: String, x: Int32) {}",
        (1, 20),
        1,
        crate::ErrorLevel::Error,
        &NAME_BOUND_MULTIPLE_TIMES_IN_PARAMS,
        args!("x"),
    );
}

#[test]
fn show_type_param_with_name() {
    err(
        "fn test[T](T: Int32) {}",
        (1, 12),
        1,
        crate::ErrorLevel::Error,
        &SHADOW_TYPE_PARAM,
        args!("T"),
    );
}

#[test]
fn shadow_function() {
    ok("fn f() { let f = 1i32; }");
    errors(
        "fn f() { let f = 1i32; f(); }",
        vec![(
            (1, 24),
            3,
            crate::ErrorLevel::Error,
            &INDEX_GET_NOT_IMPLEMENTED,
            args!("Int32"),
        )],
    );
}

#[test]
fn shadow_var() {
    ok("fn f() { let f = 1i32; let f = 2i32; }");
}

#[test]
fn shadow_param() {
    err(
        "fn f(a: Int32, b: Int32, a: String) {}",
        (1, 26),
        1,
        crate::ErrorLevel::Error,
        &NAME_BOUND_MULTIPLE_TIMES_IN_PARAMS,
        args!("a"),
    );
}

#[test]
fn multiple_params() {
    ok("fn f(a: Int32, b: Int32, c:String) {}");
}

#[test]
fn undefined_variable() {
    err(
        "fn f() { let b = a; }",
        (1, 18),
        1,
        crate::ErrorLevel::Error,
        &UNKNOWN_IDENTIFIER,
        args!("a"),
    );
    err(
        "fn f() { a; }",
        (1, 10),
        1,
        crate::ErrorLevel::Error,
        &UNKNOWN_IDENTIFIER,
        args!("a"),
    );
}

#[test]
fn undefined_function() {
    err(
        "fn f() { foo(); }",
        (1, 10),
        3,
        crate::ErrorLevel::Error,
        &UNKNOWN_IDENTIFIER,
        args!("foo"),
    );
}

#[test]
fn recursive_function_call() {
    ok("fn f() { f(); }");
}

#[test]
fn function_call() {
    ok("fn a() {} fn b() { a(); }");

    // non-forward definition of functions
    ok("fn a() { b(); } fn b() {}");
}

#[test]
fn variable_outside_of_scope() {
    err(
        "fn f(): Int32 { { let a = 1i32; } return a; }",
        (1, 42),
        1,
        crate::ErrorLevel::Error,
        &UNKNOWN_IDENTIFIER,
        args!("a"),
    );

    ok("fn f(): Int32 { let a = 1i32; { let a = 2i32; } return a; }");
}

#[test]
fn const_value() {
    ok("const one: Int32 = 1i32;
        fn f(): Int32 { return one; }");
}

#[test]
fn for_var() {
    ok("fn f() { for i in std::range(0, 4) { i; } }");
}

#[test]
fn mod_fct_call() {
    err(
        "
        fn f() { foo::g(); }
        mod foo { fn g() {} }
    ",
        (2, 18),
        8,
        crate::ErrorLevel::Error,
        &NOT_ACCESSIBLE,
        args!(),
    );

    ok("
        fn f() { foo::g(); }
        mod foo { pub fn g() {} }
    ");

    ok("
        fn f() { foo::bar::baz(); }
        mod foo {
            pub mod bar {
                pub fn baz() {}
            }
        }
    ");

    err(
        "
        fn f() { foo::bar::baz(); }
        mod foo {
            pub mod bar {
                fn baz() {}
            }
        }
    ",
        (2, 18),
        15,
        crate::ErrorLevel::Error,
        &NOT_ACCESSIBLE,
        args!(),
    );
}

#[test]
fn mod_ctor_call() {
    ok("
        fn f() { foo::Foo(); }
        mod foo { pub class Foo }
    ");

    err(
        "
        fn f() { foo::Foo(); }
        mod foo { class Foo }
    ",
        (2, 18),
        10,
        crate::ErrorLevel::Error,
        &NOT_ACCESSIBLE,
        args!(),
    );

    ok("
        fn f() { foo::bar::Foo(); }
        mod foo { pub mod bar { pub class Foo } }
    ");

    err(
        "
        fn f() { foo::bar::Foo(); }
        mod foo { pub mod bar { class Foo } }
    ",
        (2, 18),
        15,
        crate::ErrorLevel::Error,
        &NOT_ACCESSIBLE,
        args!(),
    );
}

#[test]
fn mod_class_field() {
    err(
        "
        fn f(x: foo::Foo) { let a = x.bar; }
        mod foo { pub class Foo { bar: Int32 } }
    ",
        (2, 37),
        5,
        crate::ErrorLevel::Error,
        &NOT_ACCESSIBLE,
        args!(),
    );

    err(
        "
        fn f(x: foo::Foo) { let a = x.bar(10i64); }
        mod foo { pub class Foo { bar: Array[Int32] } }
    ",
        (2, 37),
        5,
        crate::ErrorLevel::Error,
        &NOT_ACCESSIBLE,
        args!(),
    );

    err(
        "
        fn f(x: foo::Foo) { x.bar(10i64) = 10i32; }
        mod foo { pub class Foo { bar: Array[Int32] } }
    ",
        (2, 29),
        5,
        crate::ErrorLevel::Error,
        &NOT_ACCESSIBLE,
        args!(),
    );

    ok("
        fn f(x: foo::Foo) { let a = x.bar; }
        mod foo { pub class Foo { pub bar: Int32 } }
    ");
}

#[test]
fn mod_class_method() {
    ok("
        fn f(x: foo::Foo) { x.bar(); }
        mod foo {
            pub class Foo
            impl Foo { pub fn bar() {} }
        }
    ");

    err(
        "
        fn f(x: foo::Foo) { x.bar(); }
        mod foo {
            pub class Foo
            impl Foo { fn bar() {} }
        }
    ",
        (2, 29),
        7,
        crate::ErrorLevel::Error,
        &NOT_ACCESSIBLE,
        args!(),
    );
}

#[test]
fn mod_class_static_method() {
    ok("
        fn f() { foo::Foo::bar(); }
        mod foo {
            pub class Foo
            impl Foo { pub static fn bar() {} }
        }
    ");

    err(
        "
        fn f() { foo::Foo::bar(); }
        mod foo {
            pub class Foo
            impl Foo { static fn bar() {} }
        }
    ",
        (2, 18),
        15,
        crate::ErrorLevel::Error,
        &NOT_ACCESSIBLE,
        args!(),
    );
}

#[test]
fn mod_struct_field() {
    err(
        "
        fn f(x: foo::Foo) { let a = x.bar; }
        mod foo { pub struct Foo { bar: Int32 } }
    ",
        (2, 37),
        5,
        crate::ErrorLevel::Error,
        &NOT_ACCESSIBLE,
        args!(),
    );

    ok("
        fn f(x: foo::Foo) { let a = x.bar(10i64); }
        mod foo { pub struct Foo {
            pub bar: Array[Int32]
        } }
    ");

    err(
        "
        fn f(x: foo::Foo) { let a = x.bar(10i64); }
        mod foo { pub struct Foo { bar: Array[Int32] } }
    ",
        (2, 37),
        5,
        crate::ErrorLevel::Error,
        &NOT_ACCESSIBLE,
        args!(),
    );

    err(
        "
        fn f(x: foo::Foo) { x.bar(10i64) = 10i32; }
        mod foo { pub struct Foo { bar: Array[Int32] } }
    ",
        (2, 29),
        5,
        crate::ErrorLevel::Error,
        &NOT_ACCESSIBLE,
        args!(),
    );

    ok("
        fn f(x: foo::Foo) { let a = x.bar; }
        mod foo { pub struct Foo { pub bar: Int32 } }
    ");
}

#[test]
fn mod_path_in_type() {
    ok("
        fn f(): foo::Foo { foo::Foo() }
        mod foo { pub class Foo }
    ");

    err(
        "
        fn f(): bar::Foo { 1i32 }
    ",
        (2, 17),
        3,
        crate::ErrorLevel::Error,
        &UNKNOWN_IDENTIFIER,
        args!("bar"),
    );

    err(
        "
        fn bar() {}
        fn f(): bar::Foo { 1i32 }
    ",
        (3, 17),
        8,
        crate::ErrorLevel::Error,
        &EXPECTED_PATH,
        args!(),
    );

    err(
        "
        fn f(): foo::bar::Foo { 1i32 }
        mod foo {}
    ",
        (2, 22),
        3,
        crate::ErrorLevel::Error,
        &UNKNOWN_IDENTIFIER_IN_MODULE,
        args!("foo", "bar"),
    );
}

#[test]
fn mod_global() {
    ok("
        fn f(): Int32 { foo::x }
        mod foo { pub let x: Int32 = 1i32; }
    ");

    err(
        "
        fn f(): Int32 { foo::x }
        mod foo { let x: Int32 = 1i32; }
    ",
        (2, 25),
        6,
        crate::ErrorLevel::Error,
        &NOT_ACCESSIBLE,
        args!(),
    );
}

#[test]
fn mod_trait() {
    ok("
        mod foo {
            class Foo
            trait Bar { fn f(x: Foo); }
        }
    ");
}

#[test]
fn mod_impl() {
    ok("
        mod foo {
            class Foo
            trait Bar { fn f(x: Foo); }
            class AnotherClass
            impl Bar for AnotherClass {
                fn f(x: Foo) {}
            }
        }
    ");
}

#[test]
fn mod_class() {
    ok("
        mod foo {
            class Foo(Bar)
            impl Foo {
                fn foo(x: Bar) {}
            }
            class Bar
        }
    ");
}

#[test]
fn mod_class_new() {
    ok("
        mod foo {
            class Foo(Bar)
            class Bar
        }
    ");

    err(
        "
        fn f() { foo::Foo(1i32); }
        mod foo {
            class Foo(Int32)
        }
    ",
        (2, 18),
        14,
        crate::ErrorLevel::Error,
        &NOT_ACCESSIBLE,
        args!(),
    );

    err(
        "
        fn f() { foo::Foo(1i32); }
        mod foo {
            class Foo(pub Int32)
        }
    ",
        (2, 18),
        14,
        crate::ErrorLevel::Error,
        &NOT_ACCESSIBLE,
        args!(),
    );

    err(
        "
        fn f() { foo::Foo(1i32); }
        mod foo {
            pub class Foo(Int32)
        }
    ",
        (2, 18),
        14,
        crate::ErrorLevel::Error,
        &CLASS_CONSTRUCTOR_NOT_ACCESSIBLE,
        args!("foo::Foo"),
    );
}

#[test]
fn mod_struct() {
    err(
        "
        fn f() { foo::Foo(1i32); }
        mod foo {
            struct Foo(Int32)
        }
    ",
        (2, 18),
        14,
        crate::ErrorLevel::Error,
        &NOT_ACCESSIBLE,
        args!(),
    );

    err(
        "
        fn f() { foo::Foo(1i32); }
        mod foo {
            struct Foo(pub Int32)
        }
    ",
        (2, 18),
        14,
        crate::ErrorLevel::Error,
        &NOT_ACCESSIBLE,
        args!(),
    );

    err(
        "
        fn f() { foo::Foo(1i32); }
        mod foo {
            pub struct Foo(Int32)
        }
    ",
        (2, 18),
        14,
        crate::ErrorLevel::Error,
        &STRUCT_CONSTRUCTOR_NOT_ACCESSIBLE,
        args!("foo::Foo"),
    );

    ok("
        fn f() { foo::Foo(1i32); }
        mod foo {
            pub struct Foo(pub Int32)
        }
    ");

    ok("
        fn f(value: foo::Foo) {}
        mod foo {
            pub struct Foo(Int32)
        }
    ");

    err(
        "
        fn f(value: foo::Foo) {}
        mod foo {
            struct Foo(Int32)
        }
    ",
        (2, 21),
        8,
        crate::ErrorLevel::Error,
        &NOT_ACCESSIBLE,
        args!(),
    );
}

#[test]
fn mod_const() {
    ok("
        fn f(): Int32 { foo::x }
        mod foo { pub const x: Int32 = 1i32; }
    ");

    err(
        "
        fn f(): Int32 { foo::x }
        mod foo { const x: Int32 = 1i32; }
    ",
        (2, 25),
        6,
        crate::ErrorLevel::Error,
        &NOT_ACCESSIBLE,
        args!(),
    );

    ok("
        fn f(): Int32 { foo::bar::x }
        mod foo { pub mod bar { pub const x: Int32 = 1i32; } }
    ");
}

#[test]
fn mod_enum_value() {
    ok("
        fn f() { foo::A; }
        mod foo { pub enum Foo { A, B } use self::Foo::A; }
    ");

    err(
        "
        fn f() { foo::A; }
        mod foo { enum Foo { A, B } use self::Foo::A; }
    ",
        (2, 18),
        6,
        crate::ErrorLevel::Error,
        &NOT_ACCESSIBLE,
        args!(),
    );

    ok("
        fn f() { foo::bar::A; }
        mod foo { pub mod bar { pub enum Foo { A, B } use self::Foo::A; } }
    ");

    err(
        "
        fn f() { foo::bar::A; }
        mod foo { pub mod bar { enum Foo { A, B } use self::Foo::A; } }
    ",
        (2, 18),
        11,
        crate::ErrorLevel::Error,
        &NOT_ACCESSIBLE,
        args!(),
    );
}

#[test]
fn mod_enum() {
    err(
        "
        fn f() {
            foo::Foo::B;
        }
        mod foo { enum Foo { A(Bar), B } class Bar }
    ",
        (3, 13),
        11,
        crate::ErrorLevel::Error,
        &NOT_ACCESSIBLE,
        args!(),
    );

    ok("
        fn f() {
            foo::Foo::B;
        }
        mod foo { pub enum Foo { A, B } }
    ");

    ok("
        fn f() {
            foo::Foo::A(1i32);
        }
        mod foo { pub enum Foo { A(Int32), B } }
    ");

    err(
        "
        fn f() {
            foo::Foo::A(1i32);
        }
        mod foo { enum Foo { A(Int32), B } }
    ",
        (3, 13),
        17,
        crate::ErrorLevel::Error,
        &NOT_ACCESSIBLE,
        args!(),
    );
}

#[test]
fn mod_use() {
    ok("
        use self::foo::bar;
        fn f() { bar(); }
        mod foo { pub fn bar() {} }
    ");

    ok("
        use self::foo::bar::baz;
        fn f() { baz(); }
        mod foo { pub mod bar {
            pub fn baz() {}
        } }
    ");

    ok("
        use self::foo::bar as baz;
        fn f() { baz(); }
        mod foo { pub fn bar() {} }
    ");

    ok("
        use self::foo::bar;
        fn f(): Int32 { bar }
        mod foo { pub let bar: Int32 = 10i32; }
    ");

    ok("
        use self::foo::bar::baz;
        fn f(): Int32 { baz }
        mod foo { pub mod bar {
            pub let baz: Int32 = 10i32;
        } }
    ");

    ok("
        use self::foo::bar;
        fn f(): Int32 { bar }
        mod foo { pub let bar: Int32 = 10i32; }
    ");
}

#[test]
fn mod_use_class() {
    ok("
        use self::foo::Bar;
        fn f() { Bar(); }
        mod foo { pub class Bar }
    ");

    ok("
        use self::foo::Bar;
        fn f() {
            Bar();
            Bar::baz();
        }
        mod foo {
            pub class Bar
            impl Bar {
                pub static fn baz() {}
            }
        }
    ");
}

#[test]
fn mod_use_trait() {
    ok("
        use self::foo::Bar;
        mod foo { pub trait Bar{} }
    ");
}

#[test]
fn mod_use_std() {
    ok("
        use std::HashMap;
    ");
}

#[test]
fn mod_use_package() {
    ok("
        class Foo
        mod bar {
            use package::Foo;
            fn getfoo(): Foo { Foo() }
        }
    ");
}

#[test]
fn mod_use_super() {
    ok("
        mod baz {
            class Foo
            mod bar {
                use super::Foo;

                fn getfoo(): Foo { Foo() }
            }
        }
    ");

    err(
        "use super::Foo;",
        (1, 5),
        5,
        crate::ErrorLevel::Error,
        &NO_SUPER_MODULE,
        args!(),
    );
}

#[test]
fn mod_use_self() {
    ok("
        use self::bar::Foo;
        fn getfoo(): Foo { Foo() }
        mod bar { pub class Foo }
    ");
}

#[test]
fn mod_use_errors() {
    err(
        "
        use self::foo::bar::baz;
        mod foo { pub mod bar {} }
    ",
        (2, 29),
        3,
        crate::ErrorLevel::Error,
        &UNKNOWN_IDENTIFIER_IN_MODULE,
        args!("foo::bar", "baz"),
    );

    err(
        "
        use self::foo::bar;
    ",
        (2, 19),
        3,
        crate::ErrorLevel::Error,
        &UNKNOWN_IDENTIFIER_IN_MODULE,
        args!("", "foo"),
    );

    err(
        "
        use self::foo::bar;
        mod foo {}
    ",
        (2, 24),
        3,
        crate::ErrorLevel::Error,
        &UNKNOWN_IDENTIFIER_IN_MODULE,
        args!("foo", "bar"),
    );

    err(
        "
        use self::foo::bar;
        fn foo() {}
    ",
        (2, 19),
        3,
        crate::ErrorLevel::Error,
        &EXPECTED_PATH,
        args!(),
    );

    err(
        "
        use self::foo::bar::baz;
        pub mod foo { pub fn bar() {} }
    ",
        (2, 24),
        3,
        crate::ErrorLevel::Error,
        &EXPECTED_PATH,
        args!(),
    );
}

#[test]
fn mod_inside() {
    ok("
        mod foo { fn f() { g() } fn g() {} }
    ");

    ok("
        mod foo { class Foo fn g(x: Foo) {} }
    ");

    ok("
        fn f(x: foo::Foo) {}
        mod foo { pub class Foo }
    ");

    err(
        "
        fn f(x: foo::Foo) {}
        mod foo { class Foo }
    ",
        (2, 17),
        8,
        crate::ErrorLevel::Error,
        &NOT_ACCESSIBLE,
        args!(),
    );
}

#[test]
fn different_fct_call_kinds() {
    ok("fn f() { g(); } fn g() {}");
    ok("fn f() { g[Int32](); } fn g[T]() {}");
    ok("fn f(g: Array[Int32]) { g(0); }");
    err(
        "fn f(g: Array[Int32]) { g[Float32](0); }",
        (1, 25),
        10,
        crate::ErrorLevel::Error,
        &NO_TYPE_PARAMS_EXPECTED,
        args!(),
    );
    ok("class Foo fn f() { Foo(); }");
    // Note: `1i32[Int32]()` is now rejected at parse time
    ok("enum Foo { A(Int32), B } fn f() { Foo::A(1i32); }");
    ok("enum Foo[T] { A(Int32), B } fn f() { Foo[Int32]::A(1i32); }");
    ok("enum Foo[T] { A(Int32), B } fn f() { Foo::A[Int32](1i32); }");
    err(
        "enum Foo[T] { A(Int32), B } fn f() { Foo[Int32]::A[Int32](1i32); }",
        (1, 50),
        8,
        crate::ErrorLevel::Error,
        &NO_TYPE_PARAMS_EXPECTED,
        args!(),
    );
    ok("trait MyTrait { static fn foo(); } fn f[T: MyTrait]() { T::foo(); }");
    ok("class Foo impl Foo { fn bar() {} } fn f(g: Foo) { g.bar(); }");
    ok("class Foo impl Foo { fn bar[T]() {} } fn f(g: Foo) { g.bar[Int32](); }");
}

#[test]
fn trait_object_method_call() {
    ok("
        trait Foo { fn bar(): Int32; }
        fn f(x: Foo): Int32 {
            x.bar()
        }
    ");
}

#[test]
fn trait_object_method_call_with_ignore() {
    err(
        "
        trait Foo { @TraitObjectIgnore fn bar(): Int32; }
        fn f(x: Foo): Int32 {
            x.bar()
        }
    ",
        (4, 13),
        7,
        crate::ErrorLevel::Error,
        &UNKNOWN_METHOD,
        args!("Foo", "bar"),
    );
}

#[test]
fn trait_object_cast() {
    ok("
        trait Foo { fn bar(): Int32; }
        class Bar
        impl Foo for Bar {
            fn bar(): Int32 { 1i32 }
        }
        fn f(x: Foo): Int32 { x.bar() }
        fn g(): Int32 {
            f(Bar() as Foo)
        }
    ");

    ok("
        trait Foo { fn bar(): Int32; }
        class Bar
        impl Foo for Bar {
            fn bar(): Int32 { 1i32 }
        }
        fn f(): Foo { Bar() as Foo }
    ");

    ok("
        trait Foo { fn bar(): Int32; }
        fn f(x: Foo): Foo {
            let y = x;
            y
        }
    ");

    err(
        "
        trait Foo { fn bar(): Int32; }
        class Bar
        fn f(x: Foo) {}
        fn g() {
            f(Bar() as Foo)
        }
    ",
        (6, 15),
        12,
        crate::ErrorLevel::Error,
        &TYPE_NOT_IMPLEMENTING_TRAIT,
        args!("Bar", "Foo"),
    );
}

#[test]
fn infer_enum_type() {
    ok("fn f(): Option[Int32] {
        None
    }");

    ok("
        class X {
            a: Option[Int32],
            b: Option[Int32],
        }

        fn f(x: X) {
            x.a = Some(10i32);
            x.b = None;
        }
    ");

    ok("fn f() {
        let mut x: Option[Int32] = None; x = Some(10i32);
        let mut y: Option[Int32] = Some(10i32); y = None;
    }");

    ok("fn f(): Option[Int32] {
        Some(10i32)
    }");
}

#[test]
fn infer_enum_variant_in_call_arg() {
    ok("
        enum Maybe[T] { Some(T), Empty }
        fn take(value: Maybe[Int32]) {}
        fn f() { take(Maybe::Empty); }
    ");
}

#[test]
fn method_call_type_mismatch_with_type_params() {
    err(
        "
        class Foo
        impl Foo {
            fn f(a: String) {}
        }
        fn g[T](foo: Foo, value: T) {
            foo.f(value);
        }
    ",
        (7, 19),
        5,
        crate::ErrorLevel::Error,
        &WRONG_TYPE_FOR_ARGUMENT,
        args!("String", "T"),
    );
}

#[test]
fn basic_lambda() {
    ok("fn f(foo: (Int32): Int32): Int32 {
        foo(1i32)
    }");

    err(
        "fn f(foo: (Int32): Int32): Bool {
        foo(1i32)
    }",
        (1, 33),
        25,
        crate::ErrorLevel::Error,
        &RETURN_TYPE,
        args!("Bool", "Int32"),
    );

    err(
        "fn f(foo: (Int32, Int32): Int32): Int32 {
        foo(1i32)
    }",
        (2, 9),
        9,
        crate::ErrorLevel::Error,
        &MISSING_ARGUMENTS,
        args!(2, 1),
    );
}

#[test]
fn lambda_body() {
    ok("fn f(): (Int32): Int32 {
        |x: Int32|: Int32 { x }
    }");

    ok("fn f(): (Int32): Int32 {
        |x: Int32|: Int32 { x + 1i32 }
    }");

    err(
        "fn f(): (Int32): Int32 {
        |x: Int32|: Int32 { false }
    }",
        (2, 27),
        9,
        crate::ErrorLevel::Error,
        &RETURN_TYPE,
        args!("Int32", "Bool"),
    );
}

#[test]
fn lambda_closure() {
    ok("fn f() {
        let x: Int32 = 10i32;
        ||: Int32 { x };
    }");

    ok("fn f(x: Int32) {
        ||: Int32 { x };
    }");

    err(
        "fn f() {
        ||: Int32 { x };
        let x: Int32 = 10i32;
    }",
        (2, 21),
        1,
        crate::ErrorLevel::Error,
        &UNKNOWN_IDENTIFIER,
        args!("x"),
    );
}

#[test]
fn internal_class_ctor() {
    err(
        "fn f(): Array[Int32] {
        Array[Int32]()
    }",
        (2, 9),
        14,
        crate::ErrorLevel::Error,
        &CLASS_CONSTRUCTOR_NOT_ACCESSIBLE,
        args!("std::collections::Array"),
    );
}

#[test]
fn internal_struct_ctor() {
    err(
        "fn f() {
        Int32();
    }",
        (2, 9),
        7,
        crate::ErrorLevel::Error,
        &STRUCT_CONSTRUCTOR_NOT_ACCESSIBLE,
        args!("std::primitives::Int32"),
    );
}

#[test]
fn mutable_param() {
    ok("fn f(mut x: Int64) { x = 10; }");
    err(
        "fn f(x: Int64) { x = 10; }",
        (1, 18),
        6,
        crate::ErrorLevel::Error,
        &LET_REASSIGNED,
        args!(),
    );
}

#[test]
fn self_unavailable_in_lambda() {
    err(
        "fn f() { || { self; }; }",
        (1, 15),
        4,
        crate::ErrorLevel::Error,
        &THIS_UNAVAILABLE,
        args!(),
    );
}

#[test]
fn invalid_escape_sequence() {
    err(
        r#"
fn f() { '\k'; }
"#,
        (2, 11),
        2,
        crate::ErrorLevel::Error,
        &INVALID_ESCAPE_SEQUENCE,
        args!(),
    );

    err(
        r#"
fn f() { "\k"; }
"#,
        (2, 11),
        2,
        crate::ErrorLevel::Error,
        &INVALID_ESCAPE_SEQUENCE,
        args!(),
    );
}

#[test]
fn invalid_char_literal() {
    err(
        r#"
fn f() { 'abc'; }
"#,
        (2, 10),
        5,
        crate::ErrorLevel::Error,
        &INVALID_CHAR_LITERAL,
        args!(),
    );

    err(
        r#"
fn f() { 'abc'; }
"#,
        (2, 10),
        5,
        crate::ErrorLevel::Error,
        &INVALID_CHAR_LITERAL,
        args!(),
    );

    err(
        r#"
fn f() { ''; }
"#,
        (2, 10),
        2,
        crate::ErrorLevel::Error,
        &INVALID_CHAR_LITERAL,
        args!(),
    );
}

#[test]
fn immutable_struct_fields() {
    err(
        "
        struct Foo { value: Int64 }

        fn f(f: Foo) {
            f.value = 10;
        }
    ",
        (5, 13),
        12,
        crate::ErrorLevel::Error,
        &IMMUTABLE_FIELD,
        args!(),
    );
}

#[test]
fn missing_enum_arguments() {
    err(
        "
        fn f(): Option[Int64] {
            Some[Int64]
        }
    ",
        (3, 13),
        11,
        crate::ErrorLevel::Error,
        &ENUM_VARIANT_MISSING_ARGUMENTS,
        args!(),
    );
}

#[test]
fn use_needs_pub() {
    errors(
        "
        use self::test::Bar;
        fn foo(x: Bar) {}
        mod test {
            use self::Foo as Bar;
            pub struct Foo { i: Int64 }
        }
    ",
        vec![
            (
                (2, 25),
                3,
                crate::ErrorLevel::Error,
                &USE_NOT_ACCESSIBLE,
                args!(),
            ),
            (
                (3, 19),
                3,
                crate::ErrorLevel::Error,
                &UNKNOWN_IDENTIFIER,
                args!("Bar"),
            ),
        ],
    );
}

#[test]
fn type_bin_op_int32_equals_int_literal() {
    err(
        "fn f(x: Int32): Bool { x == 0 }",
        (1, 24),
        6,
        crate::ErrorLevel::Error,
        &BIN_OP_TYPE,
        args!("==", "Int32", "Int64"),
    );
}

#[test]
fn trait_for_tuple() {
    ok("
        trait Foo {}
        impl Foo for (Int64, Int64) {}
        fn f[T: Foo](x: T) {}
        fn g() {
            f[(Int64, Int64)]((1, 2));
        }
    ");
}

#[test]
fn trait_for_unit() {
    ok("
        trait Foo {}
        impl Foo for () {}
        fn f[T: Foo](x: T) {}
        fn g() {
            f[()](());
        }
    ");
}

#[test]
fn option_equals() {
    ok("
        fn ck(lhs: Option[Int64], rhs: Option[Int64]): Bool {
            lhs == rhs
        }
    ");
}

#[test]
fn equals_operator_on_type_param() {
    ok("
        fn eq[T: std::traits::Equals](lhs: T, rhs: T): Bool {
            lhs == rhs
        }
    ");

    err(
        "
        fn eq[T](lhs: T, rhs: T): Bool {
            lhs == rhs
        }
    ",
        (3, 13),
        10,
        crate::ErrorLevel::Error,
        &BIN_OP_TYPE,
        args!("==", "T", "T"),
    );

    err(
        "
        fn eq[T: std::traits::Equals](lhs: T, rhs: T): T {
            lhs == rhs
        }
    ",
        (2, 58),
        34,
        crate::ErrorLevel::Error,
        &RETURN_TYPE,
        args!("T", "Bool"),
    );

    err(
        "
        fn eq[T: std::traits::Equals](lhs: T, rhs: Int64): Bool {
            lhs == rhs
        }
    ",
        (3, 13),
        10,
        crate::ErrorLevel::Error,
        &BIN_OP_TYPE,
        args!("==", "T", "Int64"),
    );
}

#[test]
fn add_operator_on_type_param() {
    ok("
        fn plus[T: std::traits::Add](lhs: T, rhs: T): T {
            lhs + rhs
        }
    ");

    err(
        "
        fn plus[T](lhs: T, rhs: T): Bool {
            lhs + rhs
        }
    ",
        (3, 13),
        9,
        crate::ErrorLevel::Error,
        &BIN_OP_TYPE,
        args!("+", "T", "T"),
    );

    err(
        "
        fn plus[T: std::traits::Add](lhs: T, rhs: T): Bool {
            lhs + rhs
        }
    ",
        (2, 60),
        33,
        crate::ErrorLevel::Error,
        &RETURN_TYPE,
        args!("Bool", "T"),
    );

    err(
        "
        fn plus[T: std::traits::Add](lhs: T, rhs: Int64): T {
            lhs + rhs
        }
    ",
        (3, 13),
        9,
        crate::ErrorLevel::Error,
        &BIN_OP_TYPE,
        args!("+", "T", "Int64"),
    );
}

#[test]
fn test_generic_trait_method_call() {
    ok("
        trait Foo { fn foo(x: Int64); }
        fn f[T: Foo](t: T) {
            t.foo(1);
        }
    ");

    ok("
        trait Foo { fn foo(): Self; }
        fn f[T: Foo](t: T): T {
            t.foo()
        }
    ");

    ok("
        trait Foo { fn foo(rhs: Self): Self; }
        fn f[T: Foo](t: T, rhs: T): T {
            t.foo(rhs)
        }
    ");

    err(
        "
        trait Foo { fn foo(x: Int64); }
        fn f[T: Foo](t: T) {
            t.foo(1, 2);
        }
    ",
        (4, 22),
        1,
        crate::ErrorLevel::Error,
        &SUPERFLUOUS_ARGUMENT,
        args!(),
    );
}

#[test]
fn test_trait_with_type_params() {
    ok("
        trait Foo[A, B] {
            fn geta(): A;
            fn getb(): B;
        }
        fn f[T: Foo[Bool, Int64]](t: T) {
            let _: Bool = t.geta();
            let _: Int64 = t.getb();
        }
    ");

    ok("
        trait Foo[A, B] {
            fn f(a: A, b: B);
        }
        fn f[T: Foo[Bool, Int64]](t: T) {
            t.f(true, 12);
        }
    ");

    ok("
        trait Foo[A, B] {
            static fn f(a: A, b: B);
        }
        fn f[T: Foo[Bool, Int64]]() {
            T::f(true, 12);
        }
    ");
}

#[test]
fn alias_as_argument() {
    ok("
        type MyInt = Int64;
        fn x(v: MyInt) {}
        fn f() { x(6); }
    ");
}

#[test]
fn alias_in_local_type() {
    ok("
        type MyInt = Int64;
        fn f(y: Int64): Bool {
            let x: MyInt = 8;
            x == y
        }
    ");
}

#[test]
fn for_iterator_trait() {
    ok("
        use std::traits::IntoIterator;
        fn f() {
            let it = Array[Int64]::new(1, 2, 3).iter();
            let mut sum = 0;
            for x in it {
                sum = sum + x;
            }
        }
    ");
}

#[test]
fn self_in_extension_method() {
    ok("
        class X
        impl X {
            fn f(x: X) {
                let tmp: Self = x;
            }
        }
    ");
}

#[test]
fn self_in_impl_method() {
    ok("
        class X
        trait MyTrait {
            fn f(x: Self) {}
        }
        impl MyTrait for X {
            fn f(x: X) {
                let tmp: Self = x;
            }
        }
    ");
}

#[test]
fn is_pattern_no_args() {
    ok("
        enum Foo { A, B }
        fn isA(x: Foo): Bool {
            x is Foo::A
        }
    ");

    ok("
        enum Foo { A, B }
        fn isA(x: Foo): Bool {
            x is Foo
        }
    ");

    err(
        "
        enum Foo { A(Int64), B }
        fn isA(x: Foo): Bool {
            x is Foo::A
        }
    ",
        (4, 18),
        6,
        crate::ErrorLevel::Error,
        &PATTERN_WRONG_NUMBER_OF_PARAMS,
        args!(0, 1),
    );

    err(
        "
        enum Foo { A, B }
        fn isA(x: Int64): Bool {
            x is Foo::A
        }
    ",
        (4, 18),
        6,
        crate::ErrorLevel::Error,
        &PATTERN_TYPE_MISMATCH,
        args!("Int64"),
    );

    err(
        "
        enum Foo { A, B }
        enum Bar { C, D }
        fn isA(x: Bar): Bool {
            x is Foo::A
        }
    ",
        (5, 18),
        6,
        crate::ErrorLevel::Error,
        &PATTERN_TYPE_MISMATCH,
        args!("Bar"),
    );
}

#[test]
fn pattern_lit_bool() {
    ok("
        fn f(x: (String, Bool)) {
            let (y, true) = x;
        }
    ");

    err(
        "
    fn f(x: (String, Int64)) {
        let (y, true) = x;
    }
",
        (3, 17),
        4,
        crate::ErrorLevel::Error,
        &WRONG_TYPE,
        args!("Int64", "Bool"),
    );
}

#[test]
fn pattern_lit_char() {
    ok("
        fn f(x: (String, Char)) {
            let (y, 'a') = x;
        }
    ");

    err(
        "
    fn f(x: (String, Int64)) {
        let (y, 'a') = x;
    }
",
        (3, 17),
        3,
        crate::ErrorLevel::Error,
        &WRONG_TYPE,
        args!("Int64", "Char"),
    );
}

#[test]
fn pattern_lit_string() {
    ok("
        fn f(x: (String, String)) {
            let (y, \"a\") = x;
        }
    ");

    err(
        "
    fn f(x: (String, Int64)) {
        let (y, \"a\") = x;
    }
",
        (3, 17),
        3,
        crate::ErrorLevel::Error,
        &WRONG_TYPE,
        args!("Int64", "String"),
    );
}

#[test]
fn pattern_lit_int() {
    ok("
        fn f(x: (String, Int64)) {
            let (y, -2) = x;
        }
    ");

    err(
        "
    fn f(x: (String, Bool)) {
        let (y, 2) = x;
    }
",
        (3, 17),
        1,
        crate::ErrorLevel::Error,
        &WRONG_TYPE,
        args!("Bool", "Int64"),
    );
}

#[test]
fn pattern_lit_float() {
    ok("
        fn f(x: (String, Float64)) {
            let (y, -2.0) = x;
        }
    ");

    err(
        "
    fn f(x: (String, Bool)) {
        let (y, 2.0f32) = x;
    }
",
        (3, 17),
        6,
        crate::ErrorLevel::Error,
        &WRONG_TYPE,
        args!("Bool", "Float32"),
    );
}

#[test]
fn pattern_enum_variant_with_args() {
    ok("
        enum Foo { A(Int64), B }
        fn f(x: Foo): Int64 {
            let Foo::A(y) = x;
            y
        }
    ");

    err(
        "
    enum Foo { A(Int64), B }
    enum Bar { C(Int64), D }
    fn f(x: Foo): Int64 {
        let Bar::C(y) = x;
        y
    }
",
        (5, 13),
        9,
        crate::ErrorLevel::Error,
        &PATTERN_TYPE_MISMATCH,
        args!("Foo"),
    );

    err(
        "
    enum Foo { A(Int64), B }
    fn f(x: Foo): Int64 {
        let Foo::A(y, z) = x;
        z
    }
",
        (4, 13),
        12,
        crate::ErrorLevel::Error,
        &PATTERN_WRONG_NUMBER_OF_PARAMS,
        args!(2, 1),
    );
}

#[test]
fn pattern_enum_variant_no_args() {
    ok("
    enum Foo { A(Int64), B }
    fn f(x: Foo) {
        let Foo::B = x;
    }
");

    err(
        "
enum Foo { A(Int64), B }
fn f(x: Foo) {
    let Foo::A = x;
}
",
        (4, 9),
        6,
        crate::ErrorLevel::Error,
        &PATTERN_WRONG_NUMBER_OF_PARAMS,
        args!(0, 1),
    );
}

#[test]
fn pattern_underscore() {
    ok("
        enum Foo { A(Int64), B }
        fn f(x: Foo) {
            let _ = x;
        }
    ");
}

#[test]
fn pattern_in_if() {
    ok("
        enum Foo { A(Int64), B }
        fn f(x: Foo): Int64 {
            if x is Foo::A(y) {
                y
            } else {
                0
            }
        }
    ");

    err(
        "
        enum Foo { A(Int64), B }
        fn f(x: Foo): Int64 {
            if x is Foo::A(y) {
                0
            } else {
                y
            }
        }
    ",
        (7, 17),
        1,
        crate::ErrorLevel::Error,
        &UNKNOWN_IDENTIFIER,
        args!("y"),
    );
}

#[test]
fn pattern_in_if_with_condition() {
    ok("
        enum Foo { A(Int64), B }
        fn f(x: Foo): Int64 {
            if x is Foo::A(y) && y < 0 {
                y
            } else {
                0
            }
        }
    ");

    err(
        "
        enum Foo { A(Int64), B }
        fn f(x: Foo): Int64 {
            if x is Foo::A(y) && y > 0 {
                0
            } else {
                y
            }
        }
    ",
        (7, 17),
        1,
        crate::ErrorLevel::Error,
        &UNKNOWN_IDENTIFIER,
        args!("y"),
    );
}

#[test]
fn pattern_in_if_with_condition_with_parens() {
    err(
        "
        fn f(x: Option[Int]): Int {
            if (x is Some(y) && y > 0) {
                y
            } else {
                0
            }
        }
    ",
        (4, 17),
        1,
        crate::ErrorLevel::Error,
        &UNKNOWN_IDENTIFIER,
        args!("y"),
    );

    errors(
        "
        fn f(x: Option[Int], y: Option[Int]): Int {
            if (x is Some(x1) && x1 > 0) && (y is Some(y1) && y1 == 0) {
                x1 + y1
            } else {
                0
            }
        }
    ",
        vec![
            (
                (4, 17),
                2,
                crate::ErrorLevel::Error,
                &UNKNOWN_IDENTIFIER,
                args!("x1"),
            ),
            (
                (4, 22),
                2,
                crate::ErrorLevel::Error,
                &UNKNOWN_IDENTIFIER,
                args!("y1"),
            ),
        ],
    );
}

#[test]
fn multiple_pattern_in_if_with_condition() {
    ok("
        fn f(x: Option[Int], y: Option[Int], z: Option[Int]): Int {
            if x is Some(x) && y is Some(y) && z is Some(z) &&
                x > 0 && y == 0 && z < 0 {
                x + y + z
            } else {
                0
            }
        }
    ");
}

#[test]
fn pattern_in_while_with_condition() {
    ok("
        enum Foo { A(Int64), B }
        fn f(x: Foo) {
            while x is Foo::A(y) && y < 0 {
                y
            }
        }
    ");
}

#[test]
fn pattern_in_expression() {
    ok("
        enum Foo { A(Int64), B }
        fn f(x: Foo): Bool {
            x is Foo::A(y) && y > 0
        }
    ");

    err(
        "
        enum Foo { A(Int64), B }
        fn f(x: Foo): Bool {
            x is Foo::A(y) && y
        }
    ",
        (4, 31),
        1,
        crate::ErrorLevel::Error,
        &WRONG_TYPE,
        args!("Bool", "Int64"),
    );
}

#[test]
fn pattern_in_params() {
    ok("
        fn f((x, y): (Int, Int)): Int {
            x + y
        }
    ");

    err(
        "
        fn f((x, y): Int): Int {
            x + y
        }
    ",
        (2, 14),
        6,
        crate::ErrorLevel::Error,
        &PATTERN_TUPLE_EXPECTED,
        args!("Int64"),
    );
}

#[test]
fn pattern_class_with_args() {
    ok("
        class Foo(Int64, String)
        fn f(x: Foo): Int64 {
            let Foo(a, b) = x;
            a
        }
    ");

    err(
        "
    class Foo(Int64, String)
    class Bar(Int64, String)
    fn f(x: Foo): Int64 {
        let Bar(a, b) = x;
        b
    }
",
        (5, 13),
        9,
        crate::ErrorLevel::Error,
        &PATTERN_TYPE_MISMATCH,
        args!("Foo"),
    );

    err(
        "
    class Foo(Int64)
    fn f(x: Foo): Int64 {
        let Foo(a, b) = x;
        b
    }
",
        (4, 13),
        9,
        crate::ErrorLevel::Error,
        &PATTERN_WRONG_NUMBER_OF_PARAMS,
        args!(2, 1),
    );
}

#[test]
fn pattern_class_no_args() {
    ok("
    class Foo
    fn f(x: Foo) {
        let Foo = x;
    }
");

    err(
        "
            class Foo(Int64)
            fn f(x: Foo) {
                let Foo = x;
            }
        ",
        (4, 21),
        3,
        crate::ErrorLevel::Error,
        &PATTERN_WRONG_NUMBER_OF_PARAMS,
        args!(0, 1),
    );
}

#[test]
fn pattern_struct_with_args() {
    ok("
        struct Foo(Int64, String)
        fn f(x: Foo): Int64 {
            let Foo(a, b) = x;
            a
        }
    ");

    err(
        "
    struct Foo(Int64, String)
    struct Bar(Int64, String)
    fn f(x: Foo): Int64 {
        let Bar(a, b) = x;
        b
    }
",
        (5, 13),
        9,
        crate::ErrorLevel::Error,
        &PATTERN_TYPE_MISMATCH,
        args!("Foo"),
    );

    err(
        "
    struct Foo(Int64)
    fn f(x: Foo): Int64 {
        let Foo(a, b) = x;
        b
    }
",
        (4, 13),
        9,
        crate::ErrorLevel::Error,
        &PATTERN_WRONG_NUMBER_OF_PARAMS,
        args!(2, 1),
    );
}

#[test]
fn pattern_struct_no_args() {
    ok("
    struct Foo
    fn f(x: Foo) {
        let Foo = x;
    }
");

    err(
        "
struct Foo(Int64)
fn f(x: Foo) {
    let Foo = x;
}
",
        (4, 9),
        3,
        crate::ErrorLevel::Error,
        &PATTERN_WRONG_NUMBER_OF_PARAMS,
        args!(0, 1),
    );
}

#[test]
fn pattern_struct_private_ctor() {
    err(
        "
        mod n {
            pub struct Foo(pub Int64, String)
        }
        fn f(x: n::Foo): Int64 {
            let n::Foo(a, b) = x;
            a
        }
    ",
        (6, 17),
        12,
        crate::ErrorLevel::Error,
        &STRUCT_CONSTRUCTOR_NOT_ACCESSIBLE,
        args!("n::Foo"),
    );
}

#[test]
fn pattern_class_private_ctor() {
    err(
        "
        mod n {
            pub class Foo(pub Int64, String)
        }
        fn f(x: n::Foo): Int64 {
            let n::Foo(a, b) = x;
            a
        }
    ",
        (6, 17),
        12,
        crate::ErrorLevel::Error,
        &CLASS_CONSTRUCTOR_NOT_ACCESSIBLE,
        args!("n::Foo"),
    );
}

#[test]
fn pattern_duplicate_binding() {
    err(
        "
        struct Foo(Int64, String)
        fn f(x: Foo): Int64 {
            let Foo(a, a) = x;
            a
        }
    ",
        (4, 24),
        1,
        crate::ErrorLevel::Error,
        &PATTERN_DUPLICATE_BINDING,
        args!(),
    );
}

#[test]
fn pattern_bindings_in_alternatives() {
    ok("
        enum Foo { A(Int64), B(Int64, Int64), C }
        fn f(x: Foo): Int64 {
            match x {
                Foo::A(x) | Foo::B(x, _) => x,
                Foo::C => 1
            }
        }
    ");

    err(
        "
    enum Foo { A(Int64), B(Float32), C }
    fn f(x: Foo): Int64 {
        match x {
            Foo::A(x) | Foo::B(x) => x,
            Foo::C => 1
        }
    }
",
        (5, 32),
        1,
        crate::ErrorLevel::Error,
        &PATTERN_BINDING_WRONG_TYPE,
        args!("Float32", "Int64"),
    );

    err(
        "
    enum Foo { A(Int64), B(Int64, Float32), C }
    fn f(x: Foo): Int64 {
        match x {
            Foo::A(x) | Foo::B(x, y) => x,
            Foo::C => 1
        }
    }
",
        (5, 35),
        1,
        crate::ErrorLevel::Error,
        &PATTERN_BINDING_NOT_DEFINED_IN_ALL_ALTERNATIVES,
        args!("y"),
    );
}

#[test]
fn test_pattern_rest() {
    err(
        "
        fn f(x: Int64) {
            let .. = x;
        }
    ",
        (3, 17),
        2,
        crate::ErrorLevel::Error,
        &PATTERN_UNEXPECTED_REST,
        args!(),
    );

    err(
        "
        fn f(x: (Int64, Int64)) {
            let (.., a, ..) = x;
        }
    ",
        (3, 25),
        2,
        crate::ErrorLevel::Error,
        &PATTERN_MULTIPLE_REST,
        args!(),
    );

    ok("
        fn f(x: (Int64, Int64)): Int64 {
            let (.., a) = x;
            a
        }
    ");

    ok("
        fn f(x: (Int64, Int64)): Int64 {
            let (.., a, b) = x;
            a + b
        }
");

    ok("
        fn f(x: (Int64, Int64)): Int64 {
            let (a, .., b) = x;
            a + b
        }
");

    ok("
        fn f(x: (Int64, Bool, Float64)): (Int64, Float64) {
            let (a, .., b) = x;
            (a, b)
        }
");

    ok("
        fn f(x: (Int64, Int64)): Int64 {
            let (a, b, ..) = x;
            a + b
        }
");

    ok("
        fn f(x: (Int64, Int64)) {
            let (..) = x;
        }
");

    err(
        "
fn f(x: (Int64, Int64)) {
    let (a, b, c, ..) = x;
}
",
        (3, 9),
        13,
        crate::ErrorLevel::Error,
        &PATTERN_WRONG_NUMBER_OF_PARAMS,
        args!(3, 2),
    );
}

#[test]
fn type_param_failure_in_container() {
    err(
        "
        struct Foo[T](T)

        impl[T: Unknown] Foo[T] {
            fn f() {}
            fn g() {}
        }
    ",
        (4, 17),
        7,
        crate::ErrorLevel::Error,
        &UNKNOWN_IDENTIFIER,
        args!("Unknown"),
    );
}

#[test]
fn impl_method_lookup_on_missing_trait_method() {
    errors(
        "
        trait A { fn f(): Int64; }
        trait B: A { fn g(): Int64; }
        trait C: B { fn h(): Int64; }

        impl A for Int64 {
            fn f(): Int64 { 100 }
        }

        impl B for Int64 {
            fn g(): Int64 { 200 }
        }

        impl C for Int64 {
            fn g(): Int64 { 300 }
        }

        fn f(x: Int64) {
            x.f();
            x.g();
            x.h();
        }
    ",
        vec![
            (
                (14, 9),
                327,
                crate::ErrorLevel::Error,
                &ELEMENT_NOT_IN_IMPL,
                args!("h"),
            ),
            (
                (15, 13),
                21,
                crate::ErrorLevel::Error,
                &ELEMENT_NOT_IN_TRAIT,
                args!(),
            ),
            (
                (21, 13),
                5,
                crate::ErrorLevel::Error,
                &UNKNOWN_METHOD,
                args!("Int64", "h"),
            ),
        ],
    );
}

#[test]
fn call_with_named_arguments() {
    errors(
        "
        fn f(x: Int, y: Int) {}
        fn g() {
            f(1, 2, y = 3);
        }
    ",
        vec![
            (
                (4, 21),
                1,
                crate::ErrorLevel::Error,
                &UNEXPECTED_NAMED_ARGUMENT,
                args!(),
            ),
            (
                (4, 21),
                5,
                crate::ErrorLevel::Error,
                &SUPERFLUOUS_ARGUMENT,
                args!(),
            ),
        ],
    );
}

#[test]
fn duplicate_named_argument() {
    errors(
        "
        class Foo { x: Int, y: Int }
        fn g() {
            Foo(x = 1, y = 3, y = 4);
        }
    ",
        vec![(
            (4, 31),
            5,
            crate::ErrorLevel::Error,
            &DUPLICATE_NAMED_ARGUMENT,
            args!(),
        )],
    );
}

#[test]
fn class_ctor_with_named_argument() {
    ok("
        class Foo { a: Int, b: String }
        fn f() {
            Foo(a=1, b=\"bar\");
            Foo(b=\"bar\", a=1);
        }
    ");

    err(
        "
        class Foo { a: Int, b: Bool }
        fn f() {
            Foo(b=true);
        }
    ",
        (4, 13),
        11,
        crate::ErrorLevel::Error,
        &MISSING_NAMED_ARGUMENT,
        args!("a"),
    );

    err(
        "
        class Foo { a: Int, b: Bool }
        fn f() {
            Foo(b=true, a=1, c=2.4);
        }
    ",
        (4, 30),
        5,
        crate::ErrorLevel::Error,
        &USE_OF_UNKNOWN_ARGUMENT,
        args!(),
    );
}

#[test]
fn class_ctor_with_generic_assoc_named() {
    ok("
        trait Trait1 {
            type X;
        }
        class Foo[T: Trait1] { a: T, b: T::X }
        impl Trait1 for Int {
            type X = String;
        }
        fn f(): String {
            let foo = Foo[Int](a=1, b=\"bar\");
            foo.b
        }
    ");
}

#[test]
fn class_ctor_with_generic_assoc_unnamed() {
    ok("
        trait Trait1 {
            type X;
        }
        class Foo[T: Trait1](T, T::X)
        impl Trait1 for Int {
            type X = String;
        }
        fn f(): String {
            let foo = Foo[Int](1, \"bar\");
            foo.1
        }
    ");
}

#[test]
fn struct_ctor_with_generic_assoc_named() {
    ok("
        trait Trait1 {
            type X;
        }
        struct Foo[T: Trait1] { a: T, b: T::X }
        impl Trait1 for Int {
            type X = String;
        }
        fn f(): String {
            let foo = Foo[Int](a=1, b=\"bar\");
            foo.b
        }
    ");
}

#[test]
fn struct_ctor_with_generic_assoc_unnamed() {
    ok("
        trait Trait1 {
            type X;
        }
        struct Foo[T: Trait1](T, T::X)
        impl Trait1 for Int {
            type X = String;
        }
        fn f(): String {
            let foo = Foo[Int](1, \"bar\");
            foo.1
        }
    ");
}

#[test]
fn class_ctor_with_named_argument_of_wrong_type() {
    err(
        "
        class Foo { a: Int, b: Bool }
        fn f() {
            Foo(a = 1, b = 2);
        }
    ",
        (4, 24),
        5,
        crate::ErrorLevel::Error,
        &WRONG_TYPE_FOR_ARGUMENT,
        args!("Bool", "Int64"),
    );
}

#[test]
fn class_ctor_with_ident_expr_used_as_named_argument() {
    ok("
        class Foo { a: Int, b: Bool }
        fn f(a: Int, b: Bool) {
            Foo(a, b);
        }
    ");
}

#[test]
fn unnamed_class_field() {
    ok("
        class Foo(Int, Bool)
        fn f(x: Foo): Int {
            x.0
        }
    ");

    ok("
        class Foo(Int, Bool)
        fn f(x: Foo): Bool {
            x.1
        }
    ");

    err(
        "
        class Foo(Int, Bool)
        fn f(x: Foo): Bool {
            x.2
        }
    ",
        (4, 15),
        1,
        crate::ErrorLevel::Error,
        &UNKNOWN_FIELD,
        args!("2", "Foo"),
    );

    err(
        "
        mod m {
            pub class Foo(Int, Bool)
        }

        fn f(x: m::Foo): Int {
            x.0
        }
    ",
        (7, 15),
        1,
        crate::ErrorLevel::Error,
        &NOT_ACCESSIBLE,
        args!(),
    );
}

#[test]
fn unnamed_class_field_assignment() {
    ok("
        class Foo(Int, Bool)
        fn f(x: Foo, v: Int) {
            x.0 = v;
        }
    ");

    ok("
        class Foo(Int, Bool)
        fn f(x: Foo, v: Bool) {
            x.1 = v;
        }
    ");

    err(
        "
        class Foo(Int, Bool)
        fn f(x: Foo, v: String) {
            x.2 = v;
        }
    ",
        (4, 13),
        3,
        crate::ErrorLevel::Error,
        &UNKNOWN_FIELD,
        args!("2", "Foo"),
    );

    err(
        "
        mod m {
            pub class Foo(Int, Bool)
        }

        fn f(x: m::Foo, v: Int) {
            x.0 = v;
        }
    ",
        (7, 13),
        3,
        crate::ErrorLevel::Error,
        &NOT_ACCESSIBLE,
        args!(),
    );
}

#[test]
fn unnamed_class_field_assignment_to_named_field() {
    err(
        "
        class Foo { a: Int, b: Bool }
        fn f(x: Foo, v: Int) {
            x.0 = v;
        }
    ",
        (4, 13),
        3,
        crate::ErrorLevel::Error,
        &UNKNOWN_FIELD,
        args!("0", "Foo"),
    );
}

#[test]
fn unnamed_access_on_named_field() {
    err(
        "
        class Foo { a: Int, b: Bool }
        fn f(x: Foo): Int {
            x.0
        }
    ",
        (4, 15),
        1,
        crate::ErrorLevel::Error,
        &UNKNOWN_FIELD,
        args!("0", "Foo"),
    );
}

#[test]
fn unnamed_struct_field() {
    ok("
        struct Foo(Int, Bool)
        fn f(x: Foo): Int {
            x.0
        }
    ");

    ok("
        struct Foo(Int, Bool)
        fn f(x: Foo): Bool {
            x.1
        }
    ");

    err(
        "
        struct Foo(Int, Bool)
        fn f(x: Foo): Bool {
            x.2
        }
    ",
        (4, 15),
        1,
        crate::ErrorLevel::Error,
        &UNKNOWN_FIELD,
        args!("2", "Foo"),
    );

    err(
        "
        mod m {
            pub struct Foo(Int, Bool)
        }

        fn f(x: m::Foo): Int {
            x.0
        }
    ",
        (7, 15),
        1,
        crate::ErrorLevel::Error,
        &NOT_ACCESSIBLE,
        args!(),
    );
}

#[test]
fn unnamed_struct_field_assignment() {
    err(
        "
        struct Foo(Int, Bool)
        fn f(x: Foo, v: Int) {
            x.0 = v;
        }
    ",
        (4, 13),
        7,
        crate::ErrorLevel::Error,
        &IMMUTABLE_FIELD,
        args!(),
    );

    errors(
        "
        struct Foo(Int, Bool)
        fn f(x: Foo, v: Bool) {
            x.0 = v;
        }
    ",
        vec![
            (
                (4, 13),
                7,
                crate::ErrorLevel::Error,
                &ASSIGN_FIELD,
                args!("0", "Foo", "Int64", "Bool"),
            ),
            (
                (4, 13),
                7,
                crate::ErrorLevel::Error,
                &IMMUTABLE_FIELD,
                args!(),
            ),
        ],
    );

    err(
        "
        struct Foo(Int, Bool)
        fn f(x: Foo, v: Bool) {
            x.2 = v;
        }
    ",
        (4, 13),
        3,
        crate::ErrorLevel::Error,
        &UNKNOWN_FIELD,
        args!("2", "Foo"),
    );

    errors(
        "
        mod m {
            pub struct Foo(Int, Bool)
        }

        fn f(x: m::Foo, v: Int) {
            x.0 = v;
        }
    ",
        vec![
            (
                (7, 13),
                3,
                crate::ErrorLevel::Error,
                &NOT_ACCESSIBLE,
                args!(),
            ),
            (
                (7, 13),
                7,
                crate::ErrorLevel::Error,
                &IMMUTABLE_FIELD,
                args!(),
            ),
        ],
    );
}

#[test]
fn unnamed_tuple_field_assignment() {
    err(
        "
        fn f(x: (Int, Bool), v: Int) {
            x.0 = v;
        }
    ",
        (3, 13),
        7,
        crate::ErrorLevel::Error,
        &IMMUTABLE_FIELD,
        args!(),
    );

    err(
        "
        fn f(x: (Int, Bool), v: Int) {
            x.2 = v;
        }
    ",
        (3, 13),
        3,
        crate::ErrorLevel::Error,
        &UNKNOWN_FIELD,
        args!("2", "(Int64, Bool)"),
    );

    errors(
        "
        fn f(x: (Int, Bool), v: Bool) {
            x.0 = v;
        }
    ",
        vec![
            (
                (3, 13),
                7,
                crate::ErrorLevel::Error,
                &ASSIGN_FIELD,
                args!("0", "(Int64, Bool)", "Int64", "Bool"),
            ),
            (
                (3, 13),
                7,
                crate::ErrorLevel::Error,
                &IMMUTABLE_FIELD,
                args!(),
            ),
        ],
    );
}

#[test]
fn enum_variant_named_fields() {
    ok("
        enum Foo {
            A(Int, Int),
            B { a: Int, b: Int },
        }
        fn f() {
            Foo::A(1, 2);
            Foo::B(a=1, b=2);
        }
    ");
}

#[test]
fn enum_variant_missing_named_field() {
    err(
        "
        enum Foo {
            A,
            B { a: Int, b: Int },
        }
        fn f() {
            Foo::B(a=1);
        }
    ",
        (7, 13),
        11,
        crate::ErrorLevel::Error,
        &MISSING_NAMED_ARGUMENT,
        args!("b"),
    );
}

#[test]
fn enum_variant_positional_argument_for_named_field() {
    errors(
        "
        enum Foo {
            A,
            B { a: Int, b: Int },
        }
        fn f() {
            Foo::B(1, 2);
        }
    ",
        vec![
            (
                (7, 13),
                12,
                crate::ErrorLevel::Error,
                &MISSING_NAMED_ARGUMENT,
                args!("a"),
            ),
            (
                (7, 13),
                12,
                crate::ErrorLevel::Error,
                &MISSING_NAMED_ARGUMENT,
                args!("b"),
            ),
            (
                (7, 20),
                1,
                crate::ErrorLevel::Error,
                &UNEXPECTED_POSITIONAL_ARGUMENT,
                args!(),
            ),
            (
                (7, 23),
                1,
                crate::ErrorLevel::Error,
                &UNEXPECTED_POSITIONAL_ARGUMENT,
                args!(),
            ),
        ],
    );
}

#[test]
fn struct_named_pattern() {
    ok("
        struct Foo { a: Int, b: Int }
        fn f(x: Foo): Int {
            let Foo(a = a, b = b) = x;
            a + b
        }
    ");

    ok("
        struct Foo { a: Int, b: Int }
        fn f(x: Foo): Int {
            let Foo(a, b) = x;
            a + b
        }
    ");
}

#[test]
fn struct_named_pattern_missing_argument() {
    err(
        "
        struct Foo { a: Int, b: Int }
        fn f(x: Foo): Int {
            let Foo(a) = x;
            a
        }
    ",
        (4, 17),
        6,
        crate::ErrorLevel::Error,
        &MISSING_NAMED_ARGUMENT,
        args!("b"),
    );
}

#[test]
fn struct_named_pattern_unexpected_argument() {
    err(
        "
        struct Foo { a: Int, b: Int }
        fn f(x: Foo): Int {
            let Foo(a, b, c) = x;
            a
        }
    ",
        (4, 27),
        1,
        crate::ErrorLevel::Error,
        &UNEXPECTED_NAMED_ARGUMENT,
        args!(),
    );
}

#[test]
fn struct_named_pattern_duplicate_argument() {
    err(
        "
        struct Foo { a: Int, b: Int }
        fn f(x: Foo): Int {
            let Foo(a, b, a) = x;
            a
        }
    ",
        (4, 27),
        1,
        crate::ErrorLevel::Error,
        &DUPLICATE_NAMED_ARGUMENT,
        args!(),
    );
}

#[test]
fn struct_named_pattern_expected() {
    err(
        "
        struct Foo { a: Int, b: Int }
        fn f(x: Foo): Int {
            let Foo(1, a, b) = x;
            a
        }
    ",
        (4, 21),
        1,
        crate::ErrorLevel::Error,
        &EXPECTED_NAMED_PATTERN,
        args!(),
    );
}

#[test]
fn struct_named_pattern_no_args() {
    err(
        "
        struct Foo { a: Int, b: Int }
        fn f(x: Foo): Int {
            let Foo = x;
            0
        }
    ",
        (4, 17),
        3,
        crate::ErrorLevel::Error,
        &PATTERN_WRONG_NUMBER_OF_PARAMS,
        args!(0, 2),
    );
}

#[test]
fn struct_named_pattern_rest() {
    ok("
        struct Foo { a: Int, b: Int }
        fn f(x: Foo): Int {
            let Foo(a, ..) = x;
            a
        }
    ");
}

#[test]
fn struct_named_pattern_rest_last() {
    err(
        "
        struct Foo { a: Int, b: Int }
        fn f(x: Foo): Int {
            let Foo(.., a) = x;
            a
        }
    ",
        (4, 21),
        2,
        crate::ErrorLevel::Error,
        &PATTERN_REST_SHOULD_BE_LAST,
        args!(),
    );
}

#[test]
fn class_named_pattern() {
    ok("
        class Foo { a: Int, b: Int }
        fn f(x: Foo): Int {
            let Foo(a = x, b = y) = x;
            x + y
        }
    ");

    ok("
        class Foo { a: Int, b: Int }
        fn f(x: Foo): Int {
            let Foo(a, b) = x;
            a + b
        }
    ");
}

#[test]
fn enum_named_pattern() {
    ok("
        enum Foo {
            A,
            B { a: Int, b: Int }
        }

        fn f(x: Foo): Int {
            let Foo::B(a = x, b = y) = x;
            x + y
        }
    ");

    ok("
        enum Foo {
            A,
            B { a: Int, b: Int }
        }

        fn f(x: Foo): Int {
            let Foo::B(a, b) = x;
            a + b
        }
    ");
}

#[test]
fn struct_index_get() {
    ok("
        struct Foo { a: Float64, b: Float64 }
        impl std::traits::IndexGet for Foo {
            type Index = Int;
            type Item = Float64;
            fn get(index: Self::Index): Self::Item {
                if index == 0 {
                    self.a
                } else {
                    assert(index == 1);
                    self.b
                }
            }
        }
        fn f(x: Foo): Float64 {
            x(0)
        }
    ");
}

#[test]
fn struct_index_get_wrong_index_type() {
    err(
        "
        struct Foo { a: Float64, b: Float64 }
        impl std::traits::IndexGet for Foo {
            type Index = Int;
            type Item = Float64;
            fn get(index: Self::Index): Self::Item {
                if index == 0 {
                    self.a
                } else {
                    assert(index == 1);
                    self.b
                }
            }
        }
        fn f(x: Foo): Float64 {
            x(0.0)
        }
    ",
        (16, 15),
        3,
        crate::ErrorLevel::Error,
        &WRONG_TYPE_FOR_ARGUMENT,
        args!("Int64", "Float64"),
    );
}

#[test]
fn class_index_set() {
    ok("
        class Foo { a: Float64, b: Float64 }
        impl std::traits::IndexSet for Foo {
            type Index = Int;
            type Item = Float64;
            fn set(index: Self::Index, value: Self::Item) {
                if index == 0 {
                    self.a = value;
                } else {
                    assert(index == 1);
                    self.b = value;
                }
            }
        }
        fn f(x: Foo, value: Float64) {
            x(0) = value;
        }
    ");
}

#[test]
fn class_index_get_generic() {
    ok("
        class Foo[T] { a: T, b: T }
        impl[T] std::traits::IndexGet for Foo[T] {
            type Index = Int;
            type Item = T;
            fn get(index: Self::Index): Self::Item {
                if index == 0 {
                    self.a
                } else {
                    assert(index == 1);
                    self.b
                }
            }
        }
        fn f(x: Foo[Float64]): Float64 {
            x(0)
        }
    ");
}

#[test]
fn class_index_set_generic() {
    ok("
        class Foo[T] { a: T, b: T }
        impl[T] std::traits::IndexSet for Foo[T] {
            type Index = Int;
            type Item = T;
            fn set(index: Self::Index, value: Self::Item) {
                if index == 0 {
                    self.a = value;
                } else {
                    assert(index == 1);
                    self.b = value;
                }
            }
        }
        fn f(x: Foo[Float64], value: Float64) {
            x(0) = value;
        }
    ");
}

#[test]
fn class_index_set_wrong_type() {
    err(
        "
        class Foo { a: Float64, b: Float64 }
        impl std::traits::IndexSet for Foo {
            type Index = Int;
            type Item = Float64;
            fn set(index: Self::Index, value: Self::Item) {
                if index == 0 {
                    self.a = value;
                } else {
                    assert(index == 1);
                    self.b = value;
                }
            }
        }
        fn f(x: Foo, value: Float64) {
            x(0.0) = value;
        }
    ",
        (16, 15),
        3,
        crate::ErrorLevel::Error,
        &WRONG_TYPE_FOR_ARGUMENT,
        args!("Int64", "Float64"),
    );
}

#[test]
fn class_index_set_wrong_item_type() {
    err(
        "
        class Foo { a: Float64, b: Float64 }
        impl std::traits::IndexSet for Foo {
            type Index = Int;
            type Item = Float64;
            fn set(index: Self::Index, value: Self::Item) {
                if index == 0 {
                    self.a = value;
                } else {
                    assert(index == 1);
                    self.b = value;
                }
            }
        }
        fn f(x: Foo, value: Float32) {
            x(0) = value;
        }
    ",
        (16, 20),
        5,
        crate::ErrorLevel::Error,
        &WRONG_TYPE_FOR_ARGUMENT,
        args!("Float64", "Float32"),
    );
}

#[test]
fn trait_import_missing() {
    errors(
        "
        fn f(a: Int, b: Int): Int {
            a.add(b)
        }
    ",
        vec![(
            (3, 13),
            8,
            crate::ErrorLevel::Error,
            &UNKNOWN_METHOD,
            args!("Int64", "add"),
        )],
    );
}

#[test]
fn import_trait_for_impl_call() {
    ok("
        use std::traits::Add;
        fn f(a: Int, b: Int): Int {
            a.add(b)
        }
    ");
}

#[test]
fn impl_method_call_with_impl_in_another_package() {
    pkg_test(
        "
        extern package dep1;

        use dep1::MyClass;

        pub trait Bar {
            fn f();
        }

        impl Bar for MyClass {
            fn f() {}
        }
    ",
        &[(
            "dep1",
            "
        pub trait Foo {
            fn f();
        }

        pub class MyClass

        impl Foo for MyClass {
            fn f() {}
        }

        fn f(x: MyClass) {
            x.f();
        }
            ",
        )],
        &[],
    );
}

#[test]
fn impl_method_call_with_invalid_impl_in_another_package() {
    pkg_test(
        "
        extern package dep1;

        use dep1::{Bar, MyClass};

        impl Bar for MyClass {
            fn f() {}
        }
    ",
        &[(
            "dep1",
            "
        pub trait Foo {
            fn f();
        }

        pub trait Bar {
            fn f();
        }

        pub class MyClass

        impl Foo for MyClass {
            fn f() {}
        }

        fn f(x: MyClass) {
            x.f();
        }
            ",
        )],
        &[(
            (6, 9),
            None,
            crate::ErrorLevel::Error,
            &IMPL_TRAIT_FOREIGN_TYPE,
            args!(),
        )],
    );
}

#[test]
fn trait_object_argument_with_assoc_type() {
    ok("
        trait Foo {
            type X;
        }
        fn accept(x: Foo[X=Int]) {}
    ");
}

#[test]
fn convert_to_trait_object_with_assoc_type() {
    ok("
        trait Foo {
            type X;
        }
        impl Foo for Int { type X=Bool; }
        fn accept(x: Int) {
            x as Foo[X=Bool];
        }
    ");

    err(
        "
        trait Foo {
            type X;
        }
        impl Foo for Int { type X=Bool; }
        fn accept(x: Int) {
            x as Foo[X=String];
        }
    ",
        (7, 13),
        18,
        crate::ErrorLevel::Error,
        &TYPE_NOT_IMPLEMENTING_TRAIT,
        args!("Int64", "Foo[X = String]"),
    );
}

#[test]
#[ignore]
fn trait_object_method_with_assoc_type_parameter() {
    ok("
        trait Foo {
            type X;
            fn accept(v: Self::X);
        }
        impl Foo for Int {
            type X = Int;
            fn accept(v: Int) {}
        }
        fn accept(x: Foo[X=Int]) {
            x.accept(10);
        }
    ");
}

#[test]
fn call_trait_object_extension_method_with_assoc_type_parameter() {
    ok("
        trait Foo {
            type X;
            fn accept(v: Self::X);
        }
        impl Foo for Int {
            type X = Int;
            fn accept(v: Int) {}
        }
        impl Foo[X=Int] {
            fn another(v: Int) {}
        }
        fn accept(x: Foo[X=Int]) {
            x.another(10);
        }
    ");
}

#[test]
fn add_assign_operator_for_int() {
    ok("
        fn f(mut x: Int, y: Int): Int {
            x += y;
            x
        }
    ");

    err(
        "
        fn f(mut x: Int, y: Int32): Int {
            x += y;
            x
        }
    ",
        (3, 13),
        6,
        crate::ErrorLevel::Error,
        &BIN_OP_TYPE,
        args!("+=", "Int64", "Int32"),
    );
}

#[test]
fn add_assign_operator_error() {
    err(
        "
        struct MyInt(Int)
        fn f(mut x: MyInt, y: MyInt): MyInt {
            x += y;
            x
        }
    ",
        (4, 13),
        6,
        crate::ErrorLevel::Error,
        &BIN_OP_TYPE,
        args!("+=", "MyInt", "MyInt"),
    );
}

#[test]
fn sub_assign_operator_for_int() {
    ok("
        fn f(mut x: Int, y: Int): Int {
            x -= y;
            x
        }
    ");

    err(
        "
        fn f(mut x: Int, y: Int32): Int {
            x -= y;
            x
        }
    ",
        (3, 13),
        6,
        crate::ErrorLevel::Error,
        &BIN_OP_TYPE,
        args!("-=", "Int64", "Int32"),
    );
}

#[test]
fn array_compound_assignment() {
    ok("
        fn f(array: Array[Int], value: Int) {
            array(99) += value;
        }

        fn g(array: Array[Int], value: Int) {
            array(87) -= value;
        }
    ");
}

#[test]
fn array_compound_assignment_missing_op_trait() {
    err(
        "
        fn f(array: Array[Float64], value: Float64) {
            array(99) %= value;
        }
    ",
        (3, 13),
        18,
        crate::ErrorLevel::Error,
        &BIN_OP_TYPE,
        args!("%=", "Float64", "Float64"),
    );
}

#[test]
fn array_compound_assignment_missing_index_get() {
    err(
        "
        struct Foo(Int)

        impl std::traits::IndexSet for Foo {
            type Index = Int;
            type Item = Float64;

            fn set(index: Int, value: Float64) {}
        }

        fn f(array: Foo, value: Float64) {
            array(99) += value;
        }
    ",
        (12, 13),
        18,
        crate::ErrorLevel::Error,
        &INDEX_GET_NOT_IMPLEMENTED,
        args!("Foo"),
    );
}

#[test]
fn array_compound_assignment_missing_index_set() {
    err(
        "
        struct Foo(Int)

        impl std::traits::IndexGet for Foo {
            type Index = Int;
            type Item = Float64;

            fn get(index: Int): Float64 { 0.0 }
        }

        fn f(array: Foo, value: Float64) {
            array(99) += value;
        }
    ",
        (12, 13),
        18,
        crate::ErrorLevel::Error,
        &INDEX_SET_NOT_IMPLEMENTED,
        args!("Foo"),
    );
}

#[test]
fn array_compound_assignment_mismatch() {
    err(
        "
        struct Foo(Int)

        impl std::traits::IndexGet for Foo {
            type Index = Int;
            type Item = Float64;

            fn get(index: Int): Float64 { 0.0 }
        }

        impl std::traits::IndexSet for Foo {
            type Index = Int;
            type Item = Float32;

            fn set(index: Int, value: Float32) {}
        }

        fn f(array: Foo, value: Float64) {
            array(99) += value;
        }
    ",
        (19, 13),
        5,
        crate::ErrorLevel::Error,
        &INDEX_GET_AND_INDEX_SET_DO_NOT_MATCH,
        args!(),
    );
}

#[test]
fn assign_to_global_with_path() {
    ok("
        mod foo {
            pub mod bar {
                let mut x: Int = 0;
            }
        }

        fn f(value: Int) {
            foo::bar::x = value;
        }
    ");
}

#[test]
fn avoid_call_after_block() {
    ok("
        fn f(value: Bool): (Int, Int) {
            if (value) {
                1
            } else {
                2
            }

            (1, 2)
        }
    ");
}

#[test]
fn pattern_param_tuple() {
    ok("
        fn main() {
            f((2, 3));
        }

        fn f((a, b): (Int, Int)) {
            assert(a == 2);
            assert(b == 3);
        }
    ");
}

#[test]
fn pattern_param_tuple_in_lambda() {
    ok("
        fn main() {
            let add = |(a, b): (Int, Int)|: Int { a + b };
            let result = add((1, 2));
            assert(result == 3);
        }
    ");
}

#[test]
fn expr_always_returns_paren() {
    ok("
        fn f(): Int {
            (return 1);
        }
    ");
}

#[test]
fn expr_always_returns_match() {
    ok("
        fn f(value: Int): Int {
            match value {
                0 => return 1,
                _ => return 2,
            }
        }
    ");
}

#[test]
fn lambda_param_type_inference() {
    // Lambda with single inferred parameter type
    ok("fn f(): (Int32): Int32 {
        |x|: Int32 { x }
    }");

    // Lambda with multiple inferred parameter types
    ok("fn f(): (Int32, Int32): Int32 {
        |a, b|: Int32 { a + b }
    }");

    // Lambda with mixed explicit and inferred parameter types
    ok("fn f(): (Int32, Int32): Int32 {
        |a: Int32, b|: Int32 { a + b }
    }");

    // Lambda with no params (edge case)
    ok("fn f(): (): Int32 {
        ||: Int32 { 42i32 }
    }");

    // Verify explicit types still work
    ok("fn f(): (Int32): Int32 {
        |x: Int32|: Int32 { x }
    }");
}

#[test]
fn lambda_param_type_inference_errors() {
    // Lambda param without type in context without expected type
    err(
        "fn f() { |x|: Int32 { x }; }",
        (1, 10),
        16,
        crate::ErrorLevel::Error,
        &LAMBDA_PARAM_MISSING_TYPE,
        args!(),
    );

    // Lambda has more parameters than expected
    err(
        "fn f(): (Int32): Int32 { |a, b|: Int32 { a } }",
        (1, 26),
        19,
        crate::ErrorLevel::Error,
        &LAMBDA_PARAM_COUNT_MISMATCH,
        args!("2", "1"),
    );

    // Lambda has fewer parameters than expected
    err(
        "fn f(): (Int32, Int32): Int32 { |a|: Int32 { a } }",
        (1, 33),
        16,
        crate::ErrorLevel::Error,
        &LAMBDA_PARAM_COUNT_MISMATCH,
        args!("1", "2"),
    );
}

#[test]
fn nested_generic_impl_trait_bound() {
    // Array[Array[Int32]] should satisfy Printable because:
    // - Int32: Printable (explicit impl)
    // - Array[Int32]: Printable (because Int32: Printable)
    // - Array[Array[Int32]]: Printable (because Array[Int32]: Printable)
    ok("
        trait Printable {}
        impl[T: Printable] Printable for Array[T] {}
        impl Printable for Int32 {}
        fn assert_printable[T: Printable]() {}
        fn main() {
            assert_printable[Array[Array[Int32]]]();
        }
    ");

    // Array[Array[String]] should NOT satisfy Printable because String doesn't impl Printable
    err(
        "
        trait Printable {}
        impl[T: Printable] Printable for Array[T] {}
        impl Printable for Int32 {}
        fn assert_printable[T: Printable]() {}
        fn main() {
            assert_printable[Array[Array[String]]]();
        }
    ",
        (7, 13),
        40,
        crate::ErrorLevel::Error,
        &TYPE_NOT_IMPLEMENTING_TRAIT,
        args!("Array[Array[String]]", "Printable"),
    );
}

#[test]
fn trait_default_method_with_assoc_type() {
    ok("
        trait MyTrait {}

        trait Foo[T] {
            type X: MyTrait;
            static fn foo(value: Self::X) {}
        }

        class Bar
        impl MyTrait for Int32 {}

        impl Foo[Int32] for Bar {
            type X = Int32;
        }

        fn f() {
            Bar::foo(1i32);
        }
    ");
}

#[test]
fn trait_default_method_with_assoc_type_and_method_call() {
    ok("
        trait MyTrait {
            fn test() {}
        }

        trait Foo[T] {
            type X: MyTrait;
            static fn foo(value: Self::X) {
                value.test();
            }
        }

        class Bar
        impl MyTrait for Int32 {}

        impl Foo[Int32] for Bar {
            type X = Int32;
        }

        fn f() {
            Bar::foo(1i32);
        }
    ");
}

#[test]
#[ignore]
fn trait_default_method_with_assoc_type_and_static_method_call() {
    ok("
        trait MyTrait {
            static fn test() {}
        }

        trait Foo[T] {
            type X: MyTrait;
            static fn foo() {
                Self::X::test();
            }
        }

        class Bar
        impl MyTrait for Int32 {}

        impl Foo[Int32] for Bar {
            type X = Int32;
        }

        fn f() {
            Bar::foo();
        }
    ");
}
