use crate::language::error::msg::SemError;
use crate::language::sem_analysis::ConstValue;
use crate::language::tests::*;

#[test]
fn type_method_len() {
    ok("fn f(a: String): Int64 { return a.size(); }");
    ok("fn f(a: String): Int64 { return \"abc\".size(); }");
}

#[test]
fn type_object_field() {
    ok("class Foo(a:Int32) fn f(x: Foo): Int32 { return x.a; }");
    ok("class Foo(a:String) fn f(x: Foo): String { return x.a; }");
    err(
        "class Foo(a:Int32) fn f(x: Foo): Bool { return x.a; }",
        pos(1, 41),
        SemError::ReturnType("Bool".into(), "Int32".into()),
    );
    err(
        "class Foo(a:Int32) fn f(x: Foo): Int32 { return x.b; }",
        pos(1, 50),
        SemError::UnknownField("b".into(), "Foo".into()),
    );
}

#[test]
fn type_object_set_field() {
    ok("class Foo(a: Int32) fn f(x: Foo) { x.a = 1; }");
    err(
        "class Foo(a: Int32) fn f(x: Foo) { x.a = false; }",
        pos(1, 40),
        SemError::AssignField("a".into(), "Foo".into(), "Int32".into(), "Bool".into()),
    );
}

#[test]
fn type_object_field_without_self() {
    err(
        "class Foo(a: Int32) impl Foo { fn f(): Int32 { return a; } }",
        pos(1, 55),
        SemError::UnknownIdentifier("a".into()),
    );
    err(
        "class Foo(a: Int32) impl Foo { fn set(x: Int32) { a = x; } }",
        pos(1, 51),
        SemError::UnknownIdentifier("a".into()),
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
        pos(7, 32),
        SemError::ReturnType("String".into(), "Int32".into()),
    );
}

#[test]
fn return_type() {
    err(
        "
        class Foo[T]
        fn f(): Foo[Int32] { Foo[Int64]() }
    ",
        pos(3, 28),
        SemError::ReturnType("Foo[Int32]".into(), "Foo[Int64]".into()),
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
        pos(4, 18),
        SemError::MethodExists("bar".into(), pos(3, 18)),
    );

    err(
        "class Foo
        impl Foo{
                 fn bar() {}
                 fn bar(): Int32 {}
             }",
        pos(4, 18),
        SemError::MethodExists("bar".into(), pos(3, 18)),
    );

    err(
        "class Foo
        impl Foo {
                 fn bar(a: Int32) {}
                 fn bar(a: Int32): Int32 {}
             }",
        pos(4, 18),
        SemError::MethodExists("bar".into(), pos(3, 18)),
    );

    err(
        "class Foo
        impl Foo {
                fn bar(a: Int32) {}
                fn bar(a: String) {}
            }",
        pos(4, 17),
        SemError::MethodExists("bar".into(), pos(3, 17)),
    );
}

#[test]
fn type_self() {
    ok("class Foo impl Foo { fn me(): Foo { return self; } }");
    err(
        "class Foo fn me() { return self; }",
        pos(1, 28),
        SemError::ThisUnavailable,
    );

    ok("class Foo(a: Int32, b: Int32)
        impl Foo {
            fn bar(): Int32 { return self.a + self.b; }
        }");

    ok("class Foo(a: Int32)
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
        pos(6, 33),
        SemError::ParamTypesIncompatible("bar".into(), vec!["Int32".into()], Vec::new()),
    );

    err(
        "class Foo
              fn f(x: Foo) { x.bar(1i32); }",
        pos(2, 35),
        SemError::UnknownMethod("Foo".into(), "bar".into(), vec!["Int32".into()]),
    );
}

#[test]
fn type_ctor() {
    ok("class Foo fn f(): Foo { return Foo(); }");
    ok("class Foo(a: Int32) fn f(): Foo { return Foo(1i32); }");
    err(
        "class Foo fn f(): Foo { return 1i32; }",
        pos(1, 25),
        SemError::ReturnType("Foo".into(), "Int32".into()),
    );
}

#[test]
fn type_def_for_return_type() {
    ok("fn a(): Int32 { return 1i32; }");
    err(
        "fn a(): unknown {}",
        pos(1, 9),
        SemError::UnknownIdentifier("unknown".into()),
    );
}

#[test]
fn type_def_for_param() {
    ok("fn a(b: Int32) {}");
    err(
        "fn a(b: foo) {}",
        pos(1, 9),
        SemError::UnknownIdentifier("foo".into()),
    );
}

#[test]
fn type_def_for_var() {
    ok("fn a() { let a : Int32 = 1i32; }");
    err(
        "fn a() { let a : test = 1; }",
        pos(1, 18),
        SemError::UnknownIdentifier("test".into()),
    );
}

#[test]
fn type_var_wrong_type_defined() {
    ok("fn f() { let a : Int32 = 1i32; }");
    ok("fn f() { let a : Bool = false; }");
    ok("fn f() { let a : String = \"f\"; }");

    err(
        "fn f() { let a : Int32 = true; }",
        pos(1, 10),
        SemError::AssignType("a".into(), "Int32".into(), "Bool".into()),
    );
    err(
        "fn f() { let b : Bool = 2i32; }",
        pos(1, 10),
        SemError::AssignType("b".into(), "Bool".into(), "Int32".into()),
    );
}

#[test]
fn type_while() {
    ok("fn x() { while true { } }");
    ok("fn x() { while false { } }");
    err(
        "fn x() { while 2i32 { } }",
        pos(1, 10),
        SemError::WhileCondType("Int32".into()),
    );
}

#[test]
fn type_if() {
    ok("fn x() { if true { } }");
    ok("fn x() { if false { } }");
    err(
        "fn x() { if 4i32 { } }",
        pos(1, 10),
        SemError::IfCondType("Int32".into()),
    );
}

#[test]
fn type_return_unit() {
    ok("fn f() { return; }");
    err(
        "fn f() { return 1i32; }",
        pos(1, 10),
        SemError::ReturnType("()".into(), "Int32".into()),
    );
}

#[test]
fn type_return() {
    ok("fn f(): Int32 { let a = 1i32; return a; }");
    ok("fn f(): Int32 { return 1i32; }");
    err(
        "fn f(): Int32 { return; }",
        pos(1, 17),
        SemError::ReturnType("Int32".into(), "()".into()),
    );

    ok("fn f(): Int32 { return 0i32; }
            fn g(): Int32 { return f(); }");
    err(
        "fn f() { }
             fn g(): Int32 { return f(); }",
        pos(2, 30),
        SemError::ReturnType("Int32".into(), "()".into()),
    );
}

#[test]
fn type_variable() {
    ok("fn f(a: Int32) { let b: Int32 = a; }");
}

#[test]
fn type_let() {
    ok("fn f(value: (Int32, Int32)): Int32 { let (a, b) = value; a+b }");
    err(
        "fn f() { let (a, b) = true; }",
        pos(1, 14),
        SemError::LetPatternExpectedTuple("Bool".into()),
    );

    ok("fn f(value: ()) { let () = value; }");
    err(
        "fn f() { let () = true; }",
        pos(1, 14),
        SemError::LetPatternExpectedTuple("Bool".into()),
    );
    err(
        "fn f() { let (a, b) = (); }",
        pos(1, 14),
        SemError::LetPatternShouldBeUnit,
    );

    err(
        "fn f() { let (a, b) = (true,); }",
        pos(1, 14),
        SemError::LetPatternExpectedTupleWithLength("(Bool)".into(), 1, 2),
    );
    err(
        "fn f() { let () = (true,); }",
        pos(1, 14),
        SemError::LetPatternExpectedTupleWithLength("(Bool)".into(), 1, 0),
    );

    ok("fn f(value: (Int32, (Int32, Int32))): Int32 { let (a, (b, c)) = value; a+b+c }");
}

#[test]
fn type_assign_lvalue() {
    err("fn f() { 1 = 3; }", pos(1, 12), SemError::LvalueExpected);
}

#[test]
fn type_un_op() {
    ok("fn f(a: Int32) { !a; -a; +a; }");
    err(
        "fn f(a: Bool) { -a; }",
        pos(1, 17),
        SemError::UnOpType("-".into(), "Bool".into()),
    );
    err(
        "fn f(a: Bool) { +a; }",
        pos(1, 17),
        SemError::UnOpType("+".into(), "Bool".into()),
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
        pos(1, 38),
        SemError::TypesIncompatible("A".into(), "B".into()),
    );
    err(
        "class A class B fn f(a: A, b: B) { b !== a; }",
        pos(1, 38),
        SemError::TypesIncompatible("B".into(), "A".into()),
    );
    err(
        "fn f(a: Bool) { a+a; }",
        pos(1, 18),
        SemError::BinOpType("+".into(), "Bool".into(), "Bool".into()),
    );
    err(
        "fn f(a: Bool) { a^a; }",
        pos(1, 18),
        SemError::BinOpType("^".into(), "Bool".into(), "Bool".into()),
    );
    err(
        "fn f(a: Int32) { a||a; }",
        pos(1, 19),
        SemError::BinOpType("||".into(), "Int32".into(), "Int32".into()),
    );
    err(
        "fn f(a: Int32) { a&&a; }",
        pos(1, 19),
        SemError::BinOpType("&&".into(), "Int32".into(), "Int32".into()),
    );
    err(
        "fn f(a: String) { a-a; }",
        pos(1, 20),
        SemError::BinOpType("-".into(), "String".into(), "String".into()),
    );
    err(
        "fn f(a: String) { a*a; }",
        pos(1, 20),
        SemError::BinOpType("*".into(), "String".into(), "String".into()),
    );
    err(
        "fn f(a: String) { a%a; }",
        pos(1, 20),
        SemError::BinOpType("%".into(), "String".into(), "String".into()),
    );
}

#[test]
fn type_function_return_type() {
    ok("fn foo(): Int32 { return 1i32; } fn f() { let i: Int32 = foo(); }");
    err(
        "
        fn foo(): Int32 { return 1i32; }
        fn f() { let i: Bool = foo(); }",
        pos(3, 18),
        SemError::AssignType("i".into(), "Bool".into(), "Int32".into()),
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
        pos(3, 21),
        SemError::ParamTypesIncompatible("foo".into(), vec![], vec!["Int32".into()]),
    );
    err(
        "
        fn foo(a: Int32) {}
        fn f() { foo(true); }",
        pos(3, 21),
        SemError::ParamTypesIncompatible("foo".into(), vec!["Int32".into()], vec!["Bool".into()]),
    );
    err(
        "
        fn foo(a: Int32, b: Bool) {}
        fn f() { foo(1i32, 2i32); }",
        pos(3, 21),
        SemError::ParamTypesIncompatible(
            "foo".into(),
            vec!["Int32".into(), "Bool".into()],
            vec!["Int32".into(), "Int32".into()],
        ),
    );
}

#[test]
fn type_array() {
    ok("fn f(a: Array[Int32]): Int32 { return a(1i64); }");
    err(
        "fn f(a: Array[Int32]): String { return a(1i64); }",
        pos(1, 33),
        SemError::ReturnType("String".into(), "Int32".into()),
    );
}

#[test]
fn type_array_assign() {
    err(
        "fn f(a: Array[Int32]): Int32 { return a(3) = 4i32; }",
        pos(1, 32),
        SemError::ReturnType("Int32".into(), "()".into()),
    );
    err(
        "fn f(a: Array[Int32]) { a(3) = \"b\"; }",
        pos(1, 30),
        SemError::UnknownMethod(
            "Array[Int32]".into(),
            "set".into(),
            vec!["Int64".into(), "String".into()],
        ),
    );
}

#[test]
fn type_array_field() {
    ok("
        class Foo(let x: Array[Int32])
        fn f(a: Foo): Int32 { return a.x(1i64); }
    ");
}

#[test]
fn let_without_initialization() {
    err(
        "fn f() { let x: Int32; }",
        pos(1, 10),
        SemError::LetMissingInitialization,
    );
}

#[test]
fn reassign_param() {
    err(
        "fn f(a: Int32) { a = 1; }",
        pos(1, 20),
        SemError::LetReassigned,
    );
}

#[test]
fn reassign_field() {
    ok("class Foo(x: Int32) fn foo(f: Foo) { f.x = 1; }");
}

#[test]
fn reassign_var() {
    ok("fn f() { var a=1; a=2; }");
}

#[test]
fn reassign_let() {
    err(
        "fn f() { let a=1; a=2; }",
        pos(1, 20),
        SemError::LetReassigned,
    );
}

#[test]
fn reassign_self() {
    err(
        "class Foo
        impl Foo {
            fn f() { self = Foo(); }
        }",
        pos(3, 27),
        SemError::LvalueExpected,
    );
}

#[test]
fn same_names() {
    ok("class Foo { var Foo: Foo = Foo(); }");
    ok("class Foo fn foo() { let Foo: Int32 = 1i32; }");
}

#[test]
fn lit_int64() {
    ok("fn f(): Int64 { return 1i64; }");
    ok("fn f(): Int32 { return 1i32; }");

    let ret = SemError::ReturnType("Int32".into(), "Int64".into());
    err("fn f(): Int32 { return 1i64; }", pos(1, 17), ret);

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
fn overload_plus() {
    ok("class A impl A { fn plus(rhs: A): Int32 { return 0; } }
            fn f(): Int32 { return A() + A(); }");
}

#[test]
fn overload_minus() {
    ok("class A impl A { fn minus(rhs: A): Int32 { return 0; } }
            fn f(): Int32 { return A() - A(); }");
}

#[test]
fn overload_times() {
    ok("class A impl A { fn times(rhs: A): Int32 { return 0; } }
            fn f(): Int32 { return A() * A(); }");
}

#[test]
fn overload_div() {
    ok("class A impl A { fn div(rhs: A): Int32 { return 0; } }
            fn f(): Int32 { return A() / A(); }");
}

#[test]
fn overload_mod() {
    ok("class A impl A { fn modulo(rhs: A): Int32 { return 0; } }
            fn f(): Int32 { return A() % A(); }");
}

#[test]
fn overload_bitwise_or() {
    ok(
        "class A impl A { fn bitwiseOr(rhs: A): Int32 { return 0; } }
            fn f(): Int32 { return A() | A(); }",
    );
}

#[test]
fn overload_bitwise_and() {
    ok(
        "class A impl A { fn bitwiseAnd(rhs: A): Int32 { return 0i32; } }
            fn f(): Int32 { return A() & A(); }",
    );
}

#[test]
fn overload_bitwise_xor() {
    ok(
        "class A impl A { fn bitwiseXor(rhs: A): Int32 { return 0i32; } }
            fn f(): Int32 { return A() ^ A(); }",
    );
}

#[test]
fn overload_shl() {
    ok(
        "class A impl A { fn shiftLeft(rhs: A): Int32 { return 0i32; } }
            fn f(): Int32 { return A() << A(); }",
    );
}

#[test]
fn overload_sar() {
    ok(
        "class A impl A { fn shiftRightSigned(rhs: A): Int32 { return 0i32; } }
            fn f(): Int32 { return A() >> A(); }",
    );
}

#[test]
fn overload_shr() {
    ok(
        "class A impl A { fn shiftRight(rhs: A): Int32 { return 0i32; } }
            fn f(): Int32 { return A() >>> A(); }",
    );
}

#[test]
fn overload_equals() {
    ok("class A impl A { fn equals(rhs: A): Bool { return true; } }
            fn f1(): Bool { return A() == A(); }
            fn f2(): Bool { return A() != A(); }");
}

#[test]
fn overload_compare_to() {
    ok(
        "class A impl A { fn compareTo(rhs: A): Int32 { return 0; } }
            fn f1(): Bool { return A() < A(); }
            fn f2(): Bool { return A() <= A(); }
            fn f3(): Bool { return A() > A(); }
            fn f4(): Bool { return A() >= A(); }",
    );
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
    ok("fn f(a: Int64, b: Int64): Bool { return a === b; }");
    ok("fn f(a: Int64, b: Int64): Bool { return a !== b; }");
    ok("fn f(a: Int64, b: Int64): Bool { return a < b; }");
    ok("fn f(a: Int64, b: Int64): Bool { return a <= b; }");
    ok("fn f(a: Int64, b: Int64): Bool { return a > b; }");
    ok("fn f(a: Int64, b: Int64): Bool { return a >= b; }");
    ok("fn f(a: Int64): Int64 { return !a; }");
    ok("fn f(a: Int64): Int64 { return -a; }");
    ok("fn f(a: Int64): Int64 { return +a; }");
}

#[test]
fn test_literal_int_overflow() {
    err(
        "fn f() { let x = 2147483648i32; }",
        pos(1, 18),
        SemError::NumberOverflow("Int32".into()),
    );
    ok("fn f() { let x = 2147483647i32; }");
    err(
        "fn f() { let x = -2147483649i32; }",
        pos(1, 19),
        SemError::NumberOverflow("Int32".into()),
    );
    ok("fn f() { let x = -2147483648i32; }");
}

#[test]
fn test_literal_hex_int_overflow() {
    err(
        "fn f() { let x = 0x1_FF_FF_FF_FFi32; }",
        pos(1, 18),
        SemError::NumberOverflow("Int32".into()),
    );
    ok("fn f() { let x: Int32 = 0xFF_FF_FF_FFi32; }");
}

#[test]
fn test_literal_bin_int_overflow() {
    err(
        "fn f() { let x = 0b1_11111111_11111111_11111111_11111111i32; }",
        pos(1, 18),
        SemError::NumberOverflow("Int32".into()),
    );
    ok("fn f() { let x: Int32 = 0b11111111_11111111_11111111_11111111i32; }");
}

#[test]
fn test_literal_int64_overflow() {
    err(
        "fn f() { let x = 9223372036854775808i64; }",
        pos(1, 18),
        SemError::NumberOverflow("Int64".into()),
    );
    ok("fn f() { let x = 9223372036854775807i64; }");
    err(
        "fn f() { let x = -9223372036854775809i64; }",
        pos(1, 19),
        SemError::NumberOverflow("Int64".into()),
    );
    ok("fn f() { let x = -9223372036854775808i64; }");
}

#[test]
fn test_literal_float_overflow() {
    err(
        "fn f() { let x = -340282350000000000000000000000000000000f32; }",
        pos(1, 19),
        SemError::NumberOverflow("Float32".into()),
    );
    ok("fn f() { let x = -340282340000000000000000000000000000000f32; }");
    err(
        "fn f() { let x = 340282350000000000000000000000000000001f32; }",
        pos(1, 18),
        SemError::NumberOverflow("Float32".into()),
    );
    ok("fn f() { let x = 340282340000000000000000000000000000000f32; }");
}

#[test]
fn test_char() {
    ok("fn foo(): Char { return 'c'; }");
    ok("fn foo(a: Char): Char { return a; }");
    err(
        "fn foo(): Char { return false; }",
        pos(1, 18),
        SemError::ReturnType("Char".into(), "Bool".into()),
    );
    err(
        "fn foo(): Char { return 10i32; }",
        pos(1, 18),
        SemError::ReturnType("Char".into(), "Int32".into()),
    );
}

#[test]
fn test_generic_arguments_mismatch() {
    err(
        "class A[T]
            fn foo() {
                let a = A[Int32, Int32]();
            }",
        pos(3, 40),
        SemError::WrongNumberTypeParams(1, 2),
    );

    err(
        "class A[T]
            fn foo() {
                let a = A();
            }",
        pos(3, 26),
        SemError::WrongNumberTypeParams(1, 0),
    );

    err(
        "class A
            fn foo() {
                let a = A[Int32]();
            }",
        pos(3, 33),
        SemError::WrongNumberTypeParams(0, 1),
    );
}

#[test]
fn test_invoke_static_method_as_instance_method() {
    err(
        "class A
        impl A {
            @static fn foo() {}
            fn test() { self.foo(); }
        }",
        pos(4, 33),
        SemError::UnknownMethod("A".into(), "foo".into(), vec![]),
    );
}

#[test]
fn test_invoke_method_as_static() {
    err(
        "class A
        impl A {
            fn foo() {}
            @static fn test() { A::foo(); }
        }",
        pos(4, 39),
        SemError::UnknownStaticMethod("A".into(), "foo".into(), vec![]),
    );
}

#[test]
fn test_fct_with_type_params() {
    err(
        "fn f() {} fn g() { f[Int32](); }",
        pos(1, 28),
        SemError::WrongNumberTypeParams(0, 1),
    );
    err(
        "fn f[T]() {} fn g() { f(); }",
        pos(1, 24),
        SemError::WrongNumberTypeParams(1, 0),
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
        pos(4, 28),
        SemError::TypeNotImplementingTrait("T".into(), "MyTrait".into()),
    );

    err(
        "
            trait MyTraitA {}
            trait MyTraitB {}
            class Foo[T: MyTraitA + MyTraitB]
            fn bar[T: MyTraitA](arg: Foo[T]) {}
        ",
        pos(5, 38),
        SemError::TypeNotImplementingTrait("T".into(), "MyTraitB".into()),
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
        pos(7, 42),
        SemError::TypeNotImplementingTrait("T".into(), "MyTraitB".into()),
    );
}

#[test]
fn test_const_check() {
    err(
        "const one: Int32 = 1i32;
            fn f(): Int64 { return one; }",
        pos(2, 29),
        SemError::ReturnType("Int64".into(), "Int32".into()),
    );

    err(
        "const one: Int32 = 1i32;
            fn f() { let x: String = one; }",
        pos(2, 22),
        SemError::AssignType("x".into(), "String".into(), "Int32".into()),
    );
}

#[test]
fn test_const_values() {
    ok_with_test(
        "  const yes: Bool = true;
                        const x: UInt8 = 255u8;
                        const a: Int32 = 100i32;
                        const b: Int64 = 200i64;
                        const c: Char = 'A';
                        const d: Float32 = 3.0f32;
                        const e: Float64 = 6.0;",
        |sa| {
            {
                let id = sa.const_by_name("yes");
                let const_ = sa.consts.idx(id);
                let const_ = const_.read();
                assert_eq!(ConstValue::Bool(true), const_.value);
            }

            {
                let id = sa.const_by_name("x");
                let const_ = sa.consts.idx(id);
                let const_ = const_.read();
                assert_eq!(ConstValue::Int(255), const_.value);
            }

            {
                let id = sa.const_by_name("a");
                let const_ = sa.consts.idx(id);
                let const_ = const_.read();
                assert_eq!(ConstValue::Int(100), const_.value);
            }

            {
                let id = sa.const_by_name("b");
                let const_ = sa.consts.idx(id);
                let const_ = const_.read();
                assert_eq!(ConstValue::Int(200), const_.value);
            }

            {
                let id = sa.const_by_name("c");
                let const_ = sa.consts.idx(id);
                let const_ = const_.read();
                assert_eq!(ConstValue::Char('A'), const_.value);
            }

            {
                let id = sa.const_by_name("d");
                let const_ = sa.consts.idx(id);
                let const_ = const_.read();
                assert_eq!(ConstValue::Float(3.0), const_.value);
            }

            {
                let id = sa.const_by_name("e");
                let const_ = sa.consts.idx(id);
                let const_ = const_.read();
                assert_eq!(ConstValue::Float(6.0), const_.value);
            }
        },
    );
}

#[test]
fn test_assignment_to_const() {
    err(
        "const one: Int32 = 1i32;
            fn f() { one = 2i32; }",
        pos(2, 22),
        SemError::LvalueExpected,
    );
}

#[test]
fn test_unary_minus_byte() {
    err(
        "const m1: UInt8 = -1u8;",
        pos(1, 19),
        SemError::UnOpType("-".into(), "UInt8".into()),
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
        "trait Foo {}
            class X
            class A[T: Foo]
            fn f(): A[X] { A[X]() }",
        pos(4, 21),
        SemError::TypeNotImplementingTrait("X".into(), "Foo".into()),
    );

    err(
        "trait Foo {}
            fn f[T: Foo]() {}
            fn t() { f[Int32](); }",
        pos(3, 30),
        SemError::TypeNotImplementingTrait("Int32".into(), "Foo".into()),
    );
}

#[test]
fn test_operator_on_generic_type() {
    err(
        "fn f[T](a: T, b: T) { a + b; }",
        pos(1, 25),
        SemError::BinOpType("+".into(), "T".into(), "T".into()),
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
        pos(5, 34),
        SemError::ParamTypesIncompatible("foo".into(), Vec::new(), vec!["Int32".into()]),
    );

    ok("class A
            impl A { @static fn foo() {} }
            trait Foo { fn foo(a: Int32); }
            impl Foo for A { fn foo(a:  Int32) {} }
            fn test(a: A) { a.foo(1i32); }");
}

#[test]
fn test_global_get() {
    ok("var x: Int32 = 0i32; fn foo(): Int32 { return x; }");
}

#[test]
fn test_global_set() {
    ok("var x: Int32 = 0i32; fn foo(a: Int32) { x = a; }");
    err(
        "let x: Int32 = 0i32; fn foo(a: Int32) { x = a; }",
        pos(1, 43),
        SemError::LetReassigned,
    );
}

#[test]
fn lambda_assignment() {
    ok("fn f() { let x = || {}; }");
    ok("fn f() { let x = ||: Int32 { return 2i32; }; }");
    ok("fn f() { let x: (): () = || {}; }");
    ok("fn f() { let x: (): () = ||: () {}; }");
    ok("fn f() { let x: (): Int32 = ||: Int32 { return 2i32; }; }");
    err(
        "fn f() { let x: (): Int32 = || {}; }",
        pos(1, 10),
        SemError::AssignType("x".into(), "() -> Int32".into(), "() -> ()".into()),
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
        pos(8, 29),
        SemError::MultipleCandidatesForMethod("A".into(), "f".into(), Vec::new()),
    );
}

#[test]
fn generic_trait_method_call() {
    ok("trait Foo { fn bar(); }
            fn f[T: Foo](t: T) { t.bar(); }");
    ok("trait Foo { fn bar(); }
            class A[T: Foo](t: T)
            impl[T: Foo] A[T] {
                fn baz() { self.t.bar(); }
            }");
}

#[test]
fn test_generic_ctor_without_type_params() {
    err(
        "class Foo[A, B]
            fn test() { Foo(); }",
        pos(2, 28),
        SemError::WrongNumberTypeParams(2, 0),
    );
}

#[test]
fn test_generic_argument_with_trait_bound() {
    err(
        "fn f[X: std::Comparable](x: X) {}
            fn g[T](t: T) { f[T](t); }",
        pos(2, 33),
        SemError::TypeNotImplementingTrait("T".into(), "Comparable".into()),
    );
}

#[test]
fn test_for_supports_make_iterator() {
    err(
        "fn f() { for i in 1i32 {} }",
        pos(1, 19),
        SemError::TypeNotUsableInForIn("Int32".into()),
    );

    err(
        "
            class Foo
            impl Foo { fn makeIterator(): Bool { return true; } }
            fn f() { for i in Foo() {} }",
        pos(4, 34),
        SemError::TypeNotUsableInForIn("Foo".into()),
    );

    ok("
            class Foo
            impl Foo { fn makeIterator(): FooIter { return FooIter(); } }
            class FooIter
            impl std::Iterator for FooIter {
                fn next(): Option[Int32] { Some[Int32](0i32) }
            }
            fn f(): Int32 { for i in Foo() { return i; } return 0i32; }");
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

            class Bar[T](a: T)
            ",
        pos(5, 27),
        SemError::ParamTypesIncompatible("Bar".into(), vec!["T".into()], vec!["Int32".into()]),
    );
}

#[test]
fn test_fct_used_as_identifier() {
    err(
        "fn foo() {} fn bar() { foo; }",
        pos(1, 24),
        SemError::ValueExpected,
    );
}

#[test]
fn test_cls_used_as_identifier() {
    err("class X fn f() { X; }", pos(1, 18), SemError::ValueExpected);
}

#[test]
fn test_assign_fct() {
    err(
        "fn foo() {} fn bar() { foo = 1i32; }",
        pos(1, 24),
        SemError::LvalueExpected,
    );
}

#[test]
fn test_assign_class() {
    err(
        "
            class X
            fn foo() { X = 2i32; }
        ",
        pos(3, 24),
        SemError::LvalueExpected,
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
        pos(1, 21),
        SemError::ParamTypesIncompatible("g".into(), Vec::new(), vec!["Int32".into()]),
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
        pos(1, 28),
        SemError::WrongNumberTypeParams(0, 1),
    );
}

#[test]
fn test_new_call_static_method() {
    ok("class Foo impl Foo { @static fn bar() {} }
            fn f() { Foo::bar(); }");
}

#[test]
fn test_new_call_static_method_wrong_params() {
    err(
        "class Foo impl Foo { @static fn bar() {} }
            fn f() { Foo::bar(1i32); }",
        pos(2, 30),
        SemError::ParamTypesIncompatible("bar".into(), Vec::new(), vec!["Int32".into()]),
    );
}

#[test]
fn test_new_call_static_method_type_params() {
    ok("class Foo impl Foo { @static fn bar[T]() {} }
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
        pos(3, 19),
        SemError::ParamTypesIncompatible("X".into(), Vec::new(), vec!["Int32".into()]),
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
        pos(3, 30),
        SemError::WrongNumberTypeParams(0, 1),
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
        pos(4, 25),
        SemError::ParamTypesIncompatible("f".into(), Vec::new(), vec!["Int32".into()]),
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
        pos(1, 23),
        SemError::UnknownMethodForTypeParam("T".into(), "hash".into(), Vec::new()),
    );
}

#[test]
fn test_new_call_method_generic_error_multiple() {
    err(
        "
            trait TraitA { fn id(); }
            trait TraitB { fn id(); }
            fn f[T: TraitA + TraitB](t: T) { t.id(); }",
        pos(4, 50),
        SemError::MultipleCandidatesForTypeParam("T".into(), "id".into(), Vec::new()),
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
    err(
        "fn f(t: Array[Int32]){ t(0) = true; }",
        pos(1, 29),
        SemError::UnknownMethod(
            "Array[Int32]".into(),
            "set".into(),
            vec!["Int64".into(), "Bool".into()],
        ),
    );
}

#[test]
fn test_array_syntax_set_wrong_index() {
    err(
        "fn f(t: Array[Int32]){ t(\"bla\") = 9i32; }",
        pos(1, 33),
        SemError::UnknownMethod(
            "Array[Int32]".into(),
            "set".into(),
            vec!["String".into(), "Int32".into()],
        ),
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
        pos(3, 50),
        SemError::ExpectedStringable("Foo".into()),
    );
    ok("fn f[T: std::Stringable](x: T): String { return \"${x}\"; }");
}

#[test]
fn test_trait_object_as_argument() {
    ok("trait Foo { fn bar(): Int32; }
        fn f(x: Foo): Int32 { return x.bar(); }");
    err(
        "trait Foo { fn baz(); }
        fn f(x: Foo): String { return x.baz(); }",
        pos(2, 32),
        SemError::ReturnType("String".into(), "()".into()),
    );
}

#[test]
fn test_type_param_used_as_value() {
    err(
        "fn f[T](): Int32 { return T; }",
        pos(1, 27),
        SemError::ValueExpected,
    );

    err(
        "class SomeClass[T]
        impl[T] SomeClass[T] {
            fn f(): Int32 { return T; }
        }",
        pos(3, 36),
        SemError::ValueExpected,
    );
}

#[test]
fn test_assign_to_type_param() {
    err(
        "fn f[T]() { T = 10; }",
        pos(1, 13),
        SemError::LvalueExpected,
    );

    err(
        "
        class SomeClass[T]
        impl[T] SomeClass[T] {
            fn f() { T = 10; }
        }",
        pos(4, 22),
        SemError::LvalueExpected,
    );
}

#[test]
fn test_type_param_with_name_but_no_call() {
    err(
        "trait X { fn foo(): Int32; }
        fn f[T: X]() { T::foo; }",
        pos(2, 24),
        SemError::InvalidLeftSideOfSeparator,
    );

    err(
        "trait X { fn foo(): Int32; }
        class SomeClass[T: X]
        impl[T: X] SomeClass[T] {
            fn f() { T::foo; }
        }",
        pos(4, 22),
        SemError::InvalidLeftSideOfSeparator,
    );
}

#[test]
fn test_type_param_call() {
    err(
        "trait X { fn foo(): Int32; }
        fn f[T: X]() { T(); }",
        pos(2, 24),
        SemError::ValueExpected,
    );

    err(
        "trait X { fn foo(): Int32; }
        class SomeClass[T: X]
        impl[T: X] SomeClass[T] {
            fn f() { T(); }
        }",
        pos(4, 22),
        SemError::ValueExpected,
    );
}

#[test]
fn test_static_method_call_with_type_param() {
    err(
        "trait X { @static fn bar(): Int32; }
        fn f[T: X]() { T::foo(); }",
        pos(2, 30),
        SemError::UnknownStaticMethodWithTypeParam,
    );

    err(
        "trait X { @static fn foo(): Int32; }
        trait Y { @static fn foo(): String; }
        fn f[T: X + Y]() { T::foo(); }",
        pos(3, 34),
        SemError::MultipleCandidatesForStaticMethodWithTypeParam,
    );

    err(
        "trait X { @static fn foo(): Int32; }
        fn f[T: X](): Int32 { return T::foo(1i32); }",
        pos(2, 44),
        SemError::ParamTypesIncompatible("foo".into(), Vec::new(), vec!["Int32".into()]),
    );

    ok("trait X { @static fn foo(): Int32; }
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
        struct Foo(f1: Int32)
        fn f(): Foo { Foo(1i32) }
    ");
    err(
        "
        struct Foo(f1: Int32)
        fn f(): Foo { Foo() }",
        pos(3, 26),
        SemError::StructArgsIncompatible("Foo".into(), vec!["Int32".into()], Vec::new()),
    );
    err(
        "
        struct Foo(f1: Int32)
        fn f(): Foo { Foo(true) }",
        pos(3, 26),
        SemError::StructArgsIncompatible("Foo".into(), vec!["Int32".into()], vec!["Bool".into()]),
    );
}

#[test]
fn test_struct_field() {
    ok("
        struct Foo(f1: Int32)
        fn f(x: Foo): Int32 { x.f1 }
    ");

    err(
        "
        struct Foo(f1: Bool)
        fn f(x: Foo): Int32 { x.f1 }
    ",
        pos(3, 29),
        SemError::ReturnType("Int32".into(), "Bool".into()),
    );

    err(
        "
        struct Foo(f1: Bool)
        fn f(x: Foo): Int32 { x.unknown }
    ",
        pos(3, 32),
        SemError::UnknownField("unknown".into(), "Foo".into()),
    );
}

#[test]
fn test_struct_field_array() {
    ok("
        struct Foo(f1: Array[Int32])
        fn f(x: Foo): Int32 { x.f1(0) }
    ");
}

#[test]
fn test_struct_with_type_params() {
    ok("
        struct Foo[T](f1: Int32)
        fn f(): Foo[Int32] { Foo[Int32](1i32) }
    ");
    err(
        "
        struct Foo[T](f1: Int32)
        fn f(): Foo[Int32] { Foo(1i32) }
    ",
        pos(3, 33),
        SemError::WrongNumberTypeParams(1, 0),
    );
    err(
        "
        struct Foo[T](f1: Int32)
        fn f(): Foo[Int32] { Foo[Int32, Bool](1i32) }
    ",
        pos(3, 46),
        SemError::WrongNumberTypeParams(1, 2),
    );
    err(
        "
        trait MyTrait {}
        struct Foo[T: MyTrait](f1: Int32)
        fn f(): Foo[Int32] { Foo[Int32](1i32) }
    ",
        pos(4, 17),
        SemError::TypeNotImplementingTrait("Int32".into(), "MyTrait".into()),
    );
    ok("
        trait MyTrait {}
        class Bar
        impl MyTrait for Bar {}
        struct Foo[T: MyTrait](f1: Int32)
        fn f(): Foo[Bar] { Foo[Bar](1i32) }
    ");
    err(
        "
        struct Foo[T](f1: Int32)
        fn f(): Foo[Int32] { Foo[Bool](1i32) }
    ",
        pos(3, 28),
        SemError::ReturnType("Foo[Int32]".into(), "Foo[Bool]".into()),
    );
    err(
        "
        struct Foo[T](f1: T, f2: Bool)
        fn f[T](val: T): Foo[T] { Foo(val, false) }",
        pos(3, 38),
        SemError::WrongNumberTypeParams(1, 0),
    );
}

#[test]
fn test_struct_mod() {
    err(
        "
        fn f() { foo::Foo(1i32); }
        mod foo { struct Foo(f1: Int32) }
        ",
        pos(2, 26),
        SemError::NotAccessible("foo::Foo".into()),
    );
}

#[test]
fn test_struct_with_static_method() {
    ok("
        struct Foo(value: Int32)
        impl Foo {
            @static fn bar() {}
        }
        fn f() {
            Foo::bar();
        }
        ");

    ok("
        struct Foo[T](value: Int32)
        impl[T] Foo[T] {
            @static fn bar() {}
        }
        fn f() {
            Foo[Int32]::bar();
        }
        ");

    err(
        "
            struct Foo(value: Int32)
            fn f() {
                Foo[Int32]::bar();
            }
            ",
        pos(4, 32),
        SemError::WrongNumberTypeParams(0, 1),
    );
}

#[test]
fn test_enum_with_static_method() {
    ok("
        enum Foo { A, B }
        impl Foo {
            @static fn bar() {}
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
        pos(4, 28),
        SemError::WrongNumberTypeParams(0, 1),
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
        pos(1, 27),
        SemError::ValueExpected,
    );

    err(
        "enum A { V1 } fn f() { A = 1; }",
        pos(1, 24),
        SemError::LvalueExpected,
    );

    err(
        "enum A { V1, V2 } fn f(): A { A::V3 }",
        pos(1, 32),
        SemError::UnknownEnumVariant("V3".into()),
    );

    err(
        "enum A[T] { V1, V2 } fn f(): A[Int32] { A::V1 }",
        pos(1, 42),
        SemError::WrongNumberTypeParams(1, 0),
    );

    err(
        "enum A[T] { V1(T), V2 } fn f(): A[Int32] { A[Int32]::V1 }",
        pos(1, 52),
        SemError::EnumArgsIncompatible("A".into(), "V1".into(), vec!["T".into()], Vec::new()),
    );

    err(
        "
        enum Foo[T] { A(T, Bool), B }
        fn f[T](val: T): Foo[T] { Foo::A[T, String](val, false) }",
        pos(3, 52),
        SemError::WrongNumberTypeParams(1, 2),
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
        pos(6, 26),
        SemError::MatchBranchTypesIncompatible("Int32".into(), "String".into()),
    );
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
        pos(5, 17),
        SemError::MatchPatternNoParens,
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
        pos(5, 17),
        SemError::MatchPatternWrongNumberOfParams(0, 1),
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
        pos(5, 17),
        SemError::MatchPatternWrongNumberOfParams(4, 3),
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
        pos(6, 26),
        SemError::UnknownIdentifier("a".into()),
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
        pos(5, 26),
        SemError::VarAlreadyInPattern,
    );
}

#[test]
fn test_enum_match_missing_variants() {
    err(
        "
        enum A { V1(Int32, Int32, Int32), V2, V3 }
        fn f(x: A): Int32 {
            match x {
                A::V1(a, _, c) => a + c,
                A::V2 => 1i32,
            }
        }
    ",
        pos(4, 13),
        SemError::MatchUncoveredVariant,
    );

    err(
        "
        enum A { V1(Int32, Int32, Int32), V2, V3 }
        fn f(x: A): Int32 {
            match x {
                A::V1(a, _, c) => a + c,
                A::V2 => 1i32,
                A::V3 => 2i32,
                A::V2 => 4i32,
            }
        }
    ",
        pos(8, 17),
        SemError::MatchUnreachablePattern,
    );
}

#[test]
fn test_enum_match_underscore() {
    ok("
        enum A { V1, V2, V3 }
        fn f(x: A): Bool {
            match x {
                A::V1 => true,
                _ => false,
            }
        }
    ");

    err(
        "
        enum A { V1, V2, V3 }
        fn f(x: A): Bool {
            match x {
                _ => false,
                A::V1 => true,
            }
        }
    ",
        pos(6, 17),
        SemError::MatchUnreachablePattern,
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
        pos(4, 15),
        SemError::BinOpType("==".into(), "A".into(), "A".into()),
    );
}

#[test]
fn test_use_enum_value() {
    ok("enum A { V1(Int32), V2 } use A::V1; fn f(): A { V1(1i32) }");
    ok("enum A[T] { V1(Int32), V2 } use A::V1; fn f(): A[Int32] { V1[Int32](1i32) }");
    ok("enum A[T] { V1(Int32), V2 } use A::V1; fn f(): A[Int32] { V1(1i32) }");

    ok("enum A { V1, V2 } use A::V2; fn f(): A { V2 }");

    err(
        "enum A { V1(Int32), V2 } use A::V1; fn f(): A { V1 }",
        pos(1, 49),
        SemError::EnumArgsIncompatible("A".into(), "V1".into(), vec!["Int32".into()], Vec::new()),
    );

    err(
        "enum A { V1(Int32), V2 } use A::V2; fn f(): A { V2(0i32) }",
        pos(1, 51),
        SemError::EnumArgsIncompatible("A".into(), "V2".into(), Vec::new(), vec!["Int32".into()]),
    );

    ok("enum A[T] { V1(Int32), V2 } use A::V2; fn f(): A[Int32] { V2 }");

    ok("enum A[T] { V1, V2 } use A::V2; fn f(): A[Int32] { V2[Int32] }");

    err(
        "enum A[T] { V1, V2 } use A::V2; fn f(): A[Int32] { V2[Int32, Float32] }",
        pos(1, 54),
        SemError::WrongNumberTypeParams(1, 2),
    );
}

#[test]
fn test_enum_value_with_type_param() {
    ok("enum A[T] { V1, V2 } fn f(): A[Int32] { A::V2[Int32] }");
    ok("enum A[T] { V1, V2 } fn f(): A[Int32] { A[Int32]::V2 }");
    err(
        "enum A[T] { V1, V2 } fn f(): A[Int32] { A[Int32]::V2[Int32] }",
        pos(1, 42),
        SemError::ExpectedSomeIdentifier,
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
        pos(1, 35),
        SemError::ReturnType("(Int32)".into(), "(Int32, Bool)".into()),
    );
    err(
        "fn f(a: (Int32, Bool)): (Int32, Float32) { return a; }",
        pos(1, 44),
        SemError::ReturnType("(Int32, Float32)".into(), "(Int32, Bool)".into()),
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
        pos(2, 9),
        SemError::ReturnType("(Int32)".into(), "Int32".into()),
    );

    err(
        "fn f(): (Int32, Int32) {
        return (1i32, false);
    }",
        pos(2, 9),
        SemError::ReturnType("(Int32, Int32)".into(), "(Int32, Bool)".into()),
    );
}

#[test]
fn test_tuple_in_call() {
    ok("
        fn f(a: (Int32, Bool)) {}
        fn g() {
            f((1i32, true));
        }
    ")
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
        pos(3, 13),
        SemError::ReturnType("String".into(), "Bool".into()),
    );
}

#[test]
fn test_type_without_make_iterator() {
    err(
        "
        class Foo
        fn bar(x: Foo) {
            for i in x {
                let x: Foo = i;
            }
        }
    ",
        pos(4, 22),
        SemError::TypeNotUsableInForIn("Foo".into()),
    );
}

#[test]
fn test_type_make_iterator_not_implementing_iterator() {
    err(
        "
        class Foo
        impl Foo {
            fn makeIterator(): Int32 { 0i32 }
        }
        fn bar(x: Foo) {
            for i in x {
                let x: Foo = i;
            }
        }
    ",
        pos(7, 22),
        SemError::TypeNotUsableInForIn("Foo".into()),
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
        pos(1, 29),
        SemError::UnknownStaticMethod("Array[String]".into(), "zero".into(), vec!["Int64".into()]),
    );
}

#[test]
fn extension_method_call() {
    ok("
        class Foo(value: Int32)
        impl Foo { fn foo(): Int32 { self.value } }
        fn bar(x: Foo): Int32 { x.foo() }
    ");
}

#[test]
fn extension_class_with_type_param() {
    ok("
        class Foo[T](value: T)
        trait MyTrait {}
        impl[T: MyTrait] Foo[T] { fn foo(): Int32 { 12i32 } }
        impl MyTrait for Int32 {}
        fn bar(x: Foo[Int32]): Int32 { x.foo() }
    ");

    ok("
        class Foo[T](value: T)
        impl Foo[Int32] { fn foo() {} }
        impl Foo[Float32] { fn bar() {} }
        fn f(x: Foo[Int32]) { x.foo() }
        fn g(x: Foo[Float32]) { x.bar() }
    ");

    err(
        "
        class Foo[T](value: T)
        impl Foo[Float32] { fn bar() {} }
        fn f(x: Foo[Int32]) { x.bar() }
    ",
        pos(4, 36),
        SemError::UnknownMethod("Foo[Int32]".into(), "bar".into(), Vec::new()),
    );
}

#[test]
fn extension_class_tuple() {
    ok("
        class Foo[T](value: T)
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
        pos(7, 18),
        SemError::UnknownMethod("Foo[(Int32, Int32)]".into(), "bar".into(), Vec::new()),
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
        pos(10, 22),
        SemError::UnknownMethod("Foo[Foo[Int32]]".into(), "bar".into(), Vec::new()),
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
        pos(7, 18),
        SemError::UnknownMethod("Foo[(Int32, Float32)]".into(), "bar".into(), Vec::new()),
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
        pos(7, 18),
        SemError::UnknownMethod("Foo[(T, Float32)]".into(), "bar".into(), Vec::new()),
    );
}

#[test]
fn extension_struct_with_type_param() {
    ok("
        struct Foo[T](value: T)
        trait MyTrait {}
        impl[T: MyTrait] Foo[T] { fn foo(): Int32 { 12i32 } }
        impl MyTrait for Int32 {}
        fn bar(x: Foo[Int32]): Int32 { x.foo() }
    ");

    ok("
        struct Foo[T](value: T)
        impl Foo[Int32] { fn foo() {} }
        impl Foo[Float32] { fn bar() {} }
        fn f(x: Foo[Int32]) { x.foo() }
        fn g(x: Foo[Float32]) { x.bar() }
    ");

    err(
        "
        struct Foo[T](value: T)
        impl Foo[Float32] { fn bar() {} }
        fn f(x: Foo[Int32]) { x.bar() }
    ",
        pos(4, 36),
        SemError::UnknownMethod("Foo[Int32]".into(), "bar".into(), Vec::new()),
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
        pos(4, 36),
        SemError::UnknownMethod("Foo[Int32]".into(), "bar".into(), Vec::new()),
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
        pos(5, 38),
        SemError::UnknownMethod("Foo[Int32]".into(), "bar".into(), Vec::new()),
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
        struct Foo[T](value: T)
        impl MyTrait for Foo[String] { fn bar() {} }
        fn bar(x: Foo[Int32]) { x.bar(); }
    ",
        pos(5, 38),
        SemError::UnknownMethod("Foo[Int32]".into(), "bar".into(), Vec::new()),
    );

    ok("
        trait MyTrait { fn bar(); }
        struct Foo[T](value: T)
        impl MyTrait for Foo[Int32] { fn bar() {} }
        fn bar(x: Foo[Int32]) { x.bar(); }
    ");
}

#[test]
fn impl_struct_method_with_self() {
    ok("
        struct Foo(value: Int32)
        trait AsInt32 { fn value(): Int32; }
        impl AsInt32 for Foo { fn value(): Int32 { self.value } }
    ");
}

#[test]
fn impl_struct_with_method_overload() {
    ok("
        struct Foo(value: Int32)
        impl Foo {
            fn plus(other: Foo): Foo {
                Foo(self.value + other.value)
            }
        }
        fn f(a: Foo, b: Foo): Foo {
            a + b
        }
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
        pos(5, 38),
        SemError::UnknownMethod("Foo[Int32]".into(), "bar".into(), Vec::new()),
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
        pos(1, 22),
        SemError::UnknownMethod("()".into(), "foo".into(), Vec::new()),
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
        pos(1, 17),
        SemError::NumberOverflow("UInt8".into()),
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
        pos(4, 14),
        SemError::ParamTypesIncompatible("f".into(), vec!["Int32".into()], vec!["Bool".into()]),
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
        pos(4, 14),
        SemError::ParamTypesIncompatible(
            "f".into(),
            vec!["Int32".into(), "Int32".into()],
            Vec::new(),
        ),
    );
    err(
        "fn f(x: Int32..., y: Int32) {}",
        pos(1, 19),
        SemError::VariadicParameterNeedsToBeLast,
    );
}

#[test]
fn for_with_array() {
    ok("fn f(x: Array[Int32]): Int32 {
        var result = 0i32;
        for i in x {
            result = result + i;
        }
        result
    }");

    ok("fn f(x: Array[Float32]): Float32 {
        var result = 0.0f32;
        for i in x {
            result = result + i;
        }
        result
    }");
}

#[test]
fn for_with_vec() {
    ok("fn f(x: Vec[Int32]): Int32 {
        var result = 0i32;
        for i in x.makeIterator() {
            result = result + i;
        }
        result
    }");

    ok("fn f(x: Vec[Int32]): Int32 {
        var result = 0i32;
        for i in x {
            result = result + i;
        }
        result
    }");

    ok("fn f(x: Vec[Float32]): Float32 {
        var result = 0.0f32;
        for i in x.makeReverseIterator() {
            result = result + i;
        }
        result
    }");

    ok("fn f(x: Vec[Float32]): Float32 {
        var result = 0.0f32;
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
        pos(3, 21),
        SemError::WrongNumberTypeParams(1, 0),
    );
}

#[test]
fn check_wrong_number_type_params() {
    err(
        "
            fn foo() { bar[Int32](false); }
            fn bar[T](x: T) {}
        ",
        pos(2, 34),
        SemError::ParamTypesIncompatible("bar".into(), vec!["T".into()], vec!["Bool".into()]),
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
        pos(3, 9),
        SemError::ShadowFunction("f".into()),
    );
}

#[test]
fn shadow_type_with_function() {
    err(
        "
        class FooBar
        fn FooBar() {}
        ",
        pos(3, 9),
        SemError::ShadowClass("FooBar".into()),
    );
}

#[test]
fn define_param_name_twice() {
    err(
        "fn test(x: String, x: Int32) {}",
        pos(1, 20),
        SemError::ShadowParam("x".into()),
    );
}

#[test]
fn show_type_param_with_name() {
    err(
        "fn test[T](T: Int32) {}",
        pos(1, 12),
        SemError::ShadowTypeParam("T".into()),
    );
}

#[test]
fn shadow_type_with_var() {
    ok("fn test() { let String = 3i32; }");
}

#[test]
fn shadow_function() {
    ok("fn f() { let f = 1i32; }");
    err(
        "fn f() { let f = 1i32; f(); }",
        pos(1, 25),
        SemError::UnknownMethod("Int32".into(), "get".into(), Vec::new()),
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
        pos(1, 26),
        SemError::ShadowParam("a".into()),
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
        pos(1, 18),
        SemError::UnknownIdentifier("a".into()),
    );
    err(
        "fn f() { a; }",
        pos(1, 10),
        SemError::UnknownIdentifier("a".into()),
    );
}

#[test]
fn undefined_function() {
    err(
        "fn f() { foo(); }",
        pos(1, 10),
        SemError::UnknownIdentifier("foo".into()),
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
        pos(1, 42),
        SemError::UnknownIdentifier("a".into()),
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
    ok("fn f() { for i in std::range(0i32, 4i32) { i; } }");
}

#[test]
fn mod_fct_call() {
    err(
        "
        fn f() { foo::g(); }
        mod foo { fn g() {} }
    ",
        pos(2, 24),
        SemError::NotAccessible("foo::g".into()),
    );

    ok("
        fn f() { foo::g(); }
        mod foo { @pub fn g() {} }
    ");

    ok("
        fn f() { foo::bar::baz(); }
        mod foo {
            @pub mod bar {
                @pub fn baz() {}
            }
        }
    ");

    err(
        "
        fn f() { foo::bar::baz(); }
        mod foo {
            @pub mod bar {
                fn baz() {}
            }
        }
    ",
        pos(2, 31),
        SemError::NotAccessible("foo::bar::baz".into()),
    );
}

#[test]
fn mod_ctor_call() {
    ok("
        fn f() { foo::Foo(); }
        mod foo { @pub class Foo }
    ");

    err(
        "
        fn f() { foo::Foo(); }
        mod foo { class Foo }
    ",
        pos(2, 26),
        SemError::NotAccessible("foo::Foo".into()),
    );

    ok("
        fn f() { foo::bar::Foo(); }
        mod foo { @pub mod bar { @pub class Foo } }
    ");

    err(
        "
        fn f() { foo::bar::Foo(); }
        mod foo { @pub mod bar { class Foo } }
    ",
        pos(2, 31),
        SemError::NotAccessible("foo::bar::Foo".into()),
    );
}

#[test]
fn mod_class_field() {
    err(
        "
        fn f(x: foo::Foo) { let a = x.bar; }
        mod foo { @pub class Foo(bar: Int32) }
    ",
        pos(2, 38),
        SemError::NotAccessible("bar".into()),
    );

    err(
        "
        fn f(x: foo::Foo) { let a = x.bar(10i64); }
        mod foo { @pub class Foo(bar: Array[Int32]) }
    ",
        pos(2, 42),
        SemError::NotAccessible("bar".into()),
    );

    err(
        "
        fn f(x: foo::Foo) { x.bar(10i64) = 10i32; }
        mod foo { @pub class Foo(bar: Array[Int32]) }
    ",
        pos(2, 30),
        SemError::NotAccessible("bar".into()),
    );

    ok("
        fn f(x: foo::Foo) { let a = x.bar; }
        mod foo { @pub class Foo(@pub bar: Int32) }
    ");
}

#[test]
fn mod_class_method() {
    ok("
        fn f(x: foo::Foo) { x.bar(); }
        mod foo {
            @pub class Foo
            impl Foo { @pub fn bar() {} }
        }
    ");

    err(
        "
        fn f(x: foo::Foo) { x.bar(); }
        mod foo {
            @pub class Foo
            impl Foo { fn bar() {} }
        }
    ",
        pos(2, 34),
        SemError::NotAccessible("foo::Foo#bar".into()),
    );
}

#[test]
fn mod_class_static_method() {
    ok("
        fn f() { foo::Foo::bar(); }
        mod foo {
            @pub class Foo
            impl Foo { @pub @static fn bar() {} }
        }
    ");

    err(
        "
        fn f() { foo::Foo::bar(); }
        mod foo {
            @pub class Foo
            impl Foo { @static fn bar() {} }
        }
    ",
        pos(2, 31),
        SemError::NotAccessible("foo::Foo::bar".into()),
    );
}

#[test]
fn mod_struct_field() {
    err(
        "
        fn f(x: foo::Foo) { let a = x.bar; }
        mod foo { @pub struct Foo(bar: Int32) }
    ",
        pos(2, 38),
        SemError::NotAccessible("bar".into()),
    );

    ok("
        fn f(x: foo::Foo) { let a = x.bar(10i64); }
        mod foo { @pub struct Foo(@pub bar: Array[Int32]) }
    ");

    err(
        "
        fn f(x: foo::Foo) { let a = x.bar(10i64); }
        mod foo { @pub struct Foo(bar: Array[Int32]) }
    ",
        pos(2, 42),
        SemError::NotAccessible("bar".into()),
    );

    err(
        "
        fn f(x: foo::Foo) { x.bar(10i64) = 10i32; }
        mod foo { @pub struct Foo(bar: Array[Int32]) }
    ",
        pos(2, 30),
        SemError::NotAccessible("bar".into()),
    );

    ok("
        fn f(x: foo::Foo) { let a = x.bar; }
        mod foo { @pub struct Foo(@pub bar: Int32) }
    ");
}

#[test]
fn mod_path_in_type() {
    ok("
        fn f(): foo::Foo { foo::Foo() }
        mod foo { @pub class Foo }
    ");

    err(
        "
        fn f(): bar::Foo { 1i32 }
    ",
        pos(2, 17),
        SemError::ExpectedModule,
    );

    err(
        "
        fn bar() {}
        fn f(): bar::Foo { 1i32 }
    ",
        pos(3, 17),
        SemError::ExpectedModule,
    );

    err(
        "
        fn f(): foo::bar::Foo { 1i32 }
        mod foo {}
    ",
        pos(2, 17),
        SemError::ExpectedModule,
    );
}

#[test]
fn mod_global() {
    ok("
        fn f(): Int32 { foo::x }
        mod foo { @pub var x: Int32 = 1i32; }
    ");

    err(
        "
        fn f(): Int32 { foo::x }
        mod foo { var x: Int32 = 1i32; }
    ",
        pos(2, 28),
        SemError::NotAccessible("foo::x".into()),
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
            class Foo(x: Bar)
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
            class Foo(x: Bar)
            class Bar
        }
    ");

    err(
        "
        fn f() { foo::Foo(1i32); }
        mod foo {
            class Foo(f: Int32)
        }
    ",
        pos(2, 26),
        SemError::NotAccessible("foo::Foo".into()),
    );

    err(
        "
        fn f() { foo::Foo(1i32); }
        mod foo {
            class Foo(@pub f: Int32)
        }
    ",
        pos(2, 26),
        SemError::NotAccessible("foo::Foo".into()),
    );

    err(
        "
        fn f() { foo::Foo(1i32); }
        mod foo {
            @pub class Foo(f: Int32)
        }
    ",
        pos(2, 26),
        SemError::ClassConstructorNotAccessible("foo::Foo".into()),
    );
}

#[test]
fn mod_struct() {
    err(
        "
        fn f() { foo::Foo(1i32); }
        mod foo {
            struct Foo(f: Int32)
        }
    ",
        pos(2, 26),
        SemError::NotAccessible("foo::Foo".into()),
    );

    err(
        "
        fn f() { foo::Foo(1i32); }
        mod foo {
            struct Foo(@pub f: Int32)
        }
    ",
        pos(2, 26),
        SemError::NotAccessible("foo::Foo".into()),
    );

    err(
        "
        fn f() { foo::Foo(1i32); }
        mod foo {
            @pub struct Foo(f: Int32)
        }
    ",
        pos(2, 26),
        SemError::StructConstructorNotAccessible("foo::Foo".into()),
    );

    ok("
        fn f() { foo::Foo(1i32); }
        mod foo {
            @pub struct Foo(@pub f: Int32)
        }
    ");

    ok("
        fn f(value: foo::Foo) {}
        mod foo {
            @pub struct Foo(f: Int32)
        }
    ");

    err(
        "
        fn f(value: foo::Foo) {}
        mod foo {
            struct Foo(f: Int32)
        }
    ",
        pos(2, 21),
        SemError::NotAccessible("foo::Foo".into()),
    );
}

#[test]
fn mod_const() {
    ok("
        fn f(): Int32 { foo::x }
        mod foo { @pub const x: Int32 = 1i32; }
    ");

    err(
        "
        fn f(): Int32 { foo::x }
        mod foo { const x: Int32 = 1i32; }
    ",
        pos(2, 28),
        SemError::NotAccessible("foo::x".into()),
    );

    ok("
        fn f(): Int32 { foo::bar::x }
        mod foo { @pub mod bar { @pub const x: Int32 = 1i32; } }
    ");
}

#[test]
fn mod_enum_value() {
    ok("
        fn f() { foo::A; }
        mod foo { @pub enum Foo { A, B } use Foo::A; }
    ");

    err(
        "
        fn f() { foo::A; }
        mod foo { enum Foo { A, B } use Foo::A; }
    ",
        pos(2, 21),
        SemError::NotAccessible("foo::Foo".into()),
    );

    ok("
        fn f() { foo::bar::A; }
        mod foo { @pub mod bar { @pub enum Foo { A, B } use Foo::A; } }
    ");

    err(
        "
        fn f() { foo::bar::A; }
        mod foo { @pub mod bar { enum Foo { A, B } use Foo::A; } }
    ",
        pos(2, 26),
        SemError::NotAccessible("foo::bar::Foo".into()),
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
        pos(3, 21),
        SemError::NotAccessible("foo::Foo".into()),
    );

    ok("
        fn f() {
            foo::Foo::B;
        }
        mod foo { @pub enum Foo { A, B } }
    ");

    ok("
        fn f() {
            foo::Foo::A(1i32);
        }
        mod foo { @pub enum Foo { A(Int32), B } }
    ");

    err(
        "
        fn f() {
            foo::Foo::A(1i32);
        }
        mod foo { enum Foo { A(Int32), B } }
    ",
        pos(3, 24),
        SemError::NotAccessible("foo::Foo".into()),
    );
}

#[test]
fn mod_use() {
    ok("
        use foo::bar;
        fn f() { bar(); }
        mod foo { @pub fn bar() {} }
    ");

    ok("
        use foo::bar::baz;
        fn f() { baz(); }
        mod foo { @pub mod bar {
            @pub fn baz() {}
        } }
    ");

    ok("
        use foo::bar as baz;
        fn f() { baz(); }
        mod foo { @pub fn bar() {} }
    ");

    ok("
        use foo::bar;
        fn f(): Int32 { bar }
        mod foo { @pub var bar: Int32 = 10i32; }
    ");

    ok("
        use foo::bar::baz;
        fn f(): Int32 { baz }
        mod foo { @pub mod bar {
            @pub var baz: Int32 = 10i32;
        } }
    ");

    ok("
        use foo::bar;
        fn f(): Int32 { bar }
        mod foo { @pub var bar: Int32 = 10i32; }
    ");
}

#[test]
fn mod_use_class() {
    ok("
        use foo::Bar;
        fn f() { Bar(); }
        mod foo { @pub class Bar }
    ");

    ok("
        use foo::Bar;
        fn f() {
            Bar();
            Bar::baz();
        }
        mod foo {
            @pub class Bar
            impl Bar {
                @pub @static fn baz() {}
            }
        }
    ");
}

#[test]
fn mod_use_trait() {
    ok("
        use foo::Bar;
        mod foo { @pub trait Bar{} }
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

    err("use super::Foo;", pos(1, 5), SemError::NoSuperModule);
}

#[test]
fn mod_use_self() {
    ok("
        use self::bar::Foo;
        fn getfoo(): Foo { Foo() }
        mod bar { @pub class Foo }
    ");
}

#[test]
fn mod_use_errors() {
    err(
        "
        use foo::bar::baz;
        mod foo { @pub mod bar {} }
    ",
        pos(2, 23),
        SemError::UnknownIdentifierInModule("foo::bar".into(), "baz".into()),
    );

    err(
        "
        use foo::bar;
    ",
        pos(2, 13),
        SemError::UnknownIdentifierInModule("".into(), "foo".into()),
    );

    err(
        "
        use foo::bar;
        mod foo {}
    ",
        pos(2, 18),
        SemError::UnknownIdentifierInModule("foo".into(), "bar".into()),
    );

    err(
        "
        use foo::bar;
        fn foo() {}
    ",
        pos(2, 13),
        SemError::ExpectedPath,
    );

    err(
        "
        use foo::bar::baz;
        @pub mod foo { @pub fn bar() {} }
    ",
        pos(2, 18),
        SemError::ExpectedPath,
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
        mod foo { @pub class Foo }
    ");

    err(
        "
        fn f(x: foo::Foo) {}
        mod foo { class Foo }
    ",
        pos(2, 17),
        SemError::NotAccessible("foo::Foo".into()),
    );
}

#[test]
fn different_fct_call_kinds() {
    ok("fn f() { g(); } fn g() {}");
    ok("fn f() { g[Int32](); } fn g[T]() {}");
    ok("fn f(g: Array[Int32]) { g(0); }");
    err(
        "fn f(g: Array[Int32]) { g[Float32](0); }",
        pos(1, 26),
        SemError::NoTypeParamsExpected,
    );
    ok("class Foo fn f() { Foo(); }");
    errors(
        "fn f() { 1i32[Int32](); }",
        &[
            (pos(1, 14), SemError::NoTypeParamsExpected),
            (
                pos(1, 21),
                SemError::UnknownMethod("Int32".into(), "get".into(), Vec::new()),
            ),
        ],
    );
    ok("enum Foo { A(Int32), B } fn f() { Foo::A(1i32); }");
    ok("enum Foo[T] { A(Int32), B } fn f() { Foo[Int32]::A(1i32); }");
    ok("enum Foo[T] { A(Int32), B } fn f() { Foo::A[Int32](1i32); }");
    err(
        "enum Foo[T] { A(Int32), B } fn f() { Foo[Int32]::A[Int32](1i32); }",
        pos(1, 41),
        SemError::NoTypeParamsExpected,
    );
    ok("trait MyTrait { @static fn foo(); } fn f[T: MyTrait]() { T::foo(); }");
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
        pos(6, 21),
        SemError::TypeNotImplementingTrait("Bar".into(), "Foo".into()),
    );
}

#[test]
fn infer_enum_type() {
    ok("fn f(): Option[Int32] {
        None
    }");

    ok("
        class X
        impl X {
            var a: Option[Int32] = None;
            var b: Option[Int32] = Some(10i32);
        }

        fn f(x: X) {
            x.a = Some(10i32);
            x.b = None;
        }
    ");

    ok("fn f() {
        var x: Option[Int32] = None; x = Some(10i32);
        var y: Option[Int32] = Some(10i32); y = None;
    }");

    ok("fn f(): Option[Int32] {
        Some(10i32)
    }");
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
        pos(7, 18),
        SemError::ParamTypesIncompatible("f".into(), vec!["String".into()], vec!["T".into()]),
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
        pos(1, 33),
        SemError::ReturnType("Bool".into(), "Int32".into()),
    );

    err(
        "fn f(foo: (Int32, Int32): Int32): Int32 {
        foo(1i32)
    }",
        pos(2, 12),
        SemError::LambdaParamTypesIncompatible(
            vec!["Int32".into(), "Int32".into()],
            vec!["Int32".into()],
        ),
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
        pos(2, 27),
        SemError::ReturnType("Int32".into(), "Bool".into()),
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
        pos(2, 21),
        SemError::UnknownIdentifier("x".into()),
    );
}

#[test]
fn internal_class_ctor() {
    err(
        "fn f(): Array[Int32] {
        Array[Int32]()
    }",
        pos(2, 21),
        SemError::ClassConstructorNotAccessible("std::collections::Array".into()),
    );
}

#[test]
fn internal_struct_ctor() {
    err(
        "fn f() {
        Int32();
    }",
        pos(2, 14),
        SemError::StructConstructorNotAccessible("std::primitives::Int32".into()),
    );
}
