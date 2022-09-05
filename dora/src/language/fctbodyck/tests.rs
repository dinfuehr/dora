use crate::language::error::msg::ErrorMessage;
use crate::language::sem_analysis::ConstValue;
use crate::language::tests::*;

#[test]
fn type_method_len() {
    ok("fun f(a: String): Int64 { return a.size(); }");
    ok("fun f(a: String): Int64 { return \"abc\".size(); }");
}

#[test]
fn type_object_field() {
    ok("class Foo(a:Int32) fun f(x: Foo): Int32 { return x.a; }");
    ok("class Foo(a:String) fun f(x: Foo): String { return x.a; }");
    err(
        "class Foo(a:Int32) fun f(x: Foo): Bool { return x.a; }",
        pos(1, 42),
        ErrorMessage::ReturnType("Bool".into(), "Int32".into()),
    );
    err(
        "class Foo(a:Int32) fun f(x: Foo): Int32 { return x.b; }",
        pos(1, 51),
        ErrorMessage::UnknownField("b".into(), "Foo".into()),
    );
}

#[test]
fn type_object_set_field() {
    ok("class Foo(a: Int32) fun f(x: Foo) { x.a = 1; }");
    err(
        "class Foo(a: Int32) fun f(x: Foo) { x.a = false; }",
        pos(1, 41),
        ErrorMessage::AssignField("a".into(), "Foo".into(), "Int32".into(), "Bool".into()),
    );
}

#[test]
fn type_object_field_without_self() {
    err(
        "class Foo(a: Int32) impl Foo { fun f(): Int32 { return a; } }",
        pos(1, 56),
        ErrorMessage::UnknownIdentifier("a".into()),
    );
    err(
        "class Foo(a: Int32) impl Foo { fun set(x: Int32) { a = x; } }",
        pos(1, 52),
        ErrorMessage::UnknownIdentifier("a".into()),
    );
}

#[test]
fn type_class_method_call() {
    ok("
        class Foo
        impl Foo {
            fun bar() {}
            fun baz(): Int32 { return 1; }
        }

        fun f(x: Foo) { x.bar(); }
        fun g(x: Foo): Int32 { return x.baz(); }");

    err(
        "
        class Foo
        impl Foo {
            fun bar(): Int32 { return 0; }
        }

        fun f(x: Foo): String { return x.bar(); }",
        pos(7, 33),
        ErrorMessage::ReturnType("String".into(), "Int32".into()),
    );
}

#[test]
fn return_type() {
    err(
        "
        class Foo[T]
        fun f(): Foo[Int32] { Foo[Int64]() }
    ",
        pos(3, 29),
        ErrorMessage::ReturnType("Foo[Int32]".into(), "Foo[Int64]".into()),
    );
}

#[test]
fn type_method_defined_twice() {
    err(
        "class Foo
        impl Foo {
                 fun bar() {}
                 fun bar() {}
             }",
        pos(4, 18),
        ErrorMessage::MethodExists("bar".into(), pos(3, 18)),
    );

    err(
        "class Foo
        impl Foo{
                 fun bar() {}
                 fun bar(): Int32 {}
             }",
        pos(4, 18),
        ErrorMessage::MethodExists("bar".into(), pos(3, 18)),
    );

    err(
        "class Foo
        impl Foo {
                 fun bar(a: Int32) {}
                 fun bar(a: Int32): Int32 {}
             }",
        pos(4, 18),
        ErrorMessage::MethodExists("bar".into(), pos(3, 18)),
    );

    err(
        "class Foo
        impl Foo {
                fun bar(a: Int32) {}
                fun bar(a: String) {}
            }",
        pos(4, 17),
        ErrorMessage::MethodExists("bar".into(), pos(3, 17)),
    );
}

#[test]
fn type_self() {
    ok("class Foo impl Foo { fun me(): Foo { return self; } }");
    err(
        "class Foo fun me() { return self; }",
        pos(1, 29),
        ErrorMessage::ThisUnavailable,
    );

    ok("class Foo(a: Int32, b: Int32)
        impl Foo {
            fun bar(): Int32 { return self.a + self.b; }
        }");

    ok("class Foo(a: Int32)
        impl Foo {
            fun setA(a: Int32) { self.a = a; }
        }");

    ok("class Foo
        impl Foo {
            fun zero(): Int32 { return 0i32; }
            fun other(): Int32 { return self.zero(); }
        }");

    ok("class Foo
        impl Foo {
            fun bar() { self.bar(); }
        }");
}

#[test]
fn type_unknown_method() {
    err(
        "class Foo
            impl Foo {
                 fun bar(a: Int32) { }
            }

            fun f(x: Foo) { x.bar(); }",
        pos(6, 34),
        ErrorMessage::ParamTypesIncompatible("bar".into(), vec!["Int32".into()], Vec::new()),
    );

    err(
        "class Foo
              fun f(x: Foo) { x.bar(1i32); }",
        pos(2, 36),
        ErrorMessage::UnknownMethod("Foo".into(), "bar".into(), vec!["Int32".into()]),
    );
}

#[test]
fn type_ctor() {
    ok("class Foo fun f(): Foo { return Foo(); }");
    ok("class Foo(a: Int32) fun f(): Foo { return Foo(1i32); }");
    err(
        "class Foo fun f(): Foo { return 1i32; }",
        pos(1, 26),
        ErrorMessage::ReturnType("Foo".into(), "Int32".into()),
    );
}

#[test]
fn type_def_for_return_type() {
    ok("fun a(): Int32 { return 1i32; }");
    err(
        "fun a(): unknown {}",
        pos(1, 10),
        ErrorMessage::UnknownIdentifier("unknown".into()),
    );
}

#[test]
fn type_def_for_param() {
    ok("fun a(b: Int32) {}");
    err(
        "fun a(b: foo) {}",
        pos(1, 10),
        ErrorMessage::UnknownIdentifier("foo".into()),
    );
}

#[test]
fn type_def_for_var() {
    ok("fun a() { let a : Int32 = 1i32; }");
    err(
        "fun a() { let a : test = 1; }",
        pos(1, 19),
        ErrorMessage::UnknownIdentifier("test".into()),
    );
}

#[test]
fn type_var_wrong_type_defined() {
    ok("fun f() { let a : Int32 = 1i32; }");
    ok("fun f() { let a : Bool = false; }");
    ok("fun f() { let a : String = \"f\"; }");

    err(
        "fun f() { let a : Int32 = true; }",
        pos(1, 11),
        ErrorMessage::AssignType("a".into(), "Int32".into(), "Bool".into()),
    );
    err(
        "fun f() { let b : Bool = 2i32; }",
        pos(1, 11),
        ErrorMessage::AssignType("b".into(), "Bool".into(), "Int32".into()),
    );
}

#[test]
fn type_while() {
    ok("fun x() { while true { } }");
    ok("fun x() { while false { } }");
    err(
        "fun x() { while 2i32 { } }",
        pos(1, 11),
        ErrorMessage::WhileCondType("Int32".into()),
    );
}

#[test]
fn type_if() {
    ok("fun x() { if true { } }");
    ok("fun x() { if false { } }");
    err(
        "fun x() { if 4i32 { } }",
        pos(1, 11),
        ErrorMessage::IfCondType("Int32".into()),
    );
}

#[test]
fn type_return_unit() {
    ok("fun f() { return; }");
    err(
        "fun f() { return 1i32; }",
        pos(1, 11),
        ErrorMessage::ReturnType("()".into(), "Int32".into()),
    );
}

#[test]
fn type_return() {
    ok("fun f(): Int32 { let a = 1i32; return a; }");
    ok("fun f(): Int32 { return 1i32; }");
    err(
        "fun f(): Int32 { return; }",
        pos(1, 18),
        ErrorMessage::ReturnType("Int32".into(), "()".into()),
    );

    ok("fun f(): Int32 { return 0i32; }
            fun g(): Int32 { return f(); }");
    err(
        "fun f() { }
             fun g(): Int32 { return f(); }",
        pos(2, 31),
        ErrorMessage::ReturnType("Int32".into(), "()".into()),
    );
}

#[test]
fn type_variable() {
    ok("fun f(a: Int32) { let b: Int32 = a; }");
}

#[test]
fn type_let() {
    ok("fun f(value: (Int32, Int32)): Int32 { let (a, b) = value; a+b }");
    err(
        "fun f() { let (a, b) = true; }",
        pos(1, 15),
        ErrorMessage::LetPatternExpectedTuple("Bool".into()),
    );

    ok("fun f(value: ()) { let () = value; }");
    err(
        "fun f() { let () = true; }",
        pos(1, 15),
        ErrorMessage::LetPatternExpectedTuple("Bool".into()),
    );
    err(
        "fun f() { let (a, b) = (); }",
        pos(1, 15),
        ErrorMessage::LetPatternShouldBeUnit,
    );

    err(
        "fun f() { let (a, b) = (true,); }",
        pos(1, 15),
        ErrorMessage::LetPatternExpectedTupleWithLength("(Bool)".into(), 1, 2),
    );
    err(
        "fun f() { let () = (true,); }",
        pos(1, 15),
        ErrorMessage::LetPatternExpectedTupleWithLength("(Bool)".into(), 1, 0),
    );

    ok("fun f(value: (Int32, (Int32, Int32))): Int32 { let (a, (b, c)) = value; a+b+c }");
}

#[test]
fn type_assign_lvalue() {
    err(
        "fun f() { 1 = 3; }",
        pos(1, 13),
        ErrorMessage::LvalueExpected,
    );
}

#[test]
fn type_un_op() {
    ok("fun f(a: Int32) { -a; +a; }");
    err(
        "fun f(a: Bool) { -a; }",
        pos(1, 18),
        ErrorMessage::UnOpType("-".into(), "Bool".into()),
    );
    err(
        "fun f(a: Bool) { +a; }",
        pos(1, 18),
        ErrorMessage::UnOpType("+".into(), "Bool".into()),
    );
}

#[test]
fn type_bin_op() {
    ok("fun f(a: Int32) { a+a; a-a; a*a; a/a; a%a; }");
    ok("fun f(a: Int32) { a<a; a<=a; a==a; a!=a; a>a; a>=a; }");
    ok("fun f(a: String) { a<a; a<=a; a==a; a!=a; a>a; a>=a; }");
    ok("fun f(a: String) { a===a; a!==a; a+a; }");
    ok("class Foo fun f(a: Foo) { a===a; a!==a; }");
    ok("fun f(a: Int32) { a|a; a&a; a^a; }");
    ok("fun f(a: Bool) { a||a; a&&a; }");

    err(
        "class A class B fun f(a: A, b: B) { a === b; }",
        pos(1, 39),
        ErrorMessage::TypesIncompatible("A".into(), "B".into()),
    );
    err(
        "class A class B fun f(a: A, b: B) { b !== a; }",
        pos(1, 39),
        ErrorMessage::TypesIncompatible("B".into(), "A".into()),
    );
    err(
        "fun f(a: Bool) { a+a; }",
        pos(1, 19),
        ErrorMessage::BinOpType("+".into(), "Bool".into(), "Bool".into()),
    );
    err(
        "fun f(a: Bool) { a^a; }",
        pos(1, 19),
        ErrorMessage::BinOpType("^".into(), "Bool".into(), "Bool".into()),
    );
    err(
        "fun f(a: Int32) { a||a; }",
        pos(1, 20),
        ErrorMessage::BinOpType("||".into(), "Int32".into(), "Int32".into()),
    );
    err(
        "fun f(a: Int32) { a&&a; }",
        pos(1, 20),
        ErrorMessage::BinOpType("&&".into(), "Int32".into(), "Int32".into()),
    );
    err(
        "fun f(a: String) { a-a; }",
        pos(1, 21),
        ErrorMessage::BinOpType("-".into(), "String".into(), "String".into()),
    );
    err(
        "fun f(a: String) { a*a; }",
        pos(1, 21),
        ErrorMessage::BinOpType("*".into(), "String".into(), "String".into()),
    );
    err(
        "fun f(a: String) { a%a; }",
        pos(1, 21),
        ErrorMessage::BinOpType("%".into(), "String".into(), "String".into()),
    );
}

#[test]
fn type_function_return_type() {
    ok("fun foo(): Int32 { return 1i32; } fun f() { let i: Int32 = foo(); }");
    err(
        "
        fun foo(): Int32 { return 1i32; }
        fun f() { let i: Bool = foo(); }",
        pos(3, 19),
        ErrorMessage::AssignType("i".into(), "Bool".into(), "Int32".into()),
    );
}

#[test]
fn type_ident_in_function_params() {
    ok("
    fun f(a: Int32) {}
    fun g() { let a = 1i32; f(a); }");
}

#[test]
fn type_recursive_function_call() {
    ok("fun f(a: Int32) { f(a); }");
}

#[test]
fn type_function_params() {
    ok("fun foo() {} fun f() { foo(); }");
    ok("fun foo(a: Int32) {} fun f() { foo(1i32); }");
    ok("fun foo(a: Int32, b: Bool) {} fun f() { foo(1i32, true); }");

    err(
        "
        fun foo() {}
        fun f() { foo(1i32); }",
        pos(3, 22),
        ErrorMessage::ParamTypesIncompatible("foo".into(), vec![], vec!["Int32".into()]),
    );
    err(
        "
        fun foo(a: Int32) {}
        fun f() { foo(true); }",
        pos(3, 22),
        ErrorMessage::ParamTypesIncompatible(
            "foo".into(),
            vec!["Int32".into()],
            vec!["Bool".into()],
        ),
    );
    err(
        "
        fun foo(a: Int32, b: Bool) {}
        fun f() { foo(1i32, 2i32); }",
        pos(3, 22),
        ErrorMessage::ParamTypesIncompatible(
            "foo".into(),
            vec!["Int32".into(), "Bool".into()],
            vec!["Int32".into(), "Int32".into()],
        ),
    );
}

#[test]
fn type_array() {
    ok("fun f(a: Array[Int32]): Int32 { return a(1i64); }");
    err(
        "fun f(a: Array[Int32]): String { return a(1i64); }",
        pos(1, 34),
        ErrorMessage::ReturnType("String".into(), "Int32".into()),
    );
}

#[test]
fn type_array_assign() {
    err(
        "fun f(a: Array[Int32]): Int32 { return a(3) = 4i32; }",
        pos(1, 33),
        ErrorMessage::ReturnType("Int32".into(), "()".into()),
    );
    err(
        "fun f(a: Array[Int32]) { a(3) = \"b\"; }",
        pos(1, 31),
        ErrorMessage::UnknownMethod(
            "Array[Int32]".into(),
            "set".into(),
            vec!["Int64".into(), "String".into()],
        ),
    );
}

#[test]
fn type_array_field() {
    ok("
        class Foo(x: Array[Int32])
        fun f(a: Foo): Int32 { return a.x(1i64); }
    ");
}

#[test]
fn wrong_type_params_for_primitive() {
    err(
        "
        fun f() { let a: Int32[Bool, Char] = 10; }
    ",
        pos(2, 26),
        ErrorMessage::WrongNumberTypeParams(0, 2),
    );
}

#[test]
fn let_without_initialization() {
    err(
        "fun f() { let x: Int32; }",
        pos(1, 11),
        ErrorMessage::LetMissingInitialization,
    );
}

#[test]
fn reassign_param() {
    err(
        "fun f(a: Int32) { a = 1; }",
        pos(1, 21),
        ErrorMessage::LetReassigned,
    );
}

#[test]
fn reassign_field() {
    ok("class Foo(x: Int32) fun foo(f: Foo) { f.x = 1; }");
}

#[test]
fn reassign_var() {
    ok("fun f() { let mut a=1; a=2; }");
}

#[test]
fn reassign_let() {
    err(
        "fun f() { let a=1; a=2; }",
        pos(1, 21),
        ErrorMessage::LetReassigned,
    );
}

#[test]
fn reassign_self() {
    err(
        "class Foo
        impl Foo {
            fun f() { self = Foo(); }
        }",
        pos(3, 28),
        ErrorMessage::LvalueExpected,
    );
}

#[test]
fn same_names() {
    ok("class Foo { Foo: Foo }");
    ok("class Foo fun foo() { let Foo: Int32 = 1i32; }");
}

#[test]
fn lit_int64() {
    ok("fun f(): Int64 { return 1i64; }");
    ok("fun f(): Int32 { return 1i32; }");

    let ret = ErrorMessage::ReturnType("Int32".into(), "Int64".into());
    err("fun f(): Int32 { return 1i64; }", pos(1, 18), ret);

    ok("fun f(): Int64 { return 1; }");
}

#[test]
fn lit_int64_as_default() {
    ok("fun f(): Int64 { return 1; }");
    ok("fun f(): Int64 {
        let x = 1;
        return x;
    }");
}

#[test]
fn overload_plus() {
    ok("class A impl A { fun plus(rhs: A): Int32 { return 0; } }
            fun f(): Int32 { return A() + A(); }");
}

#[test]
fn overload_minus() {
    ok("class A impl A { fun minus(rhs: A): Int32 { return 0; } }
            fun f(): Int32 { return A() - A(); }");
}

#[test]
fn overload_times() {
    ok("class A impl A { fun times(rhs: A): Int32 { return 0; } }
            fun f(): Int32 { return A() * A(); }");
}

#[test]
fn overload_div() {
    ok("class A impl A { fun div(rhs: A): Int32 { return 0; } }
            fun f(): Int32 { return A() / A(); }");
}

#[test]
fn overload_mod() {
    ok("class A impl A { fun modulo(rhs: A): Int32 { return 0; } }
            fun f(): Int32 { return A() % A(); }");
}

#[test]
fn overload_bitwise_or() {
    ok(
        "class A impl A { fun bitwiseOr(rhs: A): Int32 { return 0; } }
            fun f(): Int32 { return A() | A(); }",
    );
}

#[test]
fn overload_bitwise_and() {
    ok(
        "class A impl A { fun bitwiseAnd(rhs: A): Int32 { return 0i32; } }
            fun f(): Int32 { return A() & A(); }",
    );
}

#[test]
fn overload_bitwise_xor() {
    ok(
        "class A impl A { fun bitwiseXor(rhs: A): Int32 { return 0i32; } }
            fun f(): Int32 { return A() ^ A(); }",
    );
}

#[test]
fn overload_equals() {
    ok(
        "class A impl A { fun equals(rhs: A): Bool { return true; } }
            fun f1(): Bool { return A() == A(); }
            fun f2(): Bool { return A() != A(); }",
    );
}

#[test]
fn overload_compare_to() {
    ok(
        "class A impl A { fun compareTo(rhs: A): Int32 { return 0; } }
            fun f1(): Bool { return A() < A(); }
            fun f2(): Bool { return A() <= A(); }
            fun f3(): Bool { return A() > A(); }
            fun f4(): Bool { return A() >= A(); }",
    );
}

#[test]
fn int64_operations() {
    ok("fun f(a: Int64, b: Int64): Int64 { return a + b; }");
    ok("fun f(a: Int64, b: Int64): Int64 { return a - b; }");
    ok("fun f(a: Int64, b: Int64): Int64 { return a * b; }");
    ok("fun f(a: Int64, b: Int64): Int64 { return a / b; }");
    ok("fun f(a: Int64, b: Int64): Int64 { return a % b; }");
    ok("fun f(a: Int64, b: Int64): Int64 { return a | b; }");
    ok("fun f(a: Int64, b: Int64): Int64 { return a & b; }");
    ok("fun f(a: Int64, b: Int64): Int64 { return a ^ b; }");
    ok("fun f(a: Int64, b: Int64): Bool { return a == b; }");
    ok("fun f(a: Int64, b: Int64): Bool { return a != b; }");
    ok("fun f(a: Int64, b: Int64): Bool { return a === b; }");
    ok("fun f(a: Int64, b: Int64): Bool { return a !== b; }");
    ok("fun f(a: Int64, b: Int64): Bool { return a < b; }");
    ok("fun f(a: Int64, b: Int64): Bool { return a <= b; }");
    ok("fun f(a: Int64, b: Int64): Bool { return a > b; }");
    ok("fun f(a: Int64, b: Int64): Bool { return a >= b; }");
    ok("fun f(a: Int64): Int64 { return -a; }");
    ok("fun f(a: Int64): Int64 { return +a; }");
}

#[test]
fn test_literal_int_overflow() {
    err(
        "fun f() { let x = 2147483648i32; }",
        pos(1, 19),
        ErrorMessage::NumberOverflow("Int32".into()),
    );
    ok("fun f() { let x = 2147483647i32; }");
    err(
        "fun f() { let x = -2147483649i32; }",
        pos(1, 20),
        ErrorMessage::NumberOverflow("Int32".into()),
    );
    ok("fun f() { let x = -2147483648i32; }");
}

#[test]
fn test_literal_hex_int_overflow() {
    err(
        "fun f() { let x = 0x1_FF_FF_FF_FFi32; }",
        pos(1, 19),
        ErrorMessage::NumberOverflow("Int32".into()),
    );
    ok("fun f() { let x: Int32 = 0xFF_FF_FF_FFi32; }");
}

#[test]
fn test_literal_bin_int_overflow() {
    err(
        "fun f() { let x = 0b1_11111111_11111111_11111111_11111111i32; }",
        pos(1, 19),
        ErrorMessage::NumberOverflow("Int32".into()),
    );
    ok("fun f() { let x: Int32 = 0b11111111_11111111_11111111_11111111i32; }");
}

#[test]
fn test_literal_int64_overflow() {
    err(
        "fun f() { let x = 9223372036854775808i64; }",
        pos(1, 19),
        ErrorMessage::NumberOverflow("Int64".into()),
    );
    ok("fun f() { let x = 9223372036854775807i64; }");
    err(
        "fun f() { let x = -9223372036854775809i64; }",
        pos(1, 20),
        ErrorMessage::NumberOverflow("Int64".into()),
    );
    ok("fun f() { let x = -9223372036854775808i64; }");
}

#[test]
fn test_literal_float_overflow() {
    err(
        "fun f() { let x = -340282350000000000000000000000000000000f32; }",
        pos(1, 20),
        ErrorMessage::NumberOverflow("Float32".into()),
    );
    ok("fun f() { let x = -340282340000000000000000000000000000000f32; }");
    err(
        "fun f() { let x = 340282350000000000000000000000000000001f32; }",
        pos(1, 19),
        ErrorMessage::NumberOverflow("Float32".into()),
    );
    ok("fun f() { let x = 340282340000000000000000000000000000000f32; }");
}

#[test]
fn test_char() {
    ok("fun foo(): Char { return 'c'; }");
    ok("fun foo(a: Char): Char { return a; }");
    err(
        "fun foo(): Char { return false; }",
        pos(1, 19),
        ErrorMessage::ReturnType("Char".into(), "Bool".into()),
    );
    err(
        "fun foo(): Char { return 10i32; }",
        pos(1, 19),
        ErrorMessage::ReturnType("Char".into(), "Int32".into()),
    );
}

#[test]
fn test_generic_arguments_mismatch() {
    err(
        "class A[T]
            fun foo() {
                let a = A[Int32, Int32]();
            }",
        pos(3, 40),
        ErrorMessage::WrongNumberTypeParams(1, 2),
    );

    err(
        "class A[T]
            fun foo() {
                let a = A();
            }",
        pos(3, 26),
        ErrorMessage::WrongNumberTypeParams(1, 0),
    );

    err(
        "class A
            fun foo() {
                let a = A[Int32]();
            }",
        pos(3, 33),
        ErrorMessage::WrongNumberTypeParams(0, 1),
    );
}

#[test]
fn test_invoke_static_method_as_instance_method() {
    err(
        "class A
        impl A {
            @static fun foo() {}
            fun test() { self.foo(); }
        }",
        pos(4, 34),
        ErrorMessage::UnknownMethod("A".into(), "foo".into(), vec![]),
    );
}

#[test]
fn test_invoke_method_as_static() {
    err(
        "class A
        impl A {
            fun foo() {}
            @static fun test() { A::foo(); }
        }",
        pos(4, 40),
        ErrorMessage::UnknownStaticMethod("A".into(), "foo".into(), vec![]),
    );
}

#[test]
fn test_fct_with_type_params() {
    err(
        "fun f() {} fun g() { f[Int32](); }",
        pos(1, 30),
        ErrorMessage::WrongNumberTypeParams(0, 1),
    );
    err(
        "fun f[T]() {} fun g() { f(); }",
        pos(1, 26),
        ErrorMessage::WrongNumberTypeParams(1, 0),
    );
    ok("fun f[T]() {} fun g() { f[Int32](); }");
    ok("fun f[T1, T2]() {} fun g() { f[Int32, String](); }");
}

#[test]
fn test_type_param_bounds_in_definition() {
    err(
        "
            trait MyTrait {}
            class Foo[T: MyTrait]
            fun bar[T](arg: Foo[T]) {}
        ",
        pos(4, 29),
        ErrorMessage::TypeNotImplementingTrait("T".into(), "MyTrait".into()),
    );

    err(
        "
            trait MyTraitA {}
            trait MyTraitB {}
            class Foo[T: MyTraitA + MyTraitB]
            fun bar[T: MyTraitA](arg: Foo[T]) {}
        ",
        pos(5, 39),
        ErrorMessage::TypeNotImplementingTrait("T".into(), "MyTraitB".into()),
    );

    err(
        "
            trait MyTraitA {}
            trait MyTraitB {}
            class Foo[T: MyTraitA + MyTraitB]
            class Baz[X]
            impl[X] Baz[X] {
                fun bar[T: MyTraitA](arg: Foo[T]) {}
            }
        ",
        pos(7, 43),
        ErrorMessage::TypeNotImplementingTrait("T".into(), "MyTraitB".into()),
    );
}

#[test]
fn test_const_check() {
    err(
        "const one: Int32 = 1i32;
            fun f(): Int64 { return one; }",
        pos(2, 30),
        ErrorMessage::ReturnType("Int64".into(), "Int32".into()),
    );

    err(
        "const one: Int32 = 1i32;
            fun f() { let x: String = one; }",
        pos(2, 23),
        ErrorMessage::AssignType("x".into(), "String".into(), "Int32".into()),
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
            fun f() { one = 2i32; }",
        pos(2, 23),
        ErrorMessage::LvalueExpected,
    );
}

#[test]
fn test_unary_minus_byte() {
    err(
        "const m1: UInt8 = -1u8;",
        pos(1, 19),
        ErrorMessage::UnOpType("-".into(), "UInt8".into()),
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
            fun f(): A[X] { A[X]() }");

    err(
        "trait Foo {}
            class X
            class A[T: Foo]
            fun f(): A[X] { A[X]() }",
        pos(4, 22),
        ErrorMessage::TypeNotImplementingTrait("X".into(), "Foo".into()),
    );

    err(
        "trait Foo {}
            fun f[T: Foo]() {}
            fun t() { f[Int32](); }",
        pos(3, 31),
        ErrorMessage::TypeNotImplementingTrait("Int32".into(), "Foo".into()),
    );
}

#[test]
fn test_operator_on_generic_type() {
    err(
        "fun f[T](a: T, b: T) { a + b; }",
        pos(1, 26),
        ErrorMessage::BinOpType("+".into(), "T".into(), "T".into()),
    );
}

#[test]
fn test_find_class_method_precedence() {
    // finding class method should have precedence over
    // trait methods
    ok("class A
            impl A { fun foo() {} }
            trait Foo { fun foo(); }
            impl Foo for A { fun foo() {} }
            fun test(a: A) { a.foo(); }");

    err(
        "class A
            impl A { fun foo() {} }
            trait Foo { fun foo(a: Int32); }
            impl Foo for A { fun foo(a: Int32) {} }
            fun test(a: A) { a.foo(1i32); }",
        pos(5, 35),
        ErrorMessage::ParamTypesIncompatible("foo".into(), Vec::new(), vec!["Int32".into()]),
    );

    ok("class A
            impl A { @static fun foo() {} }
            trait Foo { fun foo(a: Int32); }
            impl Foo for A { fun foo(a:  Int32) {} }
            fun test(a: A) { a.foo(1i32); }");
}

#[test]
fn test_global_get() {
    ok("let mut x: Int32 = 0i32; fun foo(): Int32 { return x; }");
}

#[test]
fn test_global_set() {
    ok("let mut x: Int32 = 0i32; fun foo(a: Int32) { x = a; }");
    err(
        "let x: Int32 = 0i32; fun foo(a: Int32) { x = a; }",
        pos(1, 44),
        ErrorMessage::LetReassigned,
    );
}

#[test]
fn lambda_assignment() {
    ok("fun f() { let x = || {}; }");
    ok("fun f() { let x = ||: Int32 { return 2i32; }; }");
    ok("fun f() { let x: (): () = || {}; }");
    ok("fun f() { let x: (): () = ||: () {}; }");
    ok("fun f() { let x: (): Int32 = ||: Int32 { return 2i32; }; }");
    err(
        "fun f() { let x: (): Int32 = || {}; }",
        pos(1, 11),
        ErrorMessage::AssignType("x".into(), "() -> Int32".into(), "() -> ()".into()),
    );
}

#[test]
fn method_call_with_multiple_matching_traits() {
    err(
        "class A
            trait X { fun f(); }
            trait Y { fun f(); }

            impl X for A { fun f() {} }
            impl Y for A { fun f() {} }

            fun g(a: A) { a.f(); }",
        pos(8, 30),
        ErrorMessage::MultipleCandidatesForMethod("A".into(), "f".into(), Vec::new()),
    );
}

#[test]
fn generic_trait_method_call() {
    ok("trait Foo { fun bar(); }
            fun f[T: Foo](t: T) { t.bar(); }");
    ok("trait Foo { fun bar(); }
            class A[T: Foo](t: T)
            impl[T: Foo] A[T] {
                fun baz() { self.t.bar(); }
            }");
}

#[test]
fn test_generic_ctor_without_type_params() {
    err(
        "class Foo[A, B]
            fun test() { Foo(); }",
        pos(2, 29),
        ErrorMessage::WrongNumberTypeParams(2, 0),
    );
}

#[test]
fn test_generic_argument_with_trait_bound() {
    err(
        "fun f[X: std::Comparable](x: X) {}
            fun g[T](t: T) { f[T](t); }",
        pos(2, 34),
        ErrorMessage::TypeNotImplementingTrait("T".into(), "Comparable".into()),
    );
}

#[test]
fn test_for_supports_make_iterator() {
    err(
        "fun f() { for i in 1i32 {} }",
        pos(1, 20),
        ErrorMessage::TypeNotUsableInForIn("Int32".into()),
    );

    err(
        "
            class Foo
            impl Foo { fun makeIterator(): Bool { return true; } }
            fun f() { for i in Foo() {} }",
        pos(4, 35),
        ErrorMessage::TypeNotUsableInForIn("Foo".into()),
    );

    ok("
            class Foo
            impl Foo { fun makeIterator(): FooIter { return FooIter(); } }
            class FooIter
            impl std::Iterator for FooIter {
                fun next(): Option[Int32] { Some[Int32](0i32) }
            }
            fun f(): Int32 { for i in Foo() { return i; } return 0i32; }");
}

#[test]
fn test_ctor_with_type_param() {
    err(
        "
            class Foo[T]
            impl[T] Foo[T] {
                fun foo(a: Int32) {
                    Bar[T](a);
                }
            }

            class Bar[T](a: T)
            ",
        pos(5, 27),
        ErrorMessage::ParamTypesIncompatible("Bar".into(), vec!["T".into()], vec!["Int32".into()]),
    );
}

#[test]
fn test_fct_used_as_identifier() {
    err(
        "fun foo() {} fun bar() { foo; }",
        pos(1, 26),
        ErrorMessage::ValueExpected,
    );
}

#[test]
fn test_cls_used_as_identifier() {
    err(
        "class X fun f() { X; }",
        pos(1, 19),
        ErrorMessage::ValueExpected,
    );
}

#[test]
fn test_assign_fct() {
    err(
        "fun foo() {} fun bar() { foo = 1i32; }",
        pos(1, 26),
        ErrorMessage::LvalueExpected,
    );
}

#[test]
fn test_assign_class() {
    err(
        "
            class X
            fun foo() { X = 2i32; }
        ",
        pos(3, 25),
        ErrorMessage::LvalueExpected,
    );
}

#[test]
fn test_new_call_fct() {
    ok("fun g() {} fun f() { g(); }");
}

#[test]
fn test_new_call_fct_wrong_params() {
    err(
        "fun g() {} fun f() { g(1i32); }",
        pos(1, 23),
        ErrorMessage::ParamTypesIncompatible("g".into(), Vec::new(), vec!["Int32".into()]),
    );
}

#[test]
fn test_new_call_fct_with_type_params() {
    ok("fun g[T]() {} fun f() { g[Int32](); }");
}

#[test]
fn test_new_call_fct_with_wrong_type_params() {
    err(
        "fun g() {} fun f() { g[Int32](); }",
        pos(1, 30),
        ErrorMessage::WrongNumberTypeParams(0, 1),
    );
}

#[test]
fn test_new_call_static_method() {
    ok("class Foo impl Foo { @static fun bar() {} }
            fun f() { Foo::bar(); }");
}

#[test]
fn test_new_call_static_method_wrong_params() {
    err(
        "class Foo impl Foo { @static fun bar() {} }
            fun f() { Foo::bar(1i32); }",
        pos(2, 31),
        ErrorMessage::ParamTypesIncompatible("bar".into(), Vec::new(), vec!["Int32".into()]),
    );
}

#[test]
fn test_new_call_static_method_type_params() {
    ok("class Foo impl Foo { @static fun bar[T]() {} }
            fun f() { Foo::bar[Int32](); }");
}

#[test]
fn test_new_call_class() {
    ok("
        class X
        fun f() { X(); }
    ");
}

#[test]
fn test_new_call_class_wrong_params() {
    err(
        "
        class X
        fun f() { X(1i32); }
    ",
        pos(3, 20),
        ErrorMessage::ParamTypesIncompatible("X".into(), Vec::new(), vec!["Int32".into()]),
    );
}

#[test]
fn test_new_call_class_with_type_params() {
    ok("
        class X[T]
        fun f() { X[Int32](); }
    ");
}

#[test]
fn test_new_call_class_with_wrong_type_params() {
    err(
        "
            class X
            fun f() { X[Int32](); }
        ",
        pos(3, 31),
        ErrorMessage::WrongNumberTypeParams(0, 1),
    );
}

#[test]
fn test_new_call_method() {
    ok("
        class X
        impl X { fun f() {} }
        fun f(x: X) { x.f(); }
    ");
}

#[test]
fn test_new_call_method_type_param() {
    ok("
        class X
        impl X { fun f[T]() {} }
        fun f(x: X) { x.f[Int32](); }
    ");
}

#[test]
fn test_new_call_method_wrong_params() {
    err(
        "
        class X
        impl X { fun f() {} }
        fun f(x: X) { x.f(1i32); }",
        pos(4, 26),
        ErrorMessage::ParamTypesIncompatible("f".into(), Vec::new(), vec!["Int32".into()]),
    );
}

#[test]
fn test_new_call_method_generic() {
    ok("fun f[T: std::Hash](t: T) { t.hash(); }");
}

#[test]
fn test_new_call_method_generic_error() {
    err(
        "fun f[T](t: T) { t.hash(); }",
        pos(1, 24),
        ErrorMessage::UnknownMethodForTypeParam("T".into(), "hash".into(), Vec::new()),
    );
}

#[test]
fn test_new_call_method_generic_error_multiple() {
    err(
        "
            trait TraitA { fun id(); }
            trait TraitB { fun id(); }
            fun f[T: TraitA + TraitB](t: T) { t.id(); }",
        pos(4, 51),
        ErrorMessage::MultipleCandidatesForTypeParam("T".into(), "id".into(), Vec::new()),
    );
}

#[test]
fn test_named_arguments_fail_method() {
    err(
        "
            class Foo()
            impl Foo { fun foo(x: Int64, y: Bool) {} }
            fun x() { Foo().foo(z = 23, y = true); }",
        pos(4, 32),
        ErrorMessage::ArgumentNameMismatch(
            "foo".into(),
            vec!["x: Int64".into(), "y: Bool".into()],
            vec!["z: Int64".into(), "y: Bool".into()],
        ),
    );
}

#[test]
fn test_named_arguments_fail_method_static() {
    err(
        "
            class Foo()
            impl Foo { @static fun foo(x: Int64, y: Bool) {} }
            fun x() { Foo::foo(z = 23, y = true); }",
        pos(4, 31),
        ErrorMessage::ArgumentNameMismatch(
            "foo".into(),
            vec!["x: Int64".into(), "y: Bool".into()],
            vec!["z: Int64".into(), "y: Bool".into()],
        ),
    );
}

#[test]
fn test_named_arguments_fail_method_static_generic() {
    err(
        "
            class Foo()
            impl Foo { @static fun foo[T](x: T, y: Bool) {} }
            fun x() { Foo::foo[Int64](z = 23, y = true); }",
        pos(4, 38),
        ErrorMessage::ArgumentNameMismatch(
            "foo".into(),
            vec!["x: T".into(), "y: Bool".into()],
            vec!["z: Int64".into(), "y: Bool".into()],
        ),
    );
}

#[test]
fn test_named_arguments_fail_function() {
    err(
        "
            fun foo(x: Int64, y: Bool) {}
            fun x() { foo(z = 23, y = true); }",
        pos(3, 26),
        ErrorMessage::ArgumentNameMismatch(
            "foo".into(),
            vec!["x: Int64".into(), "y: Bool".into()],
            vec!["z: Int64".into(), "y: Bool".into()],
        ),
    );
}

#[test]
fn test_named_arguments_fail_class() {
    err(
        "
            class Foo(x: Int64, y: Bool)
            fun x() { Foo(z = 23, y = true); }",
        pos(3, 26),
        ErrorMessage::ArgumentNameMismatch(
            "Foo".into(),
            vec!["x: Int64".into(), "y: Bool".into()],
            vec!["z: Int64".into(), "y: Bool".into()],
        ),
    );
}

#[test]
fn test_named_arguments_fail_struct() {
    err(
        "
            struct Foo(x: Int64, y: Bool)
            fun x() { Foo(z = 23, y = true); }",
        pos(3, 26),
        ErrorMessage::ArgumentNameMismatch(
            "Foo".into(),
            vec!["x: Int64".into(), "y: Bool".into()],
            vec!["z: Int64".into(), "y: Bool".into()],
        ),
    );
}

#[test]
fn test_array_syntax_get() {
    ok("fun f(t: Array[Int32]): Int32 { return t(0); }");
}

#[test]
fn test_array_syntax_set() {
    ok("fun f(t: Array[Int32]){ t(0) = 10i32; }");
}

#[test]
fn test_array_syntax_set_wrong_value() {
    err(
        "fun f(t: Array[Int32]){ t(0) = true; }",
        pos(1, 30),
        ErrorMessage::UnknownMethod(
            "Array[Int32]".into(),
            "set".into(),
            vec!["Int64".into(), "Bool".into()],
        ),
    );
}

#[test]
fn test_array_syntax_set_wrong_index() {
    err(
        "fun f(t: Array[Int32]){ t(\"bla\") = 9i32; }",
        pos(1, 34),
        ErrorMessage::UnknownMethod(
            "Array[Int32]".into(),
            "set".into(),
            vec!["String".into(), "Int32".into()],
        ),
    );
}

#[test]
fn test_template() {
    ok("fun f(x: Int32): String { return \"x = ${x}\"; }");
    err(
        "
            class Foo
            fun f(x: Foo): String { return \"x = ${x}\"; }
        ",
        pos(3, 51),
        ErrorMessage::ExpectedStringable("Foo".into()),
    );
    ok("fun f[T: std::Stringable](x: T): String { return \"${x}\"; }");
}

#[test]
fn test_trait_object_as_argument() {
    ok("trait Foo { fun bar(): Int32; }
        fun f(x: Foo): Int32 { return x.bar(); }");
    err(
        "trait Foo { fun baz(); }
        fun f(x: Foo): String { return x.baz(); }",
        pos(2, 33),
        ErrorMessage::ReturnType("String".into(), "()".into()),
    );
}

#[test]
fn test_type_param_used_as_value() {
    err(
        "fun f[T](): Int32 { return T; }",
        pos(1, 28),
        ErrorMessage::ValueExpected,
    );

    err(
        "class SomeClass[T]
        impl[T] SomeClass[T] {
            fun f(): Int32 { return T; }
        }",
        pos(3, 37),
        ErrorMessage::ValueExpected,
    );
}

#[test]
fn test_assign_to_type_param() {
    err(
        "fun f[T]() { T = 10; }",
        pos(1, 14),
        ErrorMessage::LvalueExpected,
    );

    err(
        "
        class SomeClass[T]
        impl[T] SomeClass[T] {
            fun f() { T = 10; }
        }",
        pos(4, 23),
        ErrorMessage::LvalueExpected,
    );
}

#[test]
fn test_type_param_with_name_but_no_call() {
    err(
        "trait X { fun foo(): Int32; }
        fun f[T: X]() { T::foo; }",
        pos(2, 25),
        ErrorMessage::InvalidLeftSideOfSeparator,
    );

    err(
        "trait X { fun foo(): Int32; }
        class SomeClass[T: X]
        impl[T: X] SomeClass[T] {
            fun f() { T::foo; }
        }",
        pos(4, 23),
        ErrorMessage::InvalidLeftSideOfSeparator,
    );
}

#[test]
fn test_type_param_call() {
    err(
        "trait X { fun foo(): Int32; }
        fun f[T: X]() { T(); }",
        pos(2, 25),
        ErrorMessage::ValueExpected,
    );

    err(
        "trait X { fun foo(): Int32; }
        class SomeClass[T: X]
        impl[T: X] SomeClass[T] {
            fun f() { T(); }
        }",
        pos(4, 23),
        ErrorMessage::ValueExpected,
    );
}

#[test]
fn test_static_method_call_with_type_param() {
    err(
        "trait X { @static fun bar(): Int32; }
        fun f[T: X]() { T::foo(); }",
        pos(2, 31),
        ErrorMessage::UnknownStaticMethodWithTypeParam,
    );

    err(
        "trait X { @static fun foo(): Int32; }
        trait Y { @static fun foo(): String; }
        fun f[T: X + Y]() { T::foo(); }",
        pos(3, 35),
        ErrorMessage::MultipleCandidatesForStaticMethodWithTypeParam,
    );

    err(
        "trait X { @static fun foo(): Int32; }
        fun f[T: X](): Int32 { return T::foo(1i32); }",
        pos(2, 45),
        ErrorMessage::ParamTypesIncompatible("foo".into(), Vec::new(), vec!["Int32".into()]),
    );

    ok("trait X { @static fun foo(): Int32; }
        fun f[T: X](): Int32 { return T::foo(); }");
}

#[test]
fn test_type_param_with_let() {
    ok("fun myid[T](val: T): T {
        let tmp: T = val;
        return tmp;
    }");
}

#[test]
fn test_fct_and_class_type_params() {
    ok("
    class A[X]
    impl[X] A[X] {
        fun test[Y]() {}
    }");

    ok("
    class A[X]
    impl[X] A[X] {
        fun t1[Y](x: X, y: Y): Y { return y; }
        fun t2[Y](x: X, y: Y): X { return x; }
    }

    fun t1(a: A[Int32]): String {
        return a.t1[String](1i32, \"bla\");
    }

    fun t2(a: A[Int32]): Int32 {
        return a.t2[String](1i32, \"bla\");
    }
    ");
}

#[test]
fn test_struct() {
    ok("
        struct Foo(f1: Int32)
        fun f(): Foo { Foo(1i32) }
    ");
    err(
        "
        struct Foo(f1: Int32)
        fun f(): Foo { Foo() }",
        pos(3, 27),
        ErrorMessage::StructArgsIncompatible("Foo".into(), vec!["Int32".into()], Vec::new()),
    );
    err(
        "
        struct Foo(f1: Int32)
        fun f(): Foo { Foo(true) }",
        pos(3, 27),
        ErrorMessage::StructArgsIncompatible(
            "Foo".into(),
            vec!["Int32".into()],
            vec!["Bool".into()],
        ),
    );
}

#[test]
fn test_struct_field() {
    ok("
        struct Foo(f1: Int32)
        fun f(x: Foo): Int32 { x.f1 }
    ");

    err(
        "
        struct Foo(f1: Bool)
        fun f(x: Foo): Int32 { x.f1 }
    ",
        pos(3, 30),
        ErrorMessage::ReturnType("Int32".into(), "Bool".into()),
    );

    err(
        "
        struct Foo(f1: Bool)
        fun f(x: Foo): Int32 { x.unknown }
    ",
        pos(3, 33),
        ErrorMessage::UnknownField("unknown".into(), "Foo".into()),
    );
}

#[test]
fn test_struct_field_array() {
    ok("
        struct Foo(f1: Array[Int32])
        fun f(x: Foo): Int32 { x.f1(0) }
    ");
}

#[test]
fn test_struct_with_type_params() {
    ok("
        struct Foo[T](f1: Int32)
        fun f(): Foo[Int32] { Foo[Int32](1i32) }
    ");
    err(
        "
        struct Foo[T](f1: Int32)
        fun f(): Foo[Int32] { Foo(1i32) }
    ",
        pos(3, 34),
        ErrorMessage::WrongNumberTypeParams(1, 0),
    );
    err(
        "
        struct Foo[T](f1: Int32)
        fun f(): Foo[Int32] { Foo[Int32, Bool](1i32) }
    ",
        pos(3, 47),
        ErrorMessage::WrongNumberTypeParams(1, 2),
    );
    err(
        "
        trait MyTrait {}
        struct Foo[T: MyTrait](f1: Int32)
        fun f(): Foo[Int32] { Foo[Int32](1i32) }
    ",
        pos(4, 18),
        ErrorMessage::TypeNotImplementingTrait("Int32".into(), "MyTrait".into()),
    );
    ok("
        trait MyTrait {}
        class Bar
        impl MyTrait for Bar {}
        struct Foo[T: MyTrait](f1: Int32)
        fun f(): Foo[Bar] { Foo[Bar](1i32) }
    ");
    err(
        "
        struct Foo[T](f1: Int32)
        fun f(): Foo[Int32] { Foo[Bool](1i32) }
    ",
        pos(3, 29),
        ErrorMessage::ReturnType("Foo[Int32]".into(), "Foo[Bool]".into()),
    );
    err(
        "
        struct Foo[T](f1: T, f2: Bool)
        fun f[T](val: T): Foo[T] { Foo(val, false) }",
        pos(3, 39),
        ErrorMessage::WrongNumberTypeParams(1, 0),
    );
}

#[test]
fn test_struct_mod() {
    err(
        "
        fun f() { foo::Foo(1i32); }
        mod foo { struct Foo(f1: Int32) }
        ",
        pos(2, 27),
        ErrorMessage::NotAccessible("foo::Foo".into()),
    );
}

#[test]
fn test_struct_with_static_method() {
    ok("
        struct Foo(value: Int32)
        impl Foo {
            @static fun bar() {}
        }
        fun f() {
            Foo::bar();
        }
        ");

    ok("
        struct Foo[T](value: Int32)
        impl[T] Foo[T] {
            @static fun bar() {}
        }
        fun f() {
            Foo[Int32]::bar();
        }
        ");

    err(
        "
            struct Foo(value: Int32)
            fun f() {
                Foo[Int32]::bar();
            }
            ",
        pos(4, 32),
        ErrorMessage::WrongNumberTypeParams(0, 1),
    );
}

#[test]
fn test_enum_with_static_method() {
    ok("
        enum Foo { A, B }
        impl Foo {
            @static fun bar() {}
        }
        fun f() {
            Foo::bar();
        }
        ");

    err(
        "
        enum Foo { A, B }
        fun f() {
            Foo[Int32]::bar();
        }
        ",
        pos(4, 28),
        ErrorMessage::WrongNumberTypeParams(0, 1),
    );
}

#[test]
fn test_enum() {
    ok("enum A { V1, V2 }");
    ok("enum A { V1, V2 } fun f(a: A): A { return a; }");
    ok("enum A { V1, V2 } fun f(): A { return A::V1; }");

    ok("enum A { V1, V2 } fun f(): Bool { return A::V1 == A::V2; }");
    ok("enum A { V1, V2 } fun f(): Bool { return A::V1 != A::V2; }");

    err(
        "enum A { V1 } fun f(): A { A }",
        pos(1, 28),
        ErrorMessage::ValueExpected,
    );

    err(
        "enum A { V1 } fun f() { A = 1; }",
        pos(1, 25),
        ErrorMessage::LvalueExpected,
    );

    err(
        "enum A { V1, V2 } fun f(): A { A::V3 }",
        pos(1, 33),
        ErrorMessage::UnknownEnumVariant("V3".into()),
    );

    err(
        "enum A[T] { V1, V2 } fun f(): A[Int32] { A::V1 }",
        pos(1, 43),
        ErrorMessage::WrongNumberTypeParams(1, 0),
    );

    err(
        "enum A[T] { V1(T), V2 } fun f(): A[Int32] { A[Int32]::V1 }",
        pos(1, 53),
        ErrorMessage::EnumArgsIncompatible("A".into(), "V1".into(), vec!["T".into()], Vec::new()),
    );

    err(
        "
        enum Foo[T] { A(T, Bool), B }
        fun f[T](val: T): Foo[T] { Foo::A[T, String](val, false) }",
        pos(3, 53),
        ErrorMessage::WrongNumberTypeParams(1, 2),
    );

    ok("
        enum Foo[T] { A(T, Bool), B }
        fun f[T](val: T): Foo[T] { Foo::A(val, false) }");
}

#[test]
fn test_enum_match() {
    ok("
        enum A { V1, V2 }
        fun f(x: A): Int32 {
            match x {
                A::V1 => 0i32,
                A::V2 => 1i32
            }
        }
    ");

    err(
        "
        enum A { V1, V2 }
        fun f(x: A): Int32 {
            match x {
                A::V1 => 0i32,
                A::V2 => \"foo\"
            }
        }
    ",
        pos(6, 26),
        ErrorMessage::MatchBranchTypesIncompatible("Int32".into(), "String".into()),
    );
}

#[test]
fn test_enum_match_with_parens() {
    err(
        "
        enum A { V1, V2 }
        fun f(x: A): Int32 {
            match x {
                A::V1() => 0i32,
                A::V2 => 1i32
            }
        }
    ",
        pos(5, 17),
        ErrorMessage::MatchPatternNoParens,
    );
}

#[test]
fn test_enum_match_wrong_number_params() {
    err(
        "
        enum A { V1(Int32), V2 }
        fun f(x: A): Int32 {
            match x {
                A::V1 => 0i32,
                A::V2 => 1i32
            }
        }
    ",
        pos(5, 17),
        ErrorMessage::MatchPatternWrongNumberOfParams(0, 1),
    );

    err(
        "
        enum A { V1(Int32, Float32, Bool), V2 }
        fun f(x: A): Int32 {
            match x {
                A::V1(a, b, c, d) => 0i32,
                A::V2 => 1i32
            }
        }
    ",
        pos(5, 17),
        ErrorMessage::MatchPatternWrongNumberOfParams(4, 3),
    );
}

#[test]
fn test_enum_match_params() {
    ok("
        enum A { V1(Int32, Int32, Int32), V2 }
        fun f(x: A): Int32 {
            match x {
                A::V1(a, _, c) => a + c,
                A::V2 => 1i32
            }
        }
    ");

    err(
        "
        enum A { V1(Int32, Int32, Int32), V2 }
        fun f(x: A): Int32 {
            match x {
                A::V1(a, _, c) => a + c,
                A::V2 => a
            }
        }
    ",
        pos(6, 26),
        ErrorMessage::UnknownIdentifier("a".into()),
    );

    err(
        "
        enum A { V1(Int32, Int32), V2 }
        fun f(x: A): Int32 {
            match x {
                A::V1(a, a) => a + a,
                A::V2 => 1i32
            }
        }
    ",
        pos(5, 26),
        ErrorMessage::VarAlreadyInPattern,
    );
}

#[test]
fn test_enum_match_missing_variants() {
    err(
        "
        enum A { V1(Int32, Int32, Int32), V2, V3 }
        fun f(x: A): Int32 {
            match x {
                A::V1(a, _, c) => a + c,
                A::V2 => 1i32,
            }
        }
    ",
        pos(4, 13),
        ErrorMessage::MatchUncoveredVariant,
    );

    err(
        "
        enum A { V1(Int32, Int32, Int32), V2, V3 }
        fun f(x: A): Int32 {
            match x {
                A::V1(a, _, c) => a + c,
                A::V2 => 1i32,
                A::V3 => 2i32,
                A::V2 => 4i32,
            }
        }
    ",
        pos(8, 17),
        ErrorMessage::MatchUnreachablePattern,
    );
}

#[test]
fn test_enum_match_underscore() {
    ok("
        enum A { V1, V2, V3 }
        fun f(x: A): Bool {
            match x {
                A::V1 => true,
                _ => false,
            }
        }
    ");

    err(
        "
        enum A { V1, V2, V3 }
        fun f(x: A): Bool {
            match x {
                _ => false,
                A::V1 => true,
            }
        }
    ",
        pos(6, 17),
        ErrorMessage::MatchUnreachablePattern,
    );
}

#[test]
fn test_enum_equals() {
    ok("
        enum A { V1, V2 }
        fun f(x: A, y: A): Bool {
            x == y
        }
    ");

    err(
        "
        enum A { V1(Int32), V2 }
        fun f(x: A, y: A): Bool {
            x == y
        }
    ",
        pos(4, 15),
        ErrorMessage::BinOpType("==".into(), "A".into(), "A".into()),
    );
}

#[test]
fn test_use_enum_value() {
    ok("enum A { V1(Int32), V2 } use A::V1; fun f(): A { V1(1i32) }");
    ok("enum A[T] { V1(Int32), V2 } use A::V1; fun f(): A[Int32] { V1[Int32](1i32) }");
    ok("enum A[T] { V1(Int32), V2 } use A::V1; fun f(): A[Int32] { V1(1i32) }");

    ok("enum A { V1, V2 } use A::V2; fun f(): A { V2 }");

    err(
        "enum A { V1(Int32), V2 } use A::V1; fun f(): A { V1 }",
        pos(1, 50),
        ErrorMessage::EnumArgsIncompatible(
            "A".into(),
            "V1".into(),
            vec!["Int32".into()],
            Vec::new(),
        ),
    );

    err(
        "enum A { V1(Int32), V2 } use A::V2; fun f(): A { V2(0i32) }",
        pos(1, 52),
        ErrorMessage::EnumArgsIncompatible(
            "A".into(),
            "V2".into(),
            Vec::new(),
            vec!["Int32".into()],
        ),
    );

    ok("enum A[T] { V1(Int32), V2 } use A::V2; fun f(): A[Int32] { V2 }");

    ok("enum A[T] { V1, V2 } use A::V2; fun f(): A[Int32] { V2[Int32] }");

    err(
        "enum A[T] { V1, V2 } use A::V2; fun f(): A[Int32] { V2[Int32, Float32] }",
        pos(1, 55),
        ErrorMessage::WrongNumberTypeParams(1, 2),
    );
}

#[test]
fn test_enum_value_with_type_param() {
    ok("enum A[T] { V1, V2 } fun f(): A[Int32] { A::V2[Int32] }");
    ok("enum A[T] { V1, V2 } fun f(): A[Int32] { A[Int32]::V2 }");
    err(
        "enum A[T] { V1, V2 } fun f(): A[Int32] { A[Int32]::V2[Int32] }",
        pos(1, 43),
        ErrorMessage::ExpectedSomeIdentifier,
    );
}

#[test]
fn test_block_value() {
    ok("fun f(): Int32 { 1i32 }");
    ok("fun f() { let x = { 1i32 }; }");
    ok("fun g(): Int32 { return 1i32; } fun f() { let x: Int32 = { g() }; }");
    ok("fun g(): Int32 { return 1i32; } fun f() { let x: Int32 = { g(); 1i32 }; }");
}

#[test]
fn test_if_expression() {
    ok("fun f(): Int32 { if true { 1i32 } else { 2i32 } }");
    ok("fun f(): Float32 { if true { 1.0f32 } else { 2.0f32 } }");
    ok("fun f(): Float64 { if true { 1.0 } else { 2.0 } }");

    ok("fun f(): Int32 { 4i32 * if true { 1i32 } else { 2i32 } }");
}

#[test]
fn test_tuple() {
    ok("fun f(a: (Int32, Bool)) {}");
    ok("fun f(a: (Int32, Bool)): (Int32, Bool) { return a; }");
    ok("fun f(a: (Int32, Bool)): (Int32, Bool) {
            let tmp = a;
            return tmp;
        }");
    err(
        "fun f(a: (Int32, Bool)): (Int32) { return a; }",
        pos(1, 36),
        ErrorMessage::ReturnType("(Int32)".into(), "(Int32, Bool)".into()),
    );
    err(
        "fun f(a: (Int32, Bool)): (Int32, Float32) { return a; }",
        pos(1, 45),
        ErrorMessage::ReturnType("(Int32, Float32)".into(), "(Int32, Bool)".into()),
    );
}

#[test]
fn test_tuple_literal() {
    ok("fun f(): (Int32, Bool) {
        return (1i32, false);
    }");

    err(
        "fun f(): (Int32) {
        return (1i32);
    }",
        pos(2, 9),
        ErrorMessage::ReturnType("(Int32)".into(), "Int32".into()),
    );

    err(
        "fun f(): (Int32, Int32) {
        return (1i32, false);
    }",
        pos(2, 9),
        ErrorMessage::ReturnType("(Int32, Int32)".into(), "(Int32, Bool)".into()),
    );
}

#[test]
fn test_tuple_in_call() {
    ok("
        fun f(a: (Int32, Bool)) {}
        fun g() {
            f((1i32, true));
        }
    ")
}

#[test]
fn test_tuple_element() {
    ok("
        fun f(a: (Int32, Bool)): Int32 {
            return a.0;
        }
    ");

    ok("
        fun f(a: (Int32, Bool)): Bool {
            return a.1;
        }
    ");

    err(
        "
        fun f(a: (Int32, Bool)): String {
            return a.1;
        }
    ",
        pos(3, 13),
        ErrorMessage::ReturnType("String".into(), "Bool".into()),
    );
}

#[test]
fn test_type_without_make_iterator() {
    err(
        "
        class Foo
        fun bar(x: Foo) {
            for i in x {
                let x: Foo = i;
            }
        }
    ",
        pos(4, 22),
        ErrorMessage::TypeNotUsableInForIn("Foo".into()),
    );
}

#[test]
fn test_type_make_iterator_not_implementing_iterator() {
    err(
        "
        class Foo
        impl Foo {
            fun makeIterator(): Int32 { 0i32 }
        }
        fun bar(x: Foo) {
            for i in x {
                let x: Foo = i;
            }
        }
    ",
        pos(7, 22),
        ErrorMessage::TypeNotUsableInForIn("Foo".into()),
    );
}

#[test]
fn zero_trait_ok() {
    ok("fun f() { Array[Int32]::zero(12i64); }");
}

#[test]
fn zero_trait_err() {
    err(
        "fun f() { Array[String]::zero(12i64); }",
        pos(1, 30),
        ErrorMessage::UnknownStaticMethod(
            "Array[String]".into(),
            "zero".into(),
            vec!["Int64".into()],
        ),
    );
}

#[test]
fn extension_method_call() {
    ok("
        class Foo(value: Int32)
        impl Foo { fun foo(): Int32 { self.value } }
        fun bar(x: Foo): Int32 { x.foo() }
    ");
}

#[test]
fn extension_class_with_type_param() {
    ok("
        class Foo[T](value: T)
        trait MyTrait {}
        impl[T: MyTrait] Foo[T] { fun foo(): Int32 { 12i32 } }
        impl MyTrait for Int32 {}
        fun bar(x: Foo[Int32]): Int32 { x.foo() }
    ");

    ok("
        class Foo[T](value: T)
        impl Foo[Int32] { fun foo() {} }
        impl Foo[Float32] { fun bar() {} }
        fun f(x: Foo[Int32]) { x.foo() }
        fun g(x: Foo[Float32]) { x.bar() }
    ");

    err(
        "
        class Foo[T](value: T)
        impl Foo[Float32] { fun bar() {} }
        fun f(x: Foo[Int32]) { x.bar() }
    ",
        pos(4, 37),
        ErrorMessage::UnknownMethod("Foo[Int32]".into(), "bar".into(), Vec::new()),
    );
}

#[test]
fn extension_class_tuple() {
    ok("
        class Foo[T](value: T)
        impl Foo[(Int32, Int32)] {
            fun bar() {}
        }
        fun f(x: Foo[(Int32, Int32)]) {
            x.bar();
        }
    ");

    ok("
        class Foo[T]
        impl[T] Foo[(T, Int32)] {
            fun bar() {}
        }
        fun f() {
            Foo[(Int32, Int32)]().bar();
            Foo[(Float32, Int32)]().bar();
        }
    ");

    err(
        "
        class Foo[T]
        impl Foo[(Int32, Float32)] {
            fun bar() {}
        }
        fun f(x: Foo[(Int32, Int32)]) {
            x.bar();
        }
    ",
        pos(7, 18),
        ErrorMessage::UnknownMethod("Foo[(Int32, Int32)]".into(), "bar".into(), Vec::new()),
    );
}

#[test]
fn extension_nested() {
    err(
        "
        class Foo[T]
        impl Foo[Foo[Foo[Int32]]] {
            fun bar() {}
        }
        fun f(value: Foo[Foo[Foo[Int32]]]) {
            value.bar();
        }
        fun g(value: Foo[Foo[Int32]]) {
            value.bar();
        }
    ",
        pos(10, 22),
        ErrorMessage::UnknownMethod("Foo[Foo[Int32]]".into(), "bar".into(), Vec::new()),
    );
}

#[test]
fn extension_bind_type_param_twice() {
    ok("
        class Foo[T]
        impl[T] Foo[(T, T)] {
            fun bar() {}
        }
        fun f(x: Foo[(Int32, Int32)]) {
            x.bar();
        }
    ");

    ok("
        class Foo[T]
        impl[T] Foo[(T, T)] {
            fun bar() {}
        }
        fun f[T](x: Foo[(T, T)]) {
            x.bar();
        }
    ");

    err(
        "
        class Foo[T]
        impl[T] Foo[(T, T)] {
            fun bar() {}
        }
        fun f(x: Foo[(Int32, Float32)]) {
            x.bar();
        }
    ",
        pos(7, 18),
        ErrorMessage::UnknownMethod("Foo[(Int32, Float32)]".into(), "bar".into(), Vec::new()),
    );

    err(
        "
        class Foo[T]
        impl[T] Foo[(T, T)] {
            fun bar() {}
        }
        fun f[T](x: Foo[(T, Float32)]) {
            x.bar();
        }
    ",
        pos(7, 18),
        ErrorMessage::UnknownMethod("Foo[(T, Float32)]".into(), "bar".into(), Vec::new()),
    );
}

#[test]
fn extension_struct_with_type_param() {
    ok("
        struct Foo[T](value: T)
        trait MyTrait {}
        impl[T: MyTrait] Foo[T] { fun foo(): Int32 { 12i32 } }
        impl MyTrait for Int32 {}
        fun bar(x: Foo[Int32]): Int32 { x.foo() }
    ");

    ok("
        struct Foo[T](value: T)
        impl Foo[Int32] { fun foo() {} }
        impl Foo[Float32] { fun bar() {} }
        fun f(x: Foo[Int32]) { x.foo() }
        fun g(x: Foo[Float32]) { x.bar() }
    ");

    err(
        "
        struct Foo[T](value: T)
        impl Foo[Float32] { fun bar() {} }
        fun f(x: Foo[Int32]) { x.bar() }
    ",
        pos(4, 37),
        ErrorMessage::UnknownMethod("Foo[Int32]".into(), "bar".into(), Vec::new()),
    );
}

#[test]
fn extension_enum_with_type_param() {
    ok("
        enum Foo[T] { A(T), B }
        trait MyTrait {}
        impl[T: MyTrait] Foo[T] { fun foo(): Int32 { 12i32 } }
        impl MyTrait for Int32 {}
        fun bar(x: Foo[Int32]): Int32 { x.foo() }
    ");

    ok("
        enum Foo[T] { A(T), B }
        impl Foo[Int32] { fun foo() {} }
        impl Foo[Float32] { fun bar() {} }
        fun f(x: Foo[Int32]) { x.foo() }
        fun g(x: Foo[Float32]) { x.bar() }
    ");

    err(
        "
        enum Foo[T] { A(T), B }
        impl Foo[Float32] { fun bar() {} }
        fun f(x: Foo[Int32]) { x.bar() }
    ",
        pos(4, 37),
        ErrorMessage::UnknownMethod("Foo[Int32]".into(), "bar".into(), Vec::new()),
    );
}

#[test]
fn impl_class_type_params() {
    err(
        "
        trait MyTrait { fun bar(); }
        class Foo[T]
        impl MyTrait for Foo[String] { fun bar() {} }
        fun bar(x: Foo[Int32]) { x.bar(); }
    ",
        pos(5, 39),
        ErrorMessage::UnknownMethod("Foo[Int32]".into(), "bar".into(), Vec::new()),
    );

    ok("
        trait MyTrait { fun bar(); }
        class Foo[T]
        impl MyTrait for Foo[Int32] { fun bar() {} }
        fun bar(x: Foo[Int32]) { x.bar(); }
    ");
}

#[test]
fn extension_with_fct_type_param() {
    ok("
        class MyClass[T]
        class Foo
        impl MyClass[Foo] {
            fun do[T](another: MyClass[T]) {}
        }
        fun f() {
            MyClass[Foo]().do[Int32](MyClass[Int32]());
            MyClass[Foo]().do[Float32](MyClass[Float32]());
        }
    ");
}

#[test]
fn impl_struct_type_params() {
    err(
        "
        trait MyTrait { fun bar(); }
        struct Foo[T](value: T)
        impl MyTrait for Foo[String] { fun bar() {} }
        fun bar(x: Foo[Int32]) { x.bar(); }
    ",
        pos(5, 39),
        ErrorMessage::UnknownMethod("Foo[Int32]".into(), "bar".into(), Vec::new()),
    );

    ok("
        trait MyTrait { fun bar(); }
        struct Foo[T](value: T)
        impl MyTrait for Foo[Int32] { fun bar() {} }
        fun bar(x: Foo[Int32]) { x.bar(); }
    ");
}

#[test]
fn impl_struct_method_with_self() {
    ok("
        struct Foo(value: Int32)
        trait AsInt32 { fun value(): Int32; }
        impl AsInt32 for Foo { fun value(): Int32 { self.value } }
    ");
}

#[test]
fn impl_struct_with_method_overload() {
    ok("
        struct Foo(value: Int32)
        impl Foo {
            fun plus(other: Foo): Foo {
                Foo(self.value + other.value)
            }
        }
        fun f(a: Foo, b: Foo): Foo {
            a + b
        }
    ");
}

#[test]
fn impl_enum_type_params() {
    err(
        "
        trait MyTrait { fun bar(); }
        enum Foo[T] { A(T), B }
        impl MyTrait for Foo[String] { fun bar() {} }
        fun bar(x: Foo[Int32]) { x.bar(); }
    ",
        pos(5, 39),
        ErrorMessage::UnknownMethod("Foo[Int32]".into(), "bar".into(), Vec::new()),
    );

    ok("
        trait MyTrait { fun bar(); }
        enum Foo[T] { A(T), B }
        impl MyTrait for Foo[Int32] { fun bar() {} }
        fun bar(x: Foo[Int32]) { x.bar(); }
    ");
}

#[test]
fn method_call_on_unit() {
    err(
        "fun foo(a: ()) { a.foo(); }",
        pos(1, 23),
        ErrorMessage::UnknownMethod("()".into(), "foo".into(), Vec::new()),
    );
}

#[test]
fn method_on_enum() {
    ok("
        enum MyEnum { A, B }
        impl MyEnum { fun foo() {} }
        fun f(x: MyEnum) { x.foo(); }
    ");
}

#[test]
fn literal_without_suffix_byte() {
    ok("fun f(): UInt8 { 1 }");
    err(
        "fun f(): UInt8 { 256 }",
        pos(1, 18),
        ErrorMessage::NumberOverflow("UInt8".into()),
    );
    ok("fun f() { let x: UInt8 = 1; }");
}

#[test]
fn literal_without_suffix_long() {
    ok("fun f(): Int64 { 1 }");
    ok("fun f() { let x: Int64 = 1; }");
}

#[test]
fn variadic_parameter() {
    ok("
        fun f(x: Int32...): Int64 {
            x.size()
        }
        fun g() {
            f(1i32, 2i32, 3i32, 4i32);
            f();
            f(1i32);
        }
    ");
    err(
        "
        fun f(x: Int32...) {}
        fun g() {
            f(true);
        }
    ",
        pos(4, 14),
        ErrorMessage::ParamTypesIncompatible("f".into(), vec!["Int32".into()], vec!["Bool".into()]),
    );
    ok("
        fun f(x: Int32, y: Int32...) {}
        fun g() {
            f(1i32, 2i32, 3i32, 4i32);
            f(1i32, 2i32);
            f(1i32);
        }
    ");
    err(
        "
        fun f(x: Int32, y: Int32...) {}
        fun g() {
            f();
        }
    ",
        pos(4, 14),
        ErrorMessage::ParamTypesIncompatible(
            "f".into(),
            vec!["Int32".into(), "Int32".into()],
            Vec::new(),
        ),
    );
    err(
        "fun f(x: Int32..., y: Int32) {}",
        pos(1, 20),
        ErrorMessage::VariadicParameterNeedsToBeLast,
    );
}

#[test]
fn for_with_array() {
    ok("fun f(x: Array[Int32]): Int32 {
        let mut result = 0i32;
        for i in x {
            result = result + i;
        }
        result
    }");

    ok("fun f(x: Array[Float32]): Float32 {
        let mut result = 0.0f32;
        for i in x {
            result = result + i;
        }
        result
    }");
}

#[test]
fn for_with_vec() {
    ok("fun f(x: Vec[Int32]): Int32 {
        let mut result = 0i32;
        for i in x.makeIterator() {
            result = result + i;
        }
        result
    }");

    ok("fun f(x: Vec[Int32]): Int32 {
        let mut result = 0i32;
        for i in x {
            result = result + i;
        }
        result
    }");

    ok("fun f(x: Vec[Float32]): Float32 {
        let mut result = 0.0f32;
        for i in x.makeReverseIterator() {
            result = result + i;
        }
        result
    }");

    ok("fun f(x: Vec[Float32]): Float32 {
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
            fun f(x: Bar) {}
        ",
        pos(3, 22),
        ErrorMessage::WrongNumberTypeParams(1, 0),
    );
}

#[test]
fn check_wrong_number_type_params() {
    err(
        "
            fun foo() { bar[Int32](false); }
            fun bar[T](x: T) {}
        ",
        pos(2, 35),
        ErrorMessage::ParamTypesIncompatible("bar".into(), vec!["T".into()], vec!["Bool".into()]),
    );
}

#[test]
fn multiple_functions() {
    ok("fun f() {} fun g() {}");
}

#[test]
fn redefine_function() {
    err(
        "
        fun f() {}
        fun f() {}",
        pos(3, 9),
        ErrorMessage::ShadowFunction("f".into()),
    );
}

#[test]
fn shadow_type_with_function() {
    err(
        "
        class FooBar
        fun FooBar() {}
        ",
        pos(3, 9),
        ErrorMessage::ShadowClass("FooBar".into()),
    );
}

#[test]
fn define_param_name_twice() {
    err(
        "fun test(x: String, x: Int32) {}",
        pos(1, 21),
        ErrorMessage::ShadowParam("x".into()),
    );
}

#[test]
fn show_type_param_with_name() {
    err(
        "fun test[T](T: Int32) {}",
        pos(1, 13),
        ErrorMessage::ShadowTypeParam("T".into()),
    );
}

#[test]
fn shadow_type_with_var() {
    ok("fun test() { let String = 3i32; }");
}

#[test]
fn shadow_function() {
    ok("fun f() { let f = 1i32; }");
    err(
        "fun f() { let f = 1i32; f(); }",
        pos(1, 26),
        ErrorMessage::UnknownMethod("Int32".into(), "get".into(), Vec::new()),
    );
}

#[test]
fn shadow_var() {
    ok("fun f() { let f = 1i32; let f = 2i32; }");
}

#[test]
fn shadow_param() {
    err(
        "fun f(a: Int32, b: Int32, a: String) {}",
        pos(1, 27),
        ErrorMessage::ShadowParam("a".into()),
    );
}

#[test]
fn multiple_params() {
    ok("fun f(a: Int32, b: Int32, c:String) {}");
}

#[test]
fn undefined_variable() {
    err(
        "fun f() { let b = a; }",
        pos(1, 19),
        ErrorMessage::UnknownIdentifier("a".into()),
    );
    err(
        "fun f() { a; }",
        pos(1, 11),
        ErrorMessage::UnknownIdentifier("a".into()),
    );
}

#[test]
fn undefined_function() {
    err(
        "fun f() { foo(); }",
        pos(1, 11),
        ErrorMessage::UnknownIdentifier("foo".into()),
    );
}

#[test]
fn recursive_function_call() {
    ok("fun f() { f(); }");
}

#[test]
fn function_call() {
    ok("fun a() {} fun b() { a(); }");

    // non-forward definition of functions
    ok("fun a() { b(); } fun b() {}");
}

#[test]
fn variable_outside_of_scope() {
    err(
        "fun f(): Int32 { { let a = 1i32; } return a; }",
        pos(1, 43),
        ErrorMessage::UnknownIdentifier("a".into()),
    );

    ok("fun f(): Int32 { let a = 1i32; { let a = 2i32; } return a; }");
}

#[test]
fn const_value() {
    ok("const one: Int32 = 1i32;
        fun f(): Int32 { return one; }");
}

#[test]
fn for_var() {
    ok("fun f() { for i in std::range(0i32, 4i32) { i; } }");
}

#[test]
fn mod_fct_call() {
    err(
        "
        fun f() { foo::g(); }
        mod foo { fun g() {} }
    ",
        pos(2, 25),
        ErrorMessage::NotAccessible("foo::g".into()),
    );

    ok("
        fun f() { foo::g(); }
        mod foo { @pub fun g() {} }
    ");

    ok("
        fun f() { foo::bar::baz(); }
        mod foo {
            @pub mod bar {
                @pub fun baz() {}
            }
        }
    ");

    err(
        "
        fun f() { foo::bar::baz(); }
        mod foo {
            @pub mod bar {
                fun baz() {}
            }
        }
    ",
        pos(2, 32),
        ErrorMessage::NotAccessible("foo::bar::baz".into()),
    );
}

#[test]
fn mod_ctor_call() {
    ok("
        fun f() { foo::Foo(); }
        mod foo { @pub class Foo }
    ");

    err(
        "
        fun f() { foo::Foo(); }
        mod foo { class Foo }
    ",
        pos(2, 27),
        ErrorMessage::NotAccessible("foo::Foo".into()),
    );

    ok("
        fun f() { foo::bar::Foo(); }
        mod foo { @pub mod bar { @pub class Foo } }
    ");

    err(
        "
        fun f() { foo::bar::Foo(); }
        mod foo { @pub mod bar { class Foo } }
    ",
        pos(2, 32),
        ErrorMessage::NotAccessible("foo::bar::Foo".into()),
    );
}

#[test]
fn mod_class_field() {
    err(
        "
        fun f(x: foo::Foo) { let a = x.bar; }
        mod foo { @pub class Foo(bar: Int32) }
    ",
        pos(2, 39),
        ErrorMessage::NotAccessible("bar".into()),
    );

    err(
        "
        fun f(x: foo::Foo) { let a = x.bar(10i64); }
        mod foo { @pub class Foo(bar: Array[Int32]) }
    ",
        pos(2, 43),
        ErrorMessage::NotAccessible("bar".into()),
    );

    err(
        "
        fun f(x: foo::Foo) { x.bar(10i64) = 10i32; }
        mod foo { @pub class Foo(bar: Array[Int32]) }
    ",
        pos(2, 31),
        ErrorMessage::NotAccessible("bar".into()),
    );

    ok("
        fun f(x: foo::Foo) { let a = x.bar; }
        mod foo { @pub class Foo(@pub bar: Int32) }
    ");
}

#[test]
fn mod_class_method() {
    ok("
        fun f(x: foo::Foo) { x.bar(); }
        mod foo {
            @pub class Foo
            impl Foo { @pub fun bar() {} }
        }
    ");

    err(
        "
        fun f(x: foo::Foo) { x.bar(); }
        mod foo {
            @pub class Foo
            impl Foo { fun bar() {} }
        }
    ",
        pos(2, 35),
        ErrorMessage::NotAccessible("foo::Foo#bar".into()),
    );
}

#[test]
fn mod_class_static_method() {
    ok("
        fun f() { foo::Foo::bar(); }
        mod foo {
            @pub class Foo
            impl Foo { @pub @static fun bar() {} }
        }
    ");

    err(
        "
        fun f() { foo::Foo::bar(); }
        mod foo {
            @pub class Foo
            impl Foo { @static fun bar() {} }
        }
    ",
        pos(2, 32),
        ErrorMessage::NotAccessible("foo::Foo::bar".into()),
    );
}

#[test]
fn mod_struct_field() {
    err(
        "
        fun f(x: foo::Foo) { let a = x.bar; }
        mod foo { @pub struct Foo(bar: Int32) }
    ",
        pos(2, 39),
        ErrorMessage::NotAccessible("bar".into()),
    );

    ok("
        fun f(x: foo::Foo) { let a = x.bar(10i64); }
        mod foo { @pub struct Foo(@pub bar: Array[Int32]) }
    ");

    err(
        "
        fun f(x: foo::Foo) { let a = x.bar(10i64); }
        mod foo { @pub struct Foo(bar: Array[Int32]) }
    ",
        pos(2, 43),
        ErrorMessage::NotAccessible("bar".into()),
    );

    err(
        "
        fun f(x: foo::Foo) { x.bar(10i64) = 10i32; }
        mod foo { @pub struct Foo(bar: Array[Int32]) }
    ",
        pos(2, 31),
        ErrorMessage::NotAccessible("bar".into()),
    );

    ok("
        fun f(x: foo::Foo) { let a = x.bar; }
        mod foo { @pub struct Foo(@pub bar: Int32) }
    ");
}

#[test]
fn mod_path_in_type() {
    ok("
        fun f(): foo::Foo { foo::Foo() }
        mod foo { @pub class Foo }
    ");

    err(
        "
        fun f(): bar::Foo { 1i32 }
    ",
        pos(2, 18),
        ErrorMessage::ExpectedModule,
    );

    err(
        "
        fun bar() {}
        fun f(): bar::Foo { 1i32 }
    ",
        pos(3, 18),
        ErrorMessage::ExpectedModule,
    );

    err(
        "
        fun f(): foo::bar::Foo { 1i32 }
        mod foo {}
    ",
        pos(2, 18),
        ErrorMessage::ExpectedModule,
    );
}

#[test]
fn mod_global() {
    ok("
        fun f(): Int32 { foo::x }
        mod foo { @pub let x: Int32 = 1i32; }
    ");

    err(
        "
        fun f(): Int32 { foo::x }
        mod foo { let x: Int32 = 1i32; }
    ",
        pos(2, 29),
        ErrorMessage::NotAccessible("foo::x".into()),
    );
}

#[test]
fn mod_trait() {
    ok("
        mod foo {
            class Foo
            trait Bar { fun f(x: Foo); }
        }
    ");
}

#[test]
fn mod_impl() {
    ok("
        mod foo {
            class Foo
            trait Bar { fun f(x: Foo); }
            class AnotherClass
            impl Bar for AnotherClass {
                fun f(x: Foo) {}
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
                fun foo(x: Bar) {}
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
        fun f() { foo::Foo(1i32); }
        mod foo {
            class Foo(f: Int32)
        }
    ",
        pos(2, 27),
        ErrorMessage::NotAccessible("foo::Foo".into()),
    );

    err(
        "
        fun f() { foo::Foo(1i32); }
        mod foo {
            class Foo(@pub f: Int32)
        }
    ",
        pos(2, 27),
        ErrorMessage::NotAccessible("foo::Foo".into()),
    );

    err(
        "
        fun f() { foo::Foo(1i32); }
        mod foo {
            @pub class Foo(f: Int32)
        }
    ",
        pos(2, 27),
        ErrorMessage::ClassConstructorNotAccessible("foo::Foo".into()),
    );
}

#[test]
fn mod_struct() {
    err(
        "
        fun f() { foo::Foo(1i32); }
        mod foo {
            struct Foo(f: Int32)
        }
    ",
        pos(2, 27),
        ErrorMessage::NotAccessible("foo::Foo".into()),
    );

    err(
        "
        fun f() { foo::Foo(1i32); }
        mod foo {
            struct Foo(@pub f: Int32)
        }
    ",
        pos(2, 27),
        ErrorMessage::NotAccessible("foo::Foo".into()),
    );

    err(
        "
        fun f() { foo::Foo(1i32); }
        mod foo {
            @pub struct Foo(f: Int32)
        }
    ",
        pos(2, 27),
        ErrorMessage::StructConstructorNotAccessible("foo::Foo".into()),
    );

    ok("
        fun f() { foo::Foo(1i32); }
        mod foo {
            @pub struct Foo(@pub f: Int32)
        }
    ");

    ok("
        fun f(value: foo::Foo) {}
        mod foo {
            @pub struct Foo(f: Int32)
        }
    ");

    err(
        "
        fun f(value: foo::Foo) {}
        mod foo {
            struct Foo(f: Int32)
        }
    ",
        pos(2, 22),
        ErrorMessage::NotAccessible("foo::Foo".into()),
    );
}

#[test]
fn mod_const() {
    ok("
        fun f(): Int32 { foo::x }
        mod foo { @pub const x: Int32 = 1i32; }
    ");

    err(
        "
        fun f(): Int32 { foo::x }
        mod foo { const x: Int32 = 1i32; }
    ",
        pos(2, 29),
        ErrorMessage::NotAccessible("foo::x".into()),
    );

    ok("
        fun f(): Int32 { foo::bar::x }
        mod foo { @pub mod bar { @pub const x: Int32 = 1i32; } }
    ");
}

#[test]
fn mod_enum_value() {
    ok("
        fun f() { foo::A; }
        mod foo { @pub enum Foo { A, B } use Foo::A; }
    ");

    err(
        "
        fun f() { foo::A; }
        mod foo { enum Foo { A, B } use Foo::A; }
    ",
        pos(2, 22),
        ErrorMessage::NotAccessible("foo::Foo".into()),
    );

    ok("
        fun f() { foo::bar::A; }
        mod foo { @pub mod bar { @pub enum Foo { A, B } use Foo::A; } }
    ");

    err(
        "
        fun f() { foo::bar::A; }
        mod foo { @pub mod bar { enum Foo { A, B } use Foo::A; } }
    ",
        pos(2, 27),
        ErrorMessage::NotAccessible("foo::bar::Foo".into()),
    );
}

#[test]
fn mod_enum() {
    err(
        "
        fun f() {
            foo::Foo::B;
        }
        mod foo { enum Foo { A(Bar), B } class Bar }
    ",
        pos(3, 21),
        ErrorMessage::NotAccessible("foo::Foo".into()),
    );

    ok("
        fun f() {
            foo::Foo::B;
        }
        mod foo { @pub enum Foo { A, B } }
    ");

    ok("
        fun f() {
            foo::Foo::A(1i32);
        }
        mod foo { @pub enum Foo { A(Int32), B } }
    ");

    err(
        "
        fun f() {
            foo::Foo::A(1i32);
        }
        mod foo { enum Foo { A(Int32), B } }
    ",
        pos(3, 24),
        ErrorMessage::NotAccessible("foo::Foo".into()),
    );
}

#[test]
fn mod_use() {
    ok("
        use foo::bar;
        fun f() { bar(); }
        mod foo { @pub fun bar() {} }
    ");

    ok("
        use foo::bar::baz;
        fun f() { baz(); }
        mod foo { @pub mod bar {
            @pub fun baz() {}
        } }
    ");

    ok("
        use foo::bar as baz;
        fun f() { baz(); }
        mod foo { @pub fun bar() {} }
    ");

    ok("
        use foo::bar;
        fun f(): Int32 { bar }
        mod foo { @pub let bar: Int32 = 10i32; }
    ");

    ok("
        use foo::bar::baz;
        fun f(): Int32 { baz }
        mod foo { @pub mod bar {
            @pub let baz: Int32 = 10i32;
        } }
    ");

    ok("
        use foo::bar;
        fun f(): Int32 { bar }
        mod foo { @pub let bar: Int32 = 10i32; }
    ");
}

#[test]
fn mod_use_class() {
    ok("
        use foo::Bar;
        fun f() { Bar(); }
        mod foo { @pub class Bar }
    ");

    ok("
        use foo::Bar;
        fun f() {
            Bar();
            Bar::baz();
        }
        mod foo {
            @pub class Bar
            impl Bar {
                @pub @static fun baz() {}
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
            fun getfoo(): Foo { Foo() }
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

                fun getfoo(): Foo { Foo() }
            }
        }
    ");

    err("use super::Foo;", pos(1, 5), ErrorMessage::NoSuperModule);
}

#[test]
fn mod_use_self() {
    ok("
        use self::bar::Foo;
        fun getfoo(): Foo { Foo() }
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
        ErrorMessage::UnknownIdentifierInModule("foo::bar".into(), "baz".into()),
    );

    err(
        "
        use foo::bar;
    ",
        pos(2, 13),
        ErrorMessage::UnknownIdentifierInModule("".into(), "foo".into()),
    );

    err(
        "
        use foo::bar;
        mod foo {}
    ",
        pos(2, 18),
        ErrorMessage::UnknownIdentifierInModule("foo".into(), "bar".into()),
    );

    err(
        "
        use foo::bar;
        fun foo() {}
    ",
        pos(2, 13),
        ErrorMessage::ExpectedPath,
    );

    err(
        "
        use foo::bar::baz;
        @pub mod foo { @pub fun bar() {} }
    ",
        pos(2, 18),
        ErrorMessage::ExpectedPath,
    );
}

#[test]
fn mod_inside() {
    ok("
        mod foo { fun f() { g() } fun g() {} }
    ");

    ok("
        mod foo { class Foo fun g(x: Foo) {} }
    ");

    ok("
        fun f(x: foo::Foo) {}
        mod foo { @pub class Foo }
    ");

    err(
        "
        fun f(x: foo::Foo) {}
        mod foo { class Foo }
    ",
        pos(2, 18),
        ErrorMessage::NotAccessible("foo::Foo".into()),
    );
}

#[test]
fn different_fct_call_kinds() {
    ok("fun f() { g(); } fun g() {}");
    ok("fun f() { g[Int32](); } fun g[T]() {}");
    ok("fun f(g: Array[Int32]) { g(0); }");
    err(
        "fun f(g: Array[Int32]) { g[Float32](0); }",
        pos(1, 27),
        ErrorMessage::NoTypeParamsExpected,
    );
    ok("class Foo fun f() { Foo(); }");
    errors(
        "fun f() { 1i32[Int32](); }",
        &[
            (pos(1, 15), ErrorMessage::NoTypeParamsExpected),
            (
                pos(1, 22),
                ErrorMessage::UnknownMethod("Int32".into(), "get".into(), Vec::new()),
            ),
        ],
    );
    ok("enum Foo { A(Int32), B } fun f() { Foo::A(1i32); }");
    ok("enum Foo[T] { A(Int32), B } fun f() { Foo[Int32]::A(1i32); }");
    ok("enum Foo[T] { A(Int32), B } fun f() { Foo::A[Int32](1i32); }");
    err(
        "enum Foo[T] { A(Int32), B } fun f() { Foo[Int32]::A[Int32](1i32); }",
        pos(1, 42),
        ErrorMessage::NoTypeParamsExpected,
    );
    ok("trait MyTrait { @static fun foo(); } fun f[T: MyTrait]() { T::foo(); }");
    ok("class Foo impl Foo { fun bar() {} } fun f(g: Foo) { g.bar(); }");
    ok("class Foo impl Foo { fun bar[T]() {} } fun f(g: Foo) { g.bar[Int32](); }");
}

#[test]
fn trait_object_method_call() {
    ok("
        trait Foo { fun bar(): Int32; }
        fun f(x: Foo): Int32 {
            x.bar()
        }
    ");
}

#[test]
fn trait_object_cast() {
    ok("
        trait Foo { fun bar(): Int32; }
        class Bar
        impl Foo for Bar {
            fun bar(): Int32 { 1i32 }
        }
        fun f(x: Foo): Int32 { x.bar() }
        fun g(): Int32 {
            f(Bar() as Foo)
        }
    ");

    ok("
        trait Foo { fun bar(): Int32; }
        class Bar
        impl Foo for Bar {
            fun bar(): Int32 { 1i32 }
        }
        fun f(): Foo { Bar() as Foo }
    ");

    ok("
        trait Foo { fun bar(): Int32; }
        fun f(x: Foo): Foo {
            let y = x;
            y
        }
    ");

    err(
        "
        trait Foo { fun bar(): Int32; }
        class Bar
        fun f(x: Foo) {}
        fun g() {
            f(Bar() as Foo)
        }
    ",
        pos(6, 21),
        ErrorMessage::TypeNotImplementingTrait("Bar".into(), "Foo".into()),
    );
}

#[test]
fn infer_enum_type() {
    ok("fun f(): Option[Int32] {
        None
    }");

    ok("
        class X {
            a: Option[Int32],
            b: Option[Int32],
        }

        fun f(x: X) {
            x.a = Some(10i32);
            x.b = None;
        }
    ");

    ok("fun f() {
        let mut x: Option[Int32] = None; x = Some(10i32);
        let mut y: Option[Int32] = Some(10i32); y = None;
    }");

    ok("fun f(): Option[Int32] {
        Some(10i32)
    }");
}

#[test]
fn method_call_type_mismatch_with_type_params() {
    err(
        "
        class Foo
        impl Foo {
            fun f(a: String) {}
        }
        fun g[T](foo: Foo, value: T) {
            foo.f(value);
        }
    ",
        pos(7, 18),
        ErrorMessage::ParamTypesIncompatible("f".into(), vec!["String".into()], vec!["T".into()]),
    );
}

#[test]
fn basic_lambda() {
    ok("fun f(foo: (Int32): Int32): Int32 {
        foo(1i32)
    }");

    err(
        "fun f(foo: (Int32): Int32): Bool {
        foo(1i32)
    }",
        pos(1, 34),
        ErrorMessage::ReturnType("Bool".into(), "Int32".into()),
    );

    err(
        "fun f(foo: (Int32, Int32): Int32): Int32 {
        foo(1i32)
    }",
        pos(2, 12),
        ErrorMessage::LambdaParamTypesIncompatible(
            vec!["Int32".into(), "Int32".into()],
            vec!["Int32".into()],
        ),
    );
}

#[test]
fn lambda_body() {
    ok("fun f(): (Int32): Int32 {
        |x: Int32|: Int32 { x }
    }");

    ok("fun f(): (Int32): Int32 {
        |x: Int32|: Int32 { x + 1i32 }
    }");

    err(
        "fun f(): (Int32): Int32 {
        |x: Int32|: Int32 { false }
    }",
        pos(2, 27),
        ErrorMessage::ReturnType("Int32".into(), "Bool".into()),
    );
}

#[test]
fn lambda_closure() {
    ok("fun f() {
        let x: Int32 = 10i32;
        ||: Int32 { x };
    }");

    ok("fun f(x: Int32) {
        ||: Int32 { x };
    }");

    err(
        "fun f() {
        ||: Int32 { x };
        let x: Int32 = 10i32;
    }",
        pos(2, 21),
        ErrorMessage::UnknownIdentifier("x".into()),
    );
}

#[test]
fn internal_class_ctor() {
    err(
        "fun f(): Array[Int32] {
        Array[Int32]()
    }",
        pos(2, 21),
        ErrorMessage::ClassConstructorNotAccessible("std::collections::Array".into()),
    );
}

#[test]
fn internal_struct_ctor() {
    err(
        "fun f() {
        Int32();
    }",
        pos(2, 14),
        ErrorMessage::StructConstructorNotAccessible("std::primitives::Int32".into()),
    );
}

#[test]
fn mutable_param() {
    ok("fun f(mut x: Int64) { x = 10; }");
    err(
        "fun f(x: Int64) { x = 10; }",
        pos(1, 21),
        ErrorMessage::LetReassigned,
    );
}

#[test]
fn self_unavailable_in_lambda() {
    err(
        "fun f() { || { self; }; }",
        pos(1, 16),
        ErrorMessage::ThisUnavailable,
    );
}
