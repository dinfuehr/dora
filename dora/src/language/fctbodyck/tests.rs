use crate::language::error::msg::SemError;
use crate::language::tests::*;
use crate::vm::ConstValue;

#[test]
fn type_method_len() {
    ok("fun f(a: String): Int64 { return a.size(); }");
    ok("fun f(a: String): Int64 { return \"abc\".size(); }");
}

#[test]
fn type_object_field() {
    ok("class Foo(let a:Int32) fun f(x: Foo): Int32 { return x.a; }");
    ok("class Foo(let a:String) fun f(x: Foo): String { return x.a; }");
    err(
        "class Foo(let a:Int32) fun f(x: Foo): Bool { return x.a; }",
        pos(1, 46),
        SemError::ReturnType("Bool".into(), "Int32".into()),
    );
    err(
        "class Foo(let a:Int32) fun f(x: Foo): Int32 { return x.b; }",
        pos(1, 55),
        SemError::UnknownField("b".into(), "Foo".into()),
    );
}

#[test]
fn type_object_set_field() {
    ok("class Foo(var a: Int32) fun f(x: Foo) { x.a = 1; }");
    err(
        "class Foo(var a: Int32) fun f(x: Foo) { x.a = false; }",
        pos(1, 45),
        SemError::AssignField("a".into(), "Foo".into(), "Int32".into(), "Bool".into()),
    );
}

#[test]
fn type_object_field_without_self() {
    err(
        "class Foo(let a: Int32) { fun f(): Int32 { return a; } }",
        pos(1, 51),
        SemError::UnknownIdentifier("a".into()),
    );
    err(
        "class Foo(var a: Int32) { fun set(x: Int32) { a = x; } }",
        pos(1, 47),
        SemError::UnknownIdentifier("a".into()),
    );
}

#[test]
fn type_class_method_call() {
    ok("class Foo {
                fun bar() {}
                fun baz(): Int32 { return 1; }
            }

            fun f(x: Foo) { x.bar(); }
            fun g(x: Foo): Int32 { return x.baz(); }");

    err(
        "class Foo {
                 fun bar(): Int32 { return 0; }
             }

             fun f(x: Foo): String { return x.bar(); }",
        pos(5, 38),
        SemError::ReturnType("String".into(), "Int32".into()),
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
        SemError::ReturnType("Foo[Int32]".into(), "Foo[Int64]".into()),
    );
}

#[test]
fn type_module_method_call() {
    ok("module Foo {
                fun bar() {}
                fun baz(): Int32 { return 1I; }
            }

            fun f2() { Foo::bar(); }
            fun g(): Int32 { return Foo::baz(); }");

    ok("module Foo {
                fun bar[T : std::Equals + std::Hash](t: T): T = t;
            }

            fun foo() { Foo::bar[Int32](1I); }");

    err(
        "module Foo {
                 fun foo(): Bar = Bar();
               }
               class Bar[T]",
        pos(2, 29),
        SemError::WrongNumberTypeParams(1, 0),
    );

    err(
        "module Foo {
                 fun foo[T](): Bar = Bar[T]();
               }
               class Bar[T]",
        pos(2, 32),
        SemError::WrongNumberTypeParams(1, 0),
    );

    ok("module Foo {
                 fun foo[T](): Bar[T] = Bar[T]();
               }
               class Bar[T]

               fun bar() {
                 Foo::foo[Int32]();
               }");

    err(
        "module Foo {
                 fun bar(): Int32 { return 0I; }
             }

             fun f(): String { return Foo::bar(); }",
        pos(5, 32),
        SemError::ReturnType("String".into(), "Int32".into()),
    );
}

#[test]
fn type_method_defined_twice() {
    err(
        "class Foo {
                 fun bar() {}
                 fun bar() {}
             }",
        pos(3, 18),
        SemError::MethodExists("bar".into(), pos(2, 18)),
    );

    err(
        "class Foo {
                 fun bar() {}
                 fun bar(): Int32 {}
             }",
        pos(3, 18),
        SemError::MethodExists("bar".into(), pos(2, 18)),
    );

    err(
        "class Foo {
                 fun bar(a: Int32) {}
                 fun bar(a: Int32): Int32 {}
             }",
        pos(3, 18),
        SemError::MethodExists("bar".into(), pos(2, 18)),
    );

    err(
        "class Foo {
                fun bar(a: Int32) {}
                fun bar(a: String) {}
            }",
        pos(3, 17),
        SemError::MethodExists("bar".into(), pos(2, 17)),
    );
}

#[test]
fn type_self() {
    ok("class Foo { fun me(): Foo { return self; } }");
    err(
        "class Foo fun me() { return self; }",
        pos(1, 29),
        SemError::ThisUnavailable,
    );

    ok("class Foo(let a: Int32, let b: Int32) {
            fun bar(): Int32 { return self.a + self.b; }
        }");

    ok("class Foo(var a: Int32) {
            fun setA(a: Int32) { self.a = a; }
        }");

    ok("class Foo {
            fun zero(): Int32 { return 0I; }
            fun other(): Int32 { return self.zero(); }
        }");

    ok("class Foo {
            fun bar() { self.bar(); }
        }");
}

#[test]
fn type_unknown_method() {
    err(
        "class Foo {
                 fun bar(a: Int32) { }
             }

             fun f(x: Foo) { x.bar(); }",
        pos(5, 35),
        SemError::ParamTypesIncompatible("bar".into(), vec!["Int32".into()], Vec::new()),
    );

    err(
        "class Foo { }
              fun f(x: Foo) { x.bar(1I); }",
        pos(2, 36),
        SemError::UnknownMethod("Foo".into(), "bar".into(), vec!["Int32".into()]),
    );
}

#[test]
fn type_ctor() {
    ok("class Foo fun f(): Foo { return Foo(); }");
    ok("class Foo(let a: Int32) fun f(): Foo { return Foo(1I); }");
    err(
        "class Foo fun f(): Foo { return 1I; }",
        pos(1, 26),
        SemError::ReturnType("Foo".into(), "Int32".into()),
    );
}

#[test]
fn type_def_for_return_type() {
    ok("fun a(): Int32 { return 1I; }");
    err(
        "fun a(): unknown {}",
        pos(1, 10),
        SemError::UnknownIdentifier("unknown".into()),
    );
}

#[test]
fn type_def_for_param() {
    ok("fun a(b: Int32) {}");
    err(
        "fun a(b: foo) {}",
        pos(1, 10),
        SemError::UnknownIdentifier("foo".into()),
    );
}

#[test]
fn type_def_for_var() {
    ok("fun a() { let a : Int32 = 1I; }");
    err(
        "fun a() { let a : test = 1; }",
        pos(1, 19),
        SemError::UnknownIdentifier("test".into()),
    );
}

#[test]
fn type_var_wrong_type_defined() {
    ok("fun f() { let a : Int32 = 1I; }");
    ok("fun f() { let a : Bool = false; }");
    ok("fun f() { let a : String = \"f\"; }");

    err(
        "fun f() { let a : Int32 = true; }",
        pos(1, 11),
        SemError::AssignType("a".into(), "Int32".into(), "Bool".into()),
    );
    err(
        "fun f() { let b : Bool = 2I; }",
        pos(1, 11),
        SemError::AssignType("b".into(), "Bool".into(), "Int32".into()),
    );
}

#[test]
fn type_while() {
    ok("fun x() { while true { } }");
    ok("fun x() { while false { } }");
    err(
        "fun x() { while 2I { } }",
        pos(1, 11),
        SemError::WhileCondType("Int32".into()),
    );
}

#[test]
fn type_if() {
    ok("fun x() { if true { } }");
    ok("fun x() { if false { } }");
    err(
        "fun x() { if 4I { } }",
        pos(1, 11),
        SemError::IfCondType("Int32".into()),
    );
}

#[test]
fn type_return_unit() {
    ok("fun f() { return; }");
    err(
        "fun f() { return 1I; }",
        pos(1, 11),
        SemError::ReturnType("()".into(), "Int32".into()),
    );
}

#[test]
fn type_return() {
    ok("fun f(): Int32 { let a = 1I; return a; }");
    ok("fun f(): Int32 { return 1I; }");
    err(
        "fun f(): Int32 { return; }",
        pos(1, 18),
        SemError::ReturnType("Int32".into(), "()".into()),
    );

    ok("fun f(): Int32 { return 0I; }
            fun g(): Int32 { return f(); }");
    err(
        "fun f() { }
             fun g(): Int32 { return f(); }",
        pos(2, 31),
        SemError::ReturnType("Int32".into(), "()".into()),
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
        SemError::LetPatternExpectedTuple("Bool".into()),
    );

    ok("fun f(value: ()) { let () = value; }");
    err(
        "fun f() { let () = true; }",
        pos(1, 15),
        SemError::LetPatternExpectedTuple("Bool".into()),
    );
    err(
        "fun f() { let (a, b) = (); }",
        pos(1, 15),
        SemError::LetPatternShouldBeUnit,
    );

    err(
        "fun f() { let (a, b) = (true,); }",
        pos(1, 15),
        SemError::LetPatternExpectedTupleWithLength("(Bool)".into(), 1, 2),
    );
    err(
        "fun f() { let () = (true,); }",
        pos(1, 15),
        SemError::LetPatternExpectedTupleWithLength("(Bool)".into(), 1, 0),
    );

    ok("fun f(value: (Int32, (Int32, Int32))): Int32 { let (a, (b, c)) = value; a+b+c }");
}

#[test]
fn type_assign_lvalue() {
    err("fun f() { 1 = 3; }", pos(1, 13), SemError::LvalueExpected);
}

#[test]
fn type_un_op() {
    ok("fun f(a: Int32) { !a; -a; +a; }");
    err(
        "fun f(a: Bool) { -a; }",
        pos(1, 18),
        SemError::UnOpType("-".into(), "Bool".into()),
    );
    err(
        "fun f(a: Bool) { +a; }",
        pos(1, 18),
        SemError::UnOpType("+".into(), "Bool".into()),
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
        SemError::TypesIncompatible("A".into(), "B".into()),
    );
    err(
        "class A class B fun f(a: A, b: B) { b !== a; }",
        pos(1, 39),
        SemError::TypesIncompatible("B".into(), "A".into()),
    );
    err(
        "fun f(a: Bool) { a+a; }",
        pos(1, 19),
        SemError::BinOpType("+".into(), "Bool".into(), "Bool".into()),
    );
    err(
        "fun f(a: Bool) { a^a; }",
        pos(1, 19),
        SemError::BinOpType("^".into(), "Bool".into(), "Bool".into()),
    );
    err(
        "fun f(a: Int32) { a||a; }",
        pos(1, 20),
        SemError::BinOpType("||".into(), "Int32".into(), "Int32".into()),
    );
    err(
        "fun f(a: Int32) { a&&a; }",
        pos(1, 20),
        SemError::BinOpType("&&".into(), "Int32".into(), "Int32".into()),
    );
    err(
        "fun f(a: String) { a-a; }",
        pos(1, 21),
        SemError::BinOpType("-".into(), "String".into(), "String".into()),
    );
    err(
        "fun f(a: String) { a*a; }",
        pos(1, 21),
        SemError::BinOpType("*".into(), "String".into(), "String".into()),
    );
    err(
        "fun f(a: String) { a%a; }",
        pos(1, 21),
        SemError::BinOpType("%".into(), "String".into(), "String".into()),
    );
}

#[test]
fn type_function_return_type() {
    ok("fun foo(): Int32 { return 1I; }\nfun f() { let i: Int32 = foo(); }");
    err(
        "fun foo(): Int32 { return 1I; }\nfun f() { let i: Bool = foo(); }",
        pos(2, 11),
        SemError::AssignType("i".into(), "Bool".into(), "Int32".into()),
    );
}

#[test]
fn type_ident_in_function_params() {
    ok("fun f(a: Int32) {}\nfun g() { let a = 1I; f(a); }");
}

#[test]
fn type_recursive_function_call() {
    ok("fun f(a: Int32) { f(a); }");
}

#[test]
fn type_function_params() {
    ok("fun foo() {}\nfun f() { foo(); }");
    ok("fun foo(a: Int32) {}\nfun f() { foo(1I); }");
    ok("fun foo(a: Int32, b: Bool) {}\nfun f() { foo(1I, true); }");

    err(
        "fun foo() {}\nfun f() { foo(1I); }",
        pos(2, 14),
        SemError::ParamTypesIncompatible("foo".into(), vec![], vec!["Int32".into()]),
    );
    err(
        "fun foo(a: Int32) {}\nfun f() { foo(true); }",
        pos(2, 14),
        SemError::ParamTypesIncompatible("foo".into(), vec!["Int32".into()], vec!["Bool".into()]),
    );
    err(
        "fun foo(a: Int32, b: Bool) {}\nfun f() { foo(1I, 2I); }",
        pos(2, 14),
        SemError::ParamTypesIncompatible(
            "foo".into(),
            vec!["Int32".into(), "Bool".into()],
            vec!["Int32".into(), "Int32".into()],
        ),
    );
}

#[test]
fn type_array() {
    ok("fun f(a: Array[Int32]): Int32 { return a(1L); }");
    err(
        "fun f(a: Array[Int32]): String { return a(1L); }",
        pos(1, 34),
        SemError::ReturnType("String".into(), "Int32".into()),
    );
}

#[test]
fn type_array_assign() {
    err(
        "fun f(a: Array[Int32]): Int32 { return a(3L) = 4I; }",
        pos(1, 33),
        SemError::ReturnType("Int32".into(), "()".into()),
    );
    err(
        "fun f(a: Array[Int32]) { a(3L) = \"b\"; }",
        pos(1, 32),
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
        fun f(a: Foo): Int32 { return a.x(1L); }
    ");
}

#[test]
fn let_without_initialization() {
    err(
        "fun f() { let x: Int32; }",
        pos(1, 11),
        SemError::LetMissingInitialization,
    );
}

#[test]
fn reassign_param() {
    err(
        "fun f(a: Int32) { a = 1; }",
        pos(1, 21),
        SemError::LetReassigned,
    );
}

#[test]
fn reassign_field() {
    ok("class Foo(var x: Int32) fun foo(f: Foo) { f.x = 1; }");
    err(
        "class Foo(let x: Int32) fun foo(f: Foo) { f.x = 1; }",
        pos(1, 47),
        SemError::LetReassigned,
    );
}

#[test]
fn reassign_var() {
    ok("fun f() { var a=1; a=2; }");
}

#[test]
fn reassign_let() {
    err(
        "fun f() { let a=1; a=2; }",
        pos(1, 21),
        SemError::LetReassigned,
    );
}

#[test]
fn reassign_self() {
    err(
        "class Foo {
            fun f() { self = Foo(); }
        }",
        pos(2, 28),
        SemError::LvalueExpected,
    );
}

#[test]
fn super_class() {
    ok("@open class A class B extends A");
    ok("@open class A class B extends A()");
    ok("@open class A(a: Int32) class B extends A(1I)");
    err(
        "@open class A(a: Int32) class B extends A(true)",
        pos(1, 41),
        SemError::UnknownCtor,
    );
}

#[test]
fn access_super_class_field() {
    ok(
        "@open class A(var a: Int32) class B(x: Int32) extends A(x*2I)
            fun foo(b: B) { b.a = b.a + 10I; }",
    );
}

#[test]
fn same_names() {
    ok("class Foo { var Foo: Foo = Foo(); }");
    ok("class Foo fun foo() { let Foo: Int32 = 1I; }");
}

#[test]
fn check_is() {
    ok("@open class A class B extends A
            fun f(a: A): Bool { return a is B; }");
    ok("@open class A class B extends A
            fun f(b: B): Bool { return b is A; }");
    ok("class A
            fun f(a: A): Bool { return a is A; }");
    err(
        "@open class A class B extends A
             fun f(a: A): Bool { return a is String; }",
        pos(2, 43),
        SemError::TypesIncompatible("A".into(), "String".into()),
    );
    err(
        "@open class A class B extends A class C
             fun f(a: A): Bool { return a is C; }",
        pos(2, 43),
        SemError::TypesIncompatible("A".into(), "C".into()),
    );

    ok("@open class A() class B() extends A() fun f(): A { return B(); }");
    ok("@open class A() class B() extends A() fun f() { let a: A = B(); }");
}

#[test]
fn check_as() {
    ok("@open class A class B extends A
            fun f(a: A): B { return a as B; }");
    ok("class A
            fun f(a: A): A { return a as A; }");
    err(
        "@open class A class B extends A
             fun f(a: A): String { return a as String; }",
        pos(2, 45),
        SemError::TypesIncompatible("A".into(), "String".into()),
    );
    err(
        "@open class A class B extends A class C
             fun f(a: A): C { return a as C; }",
        pos(2, 40),
        SemError::TypesIncompatible("A".into(), "C".into()),
    );
}

#[test]
fn check_upcast() {
    ok("@open class A class B extends A
            fun f(b: B): A {
                let a: A = b;
                return a;
                //g(b);
                //return b;
            }

            fun g(a: A) {}");
}

#[test]
fn super_delegation() {
    ok("@open class A { fun f() {} }
            class B extends A { fun g() {} }

            fun foo(b: B) {
                b.f();
                b.g();
            }");
}

#[test]
fn super_method_call() {
    ok("@open class A { @open fun f(): Int32 { return 1I; } }
            class B extends A { @override fun f(): Int32 { return super.f() + 1I; } }");
}

#[test]
fn super_as_normal_expression() {
    err(
        "@open class A { }
            class B extends A { fun me() { let x = super; } }",
        pos(2, 52),
        SemError::SuperNeedsMethodCall,
    );
}

#[test]
fn lit_int64() {
    ok("fun f(): Int64 { return 1L; }");
    ok("fun f(): Int32 { return 1I; }");

    let ret = SemError::ReturnType("Int32".into(), "Int64".into());
    err("fun f(): Int32 { return 1L; }", pos(1, 18), ret);

    ok("fun f(): Int64 { return 1; }");
}

#[test]
fn overload_plus() {
    ok("class A { fun plus(rhs: A): Int32 { return 0; } }
            fun f(): Int32 { return A() + A(); }");
}

#[test]
fn overload_minus() {
    ok("class A { fun minus(rhs: A): Int32 { return 0; } }
            fun f(): Int32 { return A() - A(); }");
}

#[test]
fn overload_times() {
    ok("class A { fun times(rhs: A): Int32 { return 0; } }
            fun f(): Int32 { return A() * A(); }");
}

#[test]
fn overload_div() {
    ok("class A { fun div(rhs: A): Int32 { return 0; } }
            fun f(): Int32 { return A() / A(); }");
}

#[test]
fn overload_mod() {
    ok("class A { fun mod(rhs: A): Int32 { return 0; } }
            fun f(): Int32 { return A() % A(); }");
}

#[test]
fn overload_bitwise_or() {
    ok("class A { fun bitwiseOr(rhs: A): Int32 { return 0; } }
            fun f(): Int32 { return A() | A(); }");
}

#[test]
fn overload_bitwise_and() {
    ok("class A { fun bitwiseAnd(rhs: A): Int32 { return 0I; } }
            fun f(): Int32 { return A() & A(); }");
}

#[test]
fn overload_bitwise_xor() {
    ok("class A { fun bitwiseXor(rhs: A): Int32 { return 0I; } }
            fun f(): Int32 { return A() ^ A(); }");
}

#[test]
fn overload_shl() {
    ok("class A { fun shiftLeft(rhs: A): Int32 { return 0I; } }
            fun f(): Int32 { return A() << A(); }");
}

#[test]
fn overload_sar() {
    ok(
        "class A { fun shiftRightSigned(rhs: A): Int32 { return 0I; } }
            fun f(): Int32 { return A() >> A(); }",
    );
}

#[test]
fn overload_shr() {
    ok("class A { fun shiftRight(rhs: A): Int32 { return 0I; } }
            fun f(): Int32 { return A() >>> A(); }");
}

#[test]
fn overload_equals() {
    ok("class A { fun equals(rhs: A): Bool { return true; } }
            fun f1(): Bool { return A() == A(); }
            fun f2(): Bool { return A() != A(); }");
}

#[test]
fn overload_compare_to() {
    ok("class A { fun compareTo(rhs: A): Int32 { return 0; } }
            fun f1(): Bool { return A() < A(); }
            fun f2(): Bool { return A() <= A(); }
            fun f3(): Bool { return A() > A(); }
            fun f4(): Bool { return A() >= A(); }");
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
    ok("fun f(a: Int64, b: Int32): Int64 { return a << b; }");
    ok("fun f(a: Int64, b: Int32): Int64 { return a >> b; }");
    ok("fun f(a: Int64, b: Int32): Int64 { return a >>> b; }");
    ok("fun f(a: Int64, b: Int64): Bool { return a == b; }");
    ok("fun f(a: Int64, b: Int64): Bool { return a != b; }");
    ok("fun f(a: Int64, b: Int64): Bool { return a === b; }");
    ok("fun f(a: Int64, b: Int64): Bool { return a !== b; }");
    ok("fun f(a: Int64, b: Int64): Bool { return a < b; }");
    ok("fun f(a: Int64, b: Int64): Bool { return a <= b; }");
    ok("fun f(a: Int64, b: Int64): Bool { return a > b; }");
    ok("fun f(a: Int64, b: Int64): Bool { return a >= b; }");
    ok("fun f(a: Int64): Int64 { return !a; }");
    ok("fun f(a: Int64): Int64 { return -a; }");
    ok("fun f(a: Int64): Int64 { return +a; }");
}

#[test]
fn test_literal_int_overflow() {
    err(
        "fun f() { let x = 2147483648I; }",
        pos(1, 19),
        SemError::NumberOverflow("Int32".into()),
    );
    ok("fun f() { let x = 2147483647I; }");
    err(
        "fun f() { let x = -2147483649I; }",
        pos(1, 20),
        SemError::NumberOverflow("Int32".into()),
    );
    ok("fun f() { let x = -2147483648I; }");
}

#[test]
fn test_literal_hex_int_overflow() {
    err(
        "fun f() { let x = 0x1_FF_FF_FF_FFI; }",
        pos(1, 19),
        SemError::NumberOverflow("Int32".into()),
    );
    ok("fun f() { let x: Int32 = 0xFF_FF_FF_FFI; }");
}

#[test]
fn test_literal_bin_int_overflow() {
    err(
        "fun f() { let x = 0b1_11111111_11111111_11111111_11111111I; }",
        pos(1, 19),
        SemError::NumberOverflow("Int32".into()),
    );
    ok("fun f() { let x: Int32 = 0b11111111_11111111_11111111_11111111I; }");
}

#[test]
fn test_literal_int64_overflow() {
    err(
        "fun f() { let x = 9223372036854775808L; }",
        pos(1, 19),
        SemError::NumberOverflow("Int64".into()),
    );
    ok("fun f() { let x = 9223372036854775807L; }");
    err(
        "fun f() { let x = -9223372036854775809L; }",
        pos(1, 20),
        SemError::NumberOverflow("Int64".into()),
    );
    ok("fun f() { let x = -9223372036854775808L; }");
}

#[test]
fn test_literal_float_overflow() {
    err(
        "fun f() { let x = -340282350000000000000000000000000000000F; }",
        pos(1, 20),
        SemError::NumberOverflow("Float32".into()),
    );
    ok("fun f() { let x = -340282340000000000000000000000000000000F; }");
    err(
        "fun f() { let x = 340282350000000000000000000000000000001F; }",
        pos(1, 19),
        SemError::NumberOverflow("Float32".into()),
    );
    ok("fun f() { let x = 340282340000000000000000000000000000000F; }");
}

#[test]
fn test_char() {
    ok("fun foo(): Char { return 'c'; }");
    ok("fun foo(a: Char): Char { return a; }");
    err(
        "fun foo(): Char { return false; }",
        pos(1, 19),
        SemError::ReturnType("Char".into(), "Bool".into()),
    );
    err(
        "fun foo(): Char { return 10I; }",
        pos(1, 19),
        SemError::ReturnType("Char".into(), "Int32".into()),
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
        SemError::WrongNumberTypeParams(1, 2),
    );

    err(
        "class A[T]
            fun foo() {
                let a = A();
            }",
        pos(3, 26),
        SemError::WrongNumberTypeParams(1, 0),
    );

    err(
        "class A
            fun foo() {
                let a = A[Int32]();
            }",
        pos(3, 33),
        SemError::WrongNumberTypeParams(0, 1),
    );
}

#[test]
fn test_invoke_static_method_as_instance_method() {
    err(
        "class A {
                @static fun foo() {}
                fun test() { self.foo(); }
            }",
        pos(3, 38),
        SemError::UnknownMethod("A".into(), "foo".into(), vec![]),
    );
}

#[test]
fn test_invoke_method_as_static() {
    err(
        "class A {
                fun foo() {}
                @static fun test() { A::foo(); }
            }",
        pos(3, 44),
        SemError::UnknownStaticMethod("A".into(), "foo".into(), vec![]),
    );
}

#[test]
fn test_fct_with_type_params() {
    err(
        "fun f() {} fun g() { f[Int32](); }",
        pos(1, 30),
        SemError::WrongNumberTypeParams(0, 1),
    );
    err(
        "fun f[T]() {} fun g() { f(); }",
        pos(1, 26),
        SemError::WrongNumberTypeParams(1, 0),
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
        SemError::TypeNotImplementingTrait("T".into(), "MyTrait".into()),
    );

    err(
        "
            trait MyTraitA {}
            trait MyTraitB {}
            class Foo[T: MyTraitA + MyTraitB]
            fun bar[T: MyTraitA](arg: Foo[T]) {}
        ",
        pos(5, 39),
        SemError::TypeNotImplementingTrait("T".into(), "MyTraitB".into()),
    );

    err(
        "
            trait MyTraitA {}
            trait MyTraitB {}
            class Foo[T: MyTraitA + MyTraitB]
            class Baz[X] {
                fun bar[T: MyTraitA](arg: Foo[T]) {}
            }
        ",
        pos(6, 43),
        SemError::TypeNotImplementingTrait("T".into(), "MyTraitB".into()),
    );
}

#[test]
fn test_const_check() {
    err(
        "const one: Int32 = 1I;
            fun f(): Int64 { return one; }",
        pos(2, 30),
        SemError::ReturnType("Int64".into(), "Int32".into()),
    );

    err(
        "const one: Int32 = 1I;
            fun f() { let x: String = one; }",
        pos(2, 23),
        SemError::AssignType("x".into(), "String".into(), "Int32".into()),
    );
}

#[test]
fn test_const_values() {
    ok_with_test(
        "  const yes: Bool = true;
                        const x: UInt8 = 255Y;
                        const a: Int32 = 100I;
                        const b: Int64 = 200L;
                        const c: Char = 'A';
                        const d: Float32 = 3.0F;
                        const e: Float64 = 6.0;",
        |sa| {
            {
                let id = sa.const_by_name("yes");
                let xconst = sa.consts.idx(id);
                let xconst = xconst.read();
                assert_eq!(ConstValue::Bool(true), xconst.value);
            }

            {
                let id = sa.const_by_name("x");
                let xconst = sa.consts.idx(id);
                let xconst = xconst.read();
                assert_eq!(ConstValue::Int(255), xconst.value);
            }

            {
                let id = sa.const_by_name("a");
                let xconst = sa.consts.idx(id);
                let xconst = xconst.read();
                assert_eq!(ConstValue::Int(100), xconst.value);
            }

            {
                let id = sa.const_by_name("b");
                let xconst = sa.consts.idx(id);
                let xconst = xconst.read();
                assert_eq!(ConstValue::Int(200), xconst.value);
            }

            {
                let id = sa.const_by_name("c");
                let xconst = sa.consts.idx(id);
                let xconst = xconst.read();
                assert_eq!(ConstValue::Char('A'), xconst.value);
            }

            {
                let id = sa.const_by_name("d");
                let xconst = sa.consts.idx(id);
                let xconst = xconst.read();
                assert_eq!(ConstValue::Float(3.0), xconst.value);
            }

            {
                let id = sa.const_by_name("e");
                let xconst = sa.consts.idx(id);
                let xconst = xconst.read();
                assert_eq!(ConstValue::Float(6.0), xconst.value);
            }
        },
    );
}

#[test]
fn test_assignment_to_const() {
    err(
        "const one: Int32 = 1I;
            fun f() { one = 2I; }",
        pos(2, 23),
        SemError::LvalueExpected,
    );
}

#[test]
fn test_unary_minus_byte() {
    err(
        "const m1: UInt8 = -1Y;",
        pos(1, 19),
        SemError::UnOpType("-".into(), "UInt8".into()),
    );
    ok("const m1: Int32 = -1I;");
    ok("const m1: Int64 = -1L;");
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
        SemError::TypeNotImplementingTrait("X".into(), "Foo".into()),
    );

    err(
        "trait Foo {}
            fun f[T: Foo]() {}
            fun t() { f[Int32](); }",
        pos(3, 31),
        SemError::TypeNotImplementingTrait("Int32".into(), "Foo".into()),
    );
}

#[test]
fn test_operator_on_generic_type() {
    err(
        "fun f[T](a: T, b: T) { a + b; }",
        pos(1, 26),
        SemError::BinOpType("+".into(), "T".into(), "T".into()),
    );
}

#[test]
fn test_find_class_method_precedence() {
    // finding class method should have precedence over
    // trait methods
    ok("class A { fun foo() {} }
            trait Foo { fun foo(); }
            impl Foo for A { fun foo() {} }
            fun test(a: A) { a.foo(); }");

    err(
        "class A { fun foo() {} }
            trait Foo { fun foo(a: Int32); }
            impl Foo for A { fun foo(a: Int32) {} }
            fun test(a: A) { a.foo(1I); }",
        pos(4, 35),
        SemError::ParamTypesIncompatible("foo".into(), Vec::new(), vec!["Int32".into()]),
    );

    ok("class A { @static fun foo() {} }
            trait Foo { fun foo(a: Int32); }
            impl Foo for A { fun foo(a:  Int32) {} }
            fun test(a: A) { a.foo(1I); }");
}

#[test]
fn test_invoke_abstract_class_ctor() {
    err(
        "@abstract class A
            fun test(): A { return A(); }",
        pos(2, 37),
        SemError::NewAbstractClass,
    );
}

#[test]
fn test_global_get() {
    ok("var x: Int32 = 0I; fun foo(): Int32 { return x; }");
}

#[test]
fn test_global_set() {
    ok("var x: Int32 = 0I; fun foo(a: Int32) { x = a; }");
    err(
        "let x: Int32 = 0I; fun foo(a: Int32) { x = a; }",
        pos(1, 42),
        SemError::LetReassigned,
    );
}

#[test]
fn lambda_assignment() {
    ok("fun f() { let x = || {}; }");
    ok("fun f() { let x = || -> Int32 { return 2I; }; }");
    ok("fun f() { let x: () -> () = || {}; }");
    ok("fun f() { let x: () -> () = || -> () {}; }");
    ok("fun f() { let x: () -> Int32 = || -> Int32 { return 2I; }; }");
    err(
        "fun f() { let x: () -> Int32 = || {}; }",
        pos(1, 11),
        SemError::AssignType("x".into(), "() -> Int32".into(), "() -> ()".into()),
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
        SemError::MultipleCandidatesForMethod("A".into(), "f".into(), Vec::new()),
    );
}

#[test]
fn generic_trait_method_call() {
    ok("trait Foo { fun bar(); }
            fun f[T: Foo](t: T) { t.bar(); }");
    ok("trait Foo { fun bar(); }
            class A[T: Foo](let t: T) {
                fun baz() { self.t.bar(); }
            }");
}

#[test]
fn test_generic_ctor_without_type_params() {
    err(
        "class Foo[A, B]()
            fun test() { Foo(); }",
        pos(2, 29),
        SemError::WrongNumberTypeParams(2, 0),
    );
}

#[test]
fn test_generic_argument_with_trait_bound() {
    err(
        "fun f[X: std::Comparable](x: X) {}
            fun g[T](t: T) { f[T](t); }",
        pos(2, 34),
        SemError::TypeNotImplementingTrait("T".into(), "Comparable".into()),
    );
}

#[test]
fn test_for_supports_make_iterator() {
    err(
        "fun f() { for i in 1I {} }",
        pos(1, 20),
        SemError::TypeNotUsableInForIn("Int32".into()),
    );

    err(
        "
            class Foo() { fun makeIterator(): Bool { return true; } }
            fun f() { for i in Foo() {} }",
        pos(3, 35),
        SemError::TypeNotUsableInForIn("Foo".into()),
    );

    ok(
        "class Foo { fun makeIterator(): FooIter { return FooIter(); } }
            class FooIter
            impl std::Iterator for FooIter {
                fun hasNext(): Bool { return false; }
                fun next(): Int32 { return 0; }
            }
            fun f(): Int32 { for i in Foo() { return i; } return 0I; }",
    );
}

#[test]
fn test_ctor_with_type_param() {
    err(
        "
            class Foo[T] {
                fun foo(a: Int32) {
                    Bar[T](a);
                }
            }

            class Bar[T](a: T)
            ",
        pos(4, 27),
        SemError::ParamTypesIncompatible("Bar".into(), vec!["T".into()], vec!["Int32".into()]),
    );
}

#[test]
fn test_fct_used_as_identifier() {
    err(
        "fun foo() {} fun bar() { foo; }",
        pos(1, 26),
        SemError::ValueExpected,
    );
}

#[test]
fn test_cls_used_as_identifier() {
    err(
        "class X fun f() { X; }",
        pos(1, 19),
        SemError::ValueExpected,
    );
}

#[test]
fn test_assign_fct() {
    err(
        "fun foo() {} fun bar() { foo = 1I; }",
        pos(1, 26),
        SemError::LvalueExpected,
    );
}

#[test]
fn test_assign_class() {
    err(
        "class X fun foo() { X = 2I; }",
        pos(1, 21),
        SemError::LvalueExpected,
    );
}

#[test]
fn test_new_call_fct() {
    ok("fun g() {} fun f() { g(); }");
}

#[test]
fn test_new_call_fct_wrong_params() {
    err(
        "fun g() {} fun f() { g(1I); }",
        pos(1, 23),
        SemError::ParamTypesIncompatible("g".into(), Vec::new(), vec!["Int32".into()]),
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
        SemError::WrongNumberTypeParams(0, 1),
    );
}

#[test]
fn test_new_call_static_method() {
    ok("class Foo { @static fun bar() {} }
            fun f() { Foo::bar(); }");
}

#[test]
fn test_new_call_static_method_wrong_params() {
    err(
        "class Foo { @static fun bar() {} }
            fun f() { Foo::bar(1I); }",
        pos(2, 31),
        SemError::ParamTypesIncompatible("bar".into(), Vec::new(), vec!["Int32".into()]),
    );
}

#[test]
fn test_new_call_static_method_type_params() {
    ok("class Foo { @static fun bar[T]() {} }
            fun f() { Foo::bar[Int32](); }");
}

#[test]
fn test_new_call_class() {
    ok("class X fun f() { X(); }");
}

#[test]
fn test_new_call_class_wrong_params() {
    err(
        "class X fun f() { X(1I); }",
        pos(1, 20),
        SemError::ParamTypesIncompatible("X".into(), Vec::new(), vec!["Int32".into()]),
    );
}

#[test]
fn test_new_call_class_with_type_params() {
    ok("class X[T] fun f() { X[Int32](); }");
}

#[test]
fn test_new_call_class_with_wrong_type_params() {
    err(
        "class X fun f() { X[Int32](); }",
        pos(1, 27),
        SemError::WrongNumberTypeParams(0, 1),
    );
}

#[test]
fn test_new_call_method() {
    ok("class X { fun f() {} }
            fun f(x: X) { x.f(); }");
}

#[test]
fn test_new_call_method_type_param() {
    ok("class X { fun f[T]() {} }
            fun f(x: X) { x.f[Int32](); }");
}

#[test]
fn test_new_call_method_wrong_params() {
    err(
        "class X { fun f() {} } fun f(x: X) { x.f(1I); }",
        pos(1, 41),
        SemError::ParamTypesIncompatible("f".into(), Vec::new(), vec!["Int32".into()]),
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
        SemError::UnknownMethodForTypeParam("T".into(), "hash".into(), Vec::new()),
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
        SemError::MultipleCandidatesForTypeParam("T".into(), "id".into(), Vec::new()),
    );
}

#[test]
fn test_array_syntax_get() {
    ok("fun f(t: Array[Int32]): Int32 { return t(0L); }");
}

#[test]
fn test_array_syntax_set() {
    ok("fun f(t: Array[Int32]){ t(0L) = 10I; }");
}

#[test]
fn test_array_syntax_set_wrong_value() {
    err(
        "fun f(t: Array[Int32]){ t(0L) = true; }",
        pos(1, 31),
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
        "fun f(t: Array[Int32]){ t(\"bla\") = 9I; }",
        pos(1, 34),
        SemError::UnknownMethod(
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
        "class Foo fun f(x: Foo): String { return \"x = ${x}\"; }",
        pos(1, 49),
        SemError::ExpectedStringable("Foo".into()),
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
        SemError::ReturnType("String".into(), "()".into()),
    );
}

#[test]
fn test_type_param_used_as_value() {
    err(
        "fun f[T](): Int32 { return T; }",
        pos(1, 28),
        SemError::ValueExpected,
    );

    err(
        "class SomeClass[T] {
            fun f(): Int32 { return T; }
        }",
        pos(2, 37),
        SemError::ValueExpected,
    );
}

#[test]
fn test_assign_to_type_param() {
    err(
        "fun f[T]() { T = 10; }",
        pos(1, 14),
        SemError::LvalueExpected,
    );

    err(
        "class SomeClass[T] {
            fun f() { T = 10; }
        }",
        pos(2, 23),
        SemError::LvalueExpected,
    );
}

#[test]
fn test_type_param_with_name_but_no_call() {
    err(
        "trait X { fun foo(): Int32; }
        fun f[T: X]() { T::foo; }",
        pos(2, 25),
        SemError::InvalidLeftSideOfSeparator,
    );

    err(
        "trait X { fun foo(): Int32; }
        class SomeClass[T: X] {
            fun f() { T::foo; }
        }",
        pos(3, 23),
        SemError::InvalidLeftSideOfSeparator,
    );
}

#[test]
fn test_type_param_call() {
    err(
        "trait X { fun foo(): Int32; }
        fun f[T: X]() { T(); }",
        pos(2, 25),
        SemError::ValueExpected,
    );

    err(
        "trait X { fun foo(): Int32; }
        class SomeClass[T: X] {
            fun f() { T(); }
        }",
        pos(3, 23),
        SemError::ValueExpected,
    );
}

#[test]
fn test_static_method_call_with_type_param() {
    err(
        "trait X { @static fun bar(): Int32; }
        fun f[T: X]() { T::foo(); }",
        pos(2, 31),
        SemError::UnknownStaticMethodWithTypeParam,
    );

    err(
        "trait X { @static fun foo(): Int32; }
        trait Y { @static fun foo(): String; }
        fun f[T: X + Y]() { T::foo(); }",
        pos(3, 35),
        SemError::MultipleCandidatesForStaticMethodWithTypeParam,
    );

    err(
        "trait X { @static fun foo(): Int32; }
        fun f[T: X](): Int32 { return T::foo(1I); }",
        pos(2, 45),
        SemError::ParamTypesIncompatible("foo".into(), Vec::new(), vec!["Int32".into()]),
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
    ok("class A[X] {
        fun test[Y]() {}
    }");

    ok("class A[X] {
        fun t1[Y](x: X, y: Y): Y { return y; }
        fun t2[Y](x: X, y: Y): X { return x; }
    }

    fun t1(a: A[Int32]): String {
        return a.t1[String](1I, \"bla\");
    }

    fun t2(a: A[Int32]): Int32 {
        return a.t2[String](1I, \"bla\");
    }
    ");
}

#[test]
fn test_subtyping() {
    ok("
    @open class A class B extends A
    class Test {
        fun foo(a: A) {}
    }
    fun bar(t: Test) { t.foo(B()); }
    ");
}

#[test]
fn test_struct() {
    ok("
        struct Foo(f1: Int32)
        fun f(): Foo { Foo(1I) }
    ");
    err(
        "
        struct Foo(f1: Int32)
        fun f(): Foo { Foo() }",
        pos(3, 27),
        SemError::StructArgsIncompatible("Foo".into(), vec!["Int32".into()], Vec::new()),
    );
    err(
        "
        struct Foo(f1: Int32)
        fun f(): Foo { Foo(true) }",
        pos(3, 27),
        SemError::StructArgsIncompatible("Foo".into(), vec!["Int32".into()], vec!["Bool".into()]),
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
        SemError::ReturnType("Int32".into(), "Bool".into()),
    );

    err(
        "
        struct Foo(f1: Bool)
        fun f(x: Foo): Int32 { x.unknown }
    ",
        pos(3, 33),
        SemError::UnknownField("unknown".into(), "Foo".into()),
    );
}

#[test]
fn test_struct_field_array() {
    ok("
        struct Foo(f1: Array[Int32])
        fun f(x: Foo): Int32 { x.f1(0L) }
    ");
}

#[test]
fn test_struct_with_type_params() {
    ok("
        struct Foo[T](f1: Int32)
        fun f(): Foo[Int32] { Foo[Int32](1I) }
    ");
    err(
        "
        struct Foo[T](f1: Int32)
        fun f(): Foo[Int32] { Foo(1I) }
    ",
        pos(3, 34),
        SemError::WrongNumberTypeParams(1, 0),
    );
    err(
        "
        struct Foo[T](f1: Int32)
        fun f(): Foo[Int32] { Foo[Int32, Bool](1I) }
    ",
        pos(3, 47),
        SemError::WrongNumberTypeParams(1, 2),
    );
    err(
        "
        trait MyTrait {}
        struct Foo[T: MyTrait](f1: Int32)
        fun f(): Foo[Int32] { Foo[Int32](1I) }
    ",
        pos(4, 18),
        SemError::TypeNotImplementingTrait("Int32".into(), "MyTrait".into()),
    );
    ok("
        trait MyTrait {}
        class Bar
        impl MyTrait for Bar {}
        struct Foo[T: MyTrait](f1: Int32)
        fun f(): Foo[Bar] { Foo[Bar](1I) }
    ");
    err(
        "
        struct Foo[T](f1: Int32)
        fun f(): Foo[Int32] { Foo[Bool](1I) }
    ",
        pos(3, 29),
        SemError::ReturnType("Foo[Int32]".into(), "Foo[Bool]".into()),
    );
    err(
        "
        struct Foo[T](f1: T, f2: Bool)
        fun f[T](val: T): Foo[T] { Foo(val, false) }",
        pos(3, 39),
        SemError::WrongNumberTypeParams(1, 0),
    );
}

#[test]
fn test_struct_namespace() {
    err(
        "
        fun f() { foo::Foo(1I); }
        namespace foo { struct Foo(f1: Int32) }
        ",
        pos(2, 27),
        SemError::NotAccessible("foo::Foo".into()),
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
        SemError::WrongNumberTypeParams(0, 1),
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
        SemError::WrongNumberTypeParams(0, 1),
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
        SemError::ValueExpected,
    );

    err(
        "enum A { V1 } fun f() { A = 1; }",
        pos(1, 25),
        SemError::LvalueExpected,
    );

    err(
        "enum A { V1, V2 } fun f(): A { A::V3 }",
        pos(1, 33),
        SemError::UnknownEnumValue("V3".into()),
    );

    err(
        "enum A[T] { V1, V2 } fun f(): A[Int32] { A::V1 }",
        pos(1, 43),
        SemError::WrongNumberTypeParams(1, 0),
    );

    err(
        "enum A[T] { V1(T), V2 } fun f(): A[Int32] { A[Int32]::V1 }",
        pos(1, 53),
        SemError::EnumArgsIncompatible("A".into(), "V1".into(), vec!["T".into()], Vec::new()),
    );

    err(
        "
        enum Foo[T] { A(T, Bool), B }
        fun f[T](val: T): Foo[T] { Foo::A[T, String](val, false) }",
        pos(3, 53),
        SemError::WrongNumberTypeParams(1, 2),
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
                A::V1 => 0I,
                A::V2 => 1I
            }
        }
    ");

    err(
        "
        enum A { V1, V2 }
        fun f(x: A): Int32 {
            match x {
                A::V1 => 0I,
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
        fun f(x: A): Int32 {
            match x {
                A::V1() => 0I,
                A::V2 => 1I
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
        fun f(x: A): Int32 {
            match x {
                A::V1 => 0I,
                A::V2 => 1I
            }
        }
    ",
        pos(5, 17),
        SemError::MatchPatternWrongNumberOfParams(0, 1),
    );

    err(
        "
        enum A { V1(Int32, Float32, Bool), V2 }
        fun f(x: A): Int32 {
            match x {
                A::V1(a, b, c, d) => 0I,
                A::V2 => 1I
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
        fun f(x: A): Int32 {
            match x {
                A::V1(a, _, c) => a + c,
                A::V2 => 1I
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
        SemError::UnknownIdentifier("a".into()),
    );

    err(
        "
        enum A { V1(Int32, Int32), V2 }
        fun f(x: A): Int32 {
            match x {
                A::V1(a, a) => a + a,
                A::V2 => 1I
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
        fun f(x: A): Int32 {
            match x {
                A::V1(a, _, c) => a + c,
                A::V2 => 1I,
            }
        }
    ",
        pos(4, 13),
        SemError::MatchUncoveredVariant,
    );

    err(
        "
        enum A { V1(Int32, Int32, Int32), V2, V3 }
        fun f(x: A): Int32 {
            match x {
                A::V1(a, _, c) => a + c,
                A::V2 => 1I,
                A::V3 => 2I,
                A::V2 => 4I,
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
        SemError::MatchUnreachablePattern,
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
        SemError::BinOpType("==".into(), "A".into(), "A".into()),
    );
}

#[test]
fn test_import_enum_value() {
    ok("enum A { V1(Int32), V2 } import A::V1; fun f(): A { V1(1I) }");
    ok("enum A[T] { V1(Int32), V2 } import A::V1; fun f(): A[Int32] { V1[Int32](1I) }");
    ok("enum A[T] { V1(Int32), V2 } import A::V1; fun f(): A[Int32] { V1(1I) }");

    ok("enum A { V1, V2 } import A::V2; fun f(): A { V2 }");

    err(
        "enum A { V1(Int32), V2 } import A::V1; fun f(): A { V1 }",
        pos(1, 53),
        SemError::EnumArgsIncompatible("A".into(), "V1".into(), vec!["Int32".into()], Vec::new()),
    );

    err(
        "enum A { V1(Int32), V2 } import A::V2; fun f(): A { V2(0I) }",
        pos(1, 55),
        SemError::EnumArgsIncompatible("A".into(), "V2".into(), Vec::new(), vec!["Int32".into()]),
    );

    ok("enum A[T] { V1(Int32), V2 } import A::V2; fun f(): A[Int32] { V2 }");

    ok("enum A[T] { V1, V2 } import A::V2; fun f(): A[Int32] { V2[Int32] }");

    err(
        "enum A[T] { V1, V2 } import A::V2; fun f(): A[Int32] { V2[Int32, Float32] }",
        pos(1, 58),
        SemError::WrongNumberTypeParams(1, 2),
    );
}

#[test]
fn test_enum_value_with_type_param() {
    ok("enum A[T] { V1, V2 } fun f(): A[Int32] { A::V2[Int32] }");
    ok("enum A[T] { V1, V2 } fun f(): A[Int32] { A[Int32]::V2 }");
    err(
        "enum A[T] { V1, V2 } fun f(): A[Int32] { A[Int32]::V2[Int32] }",
        pos(1, 43),
        SemError::ExpectedSomeIdentifier,
    );
}

#[test]
fn test_block_value() {
    ok("fun f(): Int32 { 1I }");
    ok("fun f() { let x = { 1I }; }");
    ok("fun g(): Int32 { return 1I; } fun f() { let x: Int32 = { g() }; }");
    ok("fun g(): Int32 { return 1I; } fun f() { let x: Int32 = { g(); 1I }; }");
}

#[test]
fn test_if_expression() {
    ok("fun f(): Int32 { if true { 1I } else { 2I } }");
    ok("fun f(): Float32 { if true { 1.0F } else { 2.0F } }");
    ok("fun f(): Float64 { if true { 1.0 } else { 2.0 } }");

    ok("fun f(): Int32 { 4I * if true { 1I } else { 2I } }");
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
        SemError::ReturnType("(Int32)".into(), "(Int32, Bool)".into()),
    );
    err(
        "fun f(a: (Int32, Bool)): (Int32, Float32) { return a; }",
        pos(1, 45),
        SemError::ReturnType("(Int32, Float32)".into(), "(Int32, Bool)".into()),
    );
}

#[test]
fn test_tuple_literal() {
    ok("fun f(): (Int32, Bool) {
        return (1I, false);
    }");

    err(
        "fun f(): (Int32) {
        return (1I);
    }",
        pos(2, 9),
        SemError::ReturnType("(Int32)".into(), "Int32".into()),
    );

    err(
        "fun f(): (Int32, Int32) {
        return (1I, false);
    }",
        pos(2, 9),
        SemError::ReturnType("(Int32, Int32)".into(), "(Int32, Bool)".into()),
    );
}

#[test]
fn test_tuple_in_call() {
    ok("
        fun f(a: (Int32, Bool)) {}
        fun g() {
            f((1I, true));
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
        SemError::ReturnType("String".into(), "Bool".into()),
    );
}

#[test]
fn test_inheritance_with_generics() {
    ok("
        @open class Foo[A](let a: A)
        class Bar extends Foo[Int32](10I)
    ");

    err(
        "
        @open class Foo[A](let a: A)
        class Bar extends Foo(10I)
    ",
        pos(3, 27),
        SemError::WrongNumberTypeParams(1, 0),
    );

    ok("
        @open class Foo[A](let a: A)
        class Bar[A](x: A) extends Foo[A](x)
    ");
}

#[test]
fn test_fields_with_generics() {
    ok("
        @open @abstract class Foo[A](var a: A)
        @open class Bar[A] extends Foo[Int32](10I)
        class Baz[A] extends Bar[A] {
            fun test(): Int32 { self.a }
            fun assignMe() { self.a = 10I; }
        }
    ");
}

#[test]
fn test_methods_with_generics() {
    ok("
        @open @abstract class Foo[A] {
            @abstract fun test(): A;
        }

        class Bar[A](let bar: A) extends Foo[A] {
            @override fun test(): A { self.bar }
        }

        class Baz[A](let baz: A) extends Foo[Int32] {
            @override fun test(): Int32 { 0I }
        }
    ");

    ok("
        @open @abstract class Foo[A] {
            @open fun test(x: A): A { x }
        }

        class Bar[A](let bar: A) extends Foo[A] {
            @override fun test(x: A): A { self.bar }
        }

        class Baz[A](let baz: A) extends Foo[Int32] {
            @override fun test(x: Int32): Int32 { x+x }
        }
    ");
}

#[test]
fn test_is_types() {
    err(
        "
        trait SomeTrait {}
        class Foo[A: SomeTrait] {}
        fun test(f: Object): Bool {
            return f is Foo[Int32];
        }
    ",
        pos(5, 25),
        SemError::TypeNotImplementingTrait("Int32".into(), "SomeTrait".into()),
    );
}

#[test]
fn test_type_params_with_bounds_in_subclass() {
    err(
        "
        trait SomeTrait {}
        @open class Foo[A: SomeTrait]
        class Bar extends Foo[Int32]
    ",
        pos(4, 27),
        SemError::TypeNotImplementingTrait("Int32".into(), "SomeTrait".into()),
    );
}

#[test]
fn test_type_params_with_bounds_in_subclass_wrong_order() {
    err(
        "
        trait SomeTrait {}
        class Bar extends Foo[Int32]
        @open class Foo[A: SomeTrait]
    ",
        pos(3, 27),
        SemError::TypeNotImplementingTrait("Int32".into(), "SomeTrait".into()),
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
        SemError::TypeNotUsableInForIn("Foo".into()),
    );
}

#[test]
fn test_type_make_iterator_not_implementing_iterator() {
    err(
        "
        class Foo {
            fun makeIterator(): Int32 { 0I }
        }
        fun bar(x: Foo) {
            for i in x {
                let x: Foo = i;
            }
        }
    ",
        pos(6, 22),
        SemError::TypeNotUsableInForIn("Foo".into()),
    );
}

#[test]
fn zero_trait_ok() {
    ok("fun f() { Array[Int32]::zero(12L); }");
}

#[test]
fn zero_trait_err() {
    err(
        "fun f() { Array[String]::zero(12L); }",
        pos(1, 30),
        SemError::UnknownStaticMethod("Array[String]".into(), "zero".into(), vec!["Int64".into()]),
    );
}

#[test]
fn extension_method_call() {
    ok("
        class Foo(let value: Int32)
        impl Foo { fun foo(): Int32 { self.value } }
        fun bar(x: Foo): Int32 { x.foo() }
    ");
}

#[test]
fn extension_class_with_type_param() {
    ok("
        class Foo[T](let value: T)
        trait MyTrait {}
        impl[T: MyTrait] Foo[T] { fun foo(): Int32 { 12I } }
        impl MyTrait for Int32 {}
        fun bar(x: Foo[Int32]): Int32 { x.foo() }
    ");

    ok("
        class Foo[T](let value: T)
        impl Foo[Int32] { fun foo() {} }
        impl Foo[Float32] { fun bar() {} }
        fun f(x: Foo[Int32]) { x.foo() }
        fun g(x: Foo[Float32]) { x.bar() }
    ");

    err(
        "
        class Foo[T](let value: T)
        impl Foo[Float32] { fun bar() {} }
        fun f(x: Foo[Int32]) { x.bar() }
    ",
        pos(4, 37),
        SemError::UnknownMethod("Foo[Int32]".into(), "bar".into(), Vec::new()),
    );
}

#[test]
fn extension_class_tuple() {
    ok("
        class Foo[T](let value: T)
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
        SemError::UnknownMethod("Foo[(Int32, Int32)]".into(), "bar".into(), Vec::new()),
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
        SemError::UnknownMethod("Foo[Foo[Int32]]".into(), "bar".into(), Vec::new()),
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
        SemError::UnknownMethod("Foo[(Int32, Float32)]".into(), "bar".into(), Vec::new()),
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
        SemError::UnknownMethod("Foo[(T, Float32)]".into(), "bar".into(), Vec::new()),
    );
}

#[test]
fn extension_struct_with_type_param() {
    ok("
        struct Foo[T](value: T)
        trait MyTrait {}
        impl[T: MyTrait] Foo[T] { fun foo(): Int32 { 12I } }
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
        SemError::UnknownMethod("Foo[Int32]".into(), "bar".into(), Vec::new()),
    );
}

#[test]
fn extension_enum_with_type_param() {
    ok("
        enum Foo[T] { A(T), B }
        trait MyTrait {}
        impl[T: MyTrait] Foo[T] { fun foo(): Int32 { 12I } }
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
        SemError::UnknownMethod("Foo[Int32]".into(), "bar".into(), Vec::new()),
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
        SemError::UnknownMethod("Foo[Int32]".into(), "bar".into(), Vec::new()),
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
        SemError::UnknownMethod("Foo[Int32]".into(), "bar".into(), Vec::new()),
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
        SemError::UnknownMethod("Foo[Int32]".into(), "bar".into(), Vec::new()),
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
        SemError::UnknownMethod("()".into(), "foo".into(), Vec::new()),
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
    // ok("fun f(val: UInt8) {} fun g() { f(1); }");
    err(
        "fun f(): UInt8 { 256 }",
        pos(1, 18),
        SemError::NumberOverflow("UInt8".into()),
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
            f(1I, 2I, 3I, 4I);
            f();
            f(1I);
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
        SemError::ParamTypesIncompatible("f".into(), vec!["Int32".into()], vec!["Bool".into()]),
    );
    ok("
        fun f(x: Int32, y: Int32...) {}
        fun g() {
            f(1I, 2I, 3I, 4I);
            f(1I, 2I);
            f(1I);
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
        SemError::ParamTypesIncompatible(
            "f".into(),
            vec!["Int32".into(), "Int32".into()],
            Vec::new(),
        ),
    );
    err(
        "fun f(x: Int32..., y: Int32) {}",
        pos(1, 20),
        SemError::VariadicParameterNeedsToBeLast,
    );
    err(
        "class F(x: Int32..., y: Int32) {}",
        pos(1, 22),
        SemError::VariadicParameterNeedsToBeLast,
    );
}

#[test]
fn for_with_array() {
    ok("fun f(x: Array[Int32]): Int32 {
        var result = 0I;
        for i in x {
            result = result + i;
        }
        result
    }");

    ok("fun f(x: Array[Float32]): Float32 {
        var result = 0.0F;
        for i in x {
            result = result + i;
        }
        result
    }");
}

#[test]
fn for_with_vec() {
    ok("fun f(x: Vec[Int32]): Int32 {
        var result = 0I;
        for i in x.makeIterator() {
            result = result + i;
        }
        result
    }");

    ok("fun f(x: Vec[Int32]): Int32 {
        var result = 0I;
        for i in x {
            result = result + i;
        }
        result
    }");

    ok("fun f(x: Vec[Float32]): Float32 {
        var result = 0.0F;
        for i in x.makeReverseIterator() {
            result = result + i;
        }
        result
    }");

    ok("fun f(x: Vec[Float32]): Float32 {
        var result = 0.0F;
        for i in x {
            result = result + i;
        }
        result
    }");
}

#[test]
fn check_no_type_params_with_generic_type() {
    err(
        "class Bar[T] fun f(x: Bar) {}",
        pos(1, 23),
        SemError::WrongNumberTypeParams(1, 0),
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
        SemError::ParamTypesIncompatible("bar".into(), vec!["T".into()], vec!["Bool".into()]),
    );
}

#[test]
fn multiple_functions() {
    ok("fun f() {}\nfun g() {}");
}

#[test]
fn redefine_function() {
    err(
        "fun f() {}\nfun f() {}",
        pos(2, 1),
        SemError::ShadowFunction("f".into()),
    );
}

#[test]
fn shadow_type_with_function() {
    err(
        "class FooBar fun FooBar() {}",
        pos(1, 14),
        SemError::ShadowClass("FooBar".into()),
    );
}

#[test]
fn shadow_type_with_param() {
    err(
        "fun test(Bool: String) {}",
        pos(1, 10),
        SemError::ShadowStruct("Bool".into()),
    );
}

#[test]
fn shadow_type_with_var() {
    ok("fun test() { let String = 3I; }");
}

#[test]
fn shadow_function() {
    ok("fun f() { let f = 1I; }");
    err(
        "fun f() { let f = 1I; f(); }",
        pos(1, 24),
        SemError::UnknownMethod("Int32".into(), "get".into(), Vec::new()),
    );
}

#[test]
fn shadow_var() {
    ok("fun f() { let f = 1I; let f = 2I; }");
}

#[test]
fn shadow_param() {
    err(
        "fun f(a: Int32, b: Int32, a: String) {}",
        pos(1, 27),
        SemError::ShadowParam("a".into()),
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
        SemError::UnknownIdentifier("a".into()),
    );
    err(
        "fun f() { a; }",
        pos(1, 11),
        SemError::UnknownIdentifier("a".into()),
    );
}

#[test]
fn undefined_function() {
    err(
        "fun f() { foo(); }",
        pos(1, 11),
        SemError::UnknownIdentifier("foo".into()),
    );
}

#[test]
fn recursive_function_call() {
    ok("fun f() { f(); }");
}

#[test]
fn function_call() {
    ok("fun a() {}\nfun b() { a(); }");

    // non-forward definition of functions
    ok("fun a() { b(); }\nfun b() {}");
}

#[test]
fn variable_outside_of_scope() {
    err(
        "fun f(): Int32 { { let a = 1I; } return a; }",
        pos(1, 41),
        SemError::UnknownIdentifier("a".into()),
    );

    ok("fun f(): Int32 { let a = 1I; { let a = 2I; } return a; }");
}

#[test]
fn const_value() {
    ok("const one: Int32 = 1I;
        fun f(): Int32 { return one; }");
}

#[test]
fn for_var() {
    ok("fun f() { for i in std::range(0I, 4I) { i; } }");
}

#[test]
fn namespace_fct_call() {
    err(
        "
        fun f() { foo::g(); }
        namespace foo { fun g() {} }
    ",
        pos(2, 25),
        SemError::NotAccessible("foo::g".into()),
    );

    ok("
        fun f() { foo::g(); }
        namespace foo { @pub fun g() {} }
    ");

    ok("
        fun f() { foo::bar::baz(); }
        namespace foo {
            @pub namespace bar {
                @pub fun baz() {}
            }
        }
    ");

    err(
        "
        fun f() { foo::bar::baz(); }
        namespace foo {
            @pub namespace bar {
                fun baz() {}
            }
        }
    ",
        pos(2, 32),
        SemError::NotAccessible("foo::bar::baz".into()),
    );
}

#[test]
fn namespace_ctor_call() {
    ok("
        fun f() { foo::Foo(); }
        namespace foo { @pub class Foo }
    ");

    err(
        "
        fun f() { foo::Foo(); }
        namespace foo { class Foo }
    ",
        pos(2, 27),
        SemError::NotAccessible("foo::Foo".into()),
    );

    ok("
        fun f() { foo::bar::Foo(); }
        namespace foo { @pub namespace bar { @pub class Foo } }
    ");

    err(
        "
        fun f() { foo::bar::Foo(); }
        namespace foo { @pub namespace bar { class Foo } }
    ",
        pos(2, 32),
        SemError::NotAccessible("foo::bar::Foo".into()),
    );
}

#[test]
fn namespace_class_field() {
    err(
        "
        fun f(x: foo::Foo) { let a = x.bar; }
        namespace foo { @pub class Foo { var bar: Int32 = 0I; } }
    ",
        pos(2, 39),
        SemError::NotAccessible("bar".into()),
    );

    err(
        "
        fun f(x: foo::Foo) { let a = x.bar(10L); }
        namespace foo { @pub class Foo { var bar: Array[Int32] = Array[Int32](); } }
    ",
        pos(2, 43),
        SemError::NotAccessible("bar".into()),
    );

    err(
        "
        fun f(x: foo::Foo) { x.bar(10L) = 10I; }
        namespace foo { @pub class Foo { var bar: Array[Int32] = Array[Int32](); } }
    ",
        pos(2, 31),
        SemError::NotAccessible("bar".into()),
    );

    ok("
        fun f(x: foo::Foo) { let a = x.bar; }
        namespace foo { @pub class Foo { @pub var bar: Int32 = 0I; } }
    ");
}

#[test]
fn namespace_class_method() {
    ok("
        fun f(x: foo::Foo) { x.bar(); }
        namespace foo { @pub class Foo { @pub fun bar() {} } }
    ");

    err(
        "
        fun f(x: foo::Foo) { x.bar(); }
        namespace foo { @pub class Foo { fun bar() {} } }
    ",
        pos(2, 35),
        SemError::NotAccessible("foo::bar".into()),
    );
}

#[test]
fn namespace_class_static_method() {
    ok("
        fun f() { foo::Foo::bar(); }
        namespace foo { @pub class Foo { @pub @static fun bar() {} } }
    ");

    err(
        "
        fun f() { foo::Foo::bar(); }
        namespace foo { @pub class Foo { @static fun bar() {} } }
    ",
        pos(2, 32),
        SemError::NotAccessible("foo::bar".into()),
    );
}

#[test]
fn namespace_struct_field() {
    err(
        "
        fun f(x: foo::Foo) { let a = x.bar; }
        namespace foo { @pub struct Foo(bar: Int32) }
    ",
        pos(2, 39),
        SemError::NotAccessible("bar".into()),
    );

    ok("
        fun f(x: foo::Foo) { let a = x.bar(10L); }
        namespace foo { @pub struct Foo(@pub bar: Array[Int32]) }
    ");

    err(
        "
        fun f(x: foo::Foo) { let a = x.bar(10L); }
        namespace foo { @pub struct Foo(bar: Array[Int32]) }
    ",
        pos(2, 43),
        SemError::NotAccessible("bar".into()),
    );

    err(
        "
        fun f(x: foo::Foo) { x.bar(10L) = 10I; }
        namespace foo { @pub struct Foo(bar: Array[Int32]) }
    ",
        pos(2, 31),
        SemError::NotAccessible("bar".into()),
    );

    ok("
        fun f(x: foo::Foo) { let a = x.bar; }
        namespace foo { @pub struct Foo(@pub bar: Int32) }
    ");
}

#[test]
fn namespace_path_in_type() {
    ok("
        fun f(): foo::Foo { foo::Foo() }
        namespace foo { @pub class Foo }
    ");

    err(
        "
        fun f(): bar::Foo { 1I }
    ",
        pos(2, 18),
        SemError::ExpectedNamespace,
    );

    err(
        "
        fun bar() {}
        fun f(): bar::Foo { 1I }
    ",
        pos(3, 18),
        SemError::ExpectedNamespace,
    );

    err(
        "
        fun f(): foo::bar::Foo { 1I }
        namespace foo {}
    ",
        pos(2, 18),
        SemError::ExpectedNamespace,
    );
}

#[test]
fn namespace_global() {
    ok("
        fun f(): Int32 { foo::x }
        namespace foo { @pub var x: Int32 = 1I; }
    ");

    err(
        "
        fun f(): Int32 { foo::x }
        namespace foo { var x: Int32 = 1I; }
    ",
        pos(2, 29),
        SemError::NotAccessible("foo::x".into()),
    );
}

#[test]
fn namespace_trait() {
    ok("
        namespace foo { class Foo trait Bar { fun f(x: Foo); } }
    ");
}

#[test]
fn namespace_impl() {
    ok("
        namespace foo {
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
fn namespace_class() {
    ok("
        namespace foo {
            class Foo(let x: Bar) {
                fun foo(x: Bar) {}
            }
            class Bar
        }
    ");
}

#[test]
fn namespace_struct() {
    err(
        "
        fun f() { foo::Foo(1I); }
        namespace foo {
            struct Foo(f: Int32)
        }
    ",
        pos(2, 27),
        SemError::NotAccessible("foo::Foo".into()),
    );

    ok("
        fun f() { foo::Foo(1I); }
        namespace foo {
            @pub struct Foo(f: Int32)
        }
    ");

    ok("
        fun f(value: foo::Foo) {}
        namespace foo {
            @pub struct Foo(f: Int32)
        }
    ");

    err(
        "
        fun f(value: foo::Foo) {}
        namespace foo {
            struct Foo(f: Int32)
        }
    ",
        pos(2, 22),
        SemError::NotAccessible("foo::Foo".into()),
    );
}

#[test]
fn namespace_const() {
    ok("
        fun f(): Int32 { foo::x }
        namespace foo { @pub const x: Int32 = 1I; }
    ");

    err(
        "
        fun f(): Int32 { foo::x }
        namespace foo { const x: Int32 = 1I; }
    ",
        pos(2, 29),
        SemError::NotAccessible("foo::x".into()),
    );

    ok("
        fun f(): Int32 { foo::bar::x }
        namespace foo { @pub namespace bar { @pub const x: Int32 = 1I; } }
    ");
}

#[test]
fn namespace_enum_value() {
    ok("
        fun f() { foo::A; }
        namespace foo { @pub enum Foo { A, B } import Foo::A; }
    ");

    err(
        "
        fun f() { foo::A; }
        namespace foo { enum Foo { A, B } import Foo::A; }
    ",
        pos(2, 22),
        SemError::NotAccessible("foo::Foo".into()),
    );

    ok("
        fun f() { foo::bar::A; }
        namespace foo { @pub namespace bar { @pub enum Foo { A, B } import Foo::A; } }
    ");

    err(
        "
        fun f() { foo::bar::A; }
        namespace foo { @pub namespace bar { enum Foo { A, B } import Foo::A; } }
    ",
        pos(2, 27),
        SemError::NotAccessible("foo::bar::Foo".into()),
    );
}

#[test]
fn namespace_enum() {
    err(
        "
        fun f() {
            foo::Foo::B;
        }
        namespace foo { enum Foo { A(Bar), B } class Bar }
    ",
        pos(3, 21),
        SemError::NotAccessible("foo::Foo".into()),
    );

    ok("
        fun f() {
            foo::Foo::B;
        }
        namespace foo { @pub enum Foo { A, B } }
    ");

    ok("
        fun f() {
            foo::Foo::A(1I);
        }
        namespace foo { @pub enum Foo { A(Int32), B } }
    ");

    err(
        "
        fun f() {
            foo::Foo::A(1I);
        }
        namespace foo { enum Foo { A(Int32), B } }
    ",
        pos(3, 24),
        SemError::NotAccessible("foo::Foo".into()),
    );
}

#[test]
fn namespace_import() {
    ok("
        import foo::bar;
        fun f() { bar(); }
        namespace foo { @pub fun bar() {} }
    ");

    ok("
        import foo::bar::baz;
        fun f() { baz(); }
        namespace foo { @pub namespace bar {
            @pub fun baz() {}
        } }
    ");

    ok("
        import foo::bar as baz;
        fun f() { baz(); }
        namespace foo { @pub fun bar() {} }
    ");

    ok("
        import foo::bar;
        fun f(): Int32 { bar }
        namespace foo { @pub var bar: Int32 = 10I; }
    ");

    ok("
        import foo::bar::baz;
        fun f(): Int32 { baz }
        namespace foo { @pub namespace bar {
            @pub var baz: Int32 = 10I;
        } }
    ");

    ok("
        import foo::bar;
        fun f(): Int32 { bar }
        namespace foo { @pub var bar: Int32 = 10I; }
    ");
}

#[test]
fn namespace_import_class() {
    ok("
        import foo::Bar;
        fun f() { Bar(); }
        namespace foo { @pub class Bar }
    ");

    ok("
        import foo::Bar;
        fun f() {
            Bar();
            Bar::baz();
        }
        namespace foo {
            @pub class Bar {
                @pub @static fun baz() {}
            }
        }
    ");
}

#[test]
fn namespace_import_trait() {
    ok("
        import foo::Bar;
        namespace foo { @pub trait Bar{} }
    ");
}

#[test]
fn namespace_import_std() {
    ok("
        import std::HashMap;
    ");
}

#[test]
fn namespace_import_package() {
    ok("
        class Foo
        namespace bar {
            import package::Foo;
            fun getfoo(): Foo { Foo() }
        }
    ");
}

#[test]
fn namespace_import_super() {
    ok("
        namespace baz {
            class Foo
            namespace bar {
                import super::Foo;

                fun getfoo(): Foo { Foo() }
            }
        }
    ");

    err("import super::Foo;", pos(1, 1), SemError::NoSuperNamespace);
}

#[test]
fn namespace_import_self() {
    ok("
        import self::bar::Foo;
        fun getfoo(): Foo { Foo() }
        namespace bar { @pub class Foo }
    ");
}

#[test]
fn namespace_import_module() {
    ok("
        import foo::Bar;
        fun f() {
            Bar::baz();
        }
        namespace foo {
            @pub module Bar {
                @pub fun baz() {}
            }
        }
    ");
}

#[test]
fn namespace_import_errors() {
    err(
        "
        import foo::bar::baz;
        namespace foo { @pub namespace bar {} }
    ",
        pos(2, 9),
        SemError::UnknownIdentifierInNamespace("foo::bar".into(), "baz".into()),
    );

    err(
        "
        import foo::bar;
    ",
        pos(2, 9),
        SemError::ExpectedPath,
    );

    err(
        "
        import foo::bar::baz;
    ",
        pos(2, 9),
        SemError::ExpectedPath,
    );

    err(
        "
        import foo::bar::baz;
        fun foo() {}
    ",
        pos(2, 9),
        SemError::ExpectedNamespace,
    );

    err(
        "
        import foo::bar;
        fun foo() {}
    ",
        pos(2, 9),
        SemError::ExpectedPath,
    );
}

#[test]
fn namespace_inside() {
    ok("
        namespace foo { fun f() { g() } fun g() {} }
    ");

    ok("
        namespace foo { class Foo fun g(x: Foo) {} }
    ");

    ok("
        fun f(x: foo::Foo) {}
        namespace foo { @pub class Foo }
    ");

    err(
        "
        fun f(x: foo::Foo) {}
        namespace foo { class Foo }
    ",
        pos(2, 18),
        SemError::NotAccessible("foo::Foo".into()),
    );
}

#[test]
fn different_fct_call_kinds() {
    ok("fun f() { g(); } fun g() {}");
    ok("fun f() { g[Int32](); } fun g[T]() {}");
    ok("fun f(g: Array[Int32]) { g(0L); }");
    err(
        "fun f(g: Array[Int32]) { g[Float32](0L); }",
        pos(1, 27),
        SemError::NoTypeParamsExpected,
    );
    ok("class Foo fun f() { Foo(); }");
    ok("module Foo { fun bar() {} } fun f() { Foo::bar(); }");
    err(
        "module Foo { fun bar() {} } fun f() { Foo[Int32]::bar(); }",
        pos(1, 42),
        SemError::NoTypeParamsExpected,
    );
    ok("module Foo { fun bar() {} } fun f() { Foo::bar(); }");
    errors(
        "fun f() { 1I[Int32](); }",
        &[
            (pos(1, 13), SemError::NoTypeParamsExpected),
            (
                pos(1, 20),
                SemError::UnknownMethod("Int32".into(), "get".into(), Vec::new()),
            ),
        ],
    );
    ok("enum Foo { A(Int32), B } fun f() { Foo::A(1I); }");
    ok("enum Foo[T] { A(Int32), B } fun f() { Foo[Int32]::A(1I); }");
    ok("enum Foo[T] { A(Int32), B } fun f() { Foo::A[Int32](1I); }");
    err(
        "enum Foo[T] { A(Int32), B } fun f() { Foo[Int32]::A[Int32](1I); }",
        pos(1, 42),
        SemError::NoTypeParamsExpected,
    );
    ok("trait MyTrait { @static fun foo(); } fun f[T: MyTrait]() { T::foo(); }");
    ok("class Foo { fun bar() {} } fun f(g: Foo) { g.bar(); }");
    ok("class Foo { fun bar[T]() {} } fun f(g: Foo) { g.bar[Int32](); }");
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
            fun bar(): Int32 { 1I }
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
            fun bar(): Int32 { 1I }
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
        SemError::TypeNotImplementingTrait("Bar".into(), "Foo".into()),
    );
}

#[test]
fn infer_enum_type() {
    ok("fun f(): Option[Int32] {
        None
    }");

    ok("
        class X {
            var a: Option[Int32] = None;
            var b: Option[Int32] = Some(10I);
        }

        fun f(x: X) {
            x.a = Some(10I);
            x.b = None;
        }
    ");

    ok("fun f() {
        var x: Option[Int32] = None; x = Some(10I);
        var y: Option[Int32] = Some(10I); y = None;
    }");

    ok("fun f(): Option[Int32] {
        Some(10I)
    }");
}

#[test]
fn infer_ctor_type() {
    ok("fun f(): Vec[Int32] {
        Vec(1I, 2I, 3I)
    }");
}

#[test]
fn method_call_type_mismatch_with_type_params() {
    err(
        "
        class Foo {
            fun f(a: String) {}
        }
        fun g[T](foo: Foo, value: T) {
            foo.f(value);
        }
    ",
        pos(6, 18),
        SemError::ParamTypesIncompatible("f".into(), vec!["String".into()], vec!["T".into()]),
    );
}

#[test]
fn basic_lambda() {
    ok("fun f(foo: (Int32) -> Int32): Int32 {
        foo(1I)
    }");

    err(
        "fun f(foo: (Int32) -> Int32): Bool {
        foo(1I)
    }",
        pos(1, 36),
        SemError::ReturnType("Bool".into(), "Int32".into()),
    );

    err(
        "fun f(foo: (Int32, Int32) -> Int32): Int32 {
        foo(1I)
    }",
        pos(2, 12),
        SemError::LambdaParamTypesIncompatible(
            vec!["Int32".into(), "Int32".into()],
            vec!["Int32".into()],
        ),
    );
}
