use crate::error::msg::SemError;
use crate::semck::tests::*;
use crate::vm::ConstValue;

#[test]
fn type_method_len() {
    ok("fun f(a: String) -> Int { return a.length(); }");
    ok("fun f(a: String) -> Int { return \"abc\".length(); }");
}

#[test]
fn type_object_field() {
    ok("class Foo(let a:Int) fun f(x: Foo) -> Int { return x.a; }");
    ok("class Foo(let a:String) fun f(x: Foo) -> String { return x.a; }");
    err(
        "class Foo(let a:Int) fun f(x: Foo) -> Bool { return x.a; }",
        pos(1, 46),
        SemError::ReturnType("Bool".into(), "Int".into()),
    );
    err(
        "class Foo(let a:Int) fun f(x: Foo) -> Int { return x.b; }",
        pos(1, 53),
        SemError::UnknownField("b".into(), "Foo".into()),
    );
}

#[test]
fn type_object_set_field() {
    ok("class Foo(var a: Int) fun f(x: Foo) { x.a = 1; }");
    err(
        "class Foo(var a: Int) fun f(x: Foo) { x.a = false; }",
        pos(1, 43),
        SemError::AssignField("a".into(), "Foo".into(), "Int".into(), "Bool".into()),
    );
}

#[test]
fn type_object_field_without_self() {
    err(
        "class Foo(let a: Int) { fun f() -> Int { return a; } }",
        pos(1, 49),
        SemError::UnknownIdentifier("a".into()),
    );
    err(
        "class Foo(var a: Int) { fun set(x: Int) { a = x; } }",
        pos(1, 43),
        SemError::UnknownIdentifier("a".into()),
    );
}

#[test]
fn type_class_method_call() {
    ok("class Foo {
                fun bar() {}
                fun baz() -> Int { return 1; }
            }

            fun f(x: Foo) { x.bar(); }
            fun g(x: Foo) -> Int { return x.baz(); }");

    err(
        "class Foo {
                 fun bar() -> Int { return 0; }
             }

             fun f(x: Foo) -> String { return x.bar(); }",
        pos(5, 40),
        SemError::ReturnType("String".into(), "Int".into()),
    );
}

#[test]
fn type_module_method_call_1() {
    ok("module Foo {
                fun bar() {}
                fun baz() -> Int { return 1; }
            }

            fun f1() { Foo.bar(); }
            fun f2() { Foo::bar(); }
            fun g() -> Int { return Foo.baz(); }");

    err(
        "module Foo {
                 fun bar() -> Int { return 0; }
             }

             fun f() -> String { return Foo.bar(); }",
        pos(5, 34),
        SemError::ReturnType("String".into(), "Int".into()),
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
                 fun bar() -> Int {}
             }",
        pos(3, 18),
        SemError::MethodExists("bar".into(), pos(2, 18)),
    );

    err(
        "class Foo {
                 fun bar(a: Int) {}
                 fun bar(a: Int) -> Int {}
             }",
        pos(3, 18),
        SemError::MethodExists("bar".into(), pos(2, 18)),
    );

    err(
        "class Foo {
                fun bar(a: Int) {}
                fun bar(a: String) {}
            }",
        pos(3, 17),
        SemError::MethodExists("bar".into(), pos(2, 17)),
    );
}

#[test]
fn type_self() {
    ok("class Foo { fun me() -> Foo { return self; } }");
    err(
        "class Foo fun me() { return self; }",
        pos(1, 29),
        SemError::ThisUnavailable,
    );

    ok("class Foo(let a: Int, let b: Int) {
            fun bar() -> Int { return self.a + self.b; }
        }");

    ok("class Foo(var a: Int) {
            fun setA(a: Int) { self.a = a; }
        }");

    ok("class Foo {
            fun zero() -> Int { return 0; }
            fun other() -> Int { return self.zero(); }
        }");

    ok("class Foo {
            fun bar() { self.bar(); }
        }");
}

#[test]
fn type_unknown_method() {
    err(
        "class Foo {
                 fun bar(a: Int) { }
             }

             fun f(x: Foo) { x.bar(); }",
        pos(5, 35),
        SemError::ParamTypesIncompatible("bar".into(), vec!["Int".into()], Vec::new()),
    );

    err(
        "class Foo { }
              fun f(x: Foo) { x.bar(1); }",
        pos(2, 36),
        SemError::UnknownMethod("Foo".into(), "bar".into(), vec!["Int".into()]),
    );
}

#[test]
fn type_ctor() {
    ok("class Foo fun f() -> Foo { return Foo(); }");
    ok("class Foo(let a: Int) fun f() -> Foo { return Foo(1); }");
    err(
        "class Foo fun f() -> Foo { return 1; }",
        pos(1, 28),
        SemError::ReturnType("Foo".into(), "Int".into()),
    );
}

#[test]
fn type_def_for_return_type() {
    ok("fun a() -> Int { return 1; }");
    err(
        "fun a() -> unknown {}",
        pos(1, 12),
        SemError::UnknownType("unknown".into()),
    );
}

#[test]
fn type_def_for_param() {
    ok("fun a(b: Int) {}");
    err(
        "fun a(b: foo) {}",
        pos(1, 10),
        SemError::UnknownType("foo".into()),
    );
}

#[test]
fn type_def_for_var() {
    ok("fun a() { let a : Int = 1; }");
    err(
        "fun a() { let a : test = 1; }",
        pos(1, 19),
        SemError::UnknownType("test".into()),
    );
}

#[test]
fn type_var_needs_expr_or_definition() {
    err(
        "fun a() { let a; }",
        pos(1, 11),
        SemError::VarNeedsTypeInfo("a".into()),
    );
}

#[test]
fn type_var_wrong_type_defined() {
    ok("fun f() { let a : Int = 1; }");
    ok("fun f() { let a : Bool = false; }");
    ok("fun f() { let a : String = \"f\"; }");

    err(
        "fun f() { let a : Int = true; }",
        pos(1, 11),
        SemError::AssignType("a".into(), "Int".into(), "Bool".into()),
    );
    err(
        "fun f() { let b : Bool = 2; }",
        pos(1, 11),
        SemError::AssignType("b".into(), "Bool".into(), "Int".into()),
    );
}

#[test]
fn type_while() {
    ok("fun x() { while true { } }");
    ok("fun x() { while false { } }");
    err(
        "fun x() { while 2 { } }",
        pos(1, 11),
        SemError::WhileCondType("Int".into()),
    );
}

#[test]
fn type_if() {
    ok("fun x() { if true { } }");
    ok("fun x() { if false { } }");
    err(
        "fun x() { if 4 { } }",
        pos(1, 11),
        SemError::IfCondType("Int".into()),
    );
}

#[test]
fn type_return_unit() {
    ok("fun f() { return; }");
    err(
        "fun f() { return 1; }",
        pos(1, 11),
        SemError::ReturnType("()".into(), "Int".into()),
    );
}

#[test]
fn type_return() {
    ok("fun f() -> Int { let a = 1; return a; }");
    ok("fun f() -> Int { return 1; }");
    err(
        "fun f() -> Int { return; }",
        pos(1, 18),
        SemError::ReturnType("Int".into(), "()".into()),
    );

    ok("fun f() -> Int { return 0; }
            fun g() -> Int { return f(); }");
    err(
        "fun f() { }
             fun g() -> Int { return f(); }",
        pos(2, 31),
        SemError::ReturnType("Int".into(), "()".into()),
    );
}

#[test]
fn type_variable() {
    ok("fun f(a: Int) { let b: Int = a; }");
}

#[test]
fn type_assign_lvalue() {
    err("fun f() { 1 = 3; }", pos(1, 13), SemError::LvalueExpected);
}

#[test]
fn type_un_op() {
    ok("fun f(a: Int) { !a; -a; +a; }");
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
    ok("fun f(a: Int) { a+a; a-a; a*a; a/a; a%a; }");
    ok("fun f(a: Int) { a<a; a<=a; a==a; a!=a; a>a; a>=a; }");
    ok("fun f(a: String) { a<a; a<=a; a==a; a!=a; a>a; a>=a; }");
    ok("fun f(a: String) { a===a; a!==a; a+a; }");
    ok("class Foo fun f(a: Foo) { a===a; a!==a; }");
    ok("fun f(a: Int) { a|a; a&a; a^a; }");
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
        "fun f(a: Int) { a||a; }",
        pos(1, 18),
        SemError::BinOpType("||".into(), "Int".into(), "Int".into()),
    );
    err(
        "fun f(a: Int) { a&&a; }",
        pos(1, 18),
        SemError::BinOpType("&&".into(), "Int".into(), "Int".into()),
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
    ok("fun foo() -> Int { return 1; }\nfun f() { let i: Int = foo(); }");
    err(
        "fun foo() -> Int { return 1; }\nfun f() { let i: Bool = foo(); }",
        pos(2, 11),
        SemError::AssignType("i".into(), "Bool".into(), "Int".into()),
    );
}

#[test]
fn type_ident_in_function_params() {
    ok("fun f(a: Int) {}\nfun g() { let a = 1; f(a); }");
}

#[test]
fn type_recursive_function_call() {
    ok("fun f(a: Int) { f(a); }");
}

#[test]
fn type_function_params() {
    ok("fun foo() {}\nfun f() { foo(); }");
    ok("fun foo(a: Int) {}\nfun f() { foo(1); }");
    ok("fun foo(a: Int, b: Bool) {}\nfun f() { foo(1, true); }");

    err(
        "fun foo() {}\nfun f() { foo(1); }",
        pos(2, 14),
        SemError::ParamTypesIncompatible("foo".into(), vec![], vec!["Int".into()]),
    );
    err(
        "fun foo(a: Int) {}\nfun f() { foo(true); }",
        pos(2, 14),
        SemError::ParamTypesIncompatible("foo".into(), vec!["Int".into()], vec!["Bool".into()]),
    );
    err(
        "fun foo(a: Int, b: Bool) {}\nfun f() { foo(1, 2); }",
        pos(2, 14),
        SemError::ParamTypesIncompatible(
            "foo".into(),
            vec!["Int".into(), "Bool".into()],
            vec!["Int".into(), "Int".into()],
        ),
    );
}

#[test]
fn type_return_nil() {
    ok("fun foo() -> String { return nil; }");
    ok("class Bar fun foo() -> Bar { return nil; }");
    err(
        "fun foo() -> Int { return nil; }",
        pos(1, 20),
        SemError::IncompatibleWithNil("Int".into()),
    );
}

#[test]
fn type_nil_as_argument() {
    ok("fun foo(a: String) {} fun test() { foo(nil); }");
    err(
        "fun foo(a: Int) {} fun test() { foo(nil); }",
        pos(1, 36),
        SemError::ParamTypesIncompatible("foo".into(), vec!["Int".into()], vec!["nil".into()]),
    );
}

#[test]
fn type_nil_for_ctor() {
    ok("class Foo(let a: String) fun test() { Foo(nil); }");
    err(
        "class Foo(let a: Int) fun test() { Foo(nil); }",
        pos(1, 39),
        SemError::UnknownCtor("Foo".into(), vec!["nil".into()]),
    );
}

#[test]
fn type_nil_for_local_variable() {
    ok("fun f() { let x: String = nil; }");
    err(
        "fun f() { let x = nil; }",
        pos(1, 11),
        SemError::VarNeedsTypeInfo("x".into()),
    );
    err(
        "fun f() { let x: Int = nil; }",
        pos(1, 11),
        SemError::AssignType("x".into(), "Int".into(), "nil".into()),
    );
}

#[test]
fn type_nil_for_field() {
    ok("class Foo(var a: String) fun f() { Foo(nil).a = nil; }");
    err(
        "class Foo(var a: Int) fun f() { Foo(1).a = nil; }",
        pos(1, 42),
        SemError::AssignField("a".into(), "Foo".into(), "Int".into(), "nil".into()),
    );
}

#[test]
fn type_nil_method() {
    err(
        "fun f() { nil.test(); }",
        pos(1, 19),
        SemError::UnknownMethod("nil".into(), "test".into(), Vec::new()),
    );
}

#[test]
fn type_nil_as_method_argument() {
    ok("class Foo {
            fun f(a: String) {}
        } fun f() { Foo().f(nil); }");
}

#[test]
fn type_array() {
    ok("fun f(a: Array[Int]) -> Int { return a(1); }");
    err(
        "fun f(a: Array[Int]) -> String { return a(1); }",
        pos(1, 34),
        SemError::ReturnType("String".into(), "Int".into()),
    );
}

#[test]
fn type_array_assign() {
    err(
        "fun f(a: Array[Int]) -> Int { return a(3) = 4; }",
        pos(1, 31),
        SemError::ReturnType("Int".into(), "()".into()),
    );
    err(
        "fun f(a: Array[Int]) { a(3) = \"b\"; }",
        pos(1, 29),
        SemError::UnknownMethod(
            "Array[Int]".into(),
            "set".into(),
            vec!["Int".into(), "String".into()],
        ),
    );
}

#[test]
fn let_without_initialization() {
    err(
        "fun f() { let x: Int; }",
        pos(1, 11),
        SemError::LetMissingInitialization,
    );
}

#[test]
fn var_without_initialization() {
    ok("fun f() { var x: Int; }");
}

#[test]
fn reassign_param() {
    err(
        "fun f(a: Int) { a = 1; }",
        pos(1, 19),
        SemError::LetReassigned,
    );
}

#[test]
fn reassign_field() {
    ok("class Foo(var x: Int) fun foo(f: Foo) { f.x = 1; }");
    err(
        "class Foo(let x: Int) fun foo(f: Foo) { f.x = 1; }",
        pos(1, 45),
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
    ok("@open class A class B: A");
    ok("@open class A class B: A()");
    ok("@open class A(a: Int) class B: A(1)");
    err(
        "@open class A(a: Int) class B: A(true)",
        pos(1, 32),
        SemError::UnknownCtor("A".into(), vec!["Bool".into()]),
    );
}

#[test]
fn access_super_class_field() {
    ok("@open class A(var a: Int) class B(x: Int): A(x*2)
            fun foo(b: B) { b.a = b.a + 10; }");
}

#[test]
fn same_names() {
    ok("class Foo { var Foo: Foo = Foo(); }");
    ok("class Foo fun foo() { let Foo: Int = 1; }");
    err(
        "class Foo { var Foo: Foo = Foo(); } module Foo { fun Foo() -> Foo = nil; }",
        pos(1, 37),
        SemError::ShadowClassConstructor("Foo".into()),
    );
}

#[test]
fn check_is() {
    ok("@open class A class B: A
            fun f(a: A) -> Bool { return a is B; }");
    ok("@open class A class B: A
            fun f(b: B) -> Bool { return b is A; }");
    ok("class A
            fun f(a: A) -> Bool { return a is A; }");
    err(
        "@open class A class B: A
             fun f(a: A) -> Bool { return a is String; }",
        pos(2, 45),
        SemError::TypesIncompatible("A".into(), "String".into()),
    );
    err(
        "@open class A class B: A class C
             fun f(a: A) -> Bool { return a is C; }",
        pos(2, 45),
        SemError::TypesIncompatible("A".into(), "C".into()),
    );

    ok("@open class A() class B(): A() fun f() -> A { return B(); }");
    ok("@open class A() class B(): A() fun f() { let a: A = B(); }");
}

#[test]
fn check_as() {
    ok("@open class A class B: A
            fun f(a: A) -> B { return a as B; }");
    ok("class A
            fun f(a: A) -> A { return a as A; }");
    err(
        "@open class A class B: A
             fun f(a: A) -> String { return a as String; }",
        pos(2, 47),
        SemError::TypesIncompatible("A".into(), "String".into()),
    );
    err(
        "@open class A class B: A class C
             fun f(a: A) -> C { return a as C; }",
        pos(2, 42),
        SemError::TypesIncompatible("A".into(), "C".into()),
    );
}

#[test]
fn check_upcast() {
    ok("@open class A class B: A
            fun f(b: B) -> A {
                let a: A = b;
                return a;
                //g(b);
                //return b;
            }

            fun g(a: A) {}");
}

#[test]
fn check_cmp_is() {
    ok("fun f(x: String) {
                let a = nil === x;
                let b = x === nil;
                let c = nil === nil;
            }");
}

#[test]
fn super_delegation() {
    ok("@open class A { fun f() {} }
            class B: A { fun g() {} }

            fun foo(b: B) {
                b.f();
                b.g();
            }");
}

#[test]
fn super_method_call() {
    ok("@open class A { @open fun f() -> Int { return 1; } }
            class B: A { @override fun f() -> Int { return super.f() + 1; } }");
}

#[test]
fn super_as_normal_expression() {
    err(
        "@open class A { }
            class B: A { fun me() { let x = super; } }",
        pos(2, 45),
        SemError::SuperNeedsMethodCall,
    );
}

#[test]
fn struct_lit() {
    ok("struct Foo {} fun foo() -> Foo { return Foo; }");
    ok("struct Foo {} fun foo() { let x = Foo; }");
    ok("struct Foo {} fun foo() { let x: Foo = Foo; }");
    err(
        "struct Foo {} fun foo() { let x: Int = Foo; }",
        pos(1, 27),
        SemError::AssignType("x".into(), "Int".into(), "Foo".into()),
    );
    err(
        "struct Foo {} fun foo() -> Int { return Foo; }",
        pos(1, 34),
        SemError::ReturnType("Int".into(), "Foo".into()),
    );
}

#[test]
fn lit_int64() {
    ok("fun f() -> Int64 { return 1L; }");
    ok("fun f() -> Int { return 1; }");

    let ret = SemError::ReturnType("Int".into(), "Int64".into());
    err("fun f() -> Int { return 1L; }", pos(1, 18), ret);

    let ret = SemError::ReturnType("Int64".into(), "Int".into());
    err("fun f() -> Int64 { return 1; }", pos(1, 20), ret);
}

#[test]
fn overload_plus() {
    ok("class A { fun plus(rhs: A) -> Int { return 0; } }
            fun f() -> Int { return A() + A(); }");
}

#[test]
fn overload_minus() {
    ok("class A { fun minus(rhs: A) -> Int { return 0; } }
            fun f() -> Int { return A() - A(); }");
}

#[test]
fn overload_times() {
    ok("class A { fun times(rhs: A) -> Int { return 0; } }
            fun f() -> Int { return A() * A(); }");
}

#[test]
fn overload_div() {
    ok("class A { fun div(rhs: A) -> Int { return 0; } }
            fun f() -> Int { return A() / A(); }");
}

#[test]
fn overload_mod() {
    ok("class A { fun mod(rhs: A) -> Int { return 0; } }
            fun f() -> Int { return A() % A(); }");
}

#[test]
fn overload_bitwise_or() {
    ok("class A { fun bitwiseOr(rhs: A) -> Int { return 0; } }
            fun f() -> Int { return A() | A(); }");
}

#[test]
fn overload_bitwise_and() {
    ok("class A { fun bitwiseAnd(rhs: A) -> Int { return 0; } }
            fun f() -> Int { return A() & A(); }");
}

#[test]
fn overload_bitwise_xor() {
    ok("class A { fun bitwiseXor(rhs: A) -> Int { return 0; } }
            fun f() -> Int { return A() ^ A(); }");
}

#[test]
fn overload_shl() {
    ok("class A { fun shiftLeft(rhs: A) -> Int { return 0; } }
            fun f() -> Int { return A() << A(); }");
}

#[test]
fn overload_sar() {
    ok(
        "class A { fun shiftRightSigned(rhs: A) -> Int { return 0; } }
            fun f() -> Int { return A() >> A(); }",
    );
}

#[test]
fn overload_shr() {
    ok("class A { fun shiftRight(rhs: A) -> Int { return 0; } }
            fun f() -> Int { return A() >>> A(); }");
}

#[test]
fn overload_equals() {
    ok("class A { fun equals(rhs: A) -> Bool { return true; } }
            fun f1() -> Bool { return A() == A(); }
            fun f2() -> Bool { return A() != A(); }");
}

#[test]
fn overload_compare_to() {
    ok("class A { fun compareTo(rhs: A) -> Int { return 0; } }
            fun f1() -> Bool { return A() < A(); }
            fun f2() -> Bool { return A() <= A(); }
            fun f3() -> Bool { return A() > A(); }
            fun f4() -> Bool { return A() >= A(); }");
}

#[test]
fn int64_operations() {
    ok("fun f(a: Int64, b: Int64) -> Int64 { return a + b; }");
    ok("fun f(a: Int64, b: Int64) -> Int64 { return a - b; }");
    ok("fun f(a: Int64, b: Int64) -> Int64 { return a * b; }");
    ok("fun f(a: Int64, b: Int64) -> Int64 { return a / b; }");
    ok("fun f(a: Int64, b: Int64) -> Int64 { return a % b; }");
    ok("fun f(a: Int64, b: Int64) -> Int64 { return a | b; }");
    ok("fun f(a: Int64, b: Int64) -> Int64 { return a & b; }");
    ok("fun f(a: Int64, b: Int64) -> Int64 { return a ^ b; }");
    ok("fun f(a: Int64, b: Int ) -> Int64 { return a << b; }");
    ok("fun f(a: Int64, b: Int)  -> Int64 { return a >> b; }");
    ok("fun f(a: Int64, b: Int)  -> Int64 { return a >>> b; }");
    ok("fun f(a: Int64, b: Int64) -> Bool { return a == b; }");
    ok("fun f(a: Int64, b: Int64) -> Bool { return a != b; }");
    ok("fun f(a: Int64, b: Int64) -> Bool { return a === b; }");
    ok("fun f(a: Int64, b: Int64) -> Bool { return a !== b; }");
    ok("fun f(a: Int64, b: Int64) -> Bool { return a < b; }");
    ok("fun f(a: Int64, b: Int64) -> Bool { return a <= b; }");
    ok("fun f(a: Int64, b: Int64) -> Bool { return a > b; }");
    ok("fun f(a: Int64, b: Int64) -> Bool { return a >= b; }");
    ok("fun f(a: Int64) -> Int64 { return !a; }");
    ok("fun f(a: Int64) -> Int64 { return -a; }");
    ok("fun f(a: Int64) -> Int64 { return +a; }");
}

#[test]
fn test_literal_int_overflow() {
    err(
        "fun f() { let x = 2147483648; }",
        pos(1, 19),
        SemError::NumberOverflow("Int".into()),
    );
    ok("fun f() { let x = 2147483647; }");
    err(
        "fun f() { let x = -2147483649; }",
        pos(1, 20),
        SemError::NumberOverflow("Int".into()),
    );
    ok("fun f() { let x = -2147483648; }");
}

#[test]
fn test_literal_hex_int_overflow() {
    err(
        "fun f() { let x = 0x1_FF_FF_FF_FF; }",
        pos(1, 19),
        SemError::NumberOverflow("Int".into()),
    );
    ok("fun f() { let x: Int = 0xFF_FF_FF_FF; }");
}

#[test]
fn test_literal_bin_int_overflow() {
    err(
        "fun f() { let x = 0b1_11111111_11111111_11111111_11111111; }",
        pos(1, 19),
        SemError::NumberOverflow("Int".into()),
    );
    ok("fun f() { let x: Int = 0b11111111_11111111_11111111_11111111; }");
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
        SemError::NumberOverflow("Float".into()),
    );
    ok("fun f() { let x = -340282340000000000000000000000000000000F; }");
    err(
        "fun f() { let x = 340282350000000000000000000000000000001F; }",
        pos(1, 19),
        SemError::NumberOverflow("Float".into()),
    );
    ok("fun f() { let x = 340282340000000000000000000000000000000F; }");
}

#[test]
fn test_char() {
    ok("fun foo() -> Char { return 'c'; }");
    ok("fun foo(a: Char) -> Char { return a; }");
    err(
        "fun foo() -> Char { return false; }",
        pos(1, 21),
        SemError::ReturnType("Char".into(), "Bool".into()),
    );
    err(
        "fun foo() -> Char { return 10; }",
        pos(1, 21),
        SemError::ReturnType("Char".into(), "Int".into()),
    );
}

#[test]
fn test_generic_arguments_mismatch() {
    err(
        "class A[T]
            fun foo() {
                let a = A[Int, Int]();
            }",
        pos(3, 36),
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
                let a = A[Int]();
            }",
        pos(3, 31),
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
        "fun f() {} fun g() { f[Int](); }",
        pos(1, 28),
        SemError::WrongNumberTypeParams(0, 1),
    );
    err(
        "fun f[T]() {} fun g() { f(); }",
        pos(1, 26),
        SemError::WrongNumberTypeParams(1, 0),
    );
    ok("fun f[T]() {} fun g() { f[Int](); }");
    ok("fun f[T1, T2]() {} fun g() { f[Int, String](); }");
}

#[test]
fn test_const_check() {
    err(
        "const one: Int = 1;
            fun f() -> Int64 { return one; }",
        pos(2, 32),
        SemError::ReturnType("Int64".into(), "Int".into()),
    );

    err(
        "const one: Int = 1;
            fun f() { let x: String = one; }",
        pos(2, 23),
        SemError::AssignType("x".into(), "String".into(), "Int".into()),
    );
}

#[test]
fn test_const_values() {
    ok_with_test(
        "  const yes: Bool = true;
                        const x: UInt8 = 255Y;
                        const a: Int = 100;
                        const b: Int64 = 200L;
                        const c: Char = 'A';
                        const d: Float = 3.0F;
                        const e: Double = 6.0;",
        |vm| {
            {
                let id = vm.const_by_name("yes");
                let xconst = vm.consts.idx(id);
                let xconst = xconst.lock();
                assert_eq!(ConstValue::Bool(true), xconst.value);
            }

            {
                let id = vm.const_by_name("x");
                let xconst = vm.consts.idx(id);
                let xconst = xconst.lock();
                assert_eq!(ConstValue::Int(255), xconst.value);
            }

            {
                let id = vm.const_by_name("a");
                let xconst = vm.consts.idx(id);
                let xconst = xconst.lock();
                assert_eq!(ConstValue::Int(100), xconst.value);
            }

            {
                let id = vm.const_by_name("b");
                let xconst = vm.consts.idx(id);
                let xconst = xconst.lock();
                assert_eq!(ConstValue::Int(200), xconst.value);
            }

            {
                let id = vm.const_by_name("c");
                let xconst = vm.consts.idx(id);
                let xconst = xconst.lock();
                assert_eq!(ConstValue::Char('A'), xconst.value);
            }

            {
                let id = vm.const_by_name("d");
                let xconst = vm.consts.idx(id);
                let xconst = xconst.lock();
                assert_eq!(ConstValue::Float(3.0), xconst.value);
            }

            {
                let id = vm.const_by_name("e");
                let xconst = vm.consts.idx(id);
                let xconst = xconst.lock();
                assert_eq!(ConstValue::Float(6.0), xconst.value);
            }
        },
    );
}

#[test]
fn test_assignment_to_const() {
    err(
        "const one: Int = 1;
            fun f() { one = 2; }",
        pos(2, 27),
        SemError::AssignmentToConst,
    );
}

#[test]
fn test_unary_minus_byte() {
    err(
        "const m1: UInt8 = -1Y;",
        pos(1, 19),
        SemError::UnOpType("-".into(), "UInt8".into()),
    );
    ok("const m1: Int = -1;");
    ok("const m1: Int64 = -1L;");
}

#[test]
fn test_generic_trait_bounds() {
    ok("trait Foo {}
            class X
            impl Foo for X {}
            class A[T: Foo]
            fun f() -> A[X] { return nil; }");

    err(
        "trait Foo {}
            class X
            class A[T: Foo]
            fun f() -> A[X] { return nil; }",
        pos(4, 24),
        SemError::TraitBoundNotSatisfied("X".into(), "Foo".into()),
    );

    err(
        "trait Foo {}
            fun f[T: Foo]() {}
            fun t() { f[Int](); }",
        pos(3, 29),
        SemError::TraitBoundNotSatisfied("Int".into(), "Foo".into()),
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
            trait Foo { fun foo(a: Int); }
            impl Foo for A { fun foo(a:  Int) {} }
            fun test(a: A) { a.foo(1); }",
        pos(4, 35),
        SemError::ParamTypesIncompatible("foo".into(), Vec::new(), vec!["Int".into()]),
    );

    ok("class A { @static fun foo() {} }
            trait Foo { fun foo(a: Int); }
            impl Foo for A { fun foo(a:  Int) {} }
            fun test(a: A) { a.foo(1); }");
}

#[test]
fn test_invoke_abstract_class_ctor() {
    err(
        "@abstract class A
            fun test() -> A { return A(); }",
        pos(2, 39),
        SemError::NewAbstractClass,
    );
}

#[test]
fn test_global_get() {
    ok("var x: Int; fun foo() -> Int { return x; }");
}

#[test]
fn test_global_set() {
    ok("var x: Int; fun foo(a: Int) { x = a; }");
    err(
        "let x: Int; fun foo(a: Int) { x = a; }",
        pos(1, 33),
        SemError::LetReassigned,
    );
}

#[test]
fn lambda_assignment() {
    ok("fun f() { let x = || {}; }");
    ok("fun f() { let x = || -> Int { return 2; }; }");
    ok("fun f() { let x: () -> () = || {}; }");
    ok("fun f() { let x: () -> () = || -> () {}; }");
    ok("fun f() { let x: () -> Int = || -> Int { return 2; }; }");
    err(
        "fun f() { let x: () -> Int = || {}; }",
        pos(1, 11),
        SemError::AssignType("x".into(), "() -> Int".into(), "() -> ()".into()),
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
        "fun f[X: Comparable](x: X) {}
            fun g[T](t: T) { f[T](t); }",
        pos(2, 34),
        SemError::TraitBoundNotSatisfied("T".into(), "Comparable".into()),
    );
}

#[test]
fn test_for_supports_make_iterator() {
    err(
        "fun f() { for i in 1 {} }",
        pos(1, 11),
        SemError::UnknownMethod("Int".into(), "makeIterator".into(), Vec::new()),
    );

    err(
        "
            class Foo() { fun makeIterator() -> Bool { return true; } }
            fun f() { for i in Foo() {} }",
        pos(3, 35),
        SemError::MakeIteratorReturnType("Bool".into()),
    );

    ok(
        "class Foo { fun makeIterator() -> FooIter { return FooIter(); } }
            class FooIter
            impl Iterator for FooIter {
                fun hasNext() -> Bool { return false; }
                fun next() -> Int { return 0; }
            }
            fun f() -> Int { for i in Foo() { return i; } return 0; }",
    );
}

#[test]
fn test_ctor_with_type_param() {
    err(
        "
            class Foo[T] {
                fun foo(a: Int) {
                    Bar[T](a);
                }
            }

            class Bar[T](a: T)
            ",
        pos(4, 27),
        SemError::UnknownCtor("Bar".into(), vec!["Int".into()]),
    );
}

#[test]
fn test_fct_used_as_identifier() {
    err(
        "fun foo() {} fun bar() { foo; }",
        pos(1, 26),
        SemError::FctUsedAsIdentifier,
    );
}

#[test]
fn test_cls_used_as_identifier() {
    err(
        "class X fun f() { X; }",
        pos(1, 19),
        SemError::ClsUsedAsIdentifier,
    );
}

#[test]
fn test_assign_fct() {
    err(
        "fun foo() {} fun bar() { foo = 1; }",
        pos(1, 30),
        SemError::FctReassigned,
    );
}

#[test]
fn test_assign_class() {
    err(
        "class X fun foo() { X = 2; }",
        pos(1, 23),
        SemError::ClassReassigned,
    );
}

#[test]
fn test_new_call_fct() {
    ok("fun g() {} fun f() { g(); }");
}

#[test]
fn test_new_call_fct_wrong_params() {
    err(
        "fun g() {} fun f() { g(1); }",
        pos(1, 23),
        SemError::ParamTypesIncompatible("g".into(), Vec::new(), vec!["Int".into()]),
    );
}

#[test]
fn test_new_call_fct_with_type_params() {
    ok("fun g[T]() {} fun f() { g[Int](); }");
}

#[test]
fn test_new_call_fct_with_wrong_type_params() {
    err(
        "fun g() {} fun f() { g[Int](); }",
        pos(1, 28),
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
            fun f() { Foo::bar(1); }",
        pos(2, 31),
        SemError::ParamTypesIncompatible("bar".into(), Vec::new(), vec!["Int".into()]),
    );
}

#[test]
fn test_new_call_static_method_type_params() {
    ok("class Foo { @static fun bar[T]() {} }
            fun f() { Foo::bar[Int](); }");
}

#[test]
fn test_new_call_class() {
    ok("class X fun f() { X(); }");
}

#[test]
fn test_new_call_class_wrong_params() {
    err(
        "class X fun f() { X(1); }",
        pos(1, 20),
        SemError::UnknownCtor("X".into(), vec!["Int".into()]),
    );
}

#[test]
fn test_new_call_class_with_type_params() {
    ok("class X[T] fun f() { X[Int](); }");
}

#[test]
fn test_new_call_class_with_wrong_type_params() {
    err(
        "class X fun f() { X[Int](); }",
        pos(1, 25),
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
            fun f(x: X) { x.f[Int](); }");
}

#[test]
fn test_new_call_method_wrong_params() {
    err(
        "class X { fun f() {} } fun f(x: X) { x.f(1); }",
        pos(1, 41),
        SemError::ParamTypesIncompatible("f".into(), Vec::new(), vec!["Int".into()]),
    );
}

#[test]
fn test_new_call_method_generic() {
    ok("fun f[T: Hash](t: T) { t.hash(); }");
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
    ok("fun f(t: Array[Int]) -> Int { return t(0); }");
}

#[test]
fn test_array_syntax_set() {
    ok("fun f(t: Array[Int]){ t(0) = 10; }");
}

#[test]
fn test_array_syntax_set_wrong_value() {
    err(
        "fun f(t: Array[Int]){ t(0) = true; }",
        pos(1, 28),
        SemError::UnknownMethod(
            "Array[Int]".into(),
            "set".into(),
            vec!["Int".into(), "Bool".into()],
        ),
    );
}

#[test]
fn test_array_syntax_set_wrong_index() {
    err(
        "fun f(t: Array[Int]){ t(\"bla\") = 9; }",
        pos(1, 32),
        SemError::UnknownMethod(
            "Array[Int]".into(),
            "set".into(),
            vec!["String".into(), "Int".into()],
        ),
    );
}

#[test]
fn test_template() {
    ok("fun f(x: Int) -> String { return \"x = ${x}\"; }");
    err(
        "class Foo fun f(x: Foo) -> String { return \"x = ${x}\"; }",
        pos(1, 51),
        SemError::ExpectedStringable("Foo".into()),
    );
    ok("fun f[T: Stringable](x: T) -> String { return \"${x}\"; }");
}

#[test]
fn test_trait_object_as_argument() {
    ok("trait Foo { fun bar() -> Int; }
        fun f(x: Foo) -> Int { return x.bar(); }");
    err(
        "trait Foo { fun baz(); }
        fun f(x: Foo) -> String { return x.baz(); }",
        pos(2, 35),
        SemError::ReturnType("String".into(), "()".into()),
    );
}

#[test]
fn test_type_param_used_as_value() {
    err(
        "fun f[T]() -> Int { return T; }",
        pos(1, 28),
        SemError::TypeParamUsedAsIdentifier,
    );

    err(
        "class SomeClass[T] {
            fun f() -> Int { return T; }
        }",
        pos(2, 37),
        SemError::TypeParamUsedAsIdentifier,
    );
}

#[test]
fn test_assign_to_type_param() {
    err(
        "fun f[T]() { T = 10; }",
        pos(1, 16),
        SemError::TypeParamReassigned,
    );

    err(
        "class SomeClass[T] {
            fun f() { T = 10; }
        }",
        pos(2, 25),
        SemError::TypeParamReassigned,
    );
}

#[test]
fn test_type_param_with_name_but_no_call() {
    err(
        "trait X { fun foo() -> Int; }
        fun f[T: X]() { T::foo; }",
        pos(2, 26),
        SemError::FctUsedAsIdentifier,
    );

    err(
        "trait X { fun foo() -> Int; }
        class SomeClass[T: X] {
            fun f() { T::foo; }
        }",
        pos(3, 24),
        SemError::FctUsedAsIdentifier,
    );
}

#[test]
fn test_type_param_call() {
    err(
        "trait X { fun foo() -> Int; }
        fun f[T: X]() { T(); }",
        pos(2, 25),
        SemError::TypeParamUsedAsCallee,
    );

    err(
        "trait X { fun foo() -> Int; }
        class SomeClass[T: X] {
            fun f() { T(); }
        }",
        pos(3, 23),
        SemError::TypeParamUsedAsCallee,
    );
}

#[test]
fn test_static_method_call_with_type_param() {
    err(
        "trait X { @static fun bar() -> Int; }
        fun f[T: X]() { T::foo(); }",
        pos(2, 31),
        SemError::UnknownStaticMethodWithTypeParam,
    );

    err(
        "trait X { @static fun foo() -> Int; }
        trait Y { @static fun foo() -> String; }
        fun f[T: X + Y]() { T::foo(); }",
        pos(3, 35),
        SemError::MultipleCandidatesForStaticMethodWithTypeParam,
    );

    err(
        "trait X { @static fun foo() -> Int; }
        fun f[T: X]() -> Int { return T::foo(1); }",
        pos(2, 45),
        SemError::ParamTypesIncompatible("foo".into(), Vec::new(), vec!["Int".into()]),
    );

    ok("trait X { @static fun foo() -> Int; }
        fun f[T: X]() -> Int { return T::foo(); }");
}

#[test]
fn test_type_param_with_let() {
    ok("fun myid[T](val: T) -> T {
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
        fun t1[Y](x: X, y: Y) -> Y { return y; }
        fun t2[Y](x: X, y: Y) -> X { return x; }
    }

    fun t1(a: A[Int]) -> String {
        return a.t1[String](1, \"bla\");
    }

    fun t2(a: A[Int]) -> Int {
        return a.t2[String](1, \"bla\");
    }
    ");
}

#[test]
fn test_subtyping() {
    ok("
    @open class A class B: A
    class Test {
        fun foo(a: A) {}
    }
    fun bar(t: Test) { t.foo(B()); }
    ");
}

#[test]
fn test_enum() {
    ok("enum A { V1, V2 }");
    ok("enum A { V1, V2 } fun f(a: A) -> A { return a; }");
    ok("enum A { V1, V2 } fun f() -> A { return A::V1; }");

    ok("enum A { V1, V2 } fun f() -> Bool { return A::V1 == A::V2; }");
    ok("enum A { V1, V2 } fun f() -> Bool { return A::V1 != A::V2; }");

    err(
        "enum A { V1 } fun f() -> A { return A; }",
        pos(1, 37),
        SemError::EnumUsedAsIdentifier,
    );

    err(
        "enum A { V1 } fun f() { A = 1; }",
        pos(1, 27),
        SemError::InvalidLhsAssignment,
    );

    err(
        "enum A { V1, V2 } fun f() -> A { return A::V3; }",
        pos(1, 42),
        SemError::UnknownEnumValue("V3".into()),
    );
}

#[test]
fn test_block_value() {
    ok("fun f() -> Int { 1 }");
    ok("fun f() { let x = { 1 }; }");
    ok("fun g() -> Int { return 1; } fun f() { let x: Int = { g() }; }");
    ok("fun g() -> Int { return 1; } fun f() { let x: Int = { g(); 1 }; }");
}

#[test]
fn test_if_expression() {
    ok("fun f() -> Int { if true { 1 } else { 2 } }");
    ok("fun f() -> Float { if true { 1.0F } else { 2.0F } }");
    ok("fun f() -> Double { if true { 1.0 } else { 2.0 } }");

    ok("fun f() -> Int { 4 * if true { 1 } else { 2 } }");
}

#[test]
fn test_tuple() {
    ok("fun f(a: (Int, Bool)) {}");
    ok("fun f(a: (Int, Bool)) -> (Int, Bool) { return a; }");
    ok("fun f(a: (Int, Bool)) -> (Int, Bool) {
            let tmp = a;
            return tmp;
        }");
    err(
        "fun f(a: (Int, Bool)) -> (Int) { return a; }",
        pos(1, 34),
        SemError::ReturnType("(Int)".into(), "(Int, Bool)".into()),
    );
    err(
        "fun f(a: (Int, Bool)) -> (Int, Float) { return a; }",
        pos(1, 41),
        SemError::ReturnType("(Int, Float)".into(), "(Int, Bool)".into()),
    );
}

#[test]
fn test_tuple_literal() {
    ok("fun f() -> (Int, Bool) {
        return (1, false);
    }");

    err(
        "fun f() -> (Int) {
        return (1);
    }",
        pos(2, 9),
        SemError::ReturnType("(Int)".into(), "Int".into()),
    );

    err(
        "fun f() -> (Int, Int) {
        return (1, false);
    }",
        pos(2, 9),
        SemError::ReturnType("(Int, Int)".into(), "(Int, Bool)".into()),
    );
}

#[test]
fn test_tuple_in_call() {
    ok("
        fun f(a: (Int, Bool)) {}
        fun g() {
            f((1, true));
        }
    ")
}

#[test]
fn test_tuple_element() {
    ok("
        fun f(a: (Int, Bool)) -> Int {
            return a.0;
        }
    ");

    ok("
        fun f(a: (Int, Bool)) -> Bool {
            return a.1;
        }
    ");

    err(
        "
        fun f(a: (Int, Bool)) -> String {
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
        class Bar: Foo[Int](10)
    ");

    err(
        "
        @open class Foo[A](let a: A)
        class Bar: Foo(10)
    ",
        pos(3, 20),
        SemError::WrongNumberTypeParams(1, 0),
    );

    ok("
        @open class Foo[A](let a: A)
        class Bar[A](x: A): Foo[A](x)
    ");
}

#[test]
fn test_fields_with_generics() {
    ok("
        @open @abstract class Foo[A](var a: A)
        @open class Bar[A]: Foo[Int](10)
        class Baz[A]: Bar[A] {
            fun test() -> Int { self.a }
            fun assignMe() { self.a = 10; }
        }
    ");
}

#[test]
fn test_methods_with_generics() {
    ok("
        @open @abstract class Foo[A] {
            @abstract fun test() -> A;
        }

        class Bar[A](let bar: A): Foo[A] {
            @override fun test() -> A { self.bar }
        }

        class Baz[A](let baz: A): Foo[Int] {
            @override fun test() -> Int { 0 }
        }
    ");

    ok("
        @open @abstract class Foo[A] {
            @open fun test(x: A) -> A { x }
        }

        class Bar[A](let bar: A): Foo[A] {
            @override fun test(x: A) -> A { self.bar }
        }

        class Baz[A](let baz: A): Foo[Int] {
            @override fun test(x: Int) -> Int { x+x }
        }
    ");
}

#[test]
fn test_is_types() {
    err(
        "
        trait SomeTrait {}
        class Foo[A: SomeTrait] {}
        fun test(f: Object) -> Bool {
            return f is Foo[Int];
        }
    ",
        pos(5, 25),
        SemError::TraitBoundNotSatisfied("Int".into(), "SomeTrait".into()),
    );
}

#[test]
fn test_type_params_with_bounds_in_subclass() {
    err(
        "
        trait SomeTrait {}
        @open class Foo[A: SomeTrait]
        class Bar: Foo[Int]
    ",
        pos(4, 20),
        SemError::TraitBoundNotSatisfied("Int".into(), "SomeTrait".into()),
    );
}

#[test]
fn test_type_params_with_bounds_in_subclass_wrong_order() {
    err(
        "
        trait SomeTrait {}
        class Bar: Foo[Int]
        @open class Foo[A: SomeTrait]
    ",
        pos(3, 20),
        SemError::TraitBoundNotSatisfied("Int".into(), "SomeTrait".into()),
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
        pos(4, 13),
        SemError::UnknownMethod("Foo".into(), "makeIterator".into(), vec![]),
    );
}

#[test]
fn test_type_make_iterator_not_implementing_iterator() {
    err(
        "
        class Foo {
            fun makeIterator() -> Int { 0 }
        }
        fun bar(x: Foo) {
            for i in x {
                let x: Foo = i;
            }
        }
    ",
        pos(6, 22),
        SemError::MakeIteratorReturnType("Int".into()),
    );
}

#[test]
fn test_incompatible_branches() {
    err(
        "
        class Foo
        fun bar() -> Int {
            if true { 1 } else { Foo(1) }
        }
    ",
        pos(4, 37),
        SemError::UnknownCtor("Foo".into(), vec!["Int".into()]),
    );
}

#[test]
fn zero_trait_ok() {
    ok("fun f() { arrayZero[Int](12); }");
}

#[test]
fn zero_trait_err() {
    err(
        "fun f() { arrayZero[String](12); }",
        pos(1, 28),
        SemError::TraitBoundNotSatisfied("String".into(), "Zero".into()),
    );
}

#[test]
fn extension_method_call() {
    ok("
        class Foo(let value: Int)
        impl Foo { fun foo() -> Int { self.value } }
        fun bar(x: Foo) -> Int { x.foo() }
    ");
}

#[test]
fn impl_class_type_params() {
    err(
        "
        trait MyTrait { fun bar(); }
        class Foo[T]
        impl MyTrait for Foo[String] { fun bar() {} }
        fun bar(x: Foo[Int]) { x.bar(); }
    ",
        pos(5, 37),
        SemError::UnknownMethod("Foo[Int]".into(), "bar".into(), Vec::new()),
    );
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
    ok("fun f() -> UInt8 { 1 }");
    // ok("fun f(val: UInt8) {} fun g() { f(1); }");
    err(
        "fun f() -> UInt8 { 256 }",
        pos(1, 20),
        SemError::NumberOverflow("UInt8".into()),
    );
    ok("fun f() { let x: UInt8 = 1; }");
}

#[test]
fn literal_without_suffix_long() {
    ok("fun f() -> Int64 { 1 }");
    ok("fun f() { let x: Int64 = 1; }");
}

#[test]
fn variadic_parameter() {
    ok("
        fun f(x: Int...) -> Int {
            x.length()
        }
        fun g() {
            f(1, 2, 3, 4);
            f();
            f(1);
        }
    ");
    err(
        "
        fun f(x: Int...) {}
        fun g() {
            f(true);
        }
    ",
        pos(4, 14),
        SemError::ParamTypesIncompatible("f".into(), vec!["Int".into()], vec!["Bool".into()]),
    );
    ok("
        fun f(x: Int, y: Int...) {}
        fun g() {
            f(1, 2, 3, 4);
            f(1, 2);
            f(1);
        }
    ");
    err(
        "
        fun f(x: Int, y: Int...) {}
        fun g() {
            f();
        }
    ",
        pos(4, 14),
        SemError::ParamTypesIncompatible("f".into(), vec!["Int".into(), "Int".into()], Vec::new()),
    );
    err(
        "fun f(x: Int..., y: Int) {}",
        pos(1, 18),
        SemError::VariadicParameterNeedsToBeLast,
    );
}

#[test]
fn for_with_array() {
    ok("fun f(x: Array[Int]) -> Int {
        var result = 0;
        for i in x {
            result = result + i;
        }
        result
    }");

    ok("fun f(x: Array[Float]) -> Float {
        var result = 0.0F;
        for i in x {
            result = result + i;
        }
        result
    }");
}
