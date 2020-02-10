use std::collections::HashMap;
use std::mem;

use self::Bytecode::*;
use crate::bytecode::{
    self, BytecodeFunction, BytecodeOffset, BytecodeVisitor, ConstPoolIdx, Register,
};
use crate::test;
use crate::ty::TypeList;
use crate::vm::{ClassDefId, FctId, FieldId, GlobalId, VM};
use dora_parser::lexer::position::Position;

fn code(code: &'static str) -> Vec<Bytecode> {
    test::parse(code, |vm| {
        let fct_id = vm.fct_by_name("f").expect("no function `f`.");
        let tp = TypeList::empty();
        let fct = bytecode::generate_fct(vm, fct_id, &tp, &tp);
        build(&fct)
    })
}

fn position(code: &'static str) -> Vec<(u32, Position)> {
    test::parse(code, |vm| {
        let fct_id = vm.fct_by_name("f").expect("no function `f`.");
        let tp = TypeList::empty();
        let fct = bytecode::generate_fct(vm, fct_id, &tp, &tp);
        fct.positions().to_vec()
    })
}

fn code_method(code: &'static str) -> Vec<Bytecode> {
    code_method_with_class_name(code, "Foo")
}

fn code_method_with_class_name(code: &'static str, class_name: &'static str) -> Vec<Bytecode> {
    test::parse(code, |vm| {
        let fct_id = vm
            .cls_method_by_name(class_name, "f", false)
            .unwrap_or_else(|| panic!("no function `f` in Class `{}`.", class_name));
        let tp = TypeList::empty();
        let fct = bytecode::generate_fct(vm, fct_id, &tp, &tp);
        build(&fct)
    })
}

fn gen<F>(code: &'static str, testfct: F)
where
    F: FnOnce(&VM, Vec<Bytecode>),
{
    test::parse(code, |vm| {
        let fct_id = vm.fct_by_name("f").expect("no function `f`.");
        let tp = TypeList::empty();
        let fct = bytecode::generate_fct(vm, fct_id, &tp, &tp);
        let code = build(&fct);

        testfct(vm, code);
    })
}

fn gen_fct<F>(code: &'static str, testfct: F)
where
    F: FnOnce(&VM, Vec<Bytecode>, BytecodeFunction),
{
    test::parse(code, |vm| {
        let fct_id = vm.fct_by_name("f").expect("no function `f`.");
        let tp = TypeList::empty();
        let fct = bytecode::generate_fct(vm, fct_id, &tp, &tp);
        let code = build(&fct);

        testfct(vm, code, fct);
    })
}

#[test]
fn gen_load_field_byte() {
    gen(
        "class Foo(let bar: Byte) fun f(a: Foo) -> Byte { return a.bar; }",
        |vm, code| {
            let (cls, field) = vm.field_by_name("Foo", "bar");
            let expected = vec![LoadFieldByte(r(1), r(0), cls, field), RetByte(r(1))];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_load_field_bool() {
    gen(
        "class Foo(let bar: Bool) fun f(a: Foo) -> Bool { return a.bar; }",
        |vm, code| {
            let (cls, field) = vm.field_by_name("Foo", "bar");
            let expected = vec![LoadFieldBool(r(1), r(0), cls, field), RetBool(r(1))];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_load_field_char() {
    gen(
        "class Foo(let bar: Char) fun f(a: Foo) -> Char { return a.bar; }",
        |vm, code| {
            let (cls, field) = vm.field_by_name("Foo", "bar");
            let expected = vec![LoadFieldChar(r(1), r(0), cls, field), RetChar(r(1))];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_load_field_int() {
    gen(
        "class Foo(let bar: Int) fun f(a: Foo) -> Int { return a.bar; }",
        |vm, code| {
            let (cls, field) = vm.field_by_name("Foo", "bar");
            let expected = vec![LoadFieldInt(r(1), r(0), cls, field), RetInt(r(1))];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_load_field_long() {
    gen(
        "class Foo(let bar: Long) fun f(a: Foo) -> Long { return a.bar; }",
        |vm, code| {
            let (cls, field) = vm.field_by_name("Foo", "bar");
            let expected = vec![LoadFieldLong(r(1), r(0), cls, field), RetLong(r(1))];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_load_field_float() {
    gen(
        "class Foo(let bar: Float) fun f(a: Foo) -> Float { return a.bar; }",
        |vm, code| {
            let (cls, field) = vm.field_by_name("Foo", "bar");
            let expected = vec![LoadFieldFloat(r(1), r(0), cls, field), RetFloat(r(1))];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_load_field_double() {
    gen(
        "class Foo(let bar: Double) fun f(a: Foo) -> Double { return a.bar; }",
        |vm, code| {
            let (cls, field) = vm.field_by_name("Foo", "bar");
            let expected = vec![LoadFieldDouble(r(1), r(0), cls, field), RetDouble(r(1))];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_load_field_ptr() {
    gen(
        "class Foo(let bar: Object) fun f(a: Foo) -> Object { return a.bar; }",
        |vm, code| {
            let (cls, field) = vm.field_by_name("Foo", "bar");
            let expected = vec![LoadFieldPtr(r(1), r(0), cls, field), RetPtr(r(1))];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_position_load_field_byte() {
    let result = position("class Foo(let bar: Byte) fun f(a: Foo) -> Byte { return a.bar; }");
    let expected = vec![(0, p(1, 58))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_load_field_bool() {
    let result = position("class Foo(let bar: Bool) fun f(a: Foo) -> Bool { return a.bar; }");
    let expected = vec![(0, p(1, 58))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_load_field_char() {
    let result = position("class Foo(let bar: Char) fun f(a: Foo) -> Char { return a.bar; }");
    let expected = vec![(0, p(1, 58))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_load_field_int() {
    let result = position("class Foo(let bar: Int) fun f(a: Foo) -> Int { return a.bar; }");
    let expected = vec![(0, p(1, 56))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_load_field_long() {
    let result = position("class Foo(let bar: Long) fun f(a: Foo) -> Long { return a.bar; }");
    let expected = vec![(0, p(1, 58))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_load_field_float() {
    let result = position("class Foo(let bar: Float) fun f(a: Foo) -> Float { return a.bar; }");
    let expected = vec![(0, p(1, 60))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_load_field_double() {
    let result = position("class Foo(let bar: Double) fun f(a: Foo) -> Double { return a.bar; }");
    let expected = vec![(0, p(1, 62))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_load_field_ptr() {
    let result = position("class Foo(let bar: Object) fun f(a: Foo) -> Object { return a.bar; }");
    let expected = vec![(0, p(1, 62))];
    assert_eq!(expected, result);
}

#[test]
fn gen_store_field_byte() {
    gen(
        "class Foo(var bar: Byte) fun f(a: Foo, b: Byte) { a.bar = b; }",
        |vm, code| {
            let (cls, field) = vm.field_by_name("Foo", "bar");
            let expected = vec![StoreFieldByte(r(1), r(0), cls, field), RetVoid];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_store_field_bool() {
    gen(
        "class Foo(var bar: Bool) fun f(a: Foo, b: Bool) { a.bar = b; }",
        |vm, code| {
            let (cls, field) = vm.field_by_name("Foo", "bar");
            let expected = vec![StoreFieldBool(r(1), r(0), cls, field), RetVoid];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_store_field_char() {
    gen(
        "class Foo(var bar: Char) fun f(a: Foo, b: Char) { a.bar = b; }",
        |vm, code| {
            let (cls, field) = vm.field_by_name("Foo", "bar");
            let expected = vec![StoreFieldChar(r(1), r(0), cls, field), RetVoid];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_store_field_int() {
    gen(
        "class Foo(var bar: Int) fun f(a: Foo, b: Int) { a.bar = b; }",
        |vm, code| {
            let (cls, field) = vm.field_by_name("Foo", "bar");
            let expected = vec![StoreFieldInt(r(1), r(0), cls, field), RetVoid];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_store_field_long() {
    gen(
        "class Foo(var bar: Long) fun f(a: Foo, b: Long) { a.bar = b; }",
        |vm, code| {
            let (cls, field) = vm.field_by_name("Foo", "bar");
            let expected = vec![StoreFieldLong(r(1), r(0), cls, field), RetVoid];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_store_field_float() {
    gen(
        "class Foo(var bar: Float) fun f(a: Foo, b: Float) { a.bar = b; }",
        |vm, code| {
            let (cls, field) = vm.field_by_name("Foo", "bar");
            let expected = vec![StoreFieldFloat(r(1), r(0), cls, field), RetVoid];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_store_field_double() {
    gen(
        "class Foo(var bar: Double) fun f(a: Foo, b: Double) { a.bar = b; }",
        |vm, code| {
            let (cls, field) = vm.field_by_name("Foo", "bar");
            let expected = vec![StoreFieldDouble(r(1), r(0), cls, field), RetVoid];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_store_field_ptr() {
    gen(
        "class Foo(var bar: Object) fun f(a: Foo, b: Object) { a.bar = b; }",
        |vm, code| {
            let (cls, field) = vm.field_by_name("Foo", "bar");
            let expected = vec![StoreFieldPtr(r(1), r(0), cls, field), RetVoid];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_position_store_field_byte() {
    let result = position("class Foo(var bar: Byte) fun f(a: Foo, b: Byte) { a.bar = b; }");
    let expected = vec![(0, p(1, 57))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_store_field_bool() {
    let result = position("class Foo(var bar: Bool) fun f(a: Foo, b: Bool) { a.bar = b; }");
    let expected = vec![(0, p(1, 57))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_store_field_char() {
    let result = position("class Foo(var bar: Char) fun f(a: Foo, b: Char) { a.bar = b; }");
    let expected = vec![(0, p(1, 57))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_store_field_int() {
    let result = position("class Foo(var bar: Int) fun f(a: Foo, b: Int) { a.bar = b; }");
    let expected = vec![(0, p(1, 55))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_store_field_long() {
    let result = position("class Foo(var bar: Long) fun f(a: Foo, b: Long) { a.bar = b; }");
    let expected = vec![(0, p(1, 57))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_store_field_float() {
    let result = position("class Foo(var bar: Float) fun f(a: Foo, b: Float) { a.bar = b; }");
    let expected = vec![(0, p(1, 59))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_store_field_double() {
    let result = position("class Foo(var bar: Double) fun f(a: Foo, b: Double) { a.bar = b; }");
    let expected = vec![(0, p(1, 61))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_store_field_ptr() {
    let result = position("class Foo(var bar: Object) fun f(a: Foo, b: Object) { a.bar = b; }");
    let expected = vec![(0, p(1, 61))];
    assert_eq!(expected, result);
}

#[test]
fn gen_add_int() {
    let result = code("fun f() -> Int { return 1 + 2; }");
    let expected = vec![
        ConstInt(r(1), 1),
        ConstInt(r(2), 2),
        AddInt(r(0), r(1), r(2)),
        RetInt(r(0)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_add_float() {
    let result = code("fun f() -> Float { return 1F + 2F; }");
    let expected = vec![
        ConstFloat(r(1), 1_f32),
        ConstFloat(r(2), 2_f32),
        AddFloat(r(0), r(1), r(2)),
        RetFloat(r(0)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_id_int() {
    let result = code("fun f(a: Int) -> Int { return a; }");
    let expected = vec![RetInt(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_id_ptr() {
    let result = code("fun f(a: Object) -> Object { return a; }");
    let expected = vec![RetPtr(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_ptr_is() {
    let result = code("fun f(a: Object, b: Object) -> Bool { return a === b; }");
    let expected = vec![TestEqPtr(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_ptr_is_not() {
    let result = code("fun f(a: Object, b: Object) -> Bool { return a !== b; }");
    let expected = vec![TestNePtr(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_sub_int() {
    let result = code("fun f(a: Int, b: Int) -> Int { return a - b; }");
    let expected = vec![SubInt(r(2), r(0), r(1)), RetInt(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_sub_float() {
    let result = code("fun f(a: Float, b: Float) -> Float { return a - b; }");
    let expected = vec![SubFloat(r(2), r(0), r(1)), RetFloat(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_div_int() {
    let result = code("fun f(a: Int, b: Int) -> Int { return a / b; }");
    let expected = vec![DivInt(r(2), r(0), r(1)), RetInt(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_div_int() {
    let result = position("fun f(a: Int, b: Int) -> Int { return a / b; }");
    let expected = vec![(0, p(1, 41))];
    assert_eq!(expected, result);
}

#[test]
fn gen_div_float() {
    let result = code("fun f(a: Float, b: Float) -> Float { return a / b; }");
    let expected = vec![DivFloat(r(2), r(0), r(1)), RetFloat(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_mul_int() {
    let result = code("fun f(a: Int, b: Int) -> Int { return a * b; }");
    let expected = vec![MulInt(r(2), r(0), r(1)), RetInt(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_mul_float() {
    let result = code("fun f(a: Float, b: Float) -> Float { return a * b; }");
    let expected = vec![MulFloat(r(2), r(0), r(1)), RetFloat(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_stmt_var_init() {
    let result = code("fun f() { let x = 1; }");
    let expected = vec![ConstInt(r(0), 1), RetVoid];
    assert_eq!(expected, result);
}

#[test]
fn gen_stmt_while() {
    let result = code("fun f() { while true { 0; } }");
    let code = vec![ConstTrue(r(0)), JumpIfFalse(r(0), 3), JumpLoop(0), RetVoid];
    assert_eq!(code, result);
}

#[test]
fn gen_stmt_if() {
    let result = code("fun f(a: Bool) -> Int { if a { return 1; } return 0; }");
    let expected = vec![
        JumpIfFalse(r(0), 3),
        ConstInt(r(1), 1),
        RetInt(r(1)),
        ConstZeroInt(r(2)),
        RetInt(r(2)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_stmt_if_else() {
    let result = code("fun f(a: Bool) -> Int { if a { return 1; } else { return 2; } }");
    let expected = vec![
        JumpIfFalse(r(0), 4),
        ConstInt(r(1), 1),
        RetInt(r(1)),
        Jump(6),
        ConstInt(r(2), 2),
        RetInt(r(2)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_stmt_break() {
    let result = code("fun f() { while true { break; } }");
    let expected = vec![
        ConstTrue(r(0)),
        JumpIfFalse(r(0), 4),
        Jump(4),
        JumpLoop(0),
        RetVoid,
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_stmt_continue() {
    let result = code("fun f() { while true { continue; } }");
    let expected = vec![
        ConstTrue(r(0)),
        JumpIfFalse(r(0), 4),
        JumpLoop(0),
        JumpLoop(0),
        RetVoid,
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_nil() {
    let result = code("fun f() -> Object { return nil; }");
    let expected = vec![ConstNil(r(0)), RetPtr(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_char() {
    let result = code("fun f() -> Char { return '1'; }");
    let expected = vec![ConstChar(r(0), '1'), RetChar(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_int() {
    let result = code("fun f() -> Int { return 1; }");
    let expected = vec![ConstInt(r(0), 1), RetInt(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_byte() {
    let result = code("fun f() -> Byte { return 1Y; }");
    let expected = vec![ConstByte(r(0), 1), RetByte(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_long() {
    let result = code("fun f() -> Long { return 1L; }");
    let expected = vec![ConstLong(r(0), 1), RetLong(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_float() {
    let result = code("fun f() -> Float { return 1F; }");
    let expected = vec![ConstFloat(r(0), 1_f32), RetFloat(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_double() {
    let result = code("fun f() -> Double { return 1D; }");
    let expected = vec![ConstDouble(r(0), 1_f64), RetDouble(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_string() {
    let result = code("fun f() -> String { return \"z\"; }");
    let expected = vec![ConstString(r(0), "z".to_string()), RetPtr(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_string_duplicate() {
    let result = code("fun f() { let a = \"z\"; let b = \"z\"; }");
    let expected = vec![
        ConstString(r(0), "z".to_string()),
        ConstString(r(1), "z".to_string()),
        RetVoid,
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_string_multiple() {
    let result = code("fun f() { let a = \"z\"; let b = \"y\"; }");
    let expected = vec![
        ConstString(r(0), "z".to_string()),
        ConstString(r(1), "y".to_string()),
        RetVoid,
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_byte_zero() {
    let result = code("fun f() -> Byte { return 0Y; }");
    let expected = vec![ConstZeroByte(r(0)), RetByte(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_int_zero() {
    let result = code("fun f() -> Int { return 0; }");
    let expected = vec![ConstZeroInt(r(0)), RetInt(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_long_zero() {
    let result = code("fun f() -> Long { return 0L; }");
    let expected = vec![ConstZeroLong(r(0)), RetLong(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_float_zero() {
    let result = code("fun f() -> Float { return 0F; }");
    let expected = vec![ConstZeroFloat(r(0)), RetFloat(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_double_zero() {
    let result = code("fun f() -> Double { return 0D; }");
    let expected = vec![ConstZeroDouble(r(0)), RetDouble(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_or() {
    let result = code("fun f(a: Bool, b: Bool) -> Bool { return a || b; }");
    let expected = vec![
        MovBool(r(2), r(0)),
        JumpIfTrue(r(2), 3),
        MovBool(r(2), r(1)),
        RetBool(r(2)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_and() {
    let result = code("fun f(a: Bool, b: Bool) -> Bool { return a && b; }");
    let expected = vec![
        MovBool(r(2), r(0)),
        JumpIfFalse(r(2), 3),
        MovBool(r(2), r(1)),
        RetBool(r(2)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_plus() {
    let result = code("fun f(a: Int) -> Int { return +a; }");
    let expected = vec![RetInt(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_neg() {
    let result = code("fun f(a: Int) -> Int { return -a; }");
    let expected = vec![NegInt(r(1), r(0)), RetInt(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_not() {
    let result = code("fun f(a: Bool) -> Bool { return !a; }");
    let expected = vec![NotBool(r(1), r(0)), RetBool(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_mod() {
    let result = code("fun f(a: Int, b: Int) -> Int { return a % b; }");
    let expected = vec![ModInt(r(2), r(0), r(1)), RetInt(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_mod_int() {
    let result = position("fun f(a: Int, b: Int) -> Int { return a % b; }");
    let expected = vec![(0, p(1, 41))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_bit_or() {
    let result = code("fun f(a: Int, b: Int) -> Int { return a | b; }");
    let expected = vec![OrInt(r(2), r(0), r(1)), RetInt(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_bit_and() {
    let result = code("fun f(a: Int, b: Int) -> Int { return a & b; }");
    let expected = vec![AndInt(r(2), r(0), r(1)), RetInt(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_bit_xor() {
    let result = code("fun f(a: Int, b: Int) -> Int { return a ^ b; }");
    let expected = vec![XorInt(r(2), r(0), r(1)), RetInt(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_bit_shiftl() {
    let result = code("fun f(a: Int, b: Int) -> Int { return a << b; }");
    let expected = vec![ShlInt(r(2), r(0), r(1)), RetInt(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_bit_shiftr() {
    let result = code("fun f(a: Int, b: Int) -> Int { return a >>> b; }");
    let expected = vec![ShrInt(r(2), r(0), r(1)), RetInt(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_bit_ashiftr() {
    let result = code("fun f(a: Int, b: Int) -> Int { return a >> b; }");
    let expected = vec![SarInt(r(2), r(0), r(1)), RetInt(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_equal_bool() {
    let result = code("fun f(a: Bool, b: Bool) -> Bool { return a == b; }");
    let expected = vec![TestEqBool(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_notequal_bool() {
    let result = code("fun f(a: Bool, b: Bool) -> Bool { return a != b; }");
    let expected = vec![TestNeBool(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_equal_byte() {
    let result = code("fun f(a: Byte, b: Byte) -> Bool { return a == b; }");
    let expected = vec![TestEqByte(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_notequal_byte() {
    let result = code("fun f(a: Byte, b: Byte) -> Bool { return a != b; }");
    let expected = vec![TestNeByte(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_lessthan_byte() {
    let result = code("fun f(a: Byte, b: Byte) -> Bool { return a < b; }");
    let expected = vec![TestLtByte(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_lessthanequal_byte() {
    let result = code("fun f(a: Byte, b: Byte) -> Bool { return a <= b; }");
    let expected = vec![TestLeByte(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_greaterthan_byte() {
    let result = code("fun f(a: Byte, b: Byte) -> Bool { return a > b; }");
    let expected = vec![TestGtByte(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_greaterthanequal_byte() {
    let result = code("fun f(a: Byte, b: Byte) -> Bool { return a >= b; }");
    let expected = vec![TestGeByte(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_equal_char() {
    let result = code("fun f(a: Char, b: Char) -> Bool { return a == b; }");
    let expected = vec![TestEqChar(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_notequal_char() {
    let result = code("fun f(a: Char, b: Char) -> Bool { return a != b; }");
    let expected = vec![TestNeChar(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_lessthan_char() {
    let result = code("fun f(a: Char, b: Char) -> Bool { return a < b; }");
    let expected = vec![TestLtChar(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_lessthanequal_char() {
    let result = code("fun f(a: Char, b: Char) -> Bool { return a <= b; }");
    let expected = vec![TestLeChar(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_greaterthan_char() {
    let result = code("fun f(a: Char, b: Char) -> Bool { return a > b; }");
    let expected = vec![TestGtChar(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_greaterthanequal_char() {
    let result = code("fun f(a: Char, b: Char) -> Bool { return a >= b; }");
    let expected = vec![TestGeChar(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_equal_enum() {
    let result = code(
        "fun f(a: Foo, b: Foo) -> Bool { return a == b; }
         enum Foo { A, B }",
    );
    let expected = vec![TestEqEnum(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_notequal_enum() {
    let result = code(
        "fun f(a: Foo, b: Foo) -> Bool { return a != b; }
         enum Foo { A, B }",
    );
    let expected = vec![TestNeEnum(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_equal_int() {
    let result = code("fun f(a: Int, b: Int) -> Bool { return a == b; }");
    let expected = vec![TestEqInt(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_notequal_int() {
    let result = code("fun f(a: Int, b: Int) -> Bool { return a != b; }");
    let expected = vec![TestNeInt(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_lessthan_int() {
    let result = code("fun f(a: Int, b: Int) -> Bool { return a < b; }");
    let expected = vec![TestLtInt(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_lessthanequal_int() {
    let result = code("fun f(a: Int, b: Int) -> Bool { return a <= b; }");
    let expected = vec![TestLeInt(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_greaterthan_int() {
    let result = code("fun f(a: Int, b: Int) -> Bool { return a > b; }");
    let expected = vec![TestGtInt(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_greaterthanequal_int() {
    let result = code("fun f(a: Int, b: Int) -> Bool { return a >= b; }");
    let expected = vec![TestGeInt(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_equal_float() {
    let result = code("fun f(a: Float, b: Float) -> Bool { return a == b; }");
    let expected = vec![TestEqFloat(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_notequal_float() {
    let result = code("fun f(a: Float, b: Float) -> Bool { return a != b; }");
    let expected = vec![TestNeFloat(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_lessthan_float() {
    let result = code("fun f(a: Float, b: Float) -> Bool { return a < b; }");
    let expected = vec![TestLtFloat(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_lessthanequal_float() {
    let result = code("fun f(a: Float, b: Float) -> Bool { return a <= b; }");
    let expected = vec![TestLeFloat(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_greaterthan_float() {
    let result = code("fun f(a: Float, b: Float) -> Bool { return a > b; }");
    let expected = vec![TestGtFloat(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_greaterthanequal_float() {
    let result = code("fun f(a: Float, b: Float) -> Bool { return a >= b; }");
    let expected = vec![TestGeFloat(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_ident() {
    let result = code("fun f() -> Int { let x = 1; return x; }");
    let expected = vec![ConstInt(r(0), 1), RetInt(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_assign() {
    let result = code("fun f() { var x = 1; x = 2; }");
    let expected = vec![ConstInt(r(0), 1), ConstInt(r(0), 2), RetVoid];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_self() {
    let result = code_method("class Foo() { fun f() -> Foo { return self; } }");
    let expected = vec![RetPtr(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_self_assign() {
    let result = code_method("class Foo() { fun f() { let x = self; } }");
    let expected = vec![MovPtr(r(1), r(0)), RetVoid];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_return() {
    let result = code("fun f() -> Int { return 1; }");
    let expected = vec![ConstInt(r(0), 1), RetInt(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_returnvoid() {
    let result = code("fun f() { }");
    let expected = vec![RetVoid];
    assert_eq!(expected, result);
}

#[test]
fn gen_load_global_bool() {
    gen("var a: Bool; fun f() -> Bool { return a; }", |vm, code| {
        let gid = vm.global_by_name("a");
        let expected = vec![LoadGlobalBool(r(0), gid), RetBool(r(0))];
        assert_eq!(expected, code);
    });
}

#[test]
fn gen_load_global_byte() {
    gen("var a: Byte; fun f() -> Byte { return a; }", |vm, code| {
        let gid = vm.global_by_name("a");
        let expected = vec![LoadGlobalByte(r(0), gid), RetByte(r(0))];
        assert_eq!(expected, code);
    });
}

#[test]
fn gen_load_global_char() {
    gen("var a: Char; fun f() -> Char { return a; }", |vm, code| {
        let gid = vm.global_by_name("a");
        let expected = vec![LoadGlobalChar(r(0), gid), RetChar(r(0))];
        assert_eq!(expected, code);
    });
}

#[test]
fn gen_load_global_int() {
    gen("var a: Int; fun f() -> Int { return a; }", |vm, code| {
        let gid = vm.global_by_name("a");
        let expected = vec![LoadGlobalInt(r(0), gid), RetInt(r(0))];
        assert_eq!(expected, code);
    });
}

#[test]
fn gen_load_global_long() {
    gen("var a: Long; fun f() -> Long { return a; }", |vm, code| {
        let gid = vm.global_by_name("a");
        let expected = vec![LoadGlobalLong(r(0), gid), RetLong(r(0))];
        assert_eq!(expected, code);
    });
}

#[test]
fn gen_load_global_float() {
    gen(
        "var a: Float; fun f() -> Float { return a; }",
        |vm, code| {
            let gid = vm.global_by_name("a");
            let expected = vec![LoadGlobalFloat(r(0), gid), RetFloat(r(0))];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_load_global_double() {
    gen(
        "var a: Double; fun f() -> Double { return a; }",
        |vm, code| {
            let gid = vm.global_by_name("a");
            let expected = vec![LoadGlobalDouble(r(0), gid), RetDouble(r(0))];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_load_global_ptr() {
    gen(
        "var a: Object; fun f() -> Object { return a; }",
        |vm, code| {
            let gid = vm.global_by_name("a");
            let expected = vec![LoadGlobalPtr(r(0), gid), RetPtr(r(0))];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_side_effect() {
    let result = code("fun f(a: Int) { 1; 2; 3 * a; \"foo\"; 1.0F; 1.0D; a; }");
    let expected = vec![RetVoid];
    assert_eq!(expected, result);
}

#[test]
fn gen_fct_call_void_with_0_args() {
    gen(
        "
            fun f() { g(); }
            fun g() { }
            ",
        |vm, code| {
            let fct_id = vm.fct_by_name("g").expect("g not found");
            let expected = vec![InvokeStaticVoid(fct_id, r(0), 0), RetVoid];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_fct_call_int_with_0_args() {
    gen(
        "
            fun f() -> Int { return g(); }
            fun g() -> Int { return 1; }
            ",
        |vm, code| {
            let fct_id = vm.fct_by_name("g").expect("g not found");
            let expected = vec![InvokeStaticInt(r(0), fct_id, r(0), 0), RetInt(r(0))];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_fct_call_int_with_0_args_and_unused_result() {
    gen(
        "
            fun f() { g(); }
            fun g() -> Int { return 1; }
            ",
        |vm, code| {
            let fct_id = vm.fct_by_name("g").expect("g not found");
            let expected = vec![InvokeStaticVoid(fct_id, r(0), 0), RetVoid];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_fct_call_void_with_1_arg() {
    gen(
        "
            fun f() { g(1); }
            fun g(a: Int) { }
            ",
        |vm, code| {
            let fct_id = vm.fct_by_name("g").expect("g not found");
            let expected = vec![
                ConstInt(r(0), 1),
                InvokeStaticVoid(fct_id, r(0), 1),
                RetVoid,
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_fct_call_void_with_3_args() {
    gen(
        "
            fun f() { g(1, 2, 3); }
            fun g(a: Int, b: Int, c: Int) { }
            ",
        |vm, code| {
            let fct_id = vm.fct_by_name("g").expect("g not found");
            let expected = vec![
                ConstInt(r(0), 1),
                ConstInt(r(1), 2),
                ConstInt(r(2), 3),
                InvokeStaticVoid(fct_id, r(0), 3),
                RetVoid,
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_fct_call_int_with_1_arg() {
    gen(
        "
            fun f() -> Int { return g(1); }
            fun g(a: Int) -> Int { return a; }
            ",
        |vm, code| {
            let fct_id = vm.fct_by_name("g").expect("g not found");
            let expected = vec![
                ConstInt(r(1), 1),
                InvokeStaticInt(r(0), fct_id, r(1), 1),
                RetInt(r(0)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_fct_call_int_with_3_args() {
    gen(
        "
            fun f() -> Int { return g(1, 2, 3); }
            fun g(a: Int, b: Int, c: Int) -> Int { return 1; }
            ",
        |vm, code| {
            let fct_id = vm.fct_by_name("g").expect("g not found");
            let expected = vec![
                ConstInt(r(1), 1),
                ConstInt(r(2), 2),
                ConstInt(r(3), 3),
                InvokeStaticInt(r(0), fct_id, r(1), 3),
                RetInt(r(0)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_void_check_correct_self() {
    gen(
        "
            fun f(i: Int, foo: Foo) { foo.g(); }
            class Foo {
                fun g() { }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(2), r(1)),
                InvokeDirectVoid(fct_id, r(2), 1),
                RetVoid,
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_void_with_0_args() {
    gen(
        "
            fun f(foo: Foo) { foo.g(); }
            class Foo {
                fun g() { }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(1), r(0)),
                InvokeDirectVoid(fct_id, r(1), 1),
                RetVoid,
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_void_with_1_arg() {
    gen(
        "
            fun f(foo: Foo) { foo.g(1); }
            class Foo {
                fun g(a: Int) { }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(1), r(0)),
                ConstInt(r(2), 1),
                InvokeDirectVoid(fct_id, r(1), 2),
                RetVoid,
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_void_with_3_args() {
    gen(
        "
            fun f(foo: Foo) { foo.g(1, 2, 3); }
            class Foo {
                fun g(a: Int, b: Int, c: Int) { }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(1), r(0)),
                ConstInt(r(2), 1),
                ConstInt(r(3), 2),
                ConstInt(r(4), 3),
                InvokeDirectVoid(fct_id, r(1), 4),
                RetVoid,
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_bool_with_0_args() {
    gen(
        "
            fun f(foo: Foo) -> Bool { return foo.g(); }
            class Foo {
                fun g() -> Bool { return true; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(2), r(0)),
                InvokeDirectBool(r(1), fct_id, r(2), 1),
                RetBool(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_bool_with_0_args_and_unused_result() {
    gen(
        "
            fun f(foo: Foo) { foo.g(); }
            class Foo {
                fun g() -> Bool { return true; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(1), r(0)),
                InvokeDirectVoid(fct_id, r(1), 1),
                RetVoid,
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_bool_with_1_arg() {
    gen(
        "
            fun f(foo: Foo) -> Bool { return foo.g(true); }
            class Foo {
                fun g(a: Bool) -> Bool { return true; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(2), r(0)),
                ConstTrue(r(3)),
                InvokeDirectBool(r(1), fct_id, r(2), 2),
                RetBool(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_bool_with_3_args() {
    gen(
        "
            fun f(foo: Foo) -> Bool { return foo.g(true, false, true); }
            class Foo {
                fun g(a: Bool, b: Bool, c: Bool) -> Bool { return true; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(2), r(0)),
                ConstTrue(r(3)),
                ConstFalse(r(4)),
                ConstTrue(r(5)),
                InvokeDirectBool(r(1), fct_id, r(2), 4),
                RetBool(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_byte_with_0_args() {
    gen(
        "
            fun f(foo: Foo) -> Byte { return foo.g(); }
            class Foo {
                fun g() -> Byte { return 1Y; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(2), r(0)),
                InvokeDirectByte(r(1), fct_id, r(2), 1),
                RetByte(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_byte_with_0_args_and_unused_result() {
    gen(
        "
            fun f(foo: Foo) { foo.g(); }
            class Foo {
                fun g() -> Byte { return 1Y; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(1), r(0)),
                InvokeDirectVoid(fct_id, r(1), 1),
                RetVoid,
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_byte_with_1_arg() {
    gen(
        "
            fun f(foo: Foo) -> Byte { return foo.g(1Y); }
            class Foo {
                fun g(a: Byte) -> Byte { return 1Y; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(2), r(0)),
                ConstByte(r(3), 1),
                InvokeDirectByte(r(1), fct_id, r(2), 2),
                RetByte(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_byte_with_3_args() {
    gen(
        "
            fun f(foo: Foo) -> Byte { return foo.g(1Y, 2Y, 3Y); }
            class Foo {
                fun g(a: Byte, b: Byte, c: Byte) -> Byte { return 1Y; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(2), r(0)),
                ConstByte(r(3), 1),
                ConstByte(r(4), 2),
                ConstByte(r(5), 3),
                InvokeDirectByte(r(1), fct_id, r(2), 4),
                RetByte(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_char_with_0_args() {
    gen(
        "
            fun f(foo: Foo) -> Char { return foo.g(); }
            class Foo {
                fun g() -> Char { return '1'; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(2), r(0)),
                InvokeDirectChar(r(1), fct_id, r(2), 1),
                RetChar(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_char_with_0_args_and_unused_result() {
    gen(
        "
            fun f(foo: Foo) { foo.g(); }
            class Foo {
                fun g() -> Char { return '1'; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(1), r(0)),
                InvokeDirectVoid(fct_id, r(1), 1),
                RetVoid,
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_char_with_1_arg() {
    gen(
        "
            fun f(foo: Foo) -> Char { return foo.g('1'); }
            class Foo {
                fun g(a: Char) -> Char { return '1'; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(2), r(0)),
                ConstChar(r(3), '1'),
                InvokeDirectChar(r(1), fct_id, r(2), 2),
                RetChar(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_char_with_3_args() {
    gen(
        "
            fun f(foo: Foo) -> Char { return foo.g('1', '2', '3'); }
            class Foo {
                fun g(a: Char, b: Char, c: Char) -> Char { return '1'; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(2), r(0)),
                ConstChar(r(3), '1'),
                ConstChar(r(4), '2'),
                ConstChar(r(5), '3'),
                InvokeDirectChar(r(1), fct_id, r(2), 4),
                RetChar(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_int_with_0_args() {
    gen(
        "
            fun f(foo: Foo) -> Int { return foo.g(); }
            class Foo {
                fun g() -> Int { return 1; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(2), r(0)),
                InvokeDirectInt(r(1), fct_id, r(2), 1),
                RetInt(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_int_with_0_args_and_unused_result() {
    gen(
        "
            fun f(foo: Foo) { foo.g(); }
            class Foo {
                fun g() -> Int { return 1; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(1), r(0)),
                InvokeDirectVoid(fct_id, r(1), 1),
                RetVoid,
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_int_with_1_arg() {
    gen(
        "
            fun f(foo: Foo) -> Int { return foo.g(1); }
            class Foo {
                fun g(a: Int) -> Int { return 1; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(2), r(0)),
                ConstInt(r(3), 1),
                InvokeDirectInt(r(1), fct_id, r(2), 2),
                RetInt(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_int_with_3_args() {
    gen(
        "
            fun f(foo: Foo) -> Int { return foo.g(1, 2, 3); }
            class Foo {
                fun g(a: Int, b: Int, c: Int) -> Int { return 1; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(2), r(0)),
                ConstInt(r(3), 1),
                ConstInt(r(4), 2),
                ConstInt(r(5), 3),
                InvokeDirectInt(r(1), fct_id, r(2), 4),
                RetInt(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_long_with_0_args() {
    gen(
        "
            fun f(foo: Foo) -> Long { return foo.g(); }
            class Foo {
                fun g() -> Long { return 1L; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(2), r(0)),
                InvokeDirectLong(r(1), fct_id, r(2), 1),
                RetLong(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_long_with_0_args_and_unused_result() {
    gen(
        "
            fun f(foo: Foo) { foo.g(); }
            class Foo {
                fun g() -> Long { return 1L; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(1), r(0)),
                InvokeDirectVoid(fct_id, r(1), 1),
                RetVoid,
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_long_with_1_arg() {
    gen(
        "
            fun f(foo: Foo) -> Long { return foo.g(1L); }
            class Foo {
                fun g(a: Long) -> Long { return 1L; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(2), r(0)),
                ConstLong(r(3), 1),
                InvokeDirectLong(r(1), fct_id, r(2), 2),
                RetLong(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_long_with_3_args() {
    gen(
        "
            fun f(foo: Foo) -> Long { return foo.g(1L, 2L, 3L); }
            class Foo {
                fun g(a: Long, b: Long, c: Long) -> Long { return 1L; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(2), r(0)),
                ConstLong(r(3), 1),
                ConstLong(r(4), 2),
                ConstLong(r(5), 3),
                InvokeDirectLong(r(1), fct_id, r(2), 4),
                RetLong(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_float_with_0_args() {
    gen(
        "
            fun f(foo: Foo) -> Float { return foo.g(); }
            class Foo {
                fun g() -> Float { return 1F; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(2), r(0)),
                InvokeDirectFloat(r(1), fct_id, r(2), 1),
                RetFloat(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_float_with_0_args_and_unused_result() {
    gen(
        "
            fun f(foo: Foo) { foo.g(); }
            class Foo {
                fun g() -> Float { return 1F; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(1), r(0)),
                InvokeDirectVoid(fct_id, r(1), 1),
                RetVoid,
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_float_with_1_arg() {
    gen(
        "
            fun f(foo: Foo) -> Float { return foo.g(1F); }
            class Foo {
                fun g(a: Float) -> Float { return 1F; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(2), r(0)),
                ConstFloat(r(3), 1_f32),
                InvokeDirectFloat(r(1), fct_id, r(2), 2),
                RetFloat(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_float_with_3_args() {
    gen(
        "
            fun f(foo: Foo) -> Float { return foo.g(1F, 2F, 3F); }
            class Foo {
                fun g(a: Float, b: Float, c: Float) -> Float { return 1F; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(2), r(0)),
                ConstFloat(r(3), 1_f32),
                ConstFloat(r(4), 2_f32),
                ConstFloat(r(5), 3_f32),
                InvokeDirectFloat(r(1), fct_id, r(2), 4),
                RetFloat(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_double_with_0_args() {
    gen(
        "
            fun f(foo: Foo) -> Double { return foo.g(); }
            class Foo {
                fun g() -> Double { return 1D; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(2), r(0)),
                InvokeDirectDouble(r(1), fct_id, r(2), 1),
                RetDouble(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_double_with_0_args_and_unused_result() {
    gen(
        "
            fun f(foo: Foo) { foo.g(); }
            class Foo {
                fun g() -> Double { return 1D; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(1), r(0)),
                InvokeDirectVoid(fct_id, r(1), 1),
                RetVoid,
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_double_with_1_arg() {
    gen(
        "
            fun f(foo: Foo) -> Double { return foo.g(1D); }
            class Foo {
                fun g(a: Double) -> Double { return 1D; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(2), r(0)),
                ConstDouble(r(3), 1_f64),
                InvokeDirectDouble(r(1), fct_id, r(2), 2),
                RetDouble(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_double_with_3_args() {
    gen(
        "
            fun f(foo: Foo) -> Double { return foo.g(1D, 2D, 3D); }
            class Foo {
                fun g(a: Double, b: Double, c: Double) -> Double { return 1D; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(2), r(0)),
                ConstDouble(r(3), 1_f64),
                ConstDouble(r(4), 2_f64),
                ConstDouble(r(5), 3_f64),
                InvokeDirectDouble(r(1), fct_id, r(2), 4),
                RetDouble(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_ptr_with_0_args() {
    gen(
        "
            fun f(foo: Foo) -> String { return foo.g(); }
            class Foo {
                fun g() -> String { return \"1\"; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(2), r(0)),
                InvokeDirectPtr(r(1), fct_id, r(2), 1),
                RetPtr(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_ptr_with_0_args_and_unused_result() {
    gen(
        "
            fun f(foo: Foo) { foo.g(); }
            class Foo {
                fun g() -> String { return \"1\"; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(1), r(0)),
                InvokeDirectVoid(fct_id, r(1), 1),
                RetVoid,
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_ptr_with_1_arg() {
    gen(
        "
            fun f(foo: Foo) -> String { return foo.g(\"1\"); }
            class Foo {
                fun g(a: String) -> String { return \"1\"; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(2), r(0)),
                ConstString(r(3), "1".to_string()),
                InvokeDirectPtr(r(1), fct_id, r(2), 2),
                RetPtr(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_ptr_with_3_args() {
    gen(
        "
            fun f(foo: Foo) -> String { return foo.g(\"1\", \"2\", \"3\"); }
            class Foo {
                fun g(a: String, b: String, c: String) -> String { return \"1\"; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                MovPtr(r(2), r(0)),
                ConstString(r(3), "1".to_string()),
                ConstString(r(4), "2".to_string()),
                ConstString(r(5), "3".to_string()),
                InvokeDirectPtr(r(1), fct_id, r(2), 4),
                RetPtr(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_new_object() {
    gen("fun f() -> Object { return Object(); }", |vm, code| {
        let cls_id = vm.cls_def_by_name("Object");
        let ctor_id = vm.ctor_by_name("Object");
        let expected = vec![
            NewObject(r(0), cls_id),
            InvokeDirectVoid(ctor_id, r(0), 1),
            RetPtr(r(0)),
        ];
        assert_eq!(expected, code);
    });
}

#[test]
fn gen_new_object_assign_to_var() {
    gen(
        "fun f() -> Object { let obj = Object(); return obj; }",
        |vm, code| {
            let cls_id = vm.cls_def_by_name("Object");
            let ctor_id = vm.ctor_by_name("Object");
            let expected = vec![
                NewObject(r(1), cls_id),
                InvokeDirectVoid(ctor_id, r(1), 1),
                MovPtr(r(0), r(1)),
                RetPtr(r(0)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_position_new_object() {
    let result = position("fun f() -> Object { return Object(); }");
    let expected = vec![(0, p(1, 34))];
    assert_eq!(expected, result);
}

#[test]
fn gen_new_object_with_multiple_args() {
    gen(
        "
            class Foo(a: Int, b: Int, c: Int)
            fun f() -> Foo { return Foo(1, 2, 3); }
            ",
        |vm, code| {
            let cls_id = vm.cls_def_by_name("Foo");
            let ctor_id = vm.ctor_by_name("Foo");
            let expected = vec![
                NewObject(r(0), cls_id),
                ConstInt(r(1), 1),
                ConstInt(r(2), 2),
                ConstInt(r(3), 3),
                InvokeDirectVoid(ctor_id, r(0), 4),
                RetPtr(r(0)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_position_new_object_with_multiple_args() {
    let result = position(
        "
            class Foo(a: Int, b: Int, c: Int)
            fun f() -> Foo { return Foo(1, 2, 3); }",
    );
    let expected = vec![(0, p(3, 40))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_for_bool() {
    let result = code_method_with_class_name(
        "trait MyId { fun f() -> Self; }
            impl MyId for Bool { fun f() -> Bool { return self; } }
            ",
        "Bool",
    );
    let expected = vec![RetBool(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_for_byte() {
    let result = code_method_with_class_name(
        "trait MyId { fun f() -> Self; }
            impl MyId for Byte { fun f() -> Byte { return self; } }
            ",
        "Byte",
    );
    let expected = vec![RetByte(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_for_int() {
    let result = code_method_with_class_name(
        "trait MyId { fun f() -> Self; }
            impl MyId for Int { fun f() -> Int { return self; } }
            ",
        "Int",
    );
    let expected = vec![RetInt(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_for_long() {
    let result = code_method_with_class_name(
        "trait MyId { fun f() -> Self; }
            impl MyId for Long { fun f() -> Long { return self; } }
            ",
        "Long",
    );
    let expected = vec![RetLong(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_for_float() {
    let result = code_method_with_class_name(
        "trait MyId { fun f() -> Self; }
            impl MyId for Float { fun f() -> Float { return self; } }
            ",
        "Float",
    );
    let expected = vec![RetFloat(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_for_double() {
    let result = code_method_with_class_name(
        "trait MyId { fun f() -> Self; }
            impl MyId for Double { fun f() -> Double { return self; } }
            ",
        "Double",
    );
    let expected = vec![RetDouble(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_for_string() {
    let result = code_method_with_class_name(
        "trait MyId { fun f() -> Self; }
            impl MyId for String { fun f() -> String { return self; } }
            ",
        "String",
    );
    let expected = vec![RetPtr(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_assign_for_bool() {
    let result = code_method_with_class_name(
        "trait MyId { fun f(); }
            impl MyId for Bool { fun f() { let x = self; } }
            ",
        "Bool",
    );
    let expected = vec![MovBool(r(1), r(0)), RetVoid];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_assign_for_byte() {
    let result = code_method_with_class_name(
        "trait MyId { fun f(); }
            impl MyId for Byte { fun f() { let x = self; } }
            ",
        "Byte",
    );
    let expected = vec![MovByte(r(1), r(0)), RetVoid];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_assign_for_int() {
    let result = code_method_with_class_name(
        "trait MyId { fun f(); }
            impl MyId for Int { fun f() { let x = self; } }
            ",
        "Int",
    );
    let expected = vec![MovInt(r(1), r(0)), RetVoid];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_assign_for_long() {
    let result = code_method_with_class_name(
        "trait MyId { fun f(); }
            impl MyId for Long { fun f() { let x = self; } }
            ",
        "Long",
    );
    let expected = vec![MovLong(r(1), r(0)), RetVoid];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_assign_for_float() {
    let result = code_method_with_class_name(
        "trait MyId { fun f(); }
            impl MyId for Float { fun f() { let x = self; } }
            ",
        "Float",
    );
    let expected = vec![MovFloat(r(1), r(0)), RetVoid];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_assign_for_double() {
    let result = code_method_with_class_name(
        "trait MyId { fun f(); }
            impl MyId for Double { fun f() { let x = self; } }
            ",
        "Double",
    );
    let expected = vec![MovDouble(r(1), r(0)), RetVoid];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_assign_for_string() {
    let result = code_method_with_class_name(
        "trait MyId { fun f(); }
            impl MyId for String { fun f() { let x = self; } }
            ",
        "String",
    );
    let expected = vec![MovPtr(r(1), r(0)), RetVoid];
    assert_eq!(expected, result);
}

#[test]
fn gen_assert() {
    gen("fun f() { assert(true); }", |vm, code| {
        let cls_id = vm.cls_def_by_name("Error");
        let ctor_id = vm.ctor_by_name("Error");
        let expected = vec![
            ConstTrue(r(0)),
            JumpIfTrue(r(0), 6),
            NewObject(r(1), cls_id),
            ConstString(r(2), "assert failed".to_string()),
            InvokeDirectVoid(ctor_id, r(1), 2),
            Throw(r(1)),
            RetVoid,
        ];
        assert_eq!(expected, code);
    });
}

#[test]
fn gen_position_assert() {
    let result = position("fun f() { assert(true); }");
    let expected = vec![(5, p(1, 17))];
    assert_eq!(expected, result);
}

#[test]
fn gen_throw() {
    gen("fun f() { throw Exception(\"exception\"); }", |vm, code| {
        let cls_id = vm.cls_def_by_name("Exception");
        let ctor_id = vm.ctor_by_name("Exception");
        let expected = vec![
            NewObject(r(0), cls_id),
            ConstString(r(1), "exception".to_string()),
            InvokeDirectVoid(ctor_id, r(0), 2),
            Throw(r(0)),
            RetVoid,
        ];
        assert_eq!(expected, code);
    });
}

#[test]
fn gen_position_throw() {
    let expected = vec![(0, p(1, 23))];
    let result = position("fun f(a: Exception) { throw a; }");
    assert_eq!(expected, result);
}

fn p(line: u32, column: u32) -> Position {
    Position { line, column }
}

fn r(val: usize) -> Register {
    Register(val)
}

#[derive(PartialEq, Debug)]
pub enum Bytecode {
    AddInt(Register, Register, Register),
    AddLong(Register, Register, Register),
    AddFloat(Register, Register, Register),
    AddDouble(Register, Register, Register),

    SubInt(Register, Register, Register),
    SubLong(Register, Register, Register),
    SubFloat(Register, Register, Register),
    SubDouble(Register, Register, Register),

    NegInt(Register, Register),
    NegLong(Register, Register),
    NegFloat(Register, Register),
    NegDouble(Register, Register),

    MulInt(Register, Register, Register),
    MulLong(Register, Register, Register),
    MulFloat(Register, Register, Register),
    MulDouble(Register, Register, Register),

    DivInt(Register, Register, Register),
    DivLong(Register, Register, Register),
    DivFloat(Register, Register, Register),
    DivDouble(Register, Register, Register),

    ModInt(Register, Register, Register),
    ModLong(Register, Register, Register),

    AndInt(Register, Register, Register),
    AndLong(Register, Register, Register),
    OrInt(Register, Register, Register),
    OrLong(Register, Register, Register),
    XorInt(Register, Register, Register),
    XorLong(Register, Register, Register),
    NotBool(Register, Register),
    NotInt(Register, Register),
    NotLong(Register, Register),

    ShlInt(Register, Register, Register),
    ShrInt(Register, Register, Register),
    SarInt(Register, Register, Register),

    ShlLong(Register, Register, Register),
    ShrLong(Register, Register, Register),
    SarLong(Register, Register, Register),

    MovBool(Register, Register),
    MovByte(Register, Register),
    MovChar(Register, Register),
    MovInt(Register, Register),
    MovLong(Register, Register),
    MovFloat(Register, Register),
    MovDouble(Register, Register),
    MovPtr(Register, Register),

    LoadFieldBool(Register, Register, ClassDefId, FieldId),
    LoadFieldByte(Register, Register, ClassDefId, FieldId),
    LoadFieldChar(Register, Register, ClassDefId, FieldId),
    LoadFieldInt(Register, Register, ClassDefId, FieldId),
    LoadFieldLong(Register, Register, ClassDefId, FieldId),
    LoadFieldFloat(Register, Register, ClassDefId, FieldId),
    LoadFieldDouble(Register, Register, ClassDefId, FieldId),
    LoadFieldPtr(Register, Register, ClassDefId, FieldId),

    StoreFieldBool(Register, Register, ClassDefId, FieldId),
    StoreFieldByte(Register, Register, ClassDefId, FieldId),
    StoreFieldChar(Register, Register, ClassDefId, FieldId),
    StoreFieldInt(Register, Register, ClassDefId, FieldId),
    StoreFieldLong(Register, Register, ClassDefId, FieldId),
    StoreFieldFloat(Register, Register, ClassDefId, FieldId),
    StoreFieldDouble(Register, Register, ClassDefId, FieldId),
    StoreFieldPtr(Register, Register, ClassDefId, FieldId),

    LoadGlobalBool(Register, GlobalId),
    LoadGlobalByte(Register, GlobalId),
    LoadGlobalChar(Register, GlobalId),
    LoadGlobalInt(Register, GlobalId),
    LoadGlobalLong(Register, GlobalId),
    LoadGlobalFloat(Register, GlobalId),
    LoadGlobalDouble(Register, GlobalId),
    LoadGlobalPtr(Register, GlobalId),

    StoreGlobalBool(Register, GlobalId),
    StoreGlobalByte(Register, GlobalId),
    StoreGlobalChar(Register, GlobalId),
    StoreGlobalInt(Register, GlobalId),
    StoreGlobalLong(Register, GlobalId),
    StoreGlobalFloat(Register, GlobalId),
    StoreGlobalDouble(Register, GlobalId),
    StoreGlobalPtr(Register, GlobalId),

    ConstNil(Register),
    ConstTrue(Register),
    ConstFalse(Register),
    ConstZeroByte(Register),
    ConstZeroChar(Register),
    ConstZeroInt(Register),
    ConstZeroLong(Register),
    ConstZeroFloat(Register),
    ConstZeroDouble(Register),
    ConstByte(Register, u8),
    ConstChar(Register, char),
    ConstInt(Register, i32),
    ConstLong(Register, i64),
    ConstFloat(Register, f32),
    ConstDouble(Register, f64),
    ConstString(Register, String),

    TestEqPtr(Register, Register, Register),
    TestNePtr(Register, Register, Register),

    TestEqBool(Register, Register, Register),
    TestNeBool(Register, Register, Register),

    TestEqByte(Register, Register, Register),
    TestNeByte(Register, Register, Register),
    TestGtByte(Register, Register, Register),
    TestGeByte(Register, Register, Register),
    TestLtByte(Register, Register, Register),
    TestLeByte(Register, Register, Register),

    TestEqChar(Register, Register, Register),
    TestNeChar(Register, Register, Register),
    TestGtChar(Register, Register, Register),
    TestGeChar(Register, Register, Register),
    TestLtChar(Register, Register, Register),
    TestLeChar(Register, Register, Register),

    TestEqEnum(Register, Register, Register),
    TestNeEnum(Register, Register, Register),

    TestEqInt(Register, Register, Register),
    TestNeInt(Register, Register, Register),
    TestGtInt(Register, Register, Register),
    TestGeInt(Register, Register, Register),
    TestLtInt(Register, Register, Register),
    TestLeInt(Register, Register, Register),

    TestEqLong(Register, Register, Register),
    TestNeLong(Register, Register, Register),
    TestGtLong(Register, Register, Register),
    TestGeLong(Register, Register, Register),
    TestLtLong(Register, Register, Register),
    TestLeLong(Register, Register, Register),

    TestEqFloat(Register, Register, Register),
    TestNeFloat(Register, Register, Register),
    TestGtFloat(Register, Register, Register),
    TestGeFloat(Register, Register, Register),
    TestLtFloat(Register, Register, Register),
    TestLeFloat(Register, Register, Register),

    TestEqDouble(Register, Register, Register),
    TestNeDouble(Register, Register, Register),
    TestGtDouble(Register, Register, Register),
    TestGeDouble(Register, Register, Register),
    TestLtDouble(Register, Register, Register),
    TestLeDouble(Register, Register, Register),

    JumpLoop(usize),
    Jump(usize),
    JumpIfFalse(Register, usize),
    JumpIfTrue(Register, usize),

    InvokeDirectVoid(FctId, Register, u32),
    InvokeDirectBool(Register, FctId, Register, u32),
    InvokeDirectByte(Register, FctId, Register, u32),
    InvokeDirectChar(Register, FctId, Register, u32),
    InvokeDirectInt(Register, FctId, Register, u32),
    InvokeDirectLong(Register, FctId, Register, u32),
    InvokeDirectFloat(Register, FctId, Register, u32),
    InvokeDirectDouble(Register, FctId, Register, u32),
    InvokeDirectPtr(Register, FctId, Register, u32),

    InvokeVirtualVoid(FctId, Register, u32),
    InvokeVirtualBool(Register, FctId, Register, u32),
    InvokeVirtualByte(Register, FctId, Register, u32),
    InvokeVirtualChar(Register, FctId, Register, u32),
    InvokeVirtualInt(Register, FctId, Register, u32),
    InvokeVirtualLong(Register, FctId, Register, u32),
    InvokeVirtualFloat(Register, FctId, Register, u32),
    InvokeVirtualDouble(Register, FctId, Register, u32),
    InvokeVirtualPtr(Register, FctId, Register, u32),

    InvokeStaticVoid(FctId, Register, u32),
    InvokeStaticBool(Register, FctId, Register, u32),
    InvokeStaticByte(Register, FctId, Register, u32),
    InvokeStaticChar(Register, FctId, Register, u32),
    InvokeStaticInt(Register, FctId, Register, u32),
    InvokeStaticLong(Register, FctId, Register, u32),
    InvokeStaticFloat(Register, FctId, Register, u32),
    InvokeStaticDouble(Register, FctId, Register, u32),
    InvokeStaticPtr(Register, FctId, Register, u32),

    NewObject(Register, ClassDefId),

    Throw(Register),

    RetVoid,
    RetBool(Register),
    RetByte(Register),
    RetChar(Register),
    RetInt(Register),
    RetLong(Register),
    RetFloat(Register),
    RetDouble(Register),
    RetPtr(Register),
}

fn build(bc: &BytecodeFunction) -> Vec<Bytecode> {
    let mut visitor = BytecodeArrayBuilder::new(bc);
    bytecode::read(bc.code(), &mut visitor);
    visitor.generate()
}

struct BytecodeArrayBuilder<'a> {
    bc: &'a BytecodeFunction,
    code: Vec<Bytecode>,
    next_idx: usize,
    offset_to_index: HashMap<BytecodeOffset, usize>,
    pc: BytecodeOffset,
    jumps: Vec<(usize, BytecodeOffset)>,
}

impl<'a> BytecodeArrayBuilder<'a> {
    fn new(bc: &'a BytecodeFunction) -> BytecodeArrayBuilder<'a> {
        BytecodeArrayBuilder {
            bc,
            code: Vec::new(),
            offset_to_index: HashMap::new(),
            next_idx: 0,
            pc: BytecodeOffset(0),
            jumps: Vec::new(),
        }
    }

    fn generate(mut self) -> Vec<Bytecode> {
        self.resolve_jumps();
        self.code
    }

    fn resolve_jumps(&mut self) {
        self.offset_to_index
            .insert(BytecodeOffset(self.bc.code().len() as u32), self.next_idx);
        let jumps = mem::replace(&mut self.jumps, Vec::new());

        for (location, target) in jumps {
            let &idx = self.offset_to_index.get(&target).expect("offset not found");

            match &mut self.code[location] {
                Bytecode::Jump(ref mut target) => *target = idx,
                Bytecode::JumpIfFalse(_, ref mut target) => *target = idx,
                Bytecode::JumpIfTrue(_, ref mut target) => *target = idx,
                _ => unreachable!(),
            }
        }
    }

    fn emit(&mut self, inst: Bytecode) {
        self.code.push(inst);
    }
}

impl<'a> BytecodeVisitor for BytecodeArrayBuilder<'a> {
    fn visit_instruction(&mut self, offset: BytecodeOffset) {
        self.offset_to_index.insert(offset, self.next_idx);
        self.next_idx += 1;
        self.pc = offset;
    }

    fn visit_add_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::AddInt(dest, lhs, rhs));
    }
    fn visit_add_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::AddLong(dest, lhs, rhs));
    }
    fn visit_add_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::AddFloat(dest, lhs, rhs));
    }
    fn visit_add_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::AddDouble(dest, lhs, rhs));
    }

    fn visit_sub_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::SubInt(dest, lhs, rhs));
    }
    fn visit_sub_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::SubLong(dest, lhs, rhs));
    }
    fn visit_sub_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::SubFloat(dest, lhs, rhs));
    }
    fn visit_sub_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::SubDouble(dest, lhs, rhs));
    }

    fn visit_neg_int(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::NegInt(dest, src));
    }
    fn visit_neg_long(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::NegLong(dest, src));
    }
    fn visit_neg_float(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::NegFloat(dest, src));
    }
    fn visit_neg_double(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::NegDouble(dest, src));
    }

    fn visit_mul_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::MulInt(dest, lhs, rhs));
    }
    fn visit_mul_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::MulLong(dest, lhs, rhs));
    }
    fn visit_mul_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::MulFloat(dest, lhs, rhs));
    }
    fn visit_mul_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::MulDouble(dest, lhs, rhs));
    }

    fn visit_div_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::DivInt(dest, lhs, rhs));
    }
    fn visit_div_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::DivLong(dest, lhs, rhs));
    }
    fn visit_div_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::DivFloat(dest, lhs, rhs));
    }
    fn visit_div_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::DivDouble(dest, lhs, rhs));
    }

    fn visit_mod_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::ModInt(dest, lhs, rhs));
    }
    fn visit_mod_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::ModLong(dest, lhs, rhs));
    }

    fn visit_and_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::AndInt(dest, lhs, rhs));
    }
    fn visit_and_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::AndLong(dest, lhs, rhs));
    }

    fn visit_or_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::OrInt(dest, lhs, rhs));
    }
    fn visit_or_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::OrLong(dest, lhs, rhs));
    }

    fn visit_xor_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::XorInt(dest, lhs, rhs));
    }
    fn visit_xor_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::XorLong(dest, lhs, rhs));
    }

    fn visit_not_bool(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::NotBool(dest, src));
    }
    fn visit_not_int(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::NotInt(dest, src));
    }
    fn visit_not_long(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::NotLong(dest, src));
    }

    fn visit_shl_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::ShlInt(dest, lhs, rhs));
    }
    fn visit_shr_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::ShrInt(dest, lhs, rhs));
    }
    fn visit_sar_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::SarInt(dest, lhs, rhs));
    }

    fn visit_shl_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::ShlLong(dest, lhs, rhs));
    }
    fn visit_shr_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::ShrLong(dest, lhs, rhs));
    }
    fn visit_sar_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::SarLong(dest, lhs, rhs));
    }

    fn visit_mov_bool(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::MovBool(dest, src));
    }
    fn visit_mov_byte(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::MovByte(dest, src));
    }
    fn visit_mov_char(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::MovChar(dest, src));
    }
    fn visit_mov_int(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::MovInt(dest, src));
    }
    fn visit_mov_long(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::MovLong(dest, src));
    }
    fn visit_mov_float(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::MovFloat(dest, src));
    }
    fn visit_mov_double(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::MovDouble(dest, src));
    }
    fn visit_mov_ptr(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::MovPtr(dest, src));
    }

    fn visit_load_field_bool(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit(Bytecode::LoadFieldBool(dest, obj, cls, field));
    }
    fn visit_load_field_byte(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit(Bytecode::LoadFieldByte(dest, obj, cls, field));
    }
    fn visit_load_field_char(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit(Bytecode::LoadFieldChar(dest, obj, cls, field));
    }
    fn visit_load_field_int(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit(Bytecode::LoadFieldInt(dest, obj, cls, field));
    }
    fn visit_load_field_long(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit(Bytecode::LoadFieldLong(dest, obj, cls, field));
    }
    fn visit_load_field_float(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit(Bytecode::LoadFieldFloat(dest, obj, cls, field));
    }
    fn visit_load_field_double(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit(Bytecode::LoadFieldDouble(dest, obj, cls, field));
    }
    fn visit_load_field_ptr(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit(Bytecode::LoadFieldPtr(dest, obj, cls, field));
    }

    fn visit_store_field_bool(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit(Bytecode::StoreFieldBool(src, obj, cls, field));
    }
    fn visit_store_field_byte(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit(Bytecode::StoreFieldByte(src, obj, cls, field));
    }
    fn visit_store_field_char(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit(Bytecode::StoreFieldChar(src, obj, cls, field));
    }
    fn visit_store_field_int(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit(Bytecode::StoreFieldInt(src, obj, cls, field));
    }
    fn visit_store_field_long(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit(Bytecode::StoreFieldLong(src, obj, cls, field));
    }
    fn visit_store_field_float(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit(Bytecode::StoreFieldFloat(src, obj, cls, field));
    }
    fn visit_store_field_double(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit(Bytecode::StoreFieldDouble(src, obj, cls, field));
    }
    fn visit_store_field_ptr(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit(Bytecode::StoreFieldPtr(src, obj, cls, field));
    }

    fn visit_load_global_bool(&mut self, dest: Register, glob: GlobalId) {
        self.emit(Bytecode::LoadGlobalBool(dest, glob));
    }
    fn visit_load_global_byte(&mut self, dest: Register, glob: GlobalId) {
        self.emit(Bytecode::LoadGlobalByte(dest, glob));
    }
    fn visit_load_global_char(&mut self, dest: Register, glob: GlobalId) {
        self.emit(Bytecode::LoadGlobalChar(dest, glob));
    }
    fn visit_load_global_int(&mut self, dest: Register, glob: GlobalId) {
        self.emit(Bytecode::LoadGlobalInt(dest, glob));
    }
    fn visit_load_global_long(&mut self, dest: Register, glob: GlobalId) {
        self.emit(Bytecode::LoadGlobalLong(dest, glob));
    }
    fn visit_load_global_float(&mut self, dest: Register, glob: GlobalId) {
        self.emit(Bytecode::LoadGlobalFloat(dest, glob));
    }
    fn visit_load_global_double(&mut self, dest: Register, glob: GlobalId) {
        self.emit(Bytecode::LoadGlobalDouble(dest, glob));
    }
    fn visit_load_global_ptr(&mut self, dest: Register, glob: GlobalId) {
        self.emit(Bytecode::LoadGlobalPtr(dest, glob));
    }

    fn visit_store_global_bool(&mut self, src: Register, glob: GlobalId) {
        self.emit(Bytecode::StoreGlobalBool(src, glob));
    }
    fn visit_store_global_byte(&mut self, src: Register, glob: GlobalId) {
        self.emit(Bytecode::StoreGlobalByte(src, glob));
    }
    fn visit_store_global_char(&mut self, src: Register, glob: GlobalId) {
        self.emit(Bytecode::StoreGlobalChar(src, glob));
    }
    fn visit_store_global_int(&mut self, src: Register, glob: GlobalId) {
        self.emit(Bytecode::StoreGlobalInt(src, glob));
    }
    fn visit_store_global_long(&mut self, src: Register, glob: GlobalId) {
        self.emit(Bytecode::StoreGlobalLong(src, glob));
    }
    fn visit_store_global_float(&mut self, src: Register, glob: GlobalId) {
        self.emit(Bytecode::StoreGlobalFloat(src, glob));
    }
    fn visit_store_global_double(&mut self, src: Register, glob: GlobalId) {
        self.emit(Bytecode::StoreGlobalDouble(src, glob));
    }
    fn visit_store_global_ptr(&mut self, src: Register, glob: GlobalId) {
        self.emit(Bytecode::StoreGlobalPtr(src, glob));
    }

    fn visit_const_nil(&mut self, dest: Register) {
        self.emit(Bytecode::ConstNil(dest));
    }
    fn visit_const_true(&mut self, dest: Register) {
        self.emit(Bytecode::ConstTrue(dest));
    }
    fn visit_const_false(&mut self, dest: Register) {
        self.emit(Bytecode::ConstFalse(dest));
    }
    fn visit_const_zero_byte(&mut self, dest: Register) {
        self.emit(Bytecode::ConstZeroByte(dest));
    }
    fn visit_const_zero_char(&mut self, dest: Register) {
        self.emit(Bytecode::ConstZeroChar(dest));
    }
    fn visit_const_zero_int(&mut self, dest: Register) {
        self.emit(Bytecode::ConstZeroInt(dest));
    }
    fn visit_const_zero_long(&mut self, dest: Register) {
        self.emit(Bytecode::ConstZeroLong(dest));
    }
    fn visit_const_zero_float(&mut self, dest: Register) {
        self.emit(Bytecode::ConstZeroFloat(dest));
    }
    fn visit_const_zero_double(&mut self, dest: Register) {
        self.emit(Bytecode::ConstZeroDouble(dest));
    }
    fn visit_const_char(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self.bc.const_pool(idx).to_char().expect("char expected");
        self.emit(Bytecode::ConstChar(dest, value));
    }
    fn visit_const_byte(&mut self, dest: Register, value: u8) {
        self.emit(Bytecode::ConstByte(dest, value));
    }
    fn visit_const_int(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self.bc.const_pool(idx).to_int().expect("int expected");
        self.emit(Bytecode::ConstInt(dest, value));
    }
    fn visit_const_long(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self.bc.const_pool(idx).to_long().expect("long expected");
        self.emit(Bytecode::ConstLong(dest, value));
    }
    fn visit_const_float(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self.bc.const_pool(idx).to_float().expect("float expected");
        self.emit(Bytecode::ConstFloat(dest, value));
    }
    fn visit_const_double(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self
            .bc
            .const_pool(idx)
            .to_double()
            .expect("double expected");
        self.emit(Bytecode::ConstDouble(dest, value));
    }
    fn visit_const_string(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self
            .bc
            .const_pool(idx)
            .to_string()
            .expect("string expected")
            .to_owned();
        self.emit(Bytecode::ConstString(dest, value));
    }

    fn visit_test_eq_ptr(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestEqPtr(dest, lhs, rhs));
    }
    fn visit_test_ne_ptr(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestNePtr(dest, lhs, rhs));
    }

    fn visit_test_eq_bool(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestEqBool(dest, lhs, rhs));
    }
    fn visit_test_ne_bool(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestNeBool(dest, lhs, rhs));
    }

    fn visit_test_eq_byte(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestEqByte(dest, lhs, rhs));
    }
    fn visit_test_ne_byte(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestNeByte(dest, lhs, rhs));
    }
    fn visit_test_gt_byte(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestGtByte(dest, lhs, rhs));
    }
    fn visit_test_ge_byte(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestGeByte(dest, lhs, rhs));
    }
    fn visit_test_lt_byte(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestLtByte(dest, lhs, rhs));
    }
    fn visit_test_le_byte(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestLeByte(dest, lhs, rhs));
    }

    fn visit_test_eq_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestEqChar(dest, lhs, rhs));
    }
    fn visit_test_ne_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestNeChar(dest, lhs, rhs));
    }
    fn visit_test_gt_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestGtChar(dest, lhs, rhs));
    }
    fn visit_test_ge_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestGeChar(dest, lhs, rhs));
    }
    fn visit_test_lt_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestLtChar(dest, lhs, rhs));
    }
    fn visit_test_le_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestLeChar(dest, lhs, rhs));
    }

    fn visit_test_eq_enum(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestEqEnum(dest, lhs, rhs));
    }
    fn visit_test_ne_enum(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestNeEnum(dest, lhs, rhs));
    }

    fn visit_test_eq_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestEqInt(dest, lhs, rhs));
    }
    fn visit_test_ne_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestNeInt(dest, lhs, rhs));
    }
    fn visit_test_gt_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestGtInt(dest, lhs, rhs));
    }
    fn visit_test_ge_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestGeInt(dest, lhs, rhs));
    }
    fn visit_test_lt_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestLtInt(dest, lhs, rhs));
    }
    fn visit_test_le_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestLeInt(dest, lhs, rhs));
    }

    fn visit_test_eq_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestEqLong(dest, lhs, rhs));
    }
    fn visit_test_ne_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestNeLong(dest, lhs, rhs));
    }
    fn visit_test_gt_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestGtLong(dest, lhs, rhs));
    }
    fn visit_test_ge_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestGeLong(dest, lhs, rhs));
    }
    fn visit_test_lt_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestLtLong(dest, lhs, rhs));
    }
    fn visit_test_le_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestLeLong(dest, lhs, rhs));
    }

    fn visit_test_eq_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestEqFloat(dest, lhs, rhs));
    }
    fn visit_test_ne_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestNeFloat(dest, lhs, rhs));
    }
    fn visit_test_gt_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestGtFloat(dest, lhs, rhs));
    }
    fn visit_test_ge_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestGeFloat(dest, lhs, rhs));
    }
    fn visit_test_lt_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestLtFloat(dest, lhs, rhs));
    }
    fn visit_test_le_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestLeFloat(dest, lhs, rhs));
    }

    fn visit_test_eq_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestEqDouble(dest, lhs, rhs));
    }
    fn visit_test_ne_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestNeDouble(dest, lhs, rhs));
    }
    fn visit_test_gt_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestGtDouble(dest, lhs, rhs));
    }
    fn visit_test_ge_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestGeDouble(dest, lhs, rhs));
    }
    fn visit_test_lt_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestLtDouble(dest, lhs, rhs));
    }
    fn visit_test_le_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestLeDouble(dest, lhs, rhs));
    }

    fn visit_jump_if_false(&mut self, opnd: Register, offset: u32) {
        let offset = BytecodeOffset(self.pc.to_u32() + offset);
        self.jumps.push((self.next_idx - 1, offset));
        self.emit(Bytecode::JumpIfFalse(opnd, 0));
    }
    fn visit_jump_if_false_const(&mut self, opnd: Register, idx: ConstPoolIdx) {
        let value = self.bc.const_pool(idx).to_int().expect("int expected");
        self.visit_jump_if_false(opnd, value as u32);
    }
    fn visit_jump_if_true(&mut self, opnd: Register, offset: u32) {
        let offset = BytecodeOffset(self.pc.to_u32() + offset);
        self.jumps.push((self.next_idx - 1, offset));
        self.emit(Bytecode::JumpIfTrue(opnd, 0));
    }
    fn visit_jump_if_true_const(&mut self, opnd: Register, idx: ConstPoolIdx) {
        let value = self.bc.const_pool(idx).to_int().expect("int expected");
        self.visit_jump_if_true(opnd, value as u32);
    }
    fn visit_jump_loop(&mut self, offset: u32) {
        let offset = BytecodeOffset(self.pc.to_u32() - offset);
        let &idx = self.offset_to_index.get(&offset).expect("offset not found");
        self.emit(Bytecode::JumpLoop(idx));
    }
    fn visit_jump(&mut self, offset: u32) {
        let offset = BytecodeOffset(self.pc.to_u32() + offset);
        self.jumps.push((self.next_idx - 1, offset));
        self.emit(Bytecode::Jump(0));
    }
    fn visit_jump_const(&mut self, idx: ConstPoolIdx) {
        let value = self.bc.const_pool(idx).to_int().expect("int expected");
        self.visit_jump(value as u32);
    }

    fn visit_invoke_direct_void(&mut self, fct: FctId, start: Register, count: u32) {
        self.emit(Bytecode::InvokeDirectVoid(fct, start, count));
    }
    fn visit_invoke_direct_bool(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit(Bytecode::InvokeDirectBool(dest, fct, start, count));
    }
    fn visit_invoke_direct_byte(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit(Bytecode::InvokeDirectByte(dest, fct, start, count));
    }
    fn visit_invoke_direct_char(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit(Bytecode::InvokeDirectChar(dest, fct, start, count));
    }
    fn visit_invoke_direct_int(&mut self, dest: Register, fct: FctId, start: Register, count: u32) {
        self.emit(Bytecode::InvokeDirectInt(dest, fct, start, count));
    }
    fn visit_invoke_direct_long(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit(Bytecode::InvokeDirectLong(dest, fct, start, count));
    }
    fn visit_invoke_direct_float(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit(Bytecode::InvokeDirectFloat(dest, fct, start, count));
    }
    fn visit_invoke_direct_double(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit(Bytecode::InvokeDirectDouble(dest, fct, start, count));
    }
    fn visit_invoke_direct_ptr(&mut self, dest: Register, fct: FctId, start: Register, count: u32) {
        self.emit(Bytecode::InvokeDirectPtr(dest, fct, start, count));
    }

    fn visit_invoke_virtual_void(&mut self, fct: FctId, start: Register, count: u32) {
        self.emit(Bytecode::InvokeVirtualVoid(fct, start, count));
    }
    fn visit_invoke_virtual_bool(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit(Bytecode::InvokeVirtualBool(dest, fct, start, count));
    }
    fn visit_invoke_virtual_byte(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit(Bytecode::InvokeVirtualByte(dest, fct, start, count));
    }
    fn visit_invoke_virtual_char(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit(Bytecode::InvokeVirtualChar(dest, fct, start, count));
    }
    fn visit_invoke_virtual_int(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit(Bytecode::InvokeVirtualInt(dest, fct, start, count));
    }
    fn visit_invoke_virtual_long(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit(Bytecode::InvokeVirtualLong(dest, fct, start, count));
    }
    fn visit_invoke_virtual_float(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit(Bytecode::InvokeVirtualFloat(dest, fct, start, count));
    }
    fn visit_invoke_virtual_double(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit(Bytecode::InvokeVirtualDouble(dest, fct, start, count));
    }
    fn visit_invoke_virtual_ptr(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit(Bytecode::InvokeVirtualPtr(dest, fct, start, count));
    }

    fn visit_invoke_static_void(&mut self, fct: FctId, start: Register, count: u32) {
        self.emit(Bytecode::InvokeStaticVoid(fct, start, count));
    }
    fn visit_invoke_static_bool(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit(Bytecode::InvokeStaticBool(dest, fct, start, count));
    }
    fn visit_invoke_static_byte(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit(Bytecode::InvokeStaticByte(dest, fct, start, count));
    }
    fn visit_invoke_static_char(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit(Bytecode::InvokeStaticChar(dest, fct, start, count));
    }
    fn visit_invoke_static_int(&mut self, dest: Register, fct: FctId, start: Register, count: u32) {
        self.emit(Bytecode::InvokeStaticInt(dest, fct, start, count));
    }
    fn visit_invoke_static_long(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit(Bytecode::InvokeStaticLong(dest, fct, start, count));
    }
    fn visit_invoke_static_float(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit(Bytecode::InvokeStaticFloat(dest, fct, start, count));
    }
    fn visit_invoke_static_double(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit(Bytecode::InvokeStaticDouble(dest, fct, start, count));
    }
    fn visit_invoke_static_ptr(&mut self, dest: Register, fct: FctId, start: Register, count: u32) {
        self.emit(Bytecode::InvokeStaticPtr(dest, fct, start, count));
    }

    fn visit_new_object(&mut self, dest: Register, cls: ClassDefId) {
        self.emit(Bytecode::NewObject(dest, cls));
    }
    fn visit_throw(&mut self, opnd: Register) {
        self.emit(Bytecode::Throw(opnd));
    }

    fn visit_ret_void(&mut self) {
        self.emit(Bytecode::RetVoid);
    }
    fn visit_ret_bool(&mut self, opnd: Register) {
        self.emit(Bytecode::RetBool(opnd));
    }
    fn visit_ret_byte(&mut self, opnd: Register) {
        self.emit(Bytecode::RetByte(opnd));
    }
    fn visit_ret_char(&mut self, opnd: Register) {
        self.emit(Bytecode::RetChar(opnd));
    }
    fn visit_ret_int(&mut self, opnd: Register) {
        self.emit(Bytecode::RetInt(opnd));
    }
    fn visit_ret_long(&mut self, opnd: Register) {
        self.emit(Bytecode::RetLong(opnd));
    }
    fn visit_ret_float(&mut self, opnd: Register) {
        self.emit(Bytecode::RetFloat(opnd));
    }
    fn visit_ret_double(&mut self, opnd: Register) {
        self.emit(Bytecode::RetDouble(opnd));
    }
    fn visit_ret_ptr(&mut self, opnd: Register) {
        self.emit(Bytecode::RetPtr(opnd));
    }
}
