use std::collections::HashMap;
use std::mem;

use self::Bytecode::*;
use crate::bytecode::{
    self, BytecodeFunction, BytecodeOffset, BytecodeVisitor, ConstPoolIdx, Register,
};
use crate::test;
use crate::ty::{BuiltinType, TypeList};
use crate::vm::{ensure_tuple, ClassDefId, FctDef, FctDefId, FieldId, GlobalId, TupleId, VM};
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
fn gen_load_field_uint8() {
    gen(
        "class Foo(let bar: UInt8) fun f(a: Foo) -> UInt8 { return a.bar; }",
        |vm, code| {
            let (cls, field) = vm.field_by_name("Foo", "bar");
            let expected = vec![LoadFieldUInt8(r(1), r(0), cls, field), RetUInt8(r(1))];
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
fn gen_load_field_int32() {
    gen(
        "class Foo(let bar: Int32) fun f(a: Foo) -> Int32 { return a.bar; }",
        |vm, code| {
            let (cls, field) = vm.field_by_name("Foo", "bar");
            let expected = vec![LoadFieldInt32(r(1), r(0), cls, field), RetInt32(r(1))];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_load_field_int64() {
    gen(
        "class Foo(let bar: Int64) fun f(a: Foo) -> Int64 { return a.bar; }",
        |vm, code| {
            let (cls, field) = vm.field_by_name("Foo", "bar");
            let expected = vec![LoadFieldInt64(r(1), r(0), cls, field), RetInt64(r(1))];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_load_field_float32() {
    gen(
        "class Foo(let bar: Float32) fun f(a: Foo) -> Float32 { return a.bar; }",
        |vm, code| {
            let (cls, field) = vm.field_by_name("Foo", "bar");
            let expected = vec![LoadFieldFloat32(r(1), r(0), cls, field), RetFloat32(r(1))];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_load_field_float64() {
    gen(
        "class Foo(let bar: Float64) fun f(a: Foo) -> Float64 { return a.bar; }",
        |vm, code| {
            let (cls, field) = vm.field_by_name("Foo", "bar");
            let expected = vec![LoadFieldFloat64(r(1), r(0), cls, field), RetFloat64(r(1))];
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
fn gen_position_load_field_uint8() {
    let result = position("class Foo(let bar: UInt8) fun f(a: Foo) -> UInt8 { return a.bar; }");
    let expected = vec![(0, p(1, 60))];
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
fn gen_position_load_field_int32() {
    let result = position("class Foo(let bar: Int32) fun f(a: Foo) -> Int32 { return a.bar; }");
    let expected = vec![(0, p(1, 60))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_load_field_int64() {
    let result = position("class Foo(let bar: Int64) fun f(a: Foo) -> Int64 { return a.bar; }");
    let expected = vec![(0, p(1, 60))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_load_field_float32() {
    let result = position("class Foo(let bar: Float32) fun f(a: Foo) -> Float32 { return a.bar; }");
    let expected = vec![(0, p(1, 64))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_load_field_float64() {
    let result = position("class Foo(let bar: Float64) fun f(a: Foo) -> Float64 { return a.bar; }");
    let expected = vec![(0, p(1, 64))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_load_field_ptr() {
    let result = position("class Foo(let bar: Object) fun f(a: Foo) -> Object { return a.bar; }");
    let expected = vec![(0, p(1, 62))];
    assert_eq!(expected, result);
}

#[test]
fn gen_store_field_uint8() {
    gen(
        "class Foo(var bar: UInt8) fun f(a: Foo, b: UInt8) { a.bar = b; }",
        |vm, code| {
            let (cls, field) = vm.field_by_name("Foo", "bar");
            let expected = vec![StoreFieldUInt8(r(1), r(0), cls, field), RetVoid];
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
fn gen_store_field_int32() {
    gen(
        "class Foo(var bar: Int32) fun f(a: Foo, b: Int32) { a.bar = b; }",
        |vm, code| {
            let (cls, field) = vm.field_by_name("Foo", "bar");
            let expected = vec![StoreFieldInt32(r(1), r(0), cls, field), RetVoid];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_store_field_int64() {
    gen(
        "class Foo(var bar: Int64) fun f(a: Foo, b: Int64) { a.bar = b; }",
        |vm, code| {
            let (cls, field) = vm.field_by_name("Foo", "bar");
            let expected = vec![StoreFieldInt64(r(1), r(0), cls, field), RetVoid];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_store_field_float32() {
    gen(
        "class Foo(var bar: Float32) fun f(a: Foo, b: Float32) { a.bar = b; }",
        |vm, code| {
            let (cls, field) = vm.field_by_name("Foo", "bar");
            let expected = vec![StoreFieldFloat32(r(1), r(0), cls, field), RetVoid];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_store_field_float64() {
    gen(
        "class Foo(var bar: Float64) fun f(a: Foo, b: Float64) { a.bar = b; }",
        |vm, code| {
            let (cls, field) = vm.field_by_name("Foo", "bar");
            let expected = vec![StoreFieldFloat64(r(1), r(0), cls, field), RetVoid];
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
fn gen_position_store_field_uint8() {
    let result = position("class Foo(var bar: UInt8) fun f(a: Foo, b: UInt8) { a.bar = b; }");
    let expected = vec![(0, p(1, 59))];
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
fn gen_position_store_field_int32() {
    let result = position("class Foo(var bar: Int32) fun f(a: Foo, b: Int32) { a.bar = b; }");
    let expected = vec![(0, p(1, 59))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_store_field_int64() {
    let result = position("class Foo(var bar: Int64) fun f(a: Foo, b: Int64) { a.bar = b; }");
    let expected = vec![(0, p(1, 59))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_store_field_float32() {
    let result = position("class Foo(var bar: Float32) fun f(a: Foo, b: Float32) { a.bar = b; }");
    let expected = vec![(0, p(1, 63))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_store_field_float64() {
    let result = position("class Foo(var bar: Float64) fun f(a: Foo, b: Float64) { a.bar = b; }");
    let expected = vec![(0, p(1, 63))];
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
    let result = code("fun f() -> Int32 { return 1 + 2; }");
    let expected = vec![
        ConstInt32(r(1), 1),
        ConstInt32(r(2), 2),
        AddInt32(r(0), r(1), r(2)),
        RetInt32(r(0)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_add_float32() {
    let result = code("fun f() -> Float32 { return 1F + 2F; }");
    let expected = vec![
        ConstFloat32(r(1), 1_f32),
        ConstFloat32(r(2), 2_f32),
        AddFloat32(r(0), r(1), r(2)),
        RetFloat32(r(0)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_add_float64() {
    let result = code("fun f(a: Float64, b: Float64) -> Float64 { return a + b; }");
    let expected = vec![AddFloat64(r(2), r(0), r(1)), RetFloat64(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_id_int() {
    let result = code("fun f(a: Int32) -> Int32 { return a; }");
    let expected = vec![RetInt32(r(0))];
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
    let result = code("fun f(a: Int32, b: Int32) -> Int32 { return a - b; }");
    let expected = vec![SubInt32(r(2), r(0), r(1)), RetInt32(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_sub_float32() {
    let result = code("fun f(a: Float32, b: Float32) -> Float32 { return a - b; }");
    let expected = vec![SubFloat32(r(2), r(0), r(1)), RetFloat32(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_sub_float64() {
    let result = code("fun f(a: Float64, b: Float64) -> Float64 { return a - b; }");
    let expected = vec![SubFloat64(r(2), r(0), r(1)), RetFloat64(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_div_int() {
    let result = code("fun f(a: Int32, b: Int32) -> Int32 { return a / b; }");
    let expected = vec![DivInt32(r(2), r(0), r(1)), RetInt32(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_div_int() {
    let result = position("fun f(a: Int32, b: Int32) -> Int32 { return a / b; }");
    let expected = vec![(0, p(1, 47))];
    assert_eq!(expected, result);
}

#[test]
fn gen_div_float32() {
    let result = code("fun f(a: Float32, b: Float32) -> Float32 { return a / b; }");
    let expected = vec![DivFloat32(r(2), r(0), r(1)), RetFloat32(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_div_float64() {
    let result = code("fun f(a: Float64, b: Float64) -> Float64 { return a / b; }");
    let expected = vec![DivFloat64(r(2), r(0), r(1)), RetFloat64(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_mul_int() {
    let result = code("fun f(a: Int32, b: Int32) -> Int32 { return a * b; }");
    let expected = vec![MulInt32(r(2), r(0), r(1)), RetInt32(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_mul_float32() {
    let result = code("fun f(a: Float32, b: Float32) -> Float32 { return a * b; }");
    let expected = vec![MulFloat32(r(2), r(0), r(1)), RetFloat32(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_mul_float64() {
    let result = code("fun f(a: Float64, b: Float64) -> Float64 { return a * b; }");
    let expected = vec![MulFloat64(r(2), r(0), r(1)), RetFloat64(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_stmt_var_init() {
    let result = code("fun f() { let x = 1; }");
    let expected = vec![ConstInt32(r(0), 1), RetVoid];
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
    let result = code("fun f(a: Bool) -> Int32 { if a { return 1; } return 0; }");
    let expected = vec![
        JumpIfFalse(r(0), 3),
        ConstInt32(r(1), 1),
        RetInt32(r(1)),
        ConstZeroInt32(r(2)),
        RetInt32(r(2)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_stmt_if_else_with_return() {
    let result = code("fun f(a: Bool) -> Int32 { if a { return 1; } else { return 2; } }");
    let expected = vec![
        JumpIfFalse(r(0), 3),
        ConstInt32(r(1), 1),
        RetInt32(r(1)),
        ConstInt32(r(2), 2),
        RetInt32(r(2)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_stmt_if_else_without_return() {
    let result = code(
        "fun f(b: Bool) -> Bool {
        var a = b;
        if a { a = false; } else { a = true; }
        return a;
    }",
    );
    let expected = vec![
        MovBool(r(1), r(0)),
        JumpIfFalse(r(1), 4),
        ConstFalse(r(1)),
        Jump(5),
        ConstTrue(r(1)),
        RetBool(r(1)),
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
    let result = code("fun f() -> Int32 { return 1; }");
    let expected = vec![ConstInt32(r(0), 1), RetInt32(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_uint8() {
    let result = code("fun f() -> UInt8 { return 1Y; }");
    let expected = vec![ConstUInt8(r(0), 1), RetUInt8(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_int64() {
    let result = code("fun f() -> Int64 { return 1L; }");
    let expected = vec![ConstInt64(r(0), 1), RetInt64(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_float32() {
    let result = code("fun f() -> Float32 { return 1F; }");
    let expected = vec![ConstFloat32(r(0), 1_f32), RetFloat32(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_float64() {
    let result = code("fun f() -> Float64 { return 1D; }");
    let expected = vec![ConstFloat64(r(0), 1_f64), RetFloat64(r(0))];
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
    let result = code("fun f() -> UInt8 { return 0Y; }");
    let expected = vec![ConstZeroUInt8(r(0)), RetUInt8(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_int_zero() {
    let result = code("fun f() -> Int32 { return 0; }");
    let expected = vec![ConstZeroInt32(r(0)), RetInt32(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_int64_zero() {
    let result = code("fun f() -> Int64 { return 0L; }");
    let expected = vec![ConstZeroInt64(r(0)), RetInt64(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_float32_zero() {
    let result = code("fun f() -> Float32 { return 0F; }");
    let expected = vec![ConstZeroFloat32(r(0)), RetFloat32(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_float64_zero() {
    let result = code("fun f() -> Float64 { return 0D; }");
    let expected = vec![ConstZeroFloat64(r(0)), RetFloat64(r(0))];
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
    let result = code("fun f(a: Int32) -> Int32 { return +a; }");
    let expected = vec![RetInt32(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_neg() {
    let result = code("fun f(a: Int32) -> Int32 { return -a; }");
    let expected = vec![NegInt32(r(1), r(0)), RetInt32(r(1))];
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
    let result = code("fun f(a: Int32, b: Int32) -> Int32 { return a % b; }");
    let expected = vec![ModInt32(r(2), r(0), r(1)), RetInt32(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_mod_int32() {
    let result = position("fun f(a: Int32, b: Int32) -> Int32 { return a % b; }");
    let expected = vec![(0, p(1, 47))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_bit_or() {
    let result = code("fun f(a: Int32, b: Int32) -> Int32 { return a | b; }");
    let expected = vec![OrInt32(r(2), r(0), r(1)), RetInt32(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_bit_and() {
    let result = code("fun f(a: Int32, b: Int32) -> Int32 { return a & b; }");
    let expected = vec![AndInt32(r(2), r(0), r(1)), RetInt32(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_bit_xor() {
    let result = code("fun f(a: Int32, b: Int32) -> Int32 { return a ^ b; }");
    let expected = vec![XorInt32(r(2), r(0), r(1)), RetInt32(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_bit_shiftl() {
    let result = code("fun f(a: Int32, b: Int32) -> Int32 { return a << b; }");
    let expected = vec![ShlInt32(r(2), r(0), r(1)), RetInt32(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_bit_shiftr() {
    let result = code("fun f(a: Int32, b: Int32) -> Int32 { return a >>> b; }");
    let expected = vec![ShrInt32(r(2), r(0), r(1)), RetInt32(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_bit_ashiftr() {
    let result = code("fun f(a: Int32, b: Int32) -> Int32 { return a >> b; }");
    let expected = vec![SarInt32(r(2), r(0), r(1)), RetInt32(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_bit_rol() {
    let result = code("fun f(a: Int32, b: Int32) -> Int32 { return a.rotateLeft(b); }");
    let expected = vec![RolInt32(r(2), r(0), r(1)), RetInt32(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_bit_ror() {
    let result = code("fun f(a: Int32, b: Int32) -> Int32 { return a.rotateRight(b); }");
    let expected = vec![RorInt32(r(2), r(0), r(1)), RetInt32(r(2))];
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
fn gen_expr_test_equal_uint8() {
    let result = code("fun f(a: UInt8, b: UInt8) -> Bool { return a == b; }");
    let expected = vec![TestEqUInt8(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_notequal_uint8() {
    let result = code("fun f(a: UInt8, b: UInt8) -> Bool { return a != b; }");
    let expected = vec![TestNeUInt8(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_lessthan_uint8() {
    let result = code("fun f(a: UInt8, b: UInt8) -> Bool { return a < b; }");
    let expected = vec![TestLtUInt8(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_lessthanequal_uint8() {
    let result = code("fun f(a: UInt8, b: UInt8) -> Bool { return a <= b; }");
    let expected = vec![TestLeUInt8(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_greaterthan_uint8() {
    let result = code("fun f(a: UInt8, b: UInt8) -> Bool { return a > b; }");
    let expected = vec![TestGtUInt8(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_greaterthanequal_uint8() {
    let result = code("fun f(a: UInt8, b: UInt8) -> Bool { return a >= b; }");
    let expected = vec![TestGeUInt8(r(2), r(0), r(1)), RetBool(r(2))];
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
    let result = code("fun f(a: Int32, b: Int32) -> Bool { return a == b; }");
    let expected = vec![TestEqInt32(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_notequal_int() {
    let result = code("fun f(a: Int32, b: Int32) -> Bool { return a != b; }");
    let expected = vec![TestNeInt32(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_lessthan_int() {
    let result = code("fun f(a: Int32, b: Int32) -> Bool { return a < b; }");
    let expected = vec![TestLtInt32(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_lessthanequal_int() {
    let result = code("fun f(a: Int32, b: Int32) -> Bool { return a <= b; }");
    let expected = vec![TestLeInt32(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_greaterthan_int() {
    let result = code("fun f(a: Int32, b: Int32) -> Bool { return a > b; }");
    let expected = vec![TestGtInt32(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_greaterthanequal_int() {
    let result = code("fun f(a: Int32, b: Int32) -> Bool { return a >= b; }");
    let expected = vec![TestGeInt32(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_equal_float32() {
    let result = code("fun f(a: Float32, b: Float32) -> Bool { return a == b; }");
    let expected = vec![TestEqFloat32(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_notequal_float32() {
    let result = code("fun f(a: Float32, b: Float32) -> Bool { return a != b; }");
    let expected = vec![TestNeFloat32(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_lessthan_float32() {
    let result = code("fun f(a: Float32, b: Float32) -> Bool { return a < b; }");
    let expected = vec![TestLtFloat32(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_lessthanequal_float32() {
    let result = code("fun f(a: Float32, b: Float32) -> Bool { return a <= b; }");
    let expected = vec![TestLeFloat32(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_greaterthan_float32() {
    let result = code("fun f(a: Float32, b: Float32) -> Bool { return a > b; }");
    let expected = vec![TestGtFloat32(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_greaterthanequal_float32() {
    let result = code("fun f(a: Float32, b: Float32) -> Bool { return a >= b; }");
    let expected = vec![TestGeFloat32(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_equal_float64() {
    let result = code("fun f(a: Float64, b: Float64) -> Bool { return a == b; }");
    let expected = vec![TestEqFloat64(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_notequal_float64() {
    let result = code("fun f(a: Float64, b: Float64) -> Bool { return a != b; }");
    let expected = vec![TestNeFloat64(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_lessthan_float64() {
    let result = code("fun f(a: Float64, b: Float64) -> Bool { return a < b; }");
    let expected = vec![TestLtFloat64(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_lessthanequal_float64() {
    let result = code("fun f(a: Float64, b: Float64) -> Bool { return a <= b; }");
    let expected = vec![TestLeFloat64(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_greaterthan_float64() {
    let result = code("fun f(a: Float64, b: Float64) -> Bool { return a > b; }");
    let expected = vec![TestGtFloat64(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_greaterthanequal_float64() {
    let result = code("fun f(a: Float64, b: Float64) -> Bool { return a >= b; }");
    let expected = vec![TestGeFloat64(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_ident() {
    let result = code("fun f() -> Int32 { let x = 1; return x; }");
    let expected = vec![ConstInt32(r(0), 1), RetInt32(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_assign() {
    let result = code("fun f() { var x = 1; x = 2; }");
    let expected = vec![ConstInt32(r(0), 1), ConstInt32(r(0), 2), RetVoid];
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
    let result = code("fun f() -> Int32 { return 1; }");
    let expected = vec![ConstInt32(r(0), 1), RetInt32(r(0))];
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
fn gen_load_global_uint8() {
    gen(
        "var a: UInt8; fun f() -> UInt8 { return a; }",
        |vm, code| {
            let gid = vm.global_by_name("a");
            let expected = vec![LoadGlobalUInt8(r(0), gid), RetUInt8(r(0))];
            assert_eq!(expected, code);
        },
    );
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
    gen(
        "var a: Int32; fun f() -> Int32 { return a; }",
        |vm, code| {
            let gid = vm.global_by_name("a");
            let expected = vec![LoadGlobalInt32(r(0), gid), RetInt32(r(0))];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_load_global_int64() {
    gen(
        "var a: Int64; fun f() -> Int64 { return a; }",
        |vm, code| {
            let gid = vm.global_by_name("a");
            let expected = vec![LoadGlobalInt64(r(0), gid), RetInt64(r(0))];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_load_global_float32() {
    gen(
        "var a: Float32; fun f() -> Float32 { return a; }",
        |vm, code| {
            let gid = vm.global_by_name("a");
            let expected = vec![LoadGlobalFloat32(r(0), gid), RetFloat32(r(0))];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_load_global_float64() {
    gen(
        "var a: Float64; fun f() -> Float64 { return a; }",
        |vm, code| {
            let gid = vm.global_by_name("a");
            let expected = vec![LoadGlobalFloat64(r(0), gid), RetFloat64(r(0))];
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
fn gen_store_global_bool() {
    gen("var a: Bool; fun f(x: Bool) { a = x; }", |vm, code| {
        let gid = vm.global_by_name("a");
        let expected = vec![StoreGlobalBool(r(0), gid), RetVoid];
        assert_eq!(expected, code);
    });
}

#[test]
fn gen_store_global_uint8() {
    gen("var a: UInt8; fun f(x: UInt8) { a = x; }", |vm, code| {
        let gid = vm.global_by_name("a");
        let expected = vec![StoreGlobalUInt8(r(0), gid), RetVoid];
        assert_eq!(expected, code);
    });
}

#[test]
fn gen_store_global_char() {
    gen("var a: Char; fun f(x: Char) { a = x; }", |vm, code| {
        let gid = vm.global_by_name("a");
        let expected = vec![StoreGlobalChar(r(0), gid), RetVoid];
        assert_eq!(expected, code);
    });
}

#[test]
fn gen_store_global_int() {
    gen("var a: Int32; fun f(x: Int32) { a = x; }", |vm, code| {
        let gid = vm.global_by_name("a");
        let expected = vec![StoreGlobalInt32(r(0), gid), RetVoid];
        assert_eq!(expected, code);
    });
}

#[test]
fn gen_store_global_int64() {
    gen("var a: Int64; fun f(x: Int64) { a = x; }", |vm, code| {
        let gid = vm.global_by_name("a");
        let expected = vec![StoreGlobalInt64(r(0), gid), RetVoid];
        assert_eq!(expected, code);
    });
}

#[test]
fn gen_store_global_float32() {
    gen(
        "var a: Float32; fun f(x: Float32) { a = x; }",
        |vm, code| {
            let gid = vm.global_by_name("a");
            let expected = vec![StoreGlobalFloat32(r(0), gid), RetVoid];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_store_global_float64() {
    gen(
        "var a: Float64; fun f(x: Float64) { a = x; }",
        |vm, code| {
            let gid = vm.global_by_name("a");
            let expected = vec![StoreGlobalFloat64(r(0), gid), RetVoid];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_store_global_ptr() {
    gen("var a: Object; fun f(x: Object) { a = x; }", |vm, code| {
        let gid = vm.global_by_name("a");
        let expected = vec![StoreGlobalPtr(r(0), gid), RetVoid];
        assert_eq!(expected, code);
    });
}

#[test]
fn gen_side_effect() {
    let result = code("fun f(a: Int32) { 1; 2; 3 * a; \"foo\"; 1.0F; 1.0D; a; }");
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
            let fct_id = vm.fct_def_by_name("g").expect("g not found");
            let expected = vec![InvokeStaticVoid(fct_id), RetVoid];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_fct_call_int_with_0_args() {
    gen(
        "
            fun f() -> Int32 { return g(); }
            fun g() -> Int32 { return 1; }
            ",
        |vm, code| {
            let fct_id = vm.fct_def_by_name("g").expect("g not found");
            let expected = vec![InvokeStaticInt32(r(0), fct_id), RetInt32(r(0))];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_fct_call_int_with_0_args_and_unused_result() {
    gen(
        "
            fun f() { g(); }
            fun g() -> Int32 { return 1; }
            ",
        |vm, code| {
            let fct_id = vm.fct_def_by_name("g").expect("g not found");
            let expected = vec![InvokeStaticInt32(r(0), fct_id), RetVoid];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_fct_call_void_with_1_arg() {
    gen(
        "
            fun f() { g(1); }
            fun g(a: Int32) { }
            ",
        |vm, code| {
            let fct_id = vm.fct_def_by_name("g").expect("g not found");
            let expected = vec![
                ConstInt32(r(0), 1),
                PushRegister(r(0)),
                InvokeStaticVoid(fct_id),
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
            fun g(a: Int32, b: Int32, c: Int32) { }
            ",
        |vm, code| {
            let fct_id = vm.fct_def_by_name("g").expect("g not found");
            let expected = vec![
                ConstInt32(r(0), 1),
                ConstInt32(r(1), 2),
                ConstInt32(r(2), 3),
                PushRegister(r(0)),
                PushRegister(r(1)),
                PushRegister(r(2)),
                InvokeStaticVoid(fct_id),
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
            fun f() -> Int32 { return g(1); }
            fun g(a: Int32) -> Int32 { return a; }
            ",
        |vm, code| {
            let fct_id = vm.fct_def_by_name("g").expect("g not found");
            let expected = vec![
                ConstInt32(r(1), 1),
                PushRegister(r(1)),
                InvokeStaticInt32(r(0), fct_id),
                RetInt32(r(0)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_fct_call_int_with_3_args() {
    gen(
        "
            fun f() -> Int32 { return g(1, 2, 3); }
            fun g(a: Int32, b: Int32, c: Int32) -> Int32 { return 1; }
            ",
        |vm, code| {
            let fct_id = vm.fct_def_by_name("g").expect("g not found");
            let expected = vec![
                ConstInt32(r(1), 1),
                ConstInt32(r(2), 2),
                ConstInt32(r(3), 3),
                PushRegister(r(1)),
                PushRegister(r(2)),
                PushRegister(r(3)),
                InvokeStaticInt32(r(0), fct_id),
                RetInt32(r(0)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_void_check_correct_self() {
    gen(
        "
            fun f(i: Int32, foo: Foo) { foo.g(); }
            class Foo {
                fun g() { }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![PushRegister(r(1)), InvokeDirectVoid(fct_id), RetVoid];
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
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![PushRegister(r(0)), InvokeDirectVoid(fct_id), RetVoid];
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
                fun g(a: Int32) { }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstInt32(r(1), 1),
                PushRegister(r(0)),
                PushRegister(r(1)),
                InvokeDirectVoid(fct_id),
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
                fun g(a: Int32, b: Int32, c: Int32) { }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstInt32(r(1), 1),
                ConstInt32(r(2), 2),
                ConstInt32(r(3), 3),
                PushRegister(r(0)),
                PushRegister(r(1)),
                PushRegister(r(2)),
                PushRegister(r(3)),
                InvokeDirectVoid(fct_id),
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
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirectBool(r(1), fct_id),
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
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![PushRegister(r(0)), InvokeDirectBool(r(1), fct_id), RetVoid];
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
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstTrue(r(2)),
                PushRegister(r(0)),
                PushRegister(r(2)),
                InvokeDirectBool(r(1), fct_id),
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
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstTrue(r(2)),
                ConstFalse(r(3)),
                ConstTrue(r(4)),
                PushRegister(r(0)),
                PushRegister(r(2)),
                PushRegister(r(3)),
                PushRegister(r(4)),
                InvokeDirectBool(r(1), fct_id),
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
            fun f(foo: Foo) -> UInt8 { return foo.g(); }
            class Foo {
                fun g() -> UInt8 { return 1Y; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirectUInt8(r(1), fct_id),
                RetUInt8(r(1)),
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
                fun g() -> UInt8 { return 1Y; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![PushRegister(r(0)), InvokeDirectUInt8(r(1), fct_id), RetVoid];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_byte_with_1_arg() {
    gen(
        "
            fun f(foo: Foo) -> UInt8 { return foo.g(1Y); }
            class Foo {
                fun g(a: UInt8) -> UInt8 { return 1Y; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstUInt8(r(2), 1),
                PushRegister(r(0)),
                PushRegister(r(2)),
                InvokeDirectUInt8(r(1), fct_id),
                RetUInt8(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_byte_with_3_args() {
    gen(
        "
            fun f(foo: Foo) -> UInt8 { return foo.g(1Y, 2Y, 3Y); }
            class Foo {
                fun g(a: UInt8, b: UInt8, c: UInt8) -> UInt8 { return 1Y; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstUInt8(r(2), 1),
                ConstUInt8(r(3), 2),
                ConstUInt8(r(4), 3),
                PushRegister(r(0)),
                PushRegister(r(2)),
                PushRegister(r(3)),
                PushRegister(r(4)),
                InvokeDirectUInt8(r(1), fct_id),
                RetUInt8(r(1)),
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
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirectChar(r(1), fct_id),
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
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![PushRegister(r(0)), InvokeDirectChar(r(1), fct_id), RetVoid];
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
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstChar(r(2), '1'),
                PushRegister(r(0)),
                PushRegister(r(2)),
                InvokeDirectChar(r(1), fct_id),
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
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstChar(r(2), '1'),
                ConstChar(r(3), '2'),
                ConstChar(r(4), '3'),
                PushRegister(r(0)),
                PushRegister(r(2)),
                PushRegister(r(3)),
                PushRegister(r(4)),
                InvokeDirectChar(r(1), fct_id),
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
            fun f(foo: Foo) -> Int32 { return foo.g(); }
            class Foo {
                fun g() -> Int32 { return 1; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirectInt32(r(1), fct_id),
                RetInt32(r(1)),
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
                fun g() -> Int32 { return 1; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![PushRegister(r(0)), InvokeDirectInt32(r(1), fct_id), RetVoid];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_int_with_1_arg() {
    gen(
        "
            fun f(foo: Foo) -> Int32 { return foo.g(1); }
            class Foo {
                fun g(a: Int32) -> Int32 { return 1; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstInt32(r(2), 1),
                PushRegister(r(0)),
                PushRegister(r(2)),
                InvokeDirectInt32(r(1), fct_id),
                RetInt32(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_int_with_3_args() {
    gen(
        "
            fun f(foo: Foo) -> Int32 { return foo.g(1, 2, 3); }
            class Foo {
                fun g(a: Int32, b: Int32, c: Int32) -> Int32 { return 1; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstInt32(r(2), 1),
                ConstInt32(r(3), 2),
                ConstInt32(r(4), 3),
                PushRegister(r(0)),
                PushRegister(r(2)),
                PushRegister(r(3)),
                PushRegister(r(4)),
                InvokeDirectInt32(r(1), fct_id),
                RetInt32(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_int64_with_0_args() {
    gen(
        "
            fun f(foo: Foo) -> Int64 { return foo.g(); }
            class Foo {
                fun g() -> Int64 { return 1L; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirectInt64(r(1), fct_id),
                RetInt64(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_int64_with_0_args_and_unused_result() {
    gen(
        "
            fun f(foo: Foo) { foo.g(); }
            class Foo {
                fun g() -> Int64 { return 1L; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![PushRegister(r(0)), InvokeDirectInt64(r(1), fct_id), RetVoid];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_int64_with_1_arg() {
    gen(
        "
            fun f(foo: Foo) -> Int64 { return foo.g(1L); }
            class Foo {
                fun g(a: Int64) -> Int64 { return 1L; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstInt64(r(2), 1),
                PushRegister(r(0)),
                PushRegister(r(2)),
                InvokeDirectInt64(r(1), fct_id),
                RetInt64(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_int64_with_3_args() {
    gen(
        "
            fun f(foo: Foo) -> Int64 { return foo.g(1L, 2L, 3L); }
            class Foo {
                fun g(a: Int64, b: Int64, c: Int64) -> Int64 { return 1L; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstInt64(r(2), 1),
                ConstInt64(r(3), 2),
                ConstInt64(r(4), 3),
                PushRegister(r(0)),
                PushRegister(r(2)),
                PushRegister(r(3)),
                PushRegister(r(4)),
                InvokeDirectInt64(r(1), fct_id),
                RetInt64(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_float32_with_0_args() {
    gen(
        "
            fun f(foo: Foo) -> Float32 { return foo.g(); }
            class Foo {
                fun g() -> Float32 { return 1F; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirectFloat32(r(1), fct_id),
                RetFloat32(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_float32_with_0_args_and_unused_result() {
    gen(
        "
            fun f(foo: Foo) { foo.g(); }
            class Foo {
                fun g() -> Float32 { return 1F; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirectFloat32(r(1), fct_id),
                RetVoid,
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_float32_with_1_arg() {
    gen(
        "
            fun f(foo: Foo) -> Float32 { return foo.g(1F); }
            class Foo {
                fun g(a: Float32) -> Float32 { return 1F; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstFloat32(r(2), 1_f32),
                PushRegister(r(0)),
                PushRegister(r(2)),
                InvokeDirectFloat32(r(1), fct_id),
                RetFloat32(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_float32_with_3_args() {
    gen(
        "
            fun f(foo: Foo) -> Float32 { return foo.g(1F, 2F, 3F); }
            class Foo {
                fun g(a: Float32, b: Float32, c: Float32) -> Float32 { return 1F; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstFloat32(r(2), 1_f32),
                ConstFloat32(r(3), 2_f32),
                ConstFloat32(r(4), 3_f32),
                PushRegister(r(0)),
                PushRegister(r(2)),
                PushRegister(r(3)),
                PushRegister(r(4)),
                InvokeDirectFloat32(r(1), fct_id),
                RetFloat32(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_float64_with_0_args() {
    gen(
        "
            fun f(foo: Foo) -> Float64 { return foo.g(); }
            class Foo {
                fun g() -> Float64 { return 1D; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirectFloat64(r(1), fct_id),
                RetFloat64(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_float64_with_0_args_and_unused_result() {
    gen(
        "
            fun f(foo: Foo) { foo.g(); }
            class Foo {
                fun g() -> Float64 { return 1D; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirectFloat64(r(1), fct_id),
                RetVoid,
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_float64_with_1_arg() {
    gen(
        "
            fun f(foo: Foo) -> Float64 { return foo.g(1D); }
            class Foo {
                fun g(a: Float64) -> Float64 { return 1D; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstFloat64(r(2), 1_f64),
                PushRegister(r(0)),
                PushRegister(r(2)),
                InvokeDirectFloat64(r(1), fct_id),
                RetFloat64(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_method_call_float64_with_3_args() {
    gen(
        "
            fun f(foo: Foo) -> Float64 { return foo.g(1D, 2D, 3D); }
            class Foo {
                fun g(a: Float64, b: Float64, c: Float64) -> Float64 { return 1D; }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstFloat64(r(2), 1_f64),
                ConstFloat64(r(3), 2_f64),
                ConstFloat64(r(4), 3_f64),
                PushRegister(r(0)),
                PushRegister(r(2)),
                PushRegister(r(3)),
                PushRegister(r(4)),
                InvokeDirectFloat64(r(1), fct_id),
                RetFloat64(r(1)),
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
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirectPtr(r(1), fct_id),
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
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![PushRegister(r(0)), InvokeDirectPtr(r(1), fct_id), RetVoid];
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
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstString(r(2), "1".to_string()),
                PushRegister(r(0)),
                PushRegister(r(2)),
                InvokeDirectPtr(r(1), fct_id),
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
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstString(r(2), "1".to_string()),
                ConstString(r(3), "2".to_string()),
                ConstString(r(4), "3".to_string()),
                PushRegister(r(0)),
                PushRegister(r(2)),
                PushRegister(r(3)),
                PushRegister(r(4)),
                InvokeDirectPtr(r(1), fct_id),
                RetPtr(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_virtual_method_call_void_check_correct_self() {
    gen(
        "
            fun f(i: Int32, foo: Foo) { foo.g(); }
            @open @abstract class Bar {
                @open @abstract fun g();
            }
            class Foo : Bar {
                @override fun g() {}
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![PushRegister(r(1)), InvokeVirtualVoid(fct_id), RetVoid];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_virtual_method_call_void_with_0_args() {
    gen(
        "
            fun f(foo: Foo) { foo.g(); }
            @open @abstract class Bar {
                @open @abstract fun g();
            }
            class Foo : Bar {
                @override fun g() {}
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![PushRegister(r(0)), InvokeVirtualVoid(fct_id), RetVoid];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_virtual_method_call_void_with_1_arg() {
    gen(
        "
            fun f(foo: Foo) { foo.g(1); }
            @open @abstract class Bar {
                @open @abstract fun g(a: Int32);
            }
            class Foo : Bar {
                @override fun g(a: Int32) {}
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstInt32(r(1), 1),
                PushRegister(r(0)),
                PushRegister(r(1)),
                InvokeVirtualVoid(fct_id),
                RetVoid,
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_virtual_method_call_void_with_3_args() {
    gen(
        "
            fun f(foo: Foo) { foo.g(1, 2, 3); }
            @open @abstract class Bar {
                @open @abstract fun g(a: Int32, b: Int32, c: Int32);
            }
            class Foo : Bar {
                @override fun g(a: Int32, b: Int32, c: Int32) {}
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstInt32(r(1), 1),
                ConstInt32(r(2), 2),
                ConstInt32(r(3), 3),
                PushRegister(r(0)),
                PushRegister(r(1)),
                PushRegister(r(2)),
                PushRegister(r(3)),
                InvokeVirtualVoid(fct_id),
                RetVoid,
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_virtual_method_call_int_with_0_args() {
    gen(
        "
            fun f(foo: Foo) { foo.g(); }
            @open @abstract class Bar {
                @open @abstract fun g() -> Int32;
            }
            class Foo : Bar {
                @override fun g() -> Int32 { 1 }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                PushRegister(r(0)),
                InvokeVirtualInt32(r(1), fct_id),
                RetVoid,
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_virtual_method_call_int_with_1_arg() {
    gen(
        "
            fun f(foo: Foo) { foo.g(1); }
            @open @abstract class Bar {
                @open @abstract fun g(a: Int32) -> Int32;
            }
            class Foo : Bar {
                @override fun g(a: Int32) -> Int32 { 1 }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstInt32(r(2), 1),
                PushRegister(r(0)),
                PushRegister(r(2)),
                InvokeVirtualInt32(r(1), fct_id),
                RetVoid,
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_virtual_method_call_int_with_3_args() {
    gen(
        "
            fun f(foo: Foo) { foo.g(1, 2, 3); }
            @open @abstract class Bar {
                @open @abstract fun g(a: Int32, b: Int32, c: Int32) -> Int32;
            }
            class Foo : Bar {
                @override fun g(a: Int32, b: Int32, c: Int32) -> Int32 { 1 }
            }
            ",
        |vm, code| {
            let fct_id = vm
                .cls_method_def_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstInt32(r(2), 1),
                ConstInt32(r(3), 2),
                ConstInt32(r(4), 3),
                PushRegister(r(0)),
                PushRegister(r(2)),
                PushRegister(r(3)),
                PushRegister(r(4)),
                InvokeVirtualInt32(r(1), fct_id),
                RetVoid,
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_new_object() {
    gen("fun f() -> Object { return Object(); }", |vm, code| {
        let cls_id = vm.cls_def_by_name("Object");
        let ctor_id = vm.ctor_def_by_name("Object");
        let expected = vec![
            NewObject(r(0), cls_id),
            PushRegister(r(0)),
            InvokeDirectVoid(ctor_id),
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
            let ctor_id = vm.ctor_def_by_name("Object");
            let expected = vec![
                NewObject(r(1), cls_id),
                PushRegister(r(1)),
                InvokeDirectVoid(ctor_id),
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
fn gen_new_array() {
    gen(
        "fun f() -> Array[Int32] { return Array[Int32](1L); }",
        |vm, code| {
            let cls_id = vm.cls_def_by_name_with_type_params(
                "Array",
                TypeList::with(vec![BuiltinType::Int32]),
            );
            let ctor_id = vm.ctor_def_by_name_with_type_params(
                "Array",
                TypeList::with(vec![BuiltinType::Int32]),
            );
            let expected = vec![
                ConstInt64(r(1), 1),
                NewArray(r(0), cls_id, r(1)),
                PushRegister(r(0)),
                PushRegister(r(1)),
                InvokeDirectVoid(ctor_id),
                RetPtr(r(0)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_position_new_array() {
    let result = position("fun f() -> Array[Int32] { return Array[Int32](1L); }");
    let expected = vec![(3, p(1, 46))];
    assert_eq!(expected, result);
}

#[test]
fn gen_array_length() {
    let result = code("fun f(a: Array[Int32]) -> Int64 { return a.length(); }");
    let expected = vec![ArrayLength(r(1), r(0)), RetInt64(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_array_length() {
    let result = position("fun f(a: Array[Int32]) -> Int64 { return a.length(); }");
    let expected = vec![(0, p(1, 50))];
    assert_eq!(expected, result);
}

#[test]
fn gen_array_length_effect() {
    let result = code("fun f(a: Array[Int32]) { a.length(); }");
    let expected = vec![NilCheck(r(0)), RetVoid];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_array_length_effect() {
    let result = position("fun f(a: Array[Int32]) { a.length(); }");
    let expected = vec![(0, p(1, 34))];
    assert_eq!(expected, result);
}

#[test]
fn gen_load_array_uint8() {
    let result = code("fun f(a: Array[UInt8]) -> UInt8 { return a(0L); }");
    let expected = vec![
        ConstZeroInt64(r(2)),
        LoadArrayUInt8(r(1), r(0), r(2)),
        RetUInt8(r(1)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_load_array_bool() {
    let result = code("fun f(a: Array[Bool], idx: Int64) -> Bool { return a(idx); }");
    let expected = vec![LoadArrayBool(r(2), r(0), r(1)), RetBool(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_load_array_char() {
    let result = code("fun f(a: Array[Char], idx: Int64) -> Char { return a(idx); }");
    let expected = vec![LoadArrayChar(r(2), r(0), r(1)), RetChar(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_load_array_int32() {
    let result = code("fun f(a: Array[Int32], idx: Int64) -> Int32 { return a(idx); }");
    let expected = vec![LoadArrayInt32(r(2), r(0), r(1)), RetInt32(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_load_array_int64() {
    let result = code("fun f(a: Array[Int64], idx: Int64) -> Int64 { return a(idx); }");
    let expected = vec![LoadArrayInt64(r(2), r(0), r(1)), RetInt64(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_load_array_float32() {
    let result = code("fun f(a: Array[Float32], idx: Int64) -> Float32 { return a(idx); }");
    let expected = vec![LoadArrayFloat32(r(2), r(0), r(1)), RetFloat32(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_load_array_float64() {
    let result = code("fun f(a: Array[Float64], idx: Int64) -> Float64 { return a(idx); }");
    let expected = vec![LoadArrayFloat64(r(2), r(0), r(1)), RetFloat64(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_load_array_ptr() {
    let result = code("fun f(a: Array[Object], idx: Int64) -> Object { return a(idx); }");
    let expected = vec![LoadArrayPtr(r(2), r(0), r(1)), RetPtr(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_load_array_bool() {
    let result = position("fun f(a: Array[Bool]) -> Bool { return a(0L); }");
    let expected = vec![(2, p(1, 41))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_load_array_char() {
    let result = position("fun f(a: Array[Char]) -> Char { return a(0L); }");
    let expected = vec![(2, p(1, 41))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_load_array_int32() {
    let result = position("fun f(a: Array[Int32]) -> Int32 { return a(0L); }");
    let expected = vec![(2, p(1, 43))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_load_array_int64() {
    let result = position("fun f(a: Array[Int64]) -> Int64 { return a(0L); }");
    let expected = vec![(2, p(1, 43))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_load_array_float32() {
    let result = position("fun f(a: Array[Float32]) -> Float32 { return a(0L); }");
    let expected = vec![(2, p(1, 47))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_load_array_float64() {
    let result = position("fun f(a: Array[Float64]) -> Float64 { return a(0L); }");
    let expected = vec![(2, p(1, 47))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_load_array_ptr() {
    let result = position("fun f(a: Array[Object]) -> Object { return a(0L); }");
    let expected = vec![(2, p(1, 45))];
    assert_eq!(expected, result);
}

#[test]
fn gen_store_array_uint8() {
    let result = code("fun f(a: Array[UInt8], b: UInt8) { a(0L) = b; }");
    let expected = vec![
        ConstZeroInt64(Register(2)),
        StoreArrayUInt8(r(1), r(0), r(2)),
        RetVoid,
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_store_array_bool() {
    let result = code("fun f(a: Array[Bool], b: Bool) { a(0L) = b; }");
    let expected = vec![
        ConstZeroInt64(Register(2)),
        StoreArrayBool(r(1), r(0), r(2)),
        RetVoid,
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_store_array_char() {
    let result = code("fun f(a: Array[Char], b: Char) { a(0L) = b; }");
    let expected = vec![
        ConstZeroInt64(Register(2)),
        StoreArrayChar(r(1), r(0), r(2)),
        RetVoid,
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_store_array_int32() {
    let result = code("fun f(a: Array[Int32], b: Int32) { a(0L) = b; }");
    let expected = vec![
        ConstZeroInt64(Register(2)),
        StoreArrayInt32(r(1), r(0), r(2)),
        RetVoid,
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_store_array_int64() {
    let result = code("fun f(a: Array[Int64], b: Int64) { a(0L) = b; }");
    let expected = vec![
        ConstZeroInt64(Register(2)),
        StoreArrayInt64(r(1), r(0), r(2)),
        RetVoid,
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_store_array_float32() {
    let result = code("fun f(a: Array[Float32], b: Float32) { a(0L) = b; }");
    let expected = vec![
        ConstZeroInt64(Register(2)),
        StoreArrayFloat32(r(1), r(0), r(2)),
        RetVoid,
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_store_array_float64() {
    let result = code("fun f(a: Array[Float64], b: Float64) { a(0L) = b; }");
    let expected = vec![
        ConstZeroInt64(Register(2)),
        StoreArrayFloat64(r(1), r(0), r(2)),
        RetVoid,
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_store_array_ptr() {
    let result = code("fun f(a: Array[Object], b: Object) { a(0L) = b; }");
    let expected = vec![
        ConstZeroInt64(Register(2)),
        StoreArrayPtr(r(1), r(0), r(2)),
        RetVoid,
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_store_array_bool() {
    let result = position("fun f(a: Array[Bool], b: Bool) { a(0L) = b; }");
    let expected = vec![(2, p(1, 40))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_store_array_char() {
    let result = position("fun f(a: Array[Char], b: Char) { a(0L) = b; }");
    let expected = vec![(2, p(1, 40))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_store_array_int32() {
    let result = position("fun f(a: Array[Int32], b: Int32) { a(0L) = b; }");
    let expected = vec![(2, p(1, 42))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_store_array_int64() {
    let result = position("fun f(a: Array[Int64], b: Int64) { a(0L) = b; }");
    let expected = vec![(2, p(1, 42))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_store_array_float32() {
    let result = position("fun f(a: Array[Float32], b: Float32) { a(0L) = b; }");
    let expected = vec![(2, p(1, 46))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_store_array_float64() {
    let result = position("fun f(a: Array[Float64], b: Float64) { a(0L) = b; }");
    let expected = vec![(2, p(1, 46))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_store_array_ptr() {
    let result = position("fun f(a: Array[Object], b: Object) { a(0L) = b; }");
    let expected = vec![(2, p(1, 44))];
    assert_eq!(expected, result);
}

#[test]
fn gen_new_object_with_multiple_args() {
    gen(
        "
            class Foo(a: Int32, b: Int32, c: Int32)
            fun f() -> Foo { return Foo(1, 2, 3); }
            ",
        |vm, code| {
            let cls_id = vm.cls_def_by_name("Foo");
            let ctor_id = vm.ctor_def_by_name("Foo");
            let expected = vec![
                ConstInt32(r(1), 1),
                ConstInt32(r(2), 2),
                ConstInt32(r(3), 3),
                NewObject(r(0), cls_id),
                PushRegister(r(0)),
                PushRegister(r(1)),
                PushRegister(r(2)),
                PushRegister(r(3)),
                InvokeDirectVoid(ctor_id),
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
            class Foo(a: Int32, b: Int32, c: Int32)
            fun f() -> Foo { return Foo(1, 2, 3); }",
    );
    let expected = vec![(9, p(3, 40))];
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
fn gen_self_for_uint8() {
    let result = code_method_with_class_name(
        "trait MyId { fun f() -> Self; }
            impl MyId for UInt8 { fun f() -> UInt8 { return self; } }
            ",
        "UInt8",
    );
    let expected = vec![RetUInt8(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_for_int() {
    let result = code_method_with_class_name(
        "trait MyId { fun f() -> Self; }
            impl MyId for Int32 { fun f() -> Int32 { return self; } }
            ",
        "Int32",
    );
    let expected = vec![RetInt32(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_for_int64() {
    let result = code_method_with_class_name(
        "trait MyId { fun f() -> Self; }
            impl MyId for Int64 { fun f() -> Int64 { return self; } }
            ",
        "Int64",
    );
    let expected = vec![RetInt64(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_for_float32() {
    let result = code_method_with_class_name(
        "trait MyId { fun f() -> Self; }
            impl MyId for Float32 { fun f() -> Float32 { return self; } }
            ",
        "Float32",
    );
    let expected = vec![RetFloat32(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_for_float64() {
    let result = code_method_with_class_name(
        "trait MyId { fun f() -> Self; }
            impl MyId for Float64 { fun f() -> Float64 { return self; } }
            ",
        "Float64",
    );
    let expected = vec![RetFloat64(r(0))];
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
fn gen_self_assign_for_uint8() {
    let result = code_method_with_class_name(
        "trait MyId { fun f(); }
            impl MyId for UInt8 { fun f() { let x = self; } }
            ",
        "UInt8",
    );
    let expected = vec![MovUInt8(r(1), r(0)), RetVoid];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_assign_for_int() {
    let result = code_method_with_class_name(
        "trait MyId { fun f(); }
            impl MyId for Int32 { fun f() { let x = self; } }
            ",
        "Int32",
    );
    let expected = vec![MovInt32(r(1), r(0)), RetVoid];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_assign_for_int64() {
    let result = code_method_with_class_name(
        "trait MyId { fun f(); }
            impl MyId for Int64 { fun f() { let x = self; } }
            ",
        "Int64",
    );
    let expected = vec![MovInt64(r(1), r(0)), RetVoid];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_assign_for_float32() {
    let result = code_method_with_class_name(
        "trait MyId { fun f(); }
            impl MyId for Float32 { fun f() { let x = self; } }
            ",
        "Float32",
    );
    let expected = vec![MovFloat32(r(1), r(0)), RetVoid];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_assign_for_float64() {
    let result = code_method_with_class_name(
        "trait MyId { fun f(); }
            impl MyId for Float64 { fun f() { let x = self; } }
            ",
        "Float64",
    );
    let expected = vec![MovFloat64(r(1), r(0)), RetVoid];
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
    let result = code("fun f() { assert(true); }");
    let expected = vec![ConstTrue(r(0)), Assert(r(0)), RetVoid];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_assert() {
    let result = position("fun f() { assert(true); }");
    let expected = vec![(2, p(1, 17))];
    assert_eq!(expected, result);
}

#[test]
fn gen_reinterpret_float32_as_int32() {
    let result = code("fun f(a: Float32) -> Int32 { a.asInt32() }");
    let expected = vec![ReinterpretFloat32AsInt32(r(1), r(0)), RetInt32(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_reinterpret_int32_as_float32() {
    let result = code("fun f(a: Int32) -> Float32 { a.asFloat32() }");
    let expected = vec![ReinterpretInt32AsFloat32(r(1), r(0)), RetFloat32(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_reinterpret_float64_as_int64() {
    let result = code("fun f(a: Float64) -> Int64 { a.asInt64() }");
    let expected = vec![ReinterpretFloat64AsInt64(r(1), r(0)), RetInt64(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_reinterpret_int64_as_float64() {
    let result = code("fun f(a: Int64) -> Float64 { a.asFloat64() }");
    let expected = vec![ReinterpretInt64AsFloat64(r(1), r(0)), RetFloat64(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_float32_is_nan() {
    let result = code("fun f(a: Float32) -> Bool { a.isNan() }");
    let expected = vec![TestNeFloat32(r(1), r(0), r(0)), RetBool(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_float64_is_nan() {
    let result = code("fun f(a: Float64) -> Bool { a.isNan() }");
    let expected = vec![TestNeFloat64(r(1), r(0), r(0)), RetBool(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_extend_int_to_int64() {
    let result = code("fun f(a: Int32) -> Int64 { a.toInt64() }");
    let expected = vec![ExtendInt32ToInt64(r(1), r(0)), RetInt64(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_cast_int64_to_int() {
    let result = code("fun f(a: Int64) -> Int32 { a.toInt32() }");
    let expected = vec![CastInt64ToInt32(r(1), r(0)), RetInt32(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_convert_int_to_float32() {
    let result = code("fun f(a: Int32) -> Float32 { a.toFloat32() }");
    let expected = vec![ConvertInt32ToFloat32(r(1), r(0)), RetFloat32(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_convert_int_to_float64() {
    let result = code("fun f(a: Int32) -> Float64 { a.toFloat64() }");
    let expected = vec![ConvertInt32ToFloat64(r(1), r(0)), RetFloat64(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_convert_int64_to_float32() {
    let result = code("fun f(a: Int64) -> Float32 { a.toFloat32() }");
    let expected = vec![ConvertInt64ToFloat32(r(1), r(0)), RetFloat32(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_convert_int64_to_float64() {
    let result = code("fun f(a: Int64) -> Float64 { a.toFloat64() }");
    let expected = vec![ConvertInt64ToFloat64(r(1), r(0)), RetFloat64(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_truncate_float32_to_int() {
    let result = code("fun f(a: Float32) -> Int32 { a.toInt32() }");
    let expected = vec![TruncateFloat32ToInt32(r(1), r(0)), RetInt32(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_truncate_float32_to_int64() {
    let result = code("fun f(a: Float32) -> Int64 { a.toInt64() }");
    let expected = vec![TruncateFloat32ToInt64(r(1), r(0)), RetInt64(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_truncate_float64_to_int() {
    let result = code("fun f(a: Float64) -> Int32 { a.toInt32() }");
    let expected = vec![TruncateFloat64ToInt32(r(1), r(0)), RetInt32(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_truncate_float64_to_int64() {
    let result = code("fun f(a: Float64) -> Int64 { a.toInt64() }");
    let expected = vec![TruncateFloat64ToInt64(r(1), r(0)), RetInt64(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_instanceof() {
    gen(
        "@open class A class B: A fun f(a: A) -> Bool { a is B }",
        |vm, code| {
            let cls_id = vm.cls_def_by_name("B");
            let expected = vec![InstanceOf(r(1), r(0), cls_id), RetBool(r(1))];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_checked_cast() {
    gen(
        "@open class A class B: A fun f(a: A) -> B { a as B }",
        |vm, code| {
            let cls_id = vm.cls_def_by_name("B");
            let expected = vec![CheckedCast(r(0), cls_id), RetPtr(r(0))];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_checked_cast_effect() {
    gen(
        "@open class A
        class B: A
        fun f(a: A) -> B { let b = a as B; b }",
        |vm, code| {
            let cls_id = vm.cls_def_by_name("B");
            let expected = vec![MovPtr(r(1), r(0)), CheckedCast(r(1), cls_id), RetPtr(r(1))];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_enum_value() {
    let result = code("enum MyEnum { A, B } fun f() -> MyEnum { MyEnum::A }");
    let expected = vec![ConstInt32(r(0), 0), RetInt32(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_string_length() {
    let result = code("fun f(x: String) -> Int64 { x.length() }");
    let expected = vec![ArrayLength(r(1), r(0)), RetInt64(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_string_get_uint8() {
    let result = code("fun f(x: String, idx: Int64) -> UInt8 { x.getByte(idx) }");
    let expected = vec![LoadArrayUInt8(r(2), r(0), r(1)), RetUInt8(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_array_get() {
    let result = code("fun f(x: Array[Float32], idx: Int64) -> Float32 { x(idx) }");
    let expected = vec![LoadArrayFloat32(r(2), r(0), r(1)), RetFloat32(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_array_get_method() {
    let result = code("fun f(x: Array[Float32], idx: Int64) -> Float32 { x.get(idx) }");
    let expected = vec![LoadArrayFloat32(r(2), r(0), r(1)), RetFloat32(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_array_set_method() {
    let result =
        code("fun f(x: Array[Float32], idx: Int64, value: Float32) { x.set(idx, value); }");
    let expected = vec![StoreArrayFloat32(r(2), r(0), r(1)), RetVoid];
    assert_eq!(expected, result);
}

#[test]
fn gen_string_concat() {
    gen(
        "fun f(a: String, b: String) -> String { a + b }",
        |vm, code| {
            let fct_id = vm
                .cls_method_def_by_name("String", "plus", false)
                .expect("String::plus not found");
            let expected = vec![
                PushRegister(r(0)),
                PushRegister(r(1)),
                InvokeDirectPtr(r(2), fct_id),
                RetPtr(r(2)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_string_equals() {
    gen(
        "fun f(a: String, b: String) -> Bool { a != b }",
        |vm, code| {
            let fct_id = vm
                .cls_method_def_by_name("String", "equals", false)
                .expect("String::equals not found");
            let expected = vec![
                PushRegister(r(0)),
                PushRegister(r(1)),
                InvokeDirectBool(r(2), fct_id),
                NotBool(r(2), r(2)),
                RetBool(r(2)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_bool_to_string() {
    gen("fun f(a: Bool) -> String { a.toString() }", |vm, code| {
        let fct_id = vm
            .cls_method_def_by_name("Bool", "toString", false)
            .expect("Bool::toString not found");
        let expected = vec![
            PushRegister(r(0)),
            InvokeStaticPtr(r(1), fct_id),
            RetPtr(r(1)),
        ];
        assert_eq!(expected, code);
    });
}

#[test]
fn gen_cmp_strings() {
    gen(
        "fun f(a: String, b: String) -> Bool { a < b }",
        |vm, code| {
            let fct_id = vm
                .cls_method_def_by_name("String", "compareTo", false)
                .expect("String::compareTo not found");
            let expected = vec![
                PushRegister(r(0)),
                PushRegister(r(1)),
                InvokeDirectInt32(r(3), fct_id),
                ConstInt32(r(4), 0),
                TestLtInt32(r(2), r(3), r(4)),
                RetBool(r(2)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_extend_uint8() {
    let result = code("fun f(x: UInt8) -> Int32 { x.toInt32() }");
    let expected = vec![ExtendByteToInt32(r(1), r(0)), RetInt32(r(1))];
    assert_eq!(expected, result);

    let result = code("fun f(x: UInt8) -> Int64 { x.toInt64() }");
    let expected = vec![ExtendByteToInt64(r(1), r(0)), RetInt64(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_extend_int() {
    let result = code("fun f(x: Int32) -> Int64 { x.toInt64() }");
    let expected = vec![ExtendInt32ToInt64(r(1), r(0)), RetInt64(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_cast_char() {
    let result = code("fun f(x: Char) -> Int32 { x.toInt32() }");
    let expected = vec![CastCharToInt32(r(1), r(0)), RetInt32(r(1))];
    assert_eq!(expected, result);

    let result = code("fun f(x: Char) -> Int64 { x.toInt64() }");
    let expected = vec![ExtendCharToInt64(r(1), r(0)), RetInt64(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_cast_int() {
    let result = code("fun f(x: Int32) -> UInt8 { x.toUInt8() }");
    let expected = vec![CastInt32ToUInt8(r(1), r(0)), RetUInt8(r(1))];
    assert_eq!(expected, result);

    let result = code("fun f(x: Int32) -> Char { x.toCharUnchecked() }");
    let expected = vec![CastInt32ToChar(r(1), r(0)), RetChar(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_cast_int64() {
    let result = code("fun f(x: Int64) -> UInt8 { x.toUInt8() }");
    let expected = vec![CastInt64ToUInt8(r(1), r(0)), RetUInt8(r(1))];
    assert_eq!(expected, result);

    let result = code("fun f(x: Int64) -> Char { x.toCharUnchecked() }");
    let expected = vec![CastInt64ToChar(r(1), r(0)), RetChar(r(1))];
    assert_eq!(expected, result);

    let result = code("fun f(x: Int64) -> Int32 { x.toInt32() }");
    let expected = vec![CastInt64ToInt32(r(1), r(0)), RetInt32(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_compare_to_method() {
    let result = code("fun f(a: Int64, b: Int64) -> Int32 { a.compareTo(b) }");
    let expected = vec![
        SubInt64(r(3), r(0), r(1)),
        CastInt64ToInt32(r(2), r(3)),
        RetInt32(r(2)),
    ];
    assert_eq!(expected, result);

    let result = code("fun f(a: Int32, b: Int32) -> Int32 { a.compareTo(b) }");
    let expected = vec![SubInt32(r(2), r(0), r(1)), RetInt32(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_const_int() {
    let result = code("const X: Int32 = 1; fun f() -> Int32 { X }");
    let expected = vec![ConstInt32(r(0), 1), RetInt32(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_while_with_break() {
    let result = code("fun f(x: Bool) { while x { break; } }");
    let expected = vec![JumpIfFalse(r(0), 3), Jump(3), JumpLoop(0), RetVoid];
    assert_eq!(expected, result);
}

#[test]
fn gen_vec_load() {
    gen(
        "fun f(x: Vec[Int32], idx: Int64) -> Int32 { x(idx) }",
        |vm, code| {
            let fct_id = vm.cls_method_by_name("Vec", "get", false).unwrap();
            let fct_def_id = FctDef::fct_id_types(
                vm,
                fct_id,
                TypeList::single(BuiltinType::Int32),
                TypeList::empty(),
            );
            let expected = vec![
                PushRegister(r(0)),
                PushRegister(r(1)),
                InvokeDirectInt32(r(2), fct_def_id),
                RetInt32(r(2)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_vec_store() {
    gen(
        "fun f(x: Vec[Int32], idx: Int64, value: Int32) { x(idx) = value; }",
        |vm, code| {
            let fct_id = vm.cls_method_by_name("Vec", "set", false).unwrap();
            let fct_def_id = FctDef::fct_id_types(
                vm,
                fct_id,
                TypeList::single(BuiltinType::Int32),
                TypeList::empty(),
            );
            let expected = vec![
                PushRegister(r(0)),
                PushRegister(r(1)),
                PushRegister(r(2)),
                InvokeDirectVoid(fct_def_id),
                RetVoid,
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_byte_to_char() {
    let result = code("fun f(x: UInt8) -> Char { x.toChar() }");
    let expected = vec![ExtendByteToChar(r(1), r(0)), RetChar(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_int_min_value() {
    let result = code("fun f() -> Int32 { -2147483648 }");
    let expected = vec![ConstInt32(r(0), -2147483648), RetInt32(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_int_max_value() {
    let result = code("fun f() -> Int32 { 2147483647 }");
    let expected = vec![ConstInt32(r(0), 2147483647), RetInt32(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_int64_min_value() {
    let result = code("fun f() -> Int64 { -9223372036854775808L }");
    let expected = vec![ConstInt64(r(0), -9223372036854775808), RetInt64(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_int64_max_value() {
    let result = code("fun f() -> Int64 { 9223372036854775807L }");
    let expected = vec![ConstInt64(r(0), 9223372036854775807), RetInt64(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_tuple_var() {
    gen("fun f() { let x = (1, 2); }", |vm, code| {
        let tuple_id = ensure_tuple(vm, vec![BuiltinType::Int32, BuiltinType::Int32]);
        let expected = vec![
            ConstInt32(r(1), 1),
            ConstInt32(r(2), 2),
            PushRegister(r(1)),
            PushRegister(r(2)),
            NewTuple(r(0), tuple_id),
            RetVoid,
        ];
        assert_eq!(expected, code);
    });
}

#[test]
fn gen_tuple_move() {
    gen("fun f(x: (Int32, Int32)) { let y = x; }", |vm, code| {
        let tuple_id = ensure_tuple(vm, vec![BuiltinType::Int32, BuiltinType::Int32]);
        let expected = vec![MovTuple(r(1), r(0), tuple_id), RetVoid];
        assert_eq!(expected, code);
    });
}

#[test]
fn gen_tuple_element() {
    gen("fun f(x: (Int32, Int32)) -> Int32 { x.0 }", |vm, code| {
        let tuple_id = ensure_tuple(vm, vec![BuiltinType::Int32, BuiltinType::Int32]);
        let expected = vec![LoadTupleElement(r(1), r(0), tuple_id, 0), RetInt32(r(1))];
        assert_eq!(expected, code);
    });
}

fn p(line: u32, column: u32) -> Position {
    Position { line, column }
}

fn r(val: usize) -> Register {
    Register(val)
}

#[derive(PartialEq, Debug)]
pub enum Bytecode {
    AddInt32(Register, Register, Register),
    AddInt64(Register, Register, Register),
    AddFloat32(Register, Register, Register),
    AddFloat64(Register, Register, Register),

    SubInt32(Register, Register, Register),
    SubInt64(Register, Register, Register),
    SubFloat32(Register, Register, Register),
    SubFloat64(Register, Register, Register),

    NegInt32(Register, Register),
    NegInt64(Register, Register),
    NegFloat32(Register, Register),
    NegFloat64(Register, Register),

    MulInt32(Register, Register, Register),
    MulInt64(Register, Register, Register),
    MulFloat32(Register, Register, Register),
    MulFloat64(Register, Register, Register),

    DivInt32(Register, Register, Register),
    DivInt64(Register, Register, Register),
    DivFloat32(Register, Register, Register),
    DivFloat64(Register, Register, Register),

    ModInt32(Register, Register, Register),
    ModInt64(Register, Register, Register),

    AndInt32(Register, Register, Register),
    AndInt64(Register, Register, Register),
    OrInt32(Register, Register, Register),
    OrInt64(Register, Register, Register),
    XorInt32(Register, Register, Register),
    XorInt64(Register, Register, Register),
    NotBool(Register, Register),
    NotInt32(Register, Register),
    NotInt64(Register, Register),

    ShlInt32(Register, Register, Register),
    ShrInt32(Register, Register, Register),
    SarInt32(Register, Register, Register),

    ShlInt64(Register, Register, Register),
    ShrInt64(Register, Register, Register),
    SarInt64(Register, Register, Register),

    RolInt32(Register, Register, Register),
    RorInt32(Register, Register, Register),

    RolInt64(Register, Register, Register),
    RorInt64(Register, Register, Register),

    ReinterpretFloat32AsInt32(Register, Register),
    ReinterpretInt32AsFloat32(Register, Register),
    ReinterpretFloat64AsInt64(Register, Register),
    ReinterpretInt64AsFloat64(Register, Register),

    ExtendByteToChar(Register, Register),
    ExtendByteToInt32(Register, Register),
    ExtendByteToInt64(Register, Register),
    ExtendInt32ToInt64(Register, Register),
    ExtendCharToInt64(Register, Register),
    CastCharToInt32(Register, Register),
    CastInt32ToUInt8(Register, Register),
    CastInt32ToChar(Register, Register),
    CastInt64ToUInt8(Register, Register),
    CastInt64ToChar(Register, Register),
    CastInt64ToInt32(Register, Register),

    ConvertInt32ToFloat32(Register, Register),
    ConvertInt32ToFloat64(Register, Register),
    ConvertInt64ToFloat32(Register, Register),
    ConvertInt64ToFloat64(Register, Register),

    TruncateFloat32ToInt32(Register, Register),
    TruncateFloat32ToInt64(Register, Register),
    TruncateFloat64ToInt32(Register, Register),
    TruncateFloat64ToInt64(Register, Register),

    InstanceOf(Register, Register, ClassDefId),
    CheckedCast(Register, ClassDefId),

    MovBool(Register, Register),
    MovUInt8(Register, Register),
    MovChar(Register, Register),
    MovInt32(Register, Register),
    MovInt64(Register, Register),
    MovFloat32(Register, Register),
    MovFloat64(Register, Register),
    MovPtr(Register, Register),
    MovTuple(Register, Register, TupleId),

    LoadTupleElement(Register, Register, TupleId, u32),

    LoadFieldBool(Register, Register, ClassDefId, FieldId),
    LoadFieldUInt8(Register, Register, ClassDefId, FieldId),
    LoadFieldChar(Register, Register, ClassDefId, FieldId),
    LoadFieldInt32(Register, Register, ClassDefId, FieldId),
    LoadFieldInt64(Register, Register, ClassDefId, FieldId),
    LoadFieldFloat32(Register, Register, ClassDefId, FieldId),
    LoadFieldFloat64(Register, Register, ClassDefId, FieldId),
    LoadFieldPtr(Register, Register, ClassDefId, FieldId),

    StoreFieldBool(Register, Register, ClassDefId, FieldId),
    StoreFieldUInt8(Register, Register, ClassDefId, FieldId),
    StoreFieldChar(Register, Register, ClassDefId, FieldId),
    StoreFieldInt32(Register, Register, ClassDefId, FieldId),
    StoreFieldInt64(Register, Register, ClassDefId, FieldId),
    StoreFieldFloat32(Register, Register, ClassDefId, FieldId),
    StoreFieldFloat64(Register, Register, ClassDefId, FieldId),
    StoreFieldPtr(Register, Register, ClassDefId, FieldId),

    LoadGlobalBool(Register, GlobalId),
    LoadGlobalUInt8(Register, GlobalId),
    LoadGlobalChar(Register, GlobalId),
    LoadGlobalInt32(Register, GlobalId),
    LoadGlobalInt64(Register, GlobalId),
    LoadGlobalFloat32(Register, GlobalId),
    LoadGlobalFloat64(Register, GlobalId),
    LoadGlobalPtr(Register, GlobalId),

    StoreGlobalBool(Register, GlobalId),
    StoreGlobalUInt8(Register, GlobalId),
    StoreGlobalChar(Register, GlobalId),
    StoreGlobalInt32(Register, GlobalId),
    StoreGlobalInt64(Register, GlobalId),
    StoreGlobalFloat32(Register, GlobalId),
    StoreGlobalFloat64(Register, GlobalId),
    StoreGlobalPtr(Register, GlobalId),

    PushRegister(Register),

    ConstNil(Register),
    ConstTrue(Register),
    ConstFalse(Register),
    ConstZeroUInt8(Register),
    ConstZeroChar(Register),
    ConstZeroInt32(Register),
    ConstZeroInt64(Register),
    ConstZeroFloat32(Register),
    ConstZeroFloat64(Register),
    ConstUInt8(Register, u8),
    ConstChar(Register, char),
    ConstInt32(Register, i32),
    ConstInt64(Register, i64),
    ConstFloat32(Register, f32),
    ConstFloat64(Register, f64),
    ConstString(Register, String),

    TestEqPtr(Register, Register, Register),
    TestNePtr(Register, Register, Register),

    TestEqBool(Register, Register, Register),
    TestNeBool(Register, Register, Register),

    TestEqUInt8(Register, Register, Register),
    TestNeUInt8(Register, Register, Register),
    TestGtUInt8(Register, Register, Register),
    TestGeUInt8(Register, Register, Register),
    TestLtUInt8(Register, Register, Register),
    TestLeUInt8(Register, Register, Register),

    TestEqChar(Register, Register, Register),
    TestNeChar(Register, Register, Register),
    TestGtChar(Register, Register, Register),
    TestGeChar(Register, Register, Register),
    TestLtChar(Register, Register, Register),
    TestLeChar(Register, Register, Register),

    TestEqEnum(Register, Register, Register),
    TestNeEnum(Register, Register, Register),

    TestEqInt32(Register, Register, Register),
    TestNeInt32(Register, Register, Register),
    TestGtInt32(Register, Register, Register),
    TestGeInt32(Register, Register, Register),
    TestLtInt32(Register, Register, Register),
    TestLeInt32(Register, Register, Register),

    TestEqInt64(Register, Register, Register),
    TestNeInt64(Register, Register, Register),
    TestGtInt64(Register, Register, Register),
    TestGeInt64(Register, Register, Register),
    TestLtInt64(Register, Register, Register),
    TestLeInt64(Register, Register, Register),

    TestEqFloat32(Register, Register, Register),
    TestNeFloat32(Register, Register, Register),
    TestGtFloat32(Register, Register, Register),
    TestGeFloat32(Register, Register, Register),
    TestLtFloat32(Register, Register, Register),
    TestLeFloat32(Register, Register, Register),

    TestEqFloat64(Register, Register, Register),
    TestNeFloat64(Register, Register, Register),
    TestGtFloat64(Register, Register, Register),
    TestGeFloat64(Register, Register, Register),
    TestLtFloat64(Register, Register, Register),
    TestLeFloat64(Register, Register, Register),

    Assert(Register),

    JumpLoop(usize),
    Jump(usize),
    JumpIfFalse(Register, usize),
    JumpIfTrue(Register, usize),

    InvokeDirectVoid(FctDefId),
    InvokeDirectBool(Register, FctDefId),
    InvokeDirectUInt8(Register, FctDefId),
    InvokeDirectChar(Register, FctDefId),
    InvokeDirectInt32(Register, FctDefId),
    InvokeDirectInt64(Register, FctDefId),
    InvokeDirectFloat32(Register, FctDefId),
    InvokeDirectFloat64(Register, FctDefId),
    InvokeDirectPtr(Register, FctDefId),

    InvokeVirtualVoid(FctDefId),
    InvokeVirtualBool(Register, FctDefId),
    InvokeVirtualUInt8(Register, FctDefId),
    InvokeVirtualChar(Register, FctDefId),
    InvokeVirtualInt32(Register, FctDefId),
    InvokeVirtualInt64(Register, FctDefId),
    InvokeVirtualFloat32(Register, FctDefId),
    InvokeVirtualFloat64(Register, FctDefId),
    InvokeVirtualPtr(Register, FctDefId),

    InvokeStaticVoid(FctDefId),
    InvokeStaticBool(Register, FctDefId),
    InvokeStaticUInt8(Register, FctDefId),
    InvokeStaticChar(Register, FctDefId),
    InvokeStaticInt32(Register, FctDefId),
    InvokeStaticInt64(Register, FctDefId),
    InvokeStaticFloat32(Register, FctDefId),
    InvokeStaticFloat64(Register, FctDefId),
    InvokeStaticPtr(Register, FctDefId),

    NewObject(Register, ClassDefId),
    NewArray(Register, ClassDefId, Register),
    NewTuple(Register, TupleId),

    NilCheck(Register),

    ArrayLength(Register, Register),
    ArrayBoundCheck(Register, Register),

    LoadArrayBool(Register, Register, Register),
    LoadArrayUInt8(Register, Register, Register),
    LoadArrayChar(Register, Register, Register),
    LoadArrayInt32(Register, Register, Register),
    LoadArrayInt64(Register, Register, Register),
    LoadArrayFloat32(Register, Register, Register),
    LoadArrayFloat64(Register, Register, Register),
    LoadArrayPtr(Register, Register, Register),

    StoreArrayBool(Register, Register, Register),
    StoreArrayUInt8(Register, Register, Register),
    StoreArrayChar(Register, Register, Register),
    StoreArrayInt32(Register, Register, Register),
    StoreArrayInt64(Register, Register, Register),
    StoreArrayFloat32(Register, Register, Register),
    StoreArrayFloat64(Register, Register, Register),
    StoreArrayPtr(Register, Register, Register),

    RetVoid,
    RetBool(Register),
    RetUInt8(Register),
    RetChar(Register),
    RetInt32(Register),
    RetInt64(Register),
    RetFloat32(Register),
    RetFloat64(Register),
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

    fn visit_add_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::AddInt32(dest, lhs, rhs));
    }
    fn visit_add_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::AddInt64(dest, lhs, rhs));
    }
    fn visit_add_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::AddFloat32(dest, lhs, rhs));
    }
    fn visit_add_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::AddFloat64(dest, lhs, rhs));
    }

    fn visit_sub_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::SubInt32(dest, lhs, rhs));
    }
    fn visit_sub_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::SubInt64(dest, lhs, rhs));
    }
    fn visit_sub_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::SubFloat32(dest, lhs, rhs));
    }
    fn visit_sub_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::SubFloat64(dest, lhs, rhs));
    }

    fn visit_neg_int32(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::NegInt32(dest, src));
    }
    fn visit_neg_int64(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::NegInt64(dest, src));
    }
    fn visit_neg_float32(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::NegFloat32(dest, src));
    }
    fn visit_neg_float64(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::NegFloat64(dest, src));
    }

    fn visit_mul_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::MulInt32(dest, lhs, rhs));
    }
    fn visit_mul_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::MulInt64(dest, lhs, rhs));
    }
    fn visit_mul_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::MulFloat32(dest, lhs, rhs));
    }
    fn visit_mul_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::MulFloat64(dest, lhs, rhs));
    }

    fn visit_div_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::DivInt32(dest, lhs, rhs));
    }
    fn visit_div_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::DivInt64(dest, lhs, rhs));
    }
    fn visit_div_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::DivFloat32(dest, lhs, rhs));
    }
    fn visit_div_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::DivFloat64(dest, lhs, rhs));
    }

    fn visit_mod_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::ModInt32(dest, lhs, rhs));
    }
    fn visit_mod_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::ModInt64(dest, lhs, rhs));
    }

    fn visit_and_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::AndInt32(dest, lhs, rhs));
    }
    fn visit_and_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::AndInt64(dest, lhs, rhs));
    }

    fn visit_or_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::OrInt32(dest, lhs, rhs));
    }
    fn visit_or_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::OrInt64(dest, lhs, rhs));
    }

    fn visit_xor_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::XorInt32(dest, lhs, rhs));
    }
    fn visit_xor_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::XorInt64(dest, lhs, rhs));
    }

    fn visit_not_bool(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::NotBool(dest, src));
    }
    fn visit_not_int32(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::NotInt32(dest, src));
    }
    fn visit_not_int64(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::NotInt64(dest, src));
    }

    fn visit_shl_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::ShlInt32(dest, lhs, rhs));
    }
    fn visit_shr_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::ShrInt32(dest, lhs, rhs));
    }
    fn visit_sar_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::SarInt32(dest, lhs, rhs));
    }

    fn visit_shl_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::ShlInt64(dest, lhs, rhs));
    }
    fn visit_shr_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::ShrInt64(dest, lhs, rhs));
    }
    fn visit_sar_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::SarInt64(dest, lhs, rhs));
    }

    fn visit_rol_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::RolInt32(dest, lhs, rhs));
    }
    fn visit_ror_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::RorInt32(dest, lhs, rhs));
    }
    fn visit_rol_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::RolInt64(dest, lhs, rhs));
    }
    fn visit_ror_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::RorInt64(dest, lhs, rhs));
    }

    fn visit_reinterpret_float32_as_int32(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::ReinterpretFloat32AsInt32(dest, src));
    }
    fn visit_reinterpret_int32_as_float32(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::ReinterpretInt32AsFloat32(dest, src));
    }
    fn visit_reinterpret_float64_as_int64(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::ReinterpretFloat64AsInt64(dest, src));
    }
    fn visit_reinterpret_int64_as_float64(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::ReinterpretInt64AsFloat64(dest, src));
    }

    fn visit_extend_byte_to_char(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::ExtendByteToChar(dest, src));
    }
    fn visit_extend_byte_to_int32(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::ExtendByteToInt32(dest, src));
    }
    fn visit_extend_byte_to_int64(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::ExtendByteToInt64(dest, src));
    }
    fn visit_extend_int32_to_int64(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::ExtendInt32ToInt64(dest, src));
    }
    fn visit_extend_char_to_int64(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::ExtendCharToInt64(dest, src));
    }
    fn visit_cast_char_to_int32(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::CastCharToInt32(dest, src));
    }
    fn visit_cast_int32_to_uint8(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::CastInt32ToUInt8(dest, src));
    }
    fn visit_cast_int32_to_char(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::CastInt32ToChar(dest, src));
    }
    fn visit_cast_int64_to_uint8(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::CastInt64ToUInt8(dest, src));
    }
    fn visit_cast_int64_to_char(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::CastInt64ToChar(dest, src));
    }
    fn visit_cast_int64_to_int32(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::CastInt64ToInt32(dest, src));
    }

    fn visit_convert_int32_to_float32(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::ConvertInt32ToFloat32(dest, src));
    }
    fn visit_convert_int32_to_float64(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::ConvertInt32ToFloat64(dest, src));
    }
    fn visit_convert_int64_to_float32(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::ConvertInt64ToFloat32(dest, src));
    }
    fn visit_convert_int64_to_float64(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::ConvertInt64ToFloat64(dest, src));
    }

    fn visit_truncate_float32_to_int32(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::TruncateFloat32ToInt32(dest, src));
    }
    fn visit_truncate_float32_to_int64(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::TruncateFloat32ToInt64(dest, src));
    }
    fn visit_truncate_float64_to_int32(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::TruncateFloat64ToInt32(dest, src));
    }
    fn visit_truncate_float64_to_int64(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::TruncateFloat64ToInt64(dest, src));
    }

    fn visit_instance_of(&mut self, dest: Register, src: Register, cls_id: ClassDefId) {
        self.emit(Bytecode::InstanceOf(dest, src, cls_id));
    }
    fn visit_checked_cast(&mut self, src: Register, cls_id: ClassDefId) {
        self.emit(Bytecode::CheckedCast(src, cls_id));
    }

    fn visit_mov_bool(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::MovBool(dest, src));
    }
    fn visit_mov_uint8(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::MovUInt8(dest, src));
    }
    fn visit_mov_char(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::MovChar(dest, src));
    }
    fn visit_mov_int32(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::MovInt32(dest, src));
    }
    fn visit_mov_int64(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::MovInt64(dest, src));
    }
    fn visit_mov_float32(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::MovFloat32(dest, src));
    }
    fn visit_mov_float64(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::MovFloat64(dest, src));
    }
    fn visit_mov_ptr(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::MovPtr(dest, src));
    }
    fn visit_mov_tuple(&mut self, dest: Register, src: Register, tuple_id: TupleId) {
        self.emit(Bytecode::MovTuple(dest, src, tuple_id))
    }

    fn visit_load_tuple_element(
        &mut self,
        src: Register,
        dest: Register,
        tuple_id: TupleId,
        element: u32,
    ) {
        self.emit(Bytecode::LoadTupleElement(src, dest, tuple_id, element));
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
    fn visit_load_field_uint8(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit(Bytecode::LoadFieldUInt8(dest, obj, cls, field));
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
    fn visit_load_field_int32(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit(Bytecode::LoadFieldInt32(dest, obj, cls, field));
    }
    fn visit_load_field_int64(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit(Bytecode::LoadFieldInt64(dest, obj, cls, field));
    }
    fn visit_load_field_float32(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit(Bytecode::LoadFieldFloat32(dest, obj, cls, field));
    }
    fn visit_load_field_float64(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit(Bytecode::LoadFieldFloat64(dest, obj, cls, field));
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
    fn visit_store_field_uint8(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit(Bytecode::StoreFieldUInt8(src, obj, cls, field));
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
    fn visit_store_field_int32(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit(Bytecode::StoreFieldInt32(src, obj, cls, field));
    }
    fn visit_store_field_int64(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit(Bytecode::StoreFieldInt64(src, obj, cls, field));
    }
    fn visit_store_field_float32(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit(Bytecode::StoreFieldFloat32(src, obj, cls, field));
    }
    fn visit_store_field_float64(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit(Bytecode::StoreFieldFloat64(src, obj, cls, field));
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
    fn visit_load_global_uint8(&mut self, dest: Register, glob: GlobalId) {
        self.emit(Bytecode::LoadGlobalUInt8(dest, glob));
    }
    fn visit_load_global_char(&mut self, dest: Register, glob: GlobalId) {
        self.emit(Bytecode::LoadGlobalChar(dest, glob));
    }
    fn visit_load_global_int32(&mut self, dest: Register, glob: GlobalId) {
        self.emit(Bytecode::LoadGlobalInt32(dest, glob));
    }
    fn visit_load_global_int64(&mut self, dest: Register, glob: GlobalId) {
        self.emit(Bytecode::LoadGlobalInt64(dest, glob));
    }
    fn visit_load_global_float32(&mut self, dest: Register, glob: GlobalId) {
        self.emit(Bytecode::LoadGlobalFloat32(dest, glob));
    }
    fn visit_load_global_float64(&mut self, dest: Register, glob: GlobalId) {
        self.emit(Bytecode::LoadGlobalFloat64(dest, glob));
    }
    fn visit_load_global_ptr(&mut self, dest: Register, glob: GlobalId) {
        self.emit(Bytecode::LoadGlobalPtr(dest, glob));
    }

    fn visit_store_global_bool(&mut self, src: Register, glob: GlobalId) {
        self.emit(Bytecode::StoreGlobalBool(src, glob));
    }
    fn visit_store_global_uint8(&mut self, src: Register, glob: GlobalId) {
        self.emit(Bytecode::StoreGlobalUInt8(src, glob));
    }
    fn visit_store_global_char(&mut self, src: Register, glob: GlobalId) {
        self.emit(Bytecode::StoreGlobalChar(src, glob));
    }
    fn visit_store_global_int32(&mut self, src: Register, glob: GlobalId) {
        self.emit(Bytecode::StoreGlobalInt32(src, glob));
    }
    fn visit_store_global_int64(&mut self, src: Register, glob: GlobalId) {
        self.emit(Bytecode::StoreGlobalInt64(src, glob));
    }
    fn visit_store_global_float32(&mut self, src: Register, glob: GlobalId) {
        self.emit(Bytecode::StoreGlobalFloat32(src, glob));
    }
    fn visit_store_global_float64(&mut self, src: Register, glob: GlobalId) {
        self.emit(Bytecode::StoreGlobalFloat64(src, glob));
    }
    fn visit_store_global_ptr(&mut self, src: Register, glob: GlobalId) {
        self.emit(Bytecode::StoreGlobalPtr(src, glob));
    }

    fn visit_push_register(&mut self, src: Register) {
        self.emit(Bytecode::PushRegister(src));
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
    fn visit_const_zero_uint8(&mut self, dest: Register) {
        self.emit(Bytecode::ConstZeroUInt8(dest));
    }
    fn visit_const_zero_char(&mut self, dest: Register) {
        self.emit(Bytecode::ConstZeroChar(dest));
    }
    fn visit_const_zero_int32(&mut self, dest: Register) {
        self.emit(Bytecode::ConstZeroInt32(dest));
    }
    fn visit_const_zero_int64(&mut self, dest: Register) {
        self.emit(Bytecode::ConstZeroInt64(dest));
    }
    fn visit_const_zero_float32(&mut self, dest: Register) {
        self.emit(Bytecode::ConstZeroFloat32(dest));
    }
    fn visit_const_zero_float64(&mut self, dest: Register) {
        self.emit(Bytecode::ConstZeroFloat64(dest));
    }
    fn visit_const_char(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self.bc.const_pool(idx).to_char().expect("char expected");
        self.emit(Bytecode::ConstChar(dest, value));
    }
    fn visit_const_uint8(&mut self, dest: Register, value: u8) {
        self.emit(Bytecode::ConstUInt8(dest, value));
    }
    fn visit_const_int32(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self.bc.const_pool(idx).to_int32().expect("int expected");
        self.emit(Bytecode::ConstInt32(dest, value));
    }
    fn visit_const_int64(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self.bc.const_pool(idx).to_int64().expect("int64 expected");
        self.emit(Bytecode::ConstInt64(dest, value));
    }
    fn visit_const_float32(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self
            .bc
            .const_pool(idx)
            .to_float32()
            .expect("float32 expected");
        self.emit(Bytecode::ConstFloat32(dest, value));
    }
    fn visit_const_float64(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self
            .bc
            .const_pool(idx)
            .to_float64()
            .expect("float64 expected");
        self.emit(Bytecode::ConstFloat64(dest, value));
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

    fn visit_test_eq_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestEqUInt8(dest, lhs, rhs));
    }
    fn visit_test_ne_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestNeUInt8(dest, lhs, rhs));
    }
    fn visit_test_gt_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestGtUInt8(dest, lhs, rhs));
    }
    fn visit_test_ge_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestGeUInt8(dest, lhs, rhs));
    }
    fn visit_test_lt_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestLtUInt8(dest, lhs, rhs));
    }
    fn visit_test_le_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestLeUInt8(dest, lhs, rhs));
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

    fn visit_test_eq_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestEqInt32(dest, lhs, rhs));
    }
    fn visit_test_ne_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestNeInt32(dest, lhs, rhs));
    }
    fn visit_test_gt_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestGtInt32(dest, lhs, rhs));
    }
    fn visit_test_ge_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestGeInt32(dest, lhs, rhs));
    }
    fn visit_test_lt_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestLtInt32(dest, lhs, rhs));
    }
    fn visit_test_le_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestLeInt32(dest, lhs, rhs));
    }

    fn visit_test_eq_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestEqInt64(dest, lhs, rhs));
    }
    fn visit_test_ne_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestNeInt64(dest, lhs, rhs));
    }
    fn visit_test_gt_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestGtInt64(dest, lhs, rhs));
    }
    fn visit_test_ge_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestGeInt64(dest, lhs, rhs));
    }
    fn visit_test_lt_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestLtInt64(dest, lhs, rhs));
    }
    fn visit_test_le_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestLeInt64(dest, lhs, rhs));
    }

    fn visit_test_eq_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestEqFloat32(dest, lhs, rhs));
    }
    fn visit_test_ne_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestNeFloat32(dest, lhs, rhs));
    }
    fn visit_test_gt_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestGtFloat32(dest, lhs, rhs));
    }
    fn visit_test_ge_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestGeFloat32(dest, lhs, rhs));
    }
    fn visit_test_lt_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestLtFloat32(dest, lhs, rhs));
    }
    fn visit_test_le_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestLeFloat32(dest, lhs, rhs));
    }

    fn visit_test_eq_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestEqFloat64(dest, lhs, rhs));
    }
    fn visit_test_ne_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestNeFloat64(dest, lhs, rhs));
    }
    fn visit_test_gt_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestGtFloat64(dest, lhs, rhs));
    }
    fn visit_test_ge_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestGeFloat64(dest, lhs, rhs));
    }
    fn visit_test_lt_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestLtFloat64(dest, lhs, rhs));
    }
    fn visit_test_le_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestLeFloat64(dest, lhs, rhs));
    }

    fn visit_assert(&mut self, value: Register) {
        self.emit(Bytecode::Assert(value));
    }

    fn visit_jump_if_false(&mut self, opnd: Register, offset: u32) {
        let offset = BytecodeOffset(self.pc.to_u32() + offset);
        self.jumps.push((self.next_idx - 1, offset));
        self.emit(Bytecode::JumpIfFalse(opnd, 0));
    }
    fn visit_jump_if_false_const(&mut self, opnd: Register, idx: ConstPoolIdx) {
        let value = self.bc.const_pool(idx).to_int32().expect("int expected");
        self.visit_jump_if_false(opnd, value as u32);
    }
    fn visit_jump_if_true(&mut self, opnd: Register, offset: u32) {
        let offset = BytecodeOffset(self.pc.to_u32() + offset);
        self.jumps.push((self.next_idx - 1, offset));
        self.emit(Bytecode::JumpIfTrue(opnd, 0));
    }
    fn visit_jump_if_true_const(&mut self, opnd: Register, idx: ConstPoolIdx) {
        let value = self.bc.const_pool(idx).to_int32().expect("int expected");
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
        let value = self.bc.const_pool(idx).to_int32().expect("int expected");
        self.visit_jump(value as u32);
    }

    fn visit_invoke_direct_void(&mut self, fctdef: FctDefId) {
        self.emit(Bytecode::InvokeDirectVoid(fctdef));
    }
    fn visit_invoke_direct_bool(&mut self, dest: Register, fctdef: FctDefId) {
        self.emit(Bytecode::InvokeDirectBool(dest, fctdef));
    }
    fn visit_invoke_direct_uint8(&mut self, dest: Register, fctdef: FctDefId) {
        self.emit(Bytecode::InvokeDirectUInt8(dest, fctdef));
    }
    fn visit_invoke_direct_char(&mut self, dest: Register, fctdef: FctDefId) {
        self.emit(Bytecode::InvokeDirectChar(dest, fctdef));
    }
    fn visit_invoke_direct_int32(&mut self, dest: Register, fctdef: FctDefId) {
        self.emit(Bytecode::InvokeDirectInt32(dest, fctdef));
    }
    fn visit_invoke_direct_int64(&mut self, dest: Register, fctdef: FctDefId) {
        self.emit(Bytecode::InvokeDirectInt64(dest, fctdef));
    }
    fn visit_invoke_direct_float32(&mut self, dest: Register, fctdef: FctDefId) {
        self.emit(Bytecode::InvokeDirectFloat32(dest, fctdef));
    }
    fn visit_invoke_direct_float64(&mut self, dest: Register, fctdef: FctDefId) {
        self.emit(Bytecode::InvokeDirectFloat64(dest, fctdef));
    }
    fn visit_invoke_direct_ptr(&mut self, dest: Register, fctdef: FctDefId) {
        self.emit(Bytecode::InvokeDirectPtr(dest, fctdef));
    }

    fn visit_invoke_virtual_void(&mut self, fctdef: FctDefId) {
        self.emit(Bytecode::InvokeVirtualVoid(fctdef));
    }
    fn visit_invoke_virtual_bool(&mut self, dest: Register, fctdef: FctDefId) {
        self.emit(Bytecode::InvokeVirtualBool(dest, fctdef));
    }
    fn visit_invoke_virtual_uint8(&mut self, dest: Register, fctdef: FctDefId) {
        self.emit(Bytecode::InvokeVirtualUInt8(dest, fctdef));
    }
    fn visit_invoke_virtual_char(&mut self, dest: Register, fctdef: FctDefId) {
        self.emit(Bytecode::InvokeVirtualChar(dest, fctdef));
    }
    fn visit_invoke_virtual_int32(&mut self, dest: Register, fctdef: FctDefId) {
        self.emit(Bytecode::InvokeVirtualInt32(dest, fctdef));
    }
    fn visit_invoke_virtual_int64(&mut self, dest: Register, fctdef: FctDefId) {
        self.emit(Bytecode::InvokeVirtualInt64(dest, fctdef));
    }
    fn visit_invoke_virtual_float32(&mut self, dest: Register, fctdef: FctDefId) {
        self.emit(Bytecode::InvokeVirtualFloat32(dest, fctdef));
    }
    fn visit_invoke_virtual_float64(&mut self, dest: Register, fctdef: FctDefId) {
        self.emit(Bytecode::InvokeVirtualFloat64(dest, fctdef));
    }
    fn visit_invoke_virtual_ptr(&mut self, dest: Register, fctdef: FctDefId) {
        self.emit(Bytecode::InvokeVirtualPtr(dest, fctdef));
    }

    fn visit_invoke_static_void(&mut self, fctdef: FctDefId) {
        self.emit(Bytecode::InvokeStaticVoid(fctdef));
    }
    fn visit_invoke_static_bool(&mut self, dest: Register, fctdef: FctDefId) {
        self.emit(Bytecode::InvokeStaticBool(dest, fctdef));
    }
    fn visit_invoke_static_uint8(&mut self, dest: Register, fctdef: FctDefId) {
        self.emit(Bytecode::InvokeStaticUInt8(dest, fctdef));
    }
    fn visit_invoke_static_char(&mut self, dest: Register, fctdef: FctDefId) {
        self.emit(Bytecode::InvokeStaticChar(dest, fctdef));
    }
    fn visit_invoke_static_int32(&mut self, dest: Register, fctdef: FctDefId) {
        self.emit(Bytecode::InvokeStaticInt32(dest, fctdef));
    }
    fn visit_invoke_static_int64(&mut self, dest: Register, fctdef: FctDefId) {
        self.emit(Bytecode::InvokeStaticInt64(dest, fctdef));
    }
    fn visit_invoke_static_float32(&mut self, dest: Register, fctdef: FctDefId) {
        self.emit(Bytecode::InvokeStaticFloat32(dest, fctdef));
    }
    fn visit_invoke_static_float64(&mut self, dest: Register, fctdef: FctDefId) {
        self.emit(Bytecode::InvokeStaticFloat64(dest, fctdef));
    }
    fn visit_invoke_static_ptr(&mut self, dest: Register, fctdef: FctDefId) {
        self.emit(Bytecode::InvokeStaticPtr(dest, fctdef));
    }

    fn visit_new_object(&mut self, dest: Register, cls: ClassDefId) {
        self.emit(Bytecode::NewObject(dest, cls));
    }
    fn visit_new_array(&mut self, dest: Register, cls: ClassDefId, length: Register) {
        self.emit(Bytecode::NewArray(dest, cls, length));
    }
    fn visit_new_tuple(&mut self, dest: Register, tuple_id: TupleId) {
        self.emit(Bytecode::NewTuple(dest, tuple_id));
    }

    fn visit_nil_check(&mut self, obj: Register) {
        self.emit(Bytecode::NilCheck(obj));
    }

    fn visit_array_length(&mut self, dest: Register, arr: Register) {
        self.emit(Bytecode::ArrayLength(dest, arr));
    }
    fn visit_array_bound_check(&mut self, arr: Register, idx: Register) {
        self.emit(Bytecode::ArrayBoundCheck(arr, idx));
    }

    fn visit_load_array_bool(&mut self, dest: Register, arr: Register, idx: Register) {
        self.emit(Bytecode::LoadArrayBool(dest, arr, idx));
    }
    fn visit_load_array_uint8(&mut self, dest: Register, arr: Register, idx: Register) {
        self.emit(Bytecode::LoadArrayUInt8(dest, arr, idx));
    }
    fn visit_load_array_char(&mut self, dest: Register, arr: Register, idx: Register) {
        self.emit(Bytecode::LoadArrayChar(dest, arr, idx));
    }
    fn visit_load_array_int32(&mut self, dest: Register, arr: Register, idx: Register) {
        self.emit(Bytecode::LoadArrayInt32(dest, arr, idx));
    }
    fn visit_load_array_int64(&mut self, dest: Register, arr: Register, idx: Register) {
        self.emit(Bytecode::LoadArrayInt64(dest, arr, idx));
    }
    fn visit_load_array_float32(&mut self, dest: Register, arr: Register, idx: Register) {
        self.emit(Bytecode::LoadArrayFloat32(dest, arr, idx));
    }
    fn visit_load_array_float64(&mut self, dest: Register, arr: Register, idx: Register) {
        self.emit(Bytecode::LoadArrayFloat64(dest, arr, idx));
    }
    fn visit_load_array_ptr(&mut self, dest: Register, arr: Register, idx: Register) {
        self.emit(Bytecode::LoadArrayPtr(dest, arr, idx));
    }

    fn visit_store_array_bool(&mut self, src: Register, arr: Register, idx: Register) {
        self.emit(Bytecode::StoreArrayBool(src, arr, idx));
    }
    fn visit_store_array_uint8(&mut self, src: Register, arr: Register, idx: Register) {
        self.emit(Bytecode::StoreArrayUInt8(src, arr, idx));
    }
    fn visit_store_array_char(&mut self, src: Register, arr: Register, idx: Register) {
        self.emit(Bytecode::StoreArrayChar(src, arr, idx));
    }
    fn visit_store_array_int32(&mut self, src: Register, arr: Register, idx: Register) {
        self.emit(Bytecode::StoreArrayInt32(src, arr, idx));
    }
    fn visit_store_array_int64(&mut self, src: Register, arr: Register, idx: Register) {
        self.emit(Bytecode::StoreArrayInt64(src, arr, idx));
    }
    fn visit_store_array_float32(&mut self, src: Register, arr: Register, idx: Register) {
        self.emit(Bytecode::StoreArrayFloat32(src, arr, idx));
    }
    fn visit_store_array_float64(&mut self, src: Register, arr: Register, idx: Register) {
        self.emit(Bytecode::StoreArrayFloat64(src, arr, idx));
    }
    fn visit_store_array_ptr(&mut self, src: Register, arr: Register, idx: Register) {
        self.emit(Bytecode::StoreArrayPtr(src, arr, idx));
    }

    fn visit_ret_void(&mut self) {
        self.emit(Bytecode::RetVoid);
    }
    fn visit_ret_bool(&mut self, opnd: Register) {
        self.emit(Bytecode::RetBool(opnd));
    }
    fn visit_ret_uint8(&mut self, opnd: Register) {
        self.emit(Bytecode::RetUInt8(opnd));
    }
    fn visit_ret_char(&mut self, opnd: Register) {
        self.emit(Bytecode::RetChar(opnd));
    }
    fn visit_ret_int32(&mut self, opnd: Register) {
        self.emit(Bytecode::RetInt32(opnd));
    }
    fn visit_ret_int64(&mut self, opnd: Register) {
        self.emit(Bytecode::RetInt64(opnd));
    }
    fn visit_ret_float32(&mut self, opnd: Register) {
        self.emit(Bytecode::RetFloat32(opnd));
    }
    fn visit_ret_float64(&mut self, opnd: Register) {
        self.emit(Bytecode::RetFloat64(opnd));
    }
    fn visit_ret_ptr(&mut self, opnd: Register) {
        self.emit(Bytecode::RetPtr(opnd));
    }
}
