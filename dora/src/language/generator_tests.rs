use std::collections::HashMap;
use std::mem;

use self::Bytecode::*;
use crate::bytecode::{
    self, BytecodeFunction, BytecodeOffset, BytecodeVisitor, ConstPoolEntry, ConstPoolIdx, Register,
};
use crate::language::generator::generate_fct;
use crate::language::sem_analysis::{
    create_tuple, GlobalDefinitionId, SemAnalysis, StructDefinitionFieldId, TypeParamId,
};
use crate::language::test;
use crate::language::ty::{SourceType, SourceTypeArray};
use dora_parser::lexer::position::Position;

fn code(code: &'static str) -> Vec<Bytecode> {
    test::check_valid(code, |sa| {
        let fct_id = sa.fct_by_name("f").expect("no function `f`.");
        let fct = generate_fct(sa, fct_id);
        build(&fct)
    })
}

fn position(code: &'static str) -> Vec<(u32, Position)> {
    test::check_valid(code, |sa| {
        let fct_id = sa.fct_by_name("f").expect("no function `f`.");
        let fct = generate_fct(sa, fct_id);
        fct.positions().to_vec()
    })
}

fn code_method(code: &'static str) -> Vec<Bytecode> {
    code_method_with_class_name(code, "Foo")
}

fn code_method_with_class_name(code: &'static str, class_name: &'static str) -> Vec<Bytecode> {
    test::check_valid(code, |sa| {
        let fct_id = sa
            .cls_method_by_name(class_name, "f", false)
            .unwrap_or_else(|| panic!("no function `f` in Class `{}`.", class_name));
        let fct = generate_fct(sa, fct_id);
        build(&fct)
    })
}

fn code_method_with_struct_name(code: &'static str, struct_name: &'static str) -> Vec<Bytecode> {
    test::check_valid(code, |sa| {
        let fct_id = sa
            .struct_method_by_name(struct_name, "f", false)
            .unwrap_or_else(|| panic!("no function `f` in Class `{}`.", struct_name));
        let fct = generate_fct(sa, fct_id);
        build(&fct)
    })
}

fn gen<F>(code: &'static str, testfct: F)
where
    F: FnOnce(&SemAnalysis, Vec<Bytecode>),
{
    test::check_valid(code, |sa| {
        let fct_id = sa.fct_by_name("f").expect("no function `f`.");
        let fct = generate_fct(sa, fct_id);
        let code = build(&fct);

        testfct(sa, code);
    })
}

fn gen_fct<F>(code: &'static str, testfct: F)
where
    F: FnOnce(&SemAnalysis, Vec<Bytecode>, BytecodeFunction),
{
    test::check_valid(code, |sa| {
        let fct_id = sa.fct_by_name("f").expect("no function `f`.");
        let fct = generate_fct(sa, fct_id);
        let code = build(&fct);

        testfct(sa, code, fct);
    })
}

#[test]
fn gen_generic_identity() {
    let result = code("fun f[T](x: T): T { x }");
    let expected = vec![Ret(r(0))];
    assert_eq!(expected, result);

    let result = code("fun f[T](x: T): T { let y = x; y }");
    let expected = vec![Mov(r(1), r(0)), Ret(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_generic_static_trait() {
    let result = code("trait Foo { @static fun baz(); } fun f[T: Foo]() { T::baz() }");
    let expected = vec![
        InvokeGenericStatic(r(0), ConstPoolIdx(0)),
        Ret(r(0)),
        Ret(r(0)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_generic_direct_trait() {
    let result = code("trait Foo { fun baz(); } fun f[T: Foo](obj: T) { obj.baz() }");
    let expected = vec![
        PushRegister(r(0)),
        InvokeGenericDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
        Ret(r(1)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_load_field_uint8() {
    gen_fct(
        "class Foo(bar: UInt8) fun f(a: Foo): UInt8 { return a.bar; }",
        |sa, code, fct| {
            let (cls, field) = sa.field_by_name("Foo", "bar");
            let expected = vec![LoadField(r(1), r(0), ConstPoolIdx(0)), Ret(r(1))];
            assert_eq!(expected, code);

            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Field(cls, SourceTypeArray::empty(), field)
            );
        },
    );
}

#[test]
fn gen_position_load_field_uint8() {
    let result = position("class Foo(bar: UInt8) fun f(a: Foo): UInt8 { return a.bar; }");
    let expected = vec![(0, p(1, 54))];
    assert_eq!(expected, result);
}

#[test]
fn gen_store_field_uint8() {
    gen_fct(
        "class Foo(bar: UInt8) fun f(a: Foo, b: UInt8) { a.bar = b; }",
        |sa, code, fct| {
            let (cls, field) = sa.field_by_name("Foo", "bar");
            let expected = vec![StoreField(r(1), r(0), ConstPoolIdx(0)), Ret(r(2))];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Field(cls, SourceTypeArray::empty(), field)
            );
        },
    );
}

#[test]
fn gen_position_store_field_uint8() {
    let result = position("class Foo(bar: UInt8) fun f(a: Foo, b: UInt8) { a.bar = b; }");
    let expected = vec![(0, p(1, 55))];
    assert_eq!(expected, result);
}

#[test]
fn gen_add_int() {
    let result = code("fun f(): Int32 { return 1i32 + 2i32; }");
    let expected = vec![
        ConstInt32(r(1), 1),
        ConstInt32(r(2), 2),
        Add(r(0), r(1), r(2)),
        Ret(r(0)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_add_float32() {
    let result = code("fun f(): Float32 { return 1f32 + 2f32; }");
    let expected = vec![
        ConstFloat32(r(1), 1_f32),
        ConstFloat32(r(2), 2_f32),
        Add(r(0), r(1), r(2)),
        Ret(r(0)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_add_float64() {
    let result = code("fun f(a: Float64, b: Float64): Float64 { return a + b; }");
    let expected = vec![Add(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_id_int() {
    let result = code("fun f(a: Int32): Int32 { return a; }");
    let expected = vec![Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_id_ptr() {
    let result = code("class Object fun f(a: Object): Object { return a; }");
    let expected = vec![Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_ptr_is() {
    let result = code("class Object fun f(a: Object, b: Object): Bool { return a === b; }");
    let expected = vec![TestIdentity(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_ptr_is_not() {
    let result = code("class Object fun f(a: Object, b: Object): Bool { return a !== b; }");
    let expected = vec![TestIdentity(r(2), r(0), r(1)), Not(r(2), r(2)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_sub_int() {
    let result = code("fun f(a: Int32, b: Int32): Int32 { return a - b; }");
    let expected = vec![Sub(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_sub_float32() {
    let result = code("fun f(a: Float32, b: Float32): Float32 { return a - b; }");
    let expected = vec![Sub(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_sub_float64() {
    let result = code("fun f(a: Float64, b: Float64): Float64 { return a - b; }");
    let expected = vec![Sub(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_div_int() {
    let result = code("fun f(a: Int32, b: Int32): Int32 { return a / b; }");
    let expected = vec![Div(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_div_int() {
    let result = position("fun f(a: Int32, b: Int32): Int32 { return a / b; }");
    let expected = vec![(0, p(1, 45))];
    assert_eq!(expected, result);
}

#[test]
fn gen_div_float32() {
    let result = code("fun f(a: Float32, b: Float32): Float32 { return a / b; }");
    let expected = vec![Div(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_div_float64() {
    let result = code("fun f(a: Float64, b: Float64): Float64 { return a / b; }");
    let expected = vec![Div(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_mul_int() {
    let result = code("fun f(a: Int32, b: Int32): Int32 { return a * b; }");
    let expected = vec![Mul(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_mul_float32() {
    let result = code("fun f(a: Float32, b: Float32): Float32 { return a * b; }");
    let expected = vec![Mul(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_mul_float64() {
    let result = code("fun f(a: Float64, b: Float64): Float64 { return a * b; }");
    let expected = vec![Mul(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_stmt_var_init() {
    let result = code("fun f() { let x = 1i32; }");
    let expected = vec![ConstInt32(r(0), 1), Ret(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_stmt_let_tuple() {
    gen_fct(
        "fun f(value: (Int32, Int32)): Int32 { let (x, y) = value; x+y }",
        |sa, code, fct| {
            let tuple_ty = create_tuple(sa, vec![SourceType::Int32, SourceType::Int32]);
            let expected = vec![
                LoadTupleElement(r(1), r(0), ConstPoolIdx(0)),
                LoadTupleElement(r(2), r(0), ConstPoolIdx(1)),
                Add(r(3), r(1), r(2)),
                Ret(r(3)),
            ];
            assert_eq!(expected, code);

            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::TupleElement(tuple_ty.clone(), 0)
            );

            assert_eq!(
                fct.const_pool(ConstPoolIdx(1)),
                &ConstPoolEntry::TupleElement(tuple_ty, 1)
            );
        },
    );

    gen_fct(
        "fun f(value: (Int32, (Int32, Int32))): Int32 { let (x, (y, z)) = value; x+y+z }",
        |sa, code, fct| {
            let nested_tuple_ty = create_tuple(sa, vec![SourceType::Int32, SourceType::Int32]);
            let tuple_ty = create_tuple(sa, vec![SourceType::Int32, nested_tuple_ty.clone()]);
            let expected = vec![
                LoadTupleElement(r(1), r(0), ConstPoolIdx(0)),
                LoadTupleElement(r(2), r(0), ConstPoolIdx(1)),
                LoadTupleElement(r(3), r(2), ConstPoolIdx(2)),
                LoadTupleElement(r(4), r(2), ConstPoolIdx(3)),
                Add(r(6), r(1), r(3)),
                Add(r(5), r(6), r(4)),
                Ret(r(5)),
            ];
            assert_eq!(expected, code);

            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::TupleElement(tuple_ty.clone(), 0)
            );

            assert_eq!(
                fct.const_pool(ConstPoolIdx(1)),
                &ConstPoolEntry::TupleElement(tuple_ty, 1)
            );

            assert_eq!(
                fct.const_pool(ConstPoolIdx(2)),
                &ConstPoolEntry::TupleElement(nested_tuple_ty.clone(), 0)
            );

            assert_eq!(
                fct.const_pool(ConstPoolIdx(3)),
                &ConstPoolEntry::TupleElement(nested_tuple_ty, 1)
            );
        },
    );

    gen_fct(
        "fun f(value: (Int32, (Int32, Int32))): Int32 { let (x, (_, z)) = value; x+z }",
        |sa, code, fct| {
            let nested_tuple_ty = create_tuple(sa, vec![SourceType::Int32, SourceType::Int32]);
            let tuple_ty = create_tuple(sa, vec![SourceType::Int32, nested_tuple_ty.clone()]);
            let expected = vec![
                LoadTupleElement(r(1), r(0), ConstPoolIdx(0)),
                LoadTupleElement(r(2), r(0), ConstPoolIdx(1)),
                LoadTupleElement(r(3), r(2), ConstPoolIdx(2)),
                Add(r(4), r(1), r(3)),
                Ret(r(4)),
            ];
            assert_eq!(expected, code);

            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::TupleElement(tuple_ty.clone(), 0)
            );

            assert_eq!(
                fct.const_pool(ConstPoolIdx(1)),
                &ConstPoolEntry::TupleElement(tuple_ty, 1)
            );

            assert_eq!(
                fct.const_pool(ConstPoolIdx(2)),
                &ConstPoolEntry::TupleElement(nested_tuple_ty, 1)
            );
        },
    );
}

#[test]
fn gen_stmt_let_unit() {
    let result = code("fun f(value: ()) { let () = value; }");
    let expected = vec![Ret(r(1))];
    assert_eq!(expected, result);

    let result = code("fun f() { let x = (); }");
    let expected = vec![Ret(r(0))];
    assert_eq!(expected, result);

    gen_fct(
        "fun f(value: (Int32, (Int32, ()))): Int32 { let (x, (y, z)) = value; x+y }",
        |sa, code, fct| {
            let inner_tuple_ty = create_tuple(sa, vec![SourceType::Int32, SourceType::Unit]);
            let tuple_ty = create_tuple(sa, vec![SourceType::Int32, inner_tuple_ty.clone()]);
            let expected = vec![
                LoadTupleElement(r(1), r(0), ConstPoolIdx(0)),
                LoadTupleElement(r(2), r(0), ConstPoolIdx(1)),
                LoadTupleElement(r(3), r(2), ConstPoolIdx(2)),
                Add(r(4), r(1), r(3)),
                Ret(r(4)),
            ];
            assert_eq!(expected, code);

            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::TupleElement(tuple_ty.clone(), 0)
            );

            assert_eq!(
                fct.const_pool(ConstPoolIdx(1)),
                &ConstPoolEntry::TupleElement(tuple_ty, 1)
            );

            assert_eq!(
                fct.const_pool(ConstPoolIdx(2)),
                &ConstPoolEntry::TupleElement(inner_tuple_ty, 0)
            );
        },
    );

    gen_fct(
        "fun f(value: (Int32, (Int32, ()))): Int32 { let (x, (y, ())) = value; x+y }",
        |sa, code, fct| {
            let nested_tuple_ty = create_tuple(sa, vec![SourceType::Int32, SourceType::Unit]);
            let tuple_ty = create_tuple(sa, vec![SourceType::Int32, nested_tuple_ty.clone()]);
            let expected = vec![
                LoadTupleElement(r(1), r(0), ConstPoolIdx(0)),
                LoadTupleElement(r(2), r(0), ConstPoolIdx(1)),
                LoadTupleElement(r(3), r(2), ConstPoolIdx(2)),
                Add(r(4), r(1), r(3)),
                Ret(r(4)),
            ];
            assert_eq!(expected, code);

            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::TupleElement(tuple_ty.clone(), 0)
            );

            assert_eq!(
                fct.const_pool(ConstPoolIdx(1)),
                &ConstPoolEntry::TupleElement(tuple_ty, 1)
            );

            assert_eq!(
                fct.const_pool(ConstPoolIdx(2)),
                &ConstPoolEntry::TupleElement(nested_tuple_ty, 0)
            );
        },
    );
}

#[test]
fn gen_stmt_while() {
    let result = code("fun f() { while true { 0; } }");
    let code = vec![
        LoopStart,
        ConstTrue(r(0)),
        JumpIfFalse(r(0), 4),
        JumpLoop(0),
        Ret(r(1)),
    ];
    assert_eq!(code, result);
}

#[test]
fn gen_stmt_if() {
    let result = code("fun f(a: Bool): Int32 { if a { return 1; } return 0; }");
    let expected = vec![
        JumpIfFalse(r(0), 3),
        ConstInt32(r(1), 1),
        Ret(r(1)),
        ConstInt32(r(1), 0),
        Ret(r(1)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_stmt_if_else_with_return() {
    let result = code("fun f(a: Bool): Int32 { if a { return 1; } else { return 2; } }");
    let expected = vec![
        JumpIfFalse(r(0), 3),
        ConstInt32(r(1), 1),
        Ret(r(1)),
        ConstInt32(r(1), 2),
        Ret(r(1)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_stmt_if_else_without_return() {
    let result = code(
        "fun f(b: Bool): Bool {
        let mut a = b;
        if a { a = false; } else { a = true; }
        return a;
    }",
    );
    let expected = vec![
        Mov(r(1), r(0)),
        JumpIfFalse(r(1), 4),
        ConstFalse(r(1)),
        Jump(5),
        ConstTrue(r(1)),
        Ret(r(1)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_stmt_break() {
    let result = code("fun f() { while true { break; } }");
    let expected = vec![
        LoopStart,
        ConstTrue(r(0)),
        JumpIfFalse(r(0), 5),
        Jump(5),
        JumpLoop(0),
        Ret(r(1)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_stmt_continue() {
    let result = code("fun f() { while true { continue; } }");
    let expected = vec![
        LoopStart,
        ConstTrue(r(0)),
        JumpIfFalse(r(0), 5),
        JumpLoop(0),
        JumpLoop(0),
        Ret(r(1)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_char() {
    let result = code("fun f(): Char { return '1'; }");
    let expected = vec![ConstChar(r(0), '1'), Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_int() {
    let result = code("fun f(): Int32 { return 1; }");
    let expected = vec![ConstInt32(r(0), 1), Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_uint8() {
    let result = code("fun f(): UInt8 { return 1u8; }");
    let expected = vec![ConstUInt8(r(0), 1), Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_int64() {
    let result = code("fun f(): Int64 { return 1i64; }");
    let expected = vec![ConstInt64(r(0), 1), Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_float32() {
    let result = code("fun f(): Float32 { return 1f32; }");
    let expected = vec![ConstFloat32(r(0), 1_f32), Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_float64() {
    let result = code("fun f(): Float64 { return 1f64; }");
    let expected = vec![ConstFloat64(r(0), 1_f64), Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_string() {
    let result = code("fun f(): String { return \"z\"; }");
    let expected = vec![ConstString(r(0), "z".to_string()), Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_string_duplicate() {
    let result = code("fun f() { let a = \"z\"; let b = \"z\"; }");
    let expected = vec![
        ConstString(r(0), "z".to_string()),
        ConstString(r(1), "z".to_string()),
        Ret(r(2)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_string_multiple() {
    let result = code("fun f() { let a = \"z\"; let b = \"y\"; }");
    let expected = vec![
        ConstString(r(0), "z".to_string()),
        ConstString(r(1), "y".to_string()),
        Ret(r(2)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_byte_zero() {
    let result = code("fun f(): UInt8 { return 0u8; }");
    let expected = vec![ConstUInt8(r(0), 0), Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_int_zero() {
    let result = code("fun f(): Int32 { return 0; }");
    let expected = vec![ConstInt32(r(0), 0), Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_int64_zero() {
    let result = code("fun f(): Int64 { return 0i64; }");
    let expected = vec![ConstInt64(r(0), 0), Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_float32_zero() {
    let result = code("fun f(): Float32 { return 0f32; }");
    let expected = vec![ConstFloat32(r(0), 0.0), Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_lit_float64_zero() {
    let result = code("fun f(): Float64 { return 0f64; }");
    let expected = vec![ConstFloat64(r(0), 0.0), Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_or() {
    let result = code("fun f(a: Bool, b: Bool): Bool { return a || b; }");
    let expected = vec![
        Mov(r(2), r(0)),
        JumpIfTrue(r(2), 3),
        Mov(r(2), r(1)),
        Ret(r(2)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_and() {
    let result = code("fun f(a: Bool, b: Bool): Bool { return a && b; }");
    let expected = vec![
        Mov(r(2), r(0)),
        JumpIfFalse(r(2), 3),
        Mov(r(2), r(1)),
        Ret(r(2)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_plus() {
    let result = code("fun f(a: Int32): Int32 { return +a; }");
    let expected = vec![Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_neg() {
    let result = code("fun f(a: Int32): Int32 { return -a; }");
    let expected = vec![Neg(r(1), r(0)), Ret(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_not() {
    let result = code("fun f(a: Bool): Bool { return a.not(); }");
    let expected = vec![Not(r(1), r(0)), Ret(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_mod() {
    let result = code("fun f(a: Int32, b: Int32): Int32 { return a % b; }");
    let expected = vec![Mod(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_mod_int32() {
    let result = position("fun f(a: Int32, b: Int32): Int32 { return a % b; }");
    let expected = vec![(0, p(1, 45))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_bit_or() {
    let result = code("fun f(a: Int32, b: Int32): Int32 { return a | b; }");
    let expected = vec![Or(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_bit_and() {
    let result = code("fun f(a: Int32, b: Int32): Int32 { return a & b; }");
    let expected = vec![And(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_bit_xor() {
    let result = code("fun f(a: Int32, b: Int32): Int32 { return a ^ b; }");
    let expected = vec![Xor(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_bit_shiftl() {
    let result = code("fun f(a: Int32, b: Int32): Int32 { return a.shiftLeft(b); }");
    let expected = vec![Shl(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_bit_shiftr() {
    let result = code("fun f(a: Int32, b: Int32): Int32 { return a.shiftRight(b); }");
    let expected = vec![Shr(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_bit_ashiftr() {
    let result = code("fun f(a: Int32, b: Int32): Int32 { return a.shiftRightSigned(b); }");
    let expected = vec![Sar(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_equal_bool() {
    let result = code("fun f(a: Bool, b: Bool): Bool { return a == b; }");
    let expected = vec![TestEq(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_notequal_bool() {
    let result = code("fun f(a: Bool, b: Bool): Bool { return a != b; }");
    let expected = vec![TestNe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_equal_uint8() {
    let result = code("fun f(a: UInt8, b: UInt8): Bool { return a == b; }");
    let expected = vec![TestEq(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_notequal_uint8() {
    let result = code("fun f(a: UInt8, b: UInt8): Bool { return a != b; }");
    let expected = vec![TestNe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_lessthan_uint8() {
    let result = code("fun f(a: UInt8, b: UInt8): Bool { return a < b; }");
    let expected = vec![TestLt(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_lessthanequal_uint8() {
    let result = code("fun f(a: UInt8, b: UInt8): Bool { return a <= b; }");
    let expected = vec![TestLe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_greaterthan_uint8() {
    let result = code("fun f(a: UInt8, b: UInt8): Bool { return a > b; }");
    let expected = vec![TestGt(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_greaterthanequal_uint8() {
    let result = code("fun f(a: UInt8, b: UInt8): Bool { return a >= b; }");
    let expected = vec![TestGe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_equal_char() {
    let result = code("fun f(a: Char, b: Char): Bool { return a == b; }");
    let expected = vec![TestEq(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_notequal_char() {
    let result = code("fun f(a: Char, b: Char): Bool { return a != b; }");
    let expected = vec![TestNe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_lessthan_char() {
    let result = code("fun f(a: Char, b: Char): Bool { return a < b; }");
    let expected = vec![TestLt(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_lessthanequal_char() {
    let result = code("fun f(a: Char, b: Char): Bool { return a <= b; }");
    let expected = vec![TestLe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_greaterthan_char() {
    let result = code("fun f(a: Char, b: Char): Bool { return a > b; }");
    let expected = vec![TestGt(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_greaterthanequal_char() {
    let result = code("fun f(a: Char, b: Char): Bool { return a >= b; }");
    let expected = vec![TestGe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_equal_enum() {
    let result = code(
        "fun f(a: Foo, b: Foo): Bool { return a == b; }
         enum Foo { A, B }",
    );
    let expected = vec![TestEq(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_notequal_enum() {
    let result = code(
        "fun f(a: Foo, b: Foo): Bool { return a != b; }
         enum Foo { A, B }",
    );
    let expected = vec![TestNe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_equal_int() {
    let result = code("fun f(a: Int32, b: Int32): Bool { return a == b; }");
    let expected = vec![TestEq(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_notequal_int() {
    let result = code("fun f(a: Int32, b: Int32): Bool { return a != b; }");
    let expected = vec![TestNe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_lessthan_int() {
    let result = code("fun f(a: Int32, b: Int32): Bool { return a < b; }");
    let expected = vec![TestLt(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_lessthanequal_int() {
    let result = code("fun f(a: Int32, b: Int32): Bool { return a <= b; }");
    let expected = vec![TestLe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_greaterthan_int() {
    let result = code("fun f(a: Int32, b: Int32): Bool { return a > b; }");
    let expected = vec![TestGt(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_greaterthanequal_int() {
    let result = code("fun f(a: Int32, b: Int32): Bool { return a >= b; }");
    let expected = vec![TestGe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_equal_float32() {
    let result = code("fun f(a: Float32, b: Float32): Bool { return a == b; }");
    let expected = vec![TestEq(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_notequal_float32() {
    let result = code("fun f(a: Float32, b: Float32): Bool { return a != b; }");
    let expected = vec![TestNe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_lessthan_float32() {
    let result = code("fun f(a: Float32, b: Float32): Bool { return a < b; }");
    let expected = vec![TestLt(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_lessthanequal_float32() {
    let result = code("fun f(a: Float32, b: Float32): Bool { return a <= b; }");
    let expected = vec![TestLe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_greaterthan_float32() {
    let result = code("fun f(a: Float32, b: Float32): Bool { return a > b; }");
    let expected = vec![TestGt(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_greaterthanequal_float32() {
    let result = code("fun f(a: Float32, b: Float32): Bool { return a >= b; }");
    let expected = vec![TestGe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_equal_float64() {
    let result = code("fun f(a: Float64, b: Float64): Bool { return a == b; }");
    let expected = vec![TestEq(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_notequal_float64() {
    let result = code("fun f(a: Float64, b: Float64): Bool { return a != b; }");
    let expected = vec![TestNe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_lessthan_float64() {
    let result = code("fun f(a: Float64, b: Float64): Bool { return a < b; }");
    let expected = vec![TestLt(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_lessthanequal_float64() {
    let result = code("fun f(a: Float64, b: Float64): Bool { return a <= b; }");
    let expected = vec![TestLe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_greaterthan_float64() {
    let result = code("fun f(a: Float64, b: Float64): Bool { return a > b; }");
    let expected = vec![TestGt(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_test_greaterthanequal_float64() {
    let result = code("fun f(a: Float64, b: Float64): Bool { return a >= b; }");
    let expected = vec![TestGe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_ident() {
    let result = code("fun f(): Int32 { let x = 1i32; return x; }");
    let expected = vec![ConstInt32(r(0), 1), Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_assign() {
    let result = code("fun f() { let mut x = 1i32; x = 2i32; }");
    let expected = vec![ConstInt32(r(0), 1), ConstInt32(r(0), 2), Ret(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_self() {
    let result = code_method("class Foo impl Foo { fun f(): Foo { return self; } }");
    let expected = vec![Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_self_assign() {
    let result = code_method("class Foo impl Foo { fun f() { let x = self; } }");
    let expected = vec![Mov(r(1), r(0)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_return() {
    let result = code("fun f(): Int32 { return 1i32; }");
    let expected = vec![ConstInt32(r(0), 1), Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_returnvoid() {
    let result = code("fun f() { }");
    let expected = vec![Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_load_global() {
    gen(
        "let a: Int32 = 0i32; fun f(): Int32 { return a; }",
        |sa, code| {
            let gid = sa.global_by_name("a");
            let expected = vec![LoadGlobal(r(0), gid), Ret(r(0))];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_store_global() {
    gen(
        "let mut a: Bool = false; fun f(x: Bool) { a = x; }",
        |sa, code| {
            let gid = sa.global_by_name("a");
            let expected = vec![StoreGlobal(r(0), gid), Ret(r(1))];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_side_effect() {
    let result = code("fun f(a: Int32) { 1; 2; 3i32 * a; \"foo\"; 1.0f32; 1.0f64; a; }");
    let expected = vec![Ret(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_fct_call_void_with_0_args() {
    gen_fct(
        "
            fun f() { g(); }
            fun g() { }
            ",
        |sa, code, fct| {
            let fct_id = sa.fct_by_name("g").expect("g not found");
            let expected = vec![InvokeStatic(r(0), ConstPoolIdx(0)), Ret(r(0))];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_fct_call_int_with_0_args() {
    gen_fct(
        "
            fun f(): Int32 { return g(); }
            fun g(): Int32 { return 1i32; }
            ",
        |sa, code, fct| {
            let fct_id = sa.fct_by_name("g").expect("g not found");
            let expected = vec![InvokeStatic(r(0), ConstPoolIdx(0)), Ret(r(0))];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_fct_call_int_with_0_args_and_unused_result() {
    gen_fct(
        "
            fun f() { g(); }
            fun g(): Int32 { return 1i32; }
            ",
        |sa, code, fct| {
            let fct_id = sa.fct_by_name("g").expect("g not found");
            let expected = vec![InvokeStatic(r(0), ConstPoolIdx(0)), Ret(r(1))];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_fct_call_void_with_1_arg() {
    gen_fct(
        "
            fun f() { g(1i32); }
            fun g(a: Int32) { }
            ",
        |sa, code, fct| {
            let fct_id = sa.fct_by_name("g").expect("g not found");
            let expected = vec![
                ConstInt32(r(0), 1),
                PushRegister(r(0)),
                InvokeStatic(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_fct_call_void_with_3_args() {
    gen_fct(
        "
            fun f() { g(1i32, 2i32, 3i32); }
            fun g(a: Int32, b: Int32, c: Int32) { }
            ",
        |sa, code, fct| {
            let fct_id = sa.fct_by_name("g").expect("g not found");
            let expected = vec![
                ConstInt32(r(0), 1),
                ConstInt32(r(1), 2),
                ConstInt32(r(2), 3),
                PushRegister(r(0)),
                PushRegister(r(1)),
                PushRegister(r(2)),
                InvokeStatic(r(3), ConstPoolIdx(0)),
                Ret(r(3)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_fct_call_int_with_1_arg() {
    gen_fct(
        "
            fun f(): Int32 { return g(1i32); }
            fun g(a: Int32): Int32 { return a; }
            ",
        |sa, code, fct| {
            let fct_id = sa.fct_by_name("g").expect("g not found");
            let expected = vec![
                ConstInt32(r(1), 1),
                PushRegister(r(1)),
                InvokeStatic(r(0), ConstPoolIdx(0)),
                Ret(r(0)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_fct_call_int_with_3_args() {
    gen_fct(
        "
            fun f(): Int32 { return g(1i32, 2i32, 3i32); }
            fun g(a: Int32, b: Int32, c: Int32): Int32 { return 1i32; }
            ",
        |sa, code, fct| {
            let fct_id = sa.fct_by_name("g").expect("g not found");
            let expected = vec![
                ConstInt32(r(1), 1),
                ConstInt32(r(2), 2),
                ConstInt32(r(3), 3),
                PushRegister(r(1)),
                PushRegister(r(2)),
                PushRegister(r(3)),
                InvokeStatic(r(0), ConstPoolIdx(0)),
                Ret(r(0)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_void_check_correct_self() {
    gen_fct(
        "
            fun f(i: Int32, foo: Foo) { foo.g(); }
            class Foo
            impl Foo {
                fun g() { }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                PushRegister(r(1)),
                InvokeDirect(r(2), ConstPoolIdx(0)),
                Ret(r(2)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_void_with_0_args() {
    gen_fct(
        "
            fun f(foo: Foo) { foo.g(); }
            class Foo
            impl Foo {
                fun g() { }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_void_with_1_arg() {
    gen_fct(
        "
            fun f(foo: Foo) { foo.g(1i32); }
            class Foo
            impl Foo {
                fun g(a: Int32) { }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstInt32(r(1), 1),
                PushRegister(r(0)),
                PushRegister(r(1)),
                InvokeDirect(r(2), ConstPoolIdx(0)),
                Ret(r(2)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_void_with_3_args() {
    gen_fct(
        "
            fun f(foo: Foo) { foo.g(1i32, 2i32, 3i32); }
            class Foo
            impl Foo {
                fun g(a: Int32, b: Int32, c: Int32) { }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstInt32(r(1), 1),
                ConstInt32(r(2), 2),
                ConstInt32(r(3), 3),
                PushRegister(r(0)),
                PushRegister(r(1)),
                PushRegister(r(2)),
                PushRegister(r(3)),
                InvokeDirect(r(4), ConstPoolIdx(0)),
                Ret(r(4)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_bool_with_0_args() {
    gen_fct(
        "
            fun f(foo: Foo): Bool { return foo.g(); }
            class Foo
            impl Foo {
                fun g(): Bool { return true; }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_bool_with_0_args_and_unused_result() {
    gen_fct(
        "
            fun f(foo: Foo) { foo.g(); }
            class Foo
            impl Foo {
                fun g(): Bool { return true; }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(2)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_bool_with_1_arg() {
    gen_fct(
        "
            fun f(foo: Foo): Bool { return foo.g(true); }
            class Foo
            impl Foo {
                fun g(a: Bool): Bool { return true; }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstTrue(r(2)),
                PushRegister(r(0)),
                PushRegister(r(2)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_bool_with_3_args() {
    gen_fct(
        "
            fun f(foo: Foo): Bool { return foo.g(true, false, true); }
            class Foo
            impl Foo {
                fun g(a: Bool, b: Bool, c: Bool): Bool { return true; }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstTrue(r(2)),
                ConstFalse(r(3)),
                ConstTrue(r(4)),
                PushRegister(r(0)),
                PushRegister(r(2)),
                PushRegister(r(3)),
                PushRegister(r(4)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_byte_with_0_args() {
    gen_fct(
        "
            fun f(foo: Foo): UInt8 { return foo.g(); }
            class Foo
            impl Foo {
                fun g(): UInt8 { return 1u8; }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_byte_with_0_args_and_unused_result() {
    gen_fct(
        "
            fun f(foo: Foo) { foo.g(); }
            class Foo
            impl Foo {
                fun g(): UInt8 { return 1u8; }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(2)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_byte_with_1_arg() {
    gen_fct(
        "
            fun f(foo: Foo): UInt8 { return foo.g(1u8); }
            class Foo
            impl Foo {
                fun g(a: UInt8): UInt8 { return 1u8; }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstUInt8(r(2), 1),
                PushRegister(r(0)),
                PushRegister(r(2)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_byte_with_3_args() {
    gen_fct(
        "
            fun f(foo: Foo): UInt8 { return foo.g(1u8, 2u8, 3u8); }
            class Foo
            impl Foo {
                fun g(a: UInt8, b: UInt8, c: UInt8): UInt8 { return 1u8; }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstUInt8(r(2), 1),
                ConstUInt8(r(3), 2),
                ConstUInt8(r(4), 3),
                PushRegister(r(0)),
                PushRegister(r(2)),
                PushRegister(r(3)),
                PushRegister(r(4)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_char_with_0_args() {
    gen_fct(
        "
            fun f(foo: Foo): Char { return foo.g(); }
            class Foo
            impl Foo {
                fun g(): Char { return '1'; }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_char_with_0_args_and_unused_result() {
    gen_fct(
        "
            fun f(foo: Foo) { foo.g(); }
            class Foo
            impl Foo {
                fun g(): Char { return '1'; }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(2)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_char_with_1_arg() {
    gen_fct(
        "
            fun f(foo: Foo): Char { return foo.g('1'); }
            class Foo
            impl Foo {
                fun g(a: Char): Char { return '1'; }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstChar(r(2), '1'),
                PushRegister(r(0)),
                PushRegister(r(2)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_char_with_3_args() {
    gen_fct(
        "
            fun f(foo: Foo): Char { return foo.g('1', '2', '3'); }
            class Foo
            impl Foo {
                fun g(a: Char, b: Char, c: Char): Char { return '1'; }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstChar(r(2), '1'),
                ConstChar(r(3), '2'),
                ConstChar(r(4), '3'),
                PushRegister(r(0)),
                PushRegister(r(2)),
                PushRegister(r(3)),
                PushRegister(r(4)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_int_with_0_args() {
    gen_fct(
        "
            fun f(foo: Foo): Int32 { return foo.g(); }
            class Foo
            impl Foo {
                fun g(): Int32 { return 1; }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_int_with_0_args_and_unused_result() {
    gen_fct(
        "
            fun f(foo: Foo) { foo.g(); }
            class Foo
            impl Foo {
                fun g(): Int32 { return 1; }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(2)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_int_with_1_arg() {
    gen_fct(
        "
            fun f(foo: Foo): Int32 { return foo.g(1i32); }
            class Foo
            impl Foo {
                fun g(a: Int32): Int32 { return 1i32; }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstInt32(r(2), 1),
                PushRegister(r(0)),
                PushRegister(r(2)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_int_with_3_args() {
    gen_fct(
        "
            fun f(foo: Foo): Int32 { return foo.g(1i32, 2i32, 3i32); }
            class Foo
            impl Foo {
                fun g(a: Int32, b: Int32, c: Int32): Int32 { return 1i32; }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstInt32(r(2), 1),
                ConstInt32(r(3), 2),
                ConstInt32(r(4), 3),
                PushRegister(r(0)),
                PushRegister(r(2)),
                PushRegister(r(3)),
                PushRegister(r(4)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_int64_with_0_args() {
    gen_fct(
        "
            fun f(foo: Foo): Int64 { return foo.g(); }
            class Foo
            impl Foo {
                fun g(): Int64 { return 1i64; }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_int64_with_0_args_and_unused_result() {
    gen_fct(
        "
            fun f(foo: Foo) { foo.g(); }
            class Foo
            impl Foo {
                fun g(): Int64 { return 1i64; }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(2)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_int64_with_1_arg() {
    gen_fct(
        "
            fun f(foo: Foo): Int64 { return foo.g(1i64); }
            class Foo
            impl Foo {
                fun g(a: Int64): Int64 { return 1i64; }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstInt64(r(2), 1),
                PushRegister(r(0)),
                PushRegister(r(2)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_int64_with_3_args() {
    gen_fct(
        "
            fun f(foo: Foo): Int64 { return foo.g(1i64, 2i64, 3i64); }
            class Foo
            impl Foo {
                fun g(a: Int64, b: Int64, c: Int64): Int64 { return 1i64; }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstInt64(r(2), 1),
                ConstInt64(r(3), 2),
                ConstInt64(r(4), 3),
                PushRegister(r(0)),
                PushRegister(r(2)),
                PushRegister(r(3)),
                PushRegister(r(4)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_float32_with_0_args() {
    gen_fct(
        "
            fun f(foo: Foo): Float32 { return foo.g(); }
            class Foo
            impl Foo {
                fun g(): Float32 { return 1f32; }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_float32_with_0_args_and_unused_result() {
    gen_fct(
        "
            fun f(foo: Foo) { foo.g(); }
            class Foo
            impl Foo {
                fun g(): Float32 { return 1f32; }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(2)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_float32_with_1_arg() {
    gen_fct(
        "
            fun f(foo: Foo): Float32 { return foo.g(1f32); }
            class Foo
            impl Foo {
                fun g(a: Float32): Float32 { return 1f32; }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstFloat32(r(2), 1_f32),
                PushRegister(r(0)),
                PushRegister(r(2)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_float32_with_3_args() {
    gen_fct(
        "
            fun f(foo: Foo): Float32 { return foo.g(1f32, 2f32, 3f32); }
            class Foo
            impl Foo {
                fun g(a: Float32, b: Float32, c: Float32): Float32 { return 1f32; }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstFloat32(r(2), 1_f32),
                ConstFloat32(r(3), 2_f32),
                ConstFloat32(r(4), 3_f32),
                PushRegister(r(0)),
                PushRegister(r(2)),
                PushRegister(r(3)),
                PushRegister(r(4)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_float64_with_0_args() {
    gen_fct(
        "
            fun f(foo: Foo): Float64 { return foo.g(); }
            class Foo
            impl Foo {
                fun g(): Float64 { return 1f64; }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_float64_with_0_args_and_unused_result() {
    gen_fct(
        "
            fun f(foo: Foo) { foo.g(); }
            class Foo
            impl Foo {
                fun g(): Float64 { return 1f64; }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(2)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_float64_with_1_arg() {
    gen_fct(
        "
            fun f(foo: Foo): Float64 { return foo.g(1f64); }
            class Foo
            impl Foo {
                fun g(a: Float64): Float64 { return 1f64; }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstFloat64(r(2), 1_f64),
                PushRegister(r(0)),
                PushRegister(r(2)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_float64_with_3_args() {
    gen_fct(
        "
            fun f(foo: Foo): Float64 { return foo.g(1f64, 2f64, 3f64); }
            class Foo
            impl Foo {
                fun g(a: Float64, b: Float64, c: Float64): Float64 { return 1f64; }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstFloat64(r(2), 1_f64),
                ConstFloat64(r(3), 2_f64),
                ConstFloat64(r(4), 3_f64),
                PushRegister(r(0)),
                PushRegister(r(2)),
                PushRegister(r(3)),
                PushRegister(r(4)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_ptr_with_0_args() {
    gen_fct(
        "
            fun f(foo: Foo): String { return foo.g(); }
            class Foo
            impl Foo {
                fun g(): String { return \"1\"; }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_ptr_with_0_args_and_unused_result() {
    gen_fct(
        "
            fun f(foo: Foo) { foo.g(); }
            class Foo
            impl Foo {
                fun g(): String { return \"1\"; }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(2)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_ptr_with_1_arg() {
    gen_fct(
        "
            fun f(foo: Foo): String { return foo.g(\"1\"); }
            class Foo
            impl Foo {
                fun g(a: String): String { return \"1\"; }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstString(r(2), "1".to_string()),
                PushRegister(r(0)),
                PushRegister(r(2)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_method_call_ptr_with_3_args() {
    gen_fct(
        "
            fun f(foo: Foo): String { return foo.g(\"1\", \"2\", \"3\"); }
            class Foo
            impl Foo {
                fun g(a: String, b: String, c: String): String { return \"1\"; }
            }
            ",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("Foo", "g", false)
                .expect("g not found");
            let expected = vec![
                ConstString(r(2), "1".to_string()),
                ConstString(r(3), "2".to_string()),
                ConstString(r(4), "3".to_string()),
                PushRegister(r(0)),
                PushRegister(r(2)),
                PushRegister(r(3)),
                PushRegister(r(4)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_new_struct() {
    gen_fct(
        "
        struct Foo { f1: Int32, f2: Bool }
        fun f(): Foo { Foo(10i32, false) }
    ",
        |sa, code, fct| {
            let struct_id = sa.struct_by_name("Foo");
            let expected = vec![
                ConstInt32(r(0), 10),
                ConstFalse(r(1)),
                PushRegister(r(0)),
                PushRegister(r(1)),
                NewStruct(r(2), ConstPoolIdx(1)),
                Ret(r(2)),
            ];
            assert_eq!(expected, code);

            assert_eq!(
                fct.const_pool(ConstPoolIdx(1)),
                &ConstPoolEntry::Struct(struct_id, SourceTypeArray::empty())
            );
        },
    );

    gen_fct(
        "
        struct Foo[T] { f1: T, f2: Bool }
        fun f[T](val: T): Foo[T] { Foo[T](val, false) }
    ",
        |sa, code, fct| {
            let struct_id = sa.struct_by_name("Foo");
            let expected = vec![
                ConstFalse(r(1)),
                PushRegister(r(0)),
                PushRegister(r(1)),
                NewStruct(r(2), ConstPoolIdx(0)),
                Ret(r(2)),
            ];
            assert_eq!(expected, code);

            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Struct(
                    struct_id,
                    SourceTypeArray::single(SourceType::TypeParam(TypeParamId(0)))
                )
            );
        },
    );
}

#[test]
fn gen_move_struct() {
    let result = code(
        "
        struct Foo { f1: Int32, f2: Bool }
        fun f(x: Foo): Foo { let y = x; y }
    ",
    );
    let expected = vec![Mov(r(1), r(0)), Ret(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_struct_field() {
    gen_fct(
        "
        struct Foo { f1: Int32, f2: Bool }
        fun f(x: Foo): Int32 { x.f1 }
    ",
        |sa, code, fct| {
            let struct_id = sa.struct_by_name("Foo");
            let expected = vec![LoadStructField(r(1), r(0), ConstPoolIdx(0)), Ret(r(1))];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::StructField(
                    struct_id,
                    SourceTypeArray::empty(),
                    StructDefinitionFieldId(0)
                )
            );
        },
    );

    gen_fct(
        "
        struct Foo[T] { f1: T, f2: Bool }
        fun f(x: Foo[Int32]): Int32 { x.f1 }
    ",
        |sa, code, fct| {
            let struct_id = sa.struct_by_name("Foo");
            let expected = vec![LoadStructField(r(1), r(0), ConstPoolIdx(0)), Ret(r(1))];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::StructField(
                    struct_id,
                    SourceTypeArray::single(SourceType::Int32),
                    StructDefinitionFieldId(0)
                )
            );
        },
    );
}

#[test]
fn gen_struct_array() {
    let result = code(
        "
        struct Foo { f1: Int32, f2: Bool }
        fun f(x: Array[Foo], idx: Int64): Foo { x(idx) }
    ",
    );
    let expected = vec![LoadArray(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);

    let result = code(
        "
        struct Foo { f1: Int32, f2: Bool }
        fun f(x: Array[Foo], idx: Int64, value: Foo) { x(idx) = value; }
    ",
    );
    let expected = vec![StoreArray(r(2), r(0), r(1)), Ret(r(3))];
    assert_eq!(expected, result);
}

#[test]
fn gen_new_enum() {
    gen_fct(
        "
        enum Foo { A(Int32), B }
        fun f(): Foo { Foo::A(10i32) }
    ",
        |sa, code, fct| {
            let enum_id = sa.enum_by_name("Foo");
            let expected = vec![
                ConstInt32(r(0), 10),
                PushRegister(r(0)),
                NewEnum(r(1), ConstPoolIdx(1)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);

            assert_eq!(
                fct.const_pool(ConstPoolIdx(1)),
                &ConstPoolEntry::EnumVariant(enum_id, SourceTypeArray::empty(), 0)
            );
        },
    );

    gen_fct(
        "
        enum Foo[T] { A(T), B }
        fun f(): Foo[Int32] { Foo[Int32]::A(10i32) }
    ",
        |sa, code, fct| {
            let enum_id = sa.enum_by_name("Foo");
            let expected = vec![
                ConstInt32(r(0), 10),
                PushRegister(r(0)),
                NewEnum(r(1), ConstPoolIdx(1)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);

            assert_eq!(
                fct.const_pool(ConstPoolIdx(1)),
                &ConstPoolEntry::EnumVariant(
                    enum_id,
                    SourceTypeArray::single(SourceType::Int32),
                    0
                )
            );
        },
    );

    gen_fct(
        "
        enum Foo { A(Int32), B }
        fun f(): Foo { Foo::B }
    ",
        |sa, code, fct| {
            let enum_id = sa.enum_by_name("Foo");
            let expected = vec![NewEnum(r(0), ConstPoolIdx(0)), Ret(r(0))];
            assert_eq!(expected, code);

            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::EnumVariant(enum_id, SourceTypeArray::empty(), 1)
            );
        },
    );

    gen_fct(
        "
        enum Foo[T] { A(T), B }
        fun f(): Foo[Int32] { Foo[Int32]::B }
    ",
        |sa, code, fct| {
            let enum_id = sa.enum_by_name("Foo");
            let expected = vec![NewEnum(r(0), ConstPoolIdx(0)), Ret(r(0))];
            assert_eq!(expected, code);

            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::EnumVariant(
                    enum_id,
                    SourceTypeArray::single(SourceType::Int32),
                    1
                )
            );
        },
    );

    gen_fct(
        "
        enum Foo[T] { A(T), B }
        fun f(): Foo[Int32] { Foo::B[Int32] }
    ",
        |sa, code, fct| {
            let enum_id = sa.enum_by_name("Foo");
            let expected = vec![NewEnum(r(0), ConstPoolIdx(0)), Ret(r(0))];
            assert_eq!(expected, code);

            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::EnumVariant(
                    enum_id,
                    SourceTypeArray::single(SourceType::Int32),
                    1
                )
            );
        },
    );
}

#[test]
fn gen_new_object() {
    gen_fct(
        "class Object fun f(): Object { return Object(); }",
        |sa, code, fct| {
            let cls_id = sa.cls_by_name("Object");
            let expected = vec![NewObjectInitialized(r(0), ConstPoolIdx(0)), Ret(r(0))];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Class(cls_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_new_object_initialized() {
    gen_fct(
        "
        class Foo(a: Int64, b: Bool)
        fun f(a: Int64, b: Bool): Foo { return Foo(a, b); }",
        |sa, code, fct| {
            let cls_id = sa.cls_by_name("Foo");
            let expected = vec![
                PushRegister(r(0)),
                PushRegister(r(1)),
                NewObjectInitialized(r(2), ConstPoolIdx(0)),
                Ret(r(2)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Class(cls_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_position_new_object() {
    let result = position("class Object fun f(): Object { return Object(); }");
    let expected = vec![(0, p(1, 45))];
    assert_eq!(expected, result);
}

#[test]
fn gen_new_array() {
    gen_fct(
        "fun f(): Array[Int32] { Array[Int32]::new(1i32, 2i32, 3i32) }",
        |sa, code, fct| {
            let cls_id = sa.cls_by_name("Array");
            let expected = vec![
                ConstInt64(r(0), 3),
                NewArray(r(1), ConstPoolIdx(0), r(0)),
                ConstInt32(r(3), 1),
                ConstInt64(r(2), 0),
                StoreArray(r(3), r(1), r(2)),
                ConstInt32(r(3), 2),
                ConstInt64(r(2), 1),
                StoreArray(r(3), r(1), r(2)),
                ConstInt32(r(3), 3),
                ConstInt64(r(2), 2),
                StoreArray(r(3), r(1), r(2)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);

            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Class(cls_id, SourceTypeArray::single(SourceType::Int32))
            );
        },
    );
}

#[test]
fn gen_array_length() {
    let result = code("fun f(a: Array[Int32]): Int64 { return a.size(); }");
    let expected = vec![ArrayLength(r(1), r(0)), Ret(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_array_length() {
    let result = position("fun f(a: Array[Int32]): Int64 { return a.size(); }");
    let expected = vec![(0, p(1, 46))];
    assert_eq!(expected, result);
}

#[test]
fn gen_array_length_effect() {
    let result = code("fun f(a: Array[Int32]) { a.size(); }");
    let expected = vec![ArrayLength(r(1), r(0)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_array_length_effect() {
    let result = position("fun f(a: Array[Int32]) { a.size(); }");
    let expected = vec![(0, p(1, 32))];
    assert_eq!(expected, result);
}

#[test]
fn gen_load_array_uint8() {
    let result = code("fun f(a: Array[UInt8]): UInt8 { return a(0); }");
    let expected = vec![ConstInt64(r(2), 0), LoadArray(r(1), r(0), r(2)), Ret(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_load_array_bool() {
    let result = code("fun f(a: Array[Bool], idx: Int64): Bool { return a(idx); }");
    let expected = vec![LoadArray(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_load_array_char() {
    let result = code("fun f(a: Array[Char], idx: Int64): Char { return a(idx); }");
    let expected = vec![LoadArray(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_load_array_int32() {
    let result = code("fun f(a: Array[Int32], idx: Int64): Int32 { return a(idx); }");
    let expected = vec![LoadArray(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_load_array_int64() {
    let result = code("fun f(a: Array[Int64], idx: Int64): Int64 { return a(idx); }");
    let expected = vec![LoadArray(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_load_array_float32() {
    let result = code("fun f(a: Array[Float32], idx: Int64): Float32 { return a(idx); }");
    let expected = vec![LoadArray(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_load_array_float64() {
    let result = code("fun f(a: Array[Float64], idx: Int64): Float64 { return a(idx); }");
    let expected = vec![LoadArray(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_load_array_ptr() {
    let result =
        code("class Object fun f(a: Array[Object], idx: Int64): Object { return a(idx); }");
    let expected = vec![LoadArray(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_load_array_bool() {
    let result = position("fun f(a: Array[Bool]): Bool { return a(0); }");
    let expected = vec![(3, p(1, 39))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_load_array_char() {
    let result = position("fun f(a: Array[Char]): Char { return a(0); }");
    let expected = vec![(3, p(1, 39))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_load_array_int32() {
    let result = position("fun f(a: Array[Int32]): Int32 { return a(0); }");
    let expected = vec![(3, p(1, 41))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_load_array_int64() {
    let result = position("fun f(a: Array[Int64]): Int64 { return a(0); }");
    let expected = vec![(3, p(1, 41))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_load_array_float32() {
    let result = position("fun f(a: Array[Float32]): Float32 { return a(0); }");
    let expected = vec![(3, p(1, 45))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_load_array_float64() {
    let result = position("fun f(a: Array[Float64]): Float64 { return a(0); }");
    let expected = vec![(3, p(1, 45))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_load_array_ptr() {
    let result = position("class Object fun f(a: Array[Object]): Object { return a(0); }");
    let expected = vec![(3, p(1, 56))];
    assert_eq!(expected, result);
}

#[test]
fn gen_store_array_uint8() {
    let result = code("fun f(a: Array[UInt8], b: UInt8) { a(0) = b; }");
    let expected = vec![
        ConstInt64(Register(2), 0),
        StoreArray(r(1), r(0), r(2)),
        Ret(r(3)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_store_array_bool() {
    let result = code("fun f(a: Array[Bool], b: Bool) { a(0) = b; }");
    let expected = vec![
        ConstInt64(Register(2), 0),
        StoreArray(r(1), r(0), r(2)),
        Ret(r(3)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_store_array_char() {
    let result = code("fun f(a: Array[Char], b: Char) { a(0) = b; }");
    let expected = vec![
        ConstInt64(Register(2), 0),
        StoreArray(r(1), r(0), r(2)),
        Ret(r(3)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_store_array_int32() {
    let result = code("fun f(a: Array[Int32], b: Int32) { a(0) = b; }");
    let expected = vec![
        ConstInt64(Register(2), 0),
        StoreArray(r(1), r(0), r(2)),
        Ret(r(3)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_store_array_int64() {
    let result = code("fun f(a: Array[Int64], b: Int64) { a(0) = b; }");
    let expected = vec![
        ConstInt64(Register(2), 0),
        StoreArray(r(1), r(0), r(2)),
        Ret(r(3)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_store_array_float32() {
    let result = code("fun f(a: Array[Float32], b: Float32) { a(0) = b; }");
    let expected = vec![
        ConstInt64(Register(2), 0),
        StoreArray(r(1), r(0), r(2)),
        Ret(r(3)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_store_array_float64() {
    let result = code("fun f(a: Array[Float64], b: Float64) { a(0) = b; }");
    let expected = vec![
        ConstInt64(Register(2), 0),
        StoreArray(r(1), r(0), r(2)),
        Ret(r(3)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_store_array_ptr() {
    let result = code("class Object fun f(a: Array[Object], b: Object) { a(0) = b; }");
    let expected = vec![
        ConstInt64(Register(2), 0),
        StoreArray(r(1), r(0), r(2)),
        Ret(r(3)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_store_array_bool() {
    let result = position("fun f(a: Array[Bool], b: Bool) { a(0) = b; }");
    let expected = vec![(3, p(1, 39))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_store_array_char() {
    let result = position("fun f(a: Array[Char], b: Char) { a(0) = b; }");
    let expected = vec![(3, p(1, 39))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_store_array_int32() {
    let result = position("fun f(a: Array[Int32], b: Int32) { a(0) = b; }");
    let expected = vec![(3, p(1, 41))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_store_array_int64() {
    let result = position("fun f(a: Array[Int64], b: Int64) { a(0) = b; }");
    let expected = vec![(3, p(1, 41))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_store_array_float32() {
    let result = position("fun f(a: Array[Float32], b: Float32) { a(0) = b; }");
    let expected = vec![(3, p(1, 45))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_store_array_float64() {
    let result = position("fun f(a: Array[Float64], b: Float64) { a(0) = b; }");
    let expected = vec![(3, p(1, 45))];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_store_array_ptr() {
    let result = position("class Object fun f(a: Array[Object], b: Object) { a(0) = b; }");
    let expected = vec![(3, p(1, 56))];
    assert_eq!(expected, result);
}

#[test]
fn gen_new_object_with_multiple_args() {
    gen_fct(
        "
            class Foo(a: Int32, b: Int32, c: Int32)
            fun f(): Foo { return Foo(1i32, 2i32, 3i32); }
            ",
        |sa, code, fct| {
            let cls_id = sa.cls_by_name("Foo");
            let expected = vec![
                ConstInt32(r(0), 1),
                ConstInt32(r(1), 2),
                ConstInt32(r(2), 3),
                PushRegister(r(0)),
                PushRegister(r(1)),
                PushRegister(r(2)),
                NewObjectInitialized(r(3), ConstPoolIdx(3)),
                Ret(r(3)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(3)),
                &ConstPoolEntry::Class(cls_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_position_new_object_with_multiple_args() {
    let result = position(
        "
            class Foo(a: Int32, b: Int32, c: Int32)
            fun f(): Foo { return Foo(1i32, 2i32, 3i32); }",
    );
    let expected = vec![(15, p(3, 38))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_for_bool() {
    let result = code_method_with_struct_name(
        "trait MyId { fun f(): Self; }
            impl MyId for Bool { fun f(): Bool { return self; } }
            ",
        "Bool",
    );
    let expected = vec![Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_for_uint8() {
    let result = code_method_with_struct_name(
        "trait MyId { fun f(): Self; }
            impl MyId for UInt8 { fun f(): UInt8 { return self; } }
            ",
        "UInt8",
    );
    let expected = vec![Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_for_int() {
    let result = code_method_with_struct_name(
        "trait MyId { fun f(): Self; }
            impl MyId for Int32 { fun f(): Int32 { return self; } }
            ",
        "Int32",
    );
    let expected = vec![Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_for_int64() {
    let result = code_method_with_struct_name(
        "trait MyId { fun f(): Self; }
            impl MyId for Int64 { fun f(): Int64 { return self; } }
            ",
        "Int64",
    );
    let expected = vec![Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_for_float32() {
    let result = code_method_with_struct_name(
        "trait MyId { fun f(): Self; }
            impl MyId for Float32 { fun f(): Float32 { return self; } }
            ",
        "Float32",
    );
    let expected = vec![Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_for_float64() {
    let result = code_method_with_struct_name(
        "trait MyId { fun f(): Self; }
            impl MyId for Float64 { fun f(): Float64 { return self; } }
            ",
        "Float64",
    );
    let expected = vec![Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_for_string() {
    let result = code_method_with_class_name(
        "trait MyId { fun f(): Self; }
            impl MyId for String { fun f(): String { return self; } }
            ",
        "String",
    );
    let expected = vec![Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_assign_for_bool() {
    let result = code_method_with_struct_name(
        "trait MyId { fun f(); }
            impl MyId for Bool { fun f() { let x = self; } }
            ",
        "Bool",
    );
    let expected = vec![Mov(r(1), r(0)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_assign_for_uint8() {
    let result = code_method_with_struct_name(
        "trait MyId { fun f(); }
            impl MyId for UInt8 { fun f() { let x = self; } }
            ",
        "UInt8",
    );
    let expected = vec![Mov(r(1), r(0)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_assign_for_int() {
    let result = code_method_with_struct_name(
        "trait MyId { fun f(); }
            impl MyId for Int32 { fun f() { let x = self; } }
            ",
        "Int32",
    );
    let expected = vec![Mov(r(1), r(0)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_assign_for_int64() {
    let result = code_method_with_struct_name(
        "trait MyId { fun f(); }
            impl MyId for Int64 { fun f() { let x = self; } }
            ",
        "Int64",
    );
    let expected = vec![Mov(r(1), r(0)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_assign_for_float32() {
    let result = code_method_with_struct_name(
        "trait MyId { fun f(); }
            impl MyId for Float32 { fun f() { let x = self; } }
            ",
        "Float32",
    );
    let expected = vec![Mov(r(1), r(0)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_assign_for_float64() {
    let result = code_method_with_struct_name(
        "trait MyId { fun f(); }
            impl MyId for Float64 { fun f() { let x = self; } }
            ",
        "Float64",
    );
    let expected = vec![Mov(r(1), r(0)), Ret(r(2))];
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
    let expected = vec![Mov(r(1), r(0)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_reinterpret_float32_as_int32() {
    gen_fct(
        "fun f(a: Float32): Int32 { a.asInt32() }",
        |sa, code, fct| {
            let fct_id = sa
                .struct_method_by_name("Float32", "asInt32", false)
                .unwrap();
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_reinterpret_int32_as_float32() {
    gen_fct(
        "fun f(a: Int32): Float32 { a.asFloat32() }",
        |sa, code, fct| {
            let fct_id = sa
                .struct_method_by_name("Int32", "asFloat32", false)
                .unwrap();
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_reinterpret_float64_as_int64() {
    gen_fct(
        "fun f(a: Float64): Int64 { a.asInt64() }",
        |sa, code, fct| {
            let fct_id = sa
                .struct_method_by_name("Float64", "asInt64", false)
                .unwrap();
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_reinterpret_int64_as_float64() {
    gen_fct(
        "fun f(a: Int64): Float64 { a.asFloat64() }",
        |sa, code, fct| {
            let fct_id = sa
                .struct_method_by_name("Int64", "asFloat64", false)
                .unwrap();
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_float32_is_nan() {
    let result = code("fun f(a: Float32): Bool { a.isNan() }");
    let expected = vec![TestNe(r(1), r(0), r(0)), Ret(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_float64_is_nan() {
    let result = code("fun f(a: Float64): Bool { a.isNan() }");
    let expected = vec![TestNe(r(1), r(0), r(0)), Ret(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_extend_int_to_int64() {
    let result = code("fun f(a: Int32): Int64 { a.toInt64() }");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_cast_int64_to_int32() {
    let result = code("fun f(a: Int64): Int32 { a.toInt32() }");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_convert_int32_to_float32() {
    gen_fct(
        "fun f(a: Int32): Float32 { a.toFloat32() }",
        |sa, code, fct| {
            let fct_id = sa
                .struct_method_by_name("Int32", "toFloat32", false)
                .unwrap();
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_convert_int32_to_float64() {
    gen_fct(
        "fun f(a: Int32): Float64 { a.toFloat64() }",
        |sa, code, fct| {
            let fct_id = sa
                .struct_method_by_name("Int32", "toFloat64", false)
                .unwrap();
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_convert_int64_to_float32() {
    gen_fct(
        "fun f(a: Int64): Float32 { a.toFloat32() }",
        |sa, code, fct| {
            let fct_id = sa
                .struct_method_by_name("Int64", "toFloat32", false)
                .unwrap();
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_convert_int64_to_float64() {
    gen_fct(
        "fun f(a: Int64): Float64 { a.toFloat64() }",
        |sa, code, fct| {
            let fct_id = sa
                .struct_method_by_name("Int64", "toFloat64", false)
                .unwrap();
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_truncate_float32_to_int32() {
    gen_fct(
        "fun f(a: Float32): Int32 { a.toInt32() }",
        |sa, code, fct| {
            let fct_id = sa
                .struct_method_by_name("Float32", "toInt32", false)
                .unwrap();
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_truncate_float32_to_int64() {
    gen_fct(
        "fun f(a: Float32): Int64 { a.toInt64() }",
        |sa, code, fct| {
            let fct_id = sa
                .struct_method_by_name("Float32", "toInt64", false)
                .unwrap();
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_truncate_float64_to_int32() {
    gen_fct(
        "fun f(a: Float64): Int32 { a.toInt32() }",
        |sa, code, fct| {
            let fct_id = sa
                .struct_method_by_name("Float64", "toInt32", false)
                .unwrap();
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_truncate_float64_to_int64() {
    gen_fct(
        "fun f(a: Float64): Int64 { a.toInt64() }",
        |sa, code, fct| {
            let fct_id = sa
                .struct_method_by_name("Float64", "toInt64", false)
                .unwrap();
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_enum_value() {
    gen_fct(
        "enum MyEnum { A, B } fun f(): MyEnum { MyEnum::A }",
        |sa, code, fct| {
            let enum_id = sa.enum_by_name("MyEnum");
            let expected = vec![NewEnum(r(0), ConstPoolIdx(0)), Ret(r(0))];
            assert_eq!(expected, code);

            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::EnumVariant(enum_id, SourceTypeArray::empty(), 0)
            )
        },
    );
}

#[test]
fn gen_enum_mov_generic() {
    let result = code(
        "enum MyEnum { A(Int32), B }
        fun f(x: MyEnum): MyEnum {
            let tmp = x;
            tmp
        }",
    );
    let expected = vec![Mov(r(1), r(0)), Ret(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_unreachable() {
    gen_fct(
        "fun f(): Int32 { unreachable[Int32]() }",
        |sa, code, fct| {
            let fct_id = sa
                .fct_by_name("unreachable")
                .expect("unreachable not found");
            let expected = vec![InvokeStatic(r(0), ConstPoolIdx(0)), Ret(r(0))];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::single(SourceType::Int32))
            );
        },
    );
}

#[test]
fn gen_enum_array() {
    let result = code(
        "enum MyEnum { A(Int32), B }
        fun f(arr: Array[MyEnum], idx: Int64): MyEnum {
            arr(idx)
        }",
    );

    let expected = vec![LoadArray(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);

    let result = code(
        "enum MyEnum { A(Int32), B }
        fun f(arr: Array[MyEnum], idx: Int64, value: MyEnum) {
            arr(idx) = value;
        }",
    );
    let expected = vec![StoreArray(r(2), r(0), r(1)), Ret(r(3))];
    assert_eq!(expected, result);
}

#[test]
fn gen_string_length() {
    let result = code("fun f(x: String): Int64 { x.size() }");
    let expected = vec![StringLength(r(1), r(0)), Ret(r(1))];
    assert_eq!(expected, result);
}

#[test]
fn gen_string_get_uint8() {
    let result = code("fun f(x: String, idx: Int64): UInt8 { x.getByte(idx) }");
    let expected = vec![LoadStringUInt8(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_array_get() {
    let result = code("fun f(x: Array[Float32], idx: Int64): Float32 { x(idx) }");
    let expected = vec![LoadArray(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_array_get_method() {
    let result = code("fun f(x: Array[Float32], idx: Int64): Float32 { x.get(idx) }");
    let expected = vec![LoadArray(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_array_set_method() {
    let result =
        code("fun f(x: Array[Float32], idx: Int64, value: Float32) { x.set(idx, value); }");
    let expected = vec![StoreArray(r(2), r(0), r(1)), Ret(r(3))];
    assert_eq!(expected, result);
}

#[test]
fn gen_string_concat() {
    gen_fct(
        "fun f(a: String, b: String): String { a + b }",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("String", "plus", false)
                .expect("String::plus not found");
            let expected = vec![
                PushRegister(r(0)),
                PushRegister(r(1)),
                InvokeDirect(r(2), ConstPoolIdx(0)),
                Ret(r(2)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_string_equals() {
    gen_fct(
        "fun f(a: String, b: String): Bool { a != b }",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("String", "equals", false)
                .expect("String::equals not found");
            let expected = vec![
                PushRegister(r(0)),
                PushRegister(r(1)),
                InvokeDirect(r(2), ConstPoolIdx(0)),
                Not(r(2), r(2)),
                Ret(r(2)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_bool_to_string() {
    gen_fct(
        "fun f(a: Bool): String { a.toString() }",
        |sa, code, fct| {
            let fct_id = sa
                .struct_method_by_name("Bool", "toString", false)
                .expect("Bool::toString not found");
            let expected = vec![
                PushRegister(r(0)),
                InvokeDirect(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_cmp_strings() {
    gen_fct(
        "fun f(a: String, b: String): Bool { a < b }",
        |sa, code, fct| {
            let fct_id = sa
                .cls_method_by_name("String", "compareTo", false)
                .expect("String::compareTo not found");
            let expected = vec![
                PushRegister(r(0)),
                PushRegister(r(1)),
                InvokeDirect(r(3), ConstPoolIdx(0)),
                ConstInt32(r(4), 0),
                TestLt(r(2), r(3), r(4)),
                Ret(r(2)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_extend_uint8() {
    let result = code("fun f(x: UInt8): Int32 { x.toInt32() }");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, result);

    let result = code("fun f(x: UInt8): Int64 { x.toInt64() }");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_extend_int() {
    let result = code("fun f(x: Int32): Int64 { x.toInt64() }");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_cast_char() {
    let result = code("fun f(x: Char): Int32 { x.toInt32() }");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, result);

    let result = code("fun f(x: Char): Int64 { x.toInt64() }");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_cast_int() {
    let result = code("fun f(x: Int32): UInt8 { x.toUInt8() }");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, result);

    let result = code("fun f(x: Int32): Char { x.toCharUnchecked() }");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_cast_int64() {
    let result = code("fun f(x: Int64): UInt8 { x.toUInt8() }");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, result);

    let result = code("fun f(x: Int64): Char { x.toCharUnchecked() }");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, result);

    let result = code("fun f(x: Int64): Int32 { x.toInt32() }");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_compare_to_method() {
    gen_fct(
        "fun f(a: Int64, b: Int64): Int32 { a.compareTo(b) }",
        |sa, code, fct| {
            let fct_id = sa
                .struct_method_by_name("Int64", "compareTo", false)
                .expect("Int64::compareTo not found");
            let expected = vec![
                PushRegister(r(0)),
                PushRegister(r(1)),
                InvokeDirect(r(2), ConstPoolIdx(0)),
                Ret(r(2)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );

    gen_fct(
        "fun f(a: Int32, b: Int32): Int32 { a.compareTo(b) }",
        |sa, code, fct| {
            let fct_id = sa
                .struct_method_by_name("Int32", "compareTo", false)
                .expect("Int32::compareTo not found");
            let expected = vec![
                PushRegister(r(0)),
                PushRegister(r(1)),
                InvokeDirect(r(2), ConstPoolIdx(0)),
                Ret(r(2)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );

    gen_fct(
        "fun f(a: Float32, b: Float32): Int32 { a.compareTo(b) }",
        |sa, code, fct| {
            let fct_id = sa
                .struct_method_by_name("Float32", "compareTo", false)
                .expect("Float32::compareTo not found");
            let expected = vec![
                PushRegister(r(0)),
                PushRegister(r(1)),
                InvokeDirect(r(2), ConstPoolIdx(0)),
                Ret(r(2)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );

    gen_fct(
        "fun f(a: Float64, b: Float64): Int32 { a.compareTo(b) }",
        |sa, code, fct| {
            let fct_id = sa
                .struct_method_by_name("Float64", "compareTo", false)
                .expect("Float64::compareTo not found");
            let expected = vec![
                PushRegister(r(0)),
                PushRegister(r(1)),
                InvokeDirect(r(2), ConstPoolIdx(0)),
                Ret(r(2)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_const_int() {
    let result = code("const X: Int32 = 1i32; fun f(): Int32 { X }");
    let expected = vec![ConstInt32(r(0), 1), Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_while_with_break() {
    let result = code("fun f(x: Bool) { while x { break; } }");
    let expected = vec![
        LoopStart,
        JumpIfFalse(r(0), 4),
        Jump(4),
        JumpLoop(0),
        Ret(r(1)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_vec_load() {
    gen_fct(
        "fun f(x: Vec[Int32], idx: Int64): Int32 { x(idx) }",
        |sa, code, fct| {
            let fct_id = sa.cls_method_by_name("Vec", "get", false).unwrap();
            let expected = vec![
                PushRegister(r(0)),
                PushRegister(r(1)),
                InvokeDirect(r(2), ConstPoolIdx(0)),
                Ret(r(2)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::single(SourceType::Int32))
            );
        },
    );
}

#[test]
fn gen_vec_store() {
    gen_fct(
        "fun f(x: Vec[Int32], idx: Int64, value: Int32) { x(idx) = value; }",
        |sa, code, fct| {
            let fct_id = sa.cls_method_by_name("Vec", "set", false).unwrap();
            let expected = vec![
                PushRegister(r(0)),
                PushRegister(r(1)),
                PushRegister(r(2)),
                InvokeDirect(r(3), ConstPoolIdx(0)),
                Ret(r(3)),
            ];
            assert_eq!(expected, code);
            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::single(SourceType::Int32))
            );
        },
    );
}

#[test]
fn gen_byte_to_char() {
    let result = code("fun f(x: UInt8): Char { x.toChar() }");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, result);
}

#[test]
fn gen_int_min_value() {
    let result = code("fun f(): Int32 { -2147483648i32 }");
    let expected = vec![ConstInt32(r(0), -2147483648), Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_int_max_value() {
    let result = code("fun f(): Int32 { 2147483647i32 }");
    let expected = vec![ConstInt32(r(0), 2147483647), Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_int64_min_value() {
    let result = code("fun f(): Int64 { -9223372036854775808i64 }");
    let expected = vec![ConstInt64(r(0), -9223372036854775808), Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_int64_max_value() {
    let result = code("fun f(): Int64 { 9223372036854775807i64 }");
    let expected = vec![ConstInt64(r(0), 9223372036854775807), Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_tuple_var() {
    gen_fct("fun f() { let x = (1i32, 2i32); }", |_, code, fct| {
        let subtypes = vec![SourceType::Int32, SourceType::Int32];
        let expected = vec![
            ConstInt32(r(1), 1),
            ConstInt32(r(2), 2),
            PushRegister(r(1)),
            PushRegister(r(2)),
            NewTuple(r(0), ConstPoolIdx(2)),
            Ret(r(3)),
        ];
        assert_eq!(expected, code);

        assert_eq!(
            fct.const_pool(ConstPoolIdx(2)),
            &ConstPoolEntry::Tuple(SourceTypeArray::with(subtypes))
        );
    });
}

#[test]
fn gen_tuple_move() {
    let result = code("fun f(x: (Int32, Int32)) { let y = x; }");
    let expected = vec![Mov(r(1), r(0)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_tuple_element() {
    gen_fct(
        "fun f(x: (Int32, Int32)): Int32 { x.0 }",
        |sa, code, fct| {
            let tuple_ty = create_tuple(sa, vec![SourceType::Int32, SourceType::Int32]);
            let expected = vec![LoadTupleElement(r(1), r(0), ConstPoolIdx(0)), Ret(r(1))];
            assert_eq!(expected, code);

            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::TupleElement(tuple_ty, 0)
            );
        },
    );
}

#[test]
fn gen_trait_object() {
    gen_fct(
        "
        trait Foo { fun bar(): Int32; }
        class Bar
        impl Foo for Bar {
            fun bar(): Int32 { 1i32 }
        }
        fun f(x: Bar): Foo { x as Foo }
    ",
        |sa, code, fct| {
            let trait_id = sa.trait_by_name("Foo");
            let cls_id = sa.cls_by_name("Bar");
            let object_ty = SourceType::Class(cls_id, SourceTypeArray::empty());
            let expected = vec![NewTraitObject(r(1), ConstPoolIdx(0), r(0)), Ret(r(1))];
            assert_eq!(expected, code);

            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Trait(trait_id, SourceTypeArray::empty(), object_ty)
            );
        },
    );
}

#[test]
fn gen_trait_object_copy() {
    gen(
        "
        trait Foo { fun bar(): Int32; }
        fun f(x: Foo): Foo { let y = x; y }
    ",
        |_sa, code| {
            let expected = vec![Mov(r(1), r(0)), Ret(r(1))];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_trait_object_method_call() {
    gen_fct(
        "
        trait Foo { fun bar(): Int32; }
        fun f(x: Foo): Int32 { x.bar() }
    ",
        |sa, code, fct| {
            let fct_id = sa.trait_method_by_name("Foo", "bar");
            let expected = vec![
                PushRegister(r(0)),
                InvokeVirtual(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);

            assert_eq!(
                fct.const_pool(ConstPoolIdx(0)),
                &ConstPoolEntry::Fct(fct_id, SourceTypeArray::empty())
            );
        },
    );
}

#[test]
fn gen_new_lambda() {
    gen_fct(
        "
        fun f(): (): Int32 {
            ||: Int32 { 12 }
        }
    ",
        |_sa, code, fct| {
            let expected = vec![NewLambda(r(0), ConstPoolIdx(0)), Ret(r(0))];
            assert_eq!(expected, code);

            assert!(fct.const_pool(ConstPoolIdx(0)).is_fct());
        },
    );
}

#[test]
fn gen_context_allocated_var() {
    gen_fct(
        "
        fun f(): (): Int64 {
            let mut x = 10;
            x = 11;
            let y = x;
            ||: Int64 { x }
        }
    ",
        |_sa, code, _fct| {
            let expected = vec![
                NewObject(r(0), ConstPoolIdx(0)),
                ConstInt64(r(1), 10),
                StoreField(r(1), r(0), ConstPoolIdx(2)),
                ConstInt64(r(1), 11),
                StoreField(r(1), r(0), ConstPoolIdx(4)),
                LoadField(r(1), r(0), ConstPoolIdx(5)),
                PushRegister(r(0)),
                NewLambda(r(2), ConstPoolIdx(6)),
                Ret(r(2)),
            ];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_access_lambda_args() {
    gen_fct(
        "
        fun f(): (Int32, Int32): Int32 {
            |a: Int32, b: Int32|: Int32 { a + b }
        }
    ",
        |sa, _code, fct| {
            let lambda_id = match fct.const_pool(ConstPoolIdx(0)) {
                ConstPoolEntry::Fct(fct_id, _) => *fct_id,
                _ => unreachable!(),
            };

            let lambda = generate_fct(sa, lambda_id);
            let code = build(&lambda);

            let expected = vec![Add(r(3), r(1), r(2)), Ret(r(3))];
            assert_eq!(expected, code);
        },
    );
}

#[test]
fn gen_invoke_lambda() {
    gen_fct(
        "
        fun f(x: (): Int32): Int32 {
            x()
        }
    ",
        |_sa, code, _fct| {
            let expected = vec![
                PushRegister(r(0)),
                InvokeLambda(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );

    gen_fct(
        "
        fun f(x: (): ()) {
            x()
        }
    ",
        |_sa, code, _fct| {
            let expected = vec![
                PushRegister(r(0)),
                InvokeLambda(r(1), ConstPoolIdx(0)),
                Ret(r(1)),
                Ret(r(1)),
            ];
            assert_eq!(expected, code);
        },
    );
}

fn p(line: u32, column: u32) -> Position {
    Position { line, column }
}

fn r(val: usize) -> Register {
    Register(val)
}

#[derive(PartialEq, Debug)]
pub enum Bytecode {
    Add(Register, Register, Register),
    Sub(Register, Register, Register),
    Neg(Register, Register),
    Mul(Register, Register, Register),
    Div(Register, Register, Register),
    Mod(Register, Register, Register),
    And(Register, Register, Register),
    Or(Register, Register, Register),
    Xor(Register, Register, Register),
    Not(Register, Register),
    Shl(Register, Register, Register),
    Shr(Register, Register, Register),
    Sar(Register, Register, Register),

    Mov(Register, Register),

    LoadTupleElement(Register, Register, ConstPoolIdx),
    LoadStructField(Register, Register, ConstPoolIdx),

    LoadField(Register, Register, ConstPoolIdx),
    StoreField(Register, Register, ConstPoolIdx),

    LoadGlobal(Register, GlobalDefinitionId),
    StoreGlobal(Register, GlobalDefinitionId),

    PushRegister(Register),

    ConstTrue(Register),
    ConstFalse(Register),
    ConstUInt8(Register, u8),
    ConstChar(Register, char),
    ConstInt32(Register, i32),
    ConstInt64(Register, i64),
    ConstFloat32(Register, f32),
    ConstFloat64(Register, f64),
    ConstString(Register, String),

    TestIdentity(Register, Register, Register),
    TestEq(Register, Register, Register),
    TestNe(Register, Register, Register),
    TestGt(Register, Register, Register),
    TestGe(Register, Register, Register),
    TestLt(Register, Register, Register),
    TestLe(Register, Register, Register),

    LoopStart,
    JumpLoop(usize),
    Jump(usize),
    JumpIfFalse(Register, usize),
    JumpIfTrue(Register, usize),

    InvokeDirect(Register, ConstPoolIdx),
    InvokeVirtual(Register, ConstPoolIdx),
    InvokeStatic(Register, ConstPoolIdx),
    InvokeLambda(Register, ConstPoolIdx),
    InvokeGenericStatic(Register, ConstPoolIdx),
    InvokeGenericDirect(Register, ConstPoolIdx),

    NewObject(Register, ConstPoolIdx),
    NewObjectInitialized(Register, ConstPoolIdx),
    NewArray(Register, ConstPoolIdx, Register),
    NewTuple(Register, ConstPoolIdx),
    NewEnum(Register, ConstPoolIdx),
    NewStruct(Register, ConstPoolIdx),
    NewTraitObject(Register, ConstPoolIdx, Register),
    NewLambda(Register, ConstPoolIdx),

    ArrayLength(Register, Register),

    LoadArray(Register, Register, Register),
    StoreArray(Register, Register, Register),

    StringLength(Register, Register),
    LoadStringUInt8(Register, Register, Register),

    Ret(Register),
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

    fn visit_add(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::Add(dest, lhs, rhs));
    }

    fn visit_sub(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::Sub(dest, lhs, rhs));
    }

    fn visit_neg(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::Neg(dest, src));
    }

    fn visit_mul(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::Mul(dest, lhs, rhs));
    }

    fn visit_div(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::Div(dest, lhs, rhs));
    }

    fn visit_mod(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::Mod(dest, lhs, rhs));
    }

    fn visit_and(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::And(dest, lhs, rhs));
    }

    fn visit_or(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::Or(dest, lhs, rhs));
    }

    fn visit_xor(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::Xor(dest, lhs, rhs));
    }

    fn visit_not(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::Not(dest, src));
    }

    fn visit_shl(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::Shl(dest, lhs, rhs));
    }
    fn visit_shr(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::Shr(dest, lhs, rhs));
    }
    fn visit_sar(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::Sar(dest, lhs, rhs));
    }

    fn visit_mov(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::Mov(dest, src));
    }

    fn visit_load_tuple_element(&mut self, src: Register, dest: Register, idx: ConstPoolIdx) {
        self.emit(Bytecode::LoadTupleElement(src, dest, idx));
    }

    fn visit_load_struct_field(&mut self, dest: Register, obj: Register, idx: ConstPoolIdx) {
        self.emit(Bytecode::LoadStructField(dest, obj, idx));
    }

    fn visit_load_field(&mut self, dest: Register, obj: Register, idx: ConstPoolIdx) {
        self.emit(Bytecode::LoadField(dest, obj, idx));
    }

    fn visit_store_field(&mut self, src: Register, obj: Register, field: ConstPoolIdx) {
        self.emit(Bytecode::StoreField(src, obj, field));
    }

    fn visit_load_global(&mut self, dest: Register, global_id: GlobalDefinitionId) {
        self.emit(Bytecode::LoadGlobal(dest, global_id));
    }

    fn visit_store_global(&mut self, src: Register, global_id: GlobalDefinitionId) {
        self.emit(Bytecode::StoreGlobal(src, global_id));
    }

    fn visit_push_register(&mut self, src: Register) {
        self.emit(Bytecode::PushRegister(src));
    }

    fn visit_const_true(&mut self, dest: Register) {
        self.emit(Bytecode::ConstTrue(dest));
    }
    fn visit_const_false(&mut self, dest: Register) {
        self.emit(Bytecode::ConstFalse(dest));
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

    fn visit_test_identity(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestIdentity(dest, lhs, rhs));
    }
    fn visit_test_eq(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestEq(dest, lhs, rhs));
    }
    fn visit_test_ne(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestNe(dest, lhs, rhs));
    }
    fn visit_test_gt(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestGt(dest, lhs, rhs));
    }
    fn visit_test_ge(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestGe(dest, lhs, rhs));
    }
    fn visit_test_lt(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestLt(dest, lhs, rhs));
    }
    fn visit_test_le(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestLe(dest, lhs, rhs));
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
    fn visit_loop_start(&mut self) {
        self.emit(Bytecode::LoopStart);
    }

    fn visit_invoke_direct(&mut self, dest: Register, fctdef: ConstPoolIdx) {
        self.emit(Bytecode::InvokeDirect(dest, fctdef));
    }

    fn visit_invoke_virtual(&mut self, dest: Register, fct_idx: ConstPoolIdx) {
        self.emit(Bytecode::InvokeVirtual(dest, fct_idx));
    }

    fn visit_invoke_static(&mut self, dest: Register, fctdef: ConstPoolIdx) {
        self.emit(Bytecode::InvokeStatic(dest, fctdef));
    }

    fn visit_invoke_lambda(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit(Bytecode::InvokeLambda(dest, idx));
    }

    fn visit_invoke_generic_static(&mut self, dest: Register, fct_idx: ConstPoolIdx) {
        self.emit(Bytecode::InvokeGenericStatic(dest, fct_idx));
    }

    fn visit_invoke_generic_direct(&mut self, dest: Register, fct_idx: ConstPoolIdx) {
        self.emit(Bytecode::InvokeGenericDirect(dest, fct_idx));
    }

    fn visit_new_object(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit(Bytecode::NewObject(dest, idx));
    }
    fn visit_new_object_initialized(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit(Bytecode::NewObjectInitialized(dest, idx));
    }
    fn visit_new_array(&mut self, dest: Register, idx: ConstPoolIdx, length: Register) {
        self.emit(Bytecode::NewArray(dest, idx, length));
    }
    fn visit_new_tuple(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit(Bytecode::NewTuple(dest, idx));
    }
    fn visit_new_enum(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit(Bytecode::NewEnum(dest, idx));
    }
    fn visit_new_struct(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit(Bytecode::NewStruct(dest, idx));
    }
    fn visit_new_trait_object(&mut self, dest: Register, idx: ConstPoolIdx, src: Register) {
        self.emit(Bytecode::NewTraitObject(dest, idx, src));
    }
    fn visit_new_lambda(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit(Bytecode::NewLambda(dest, idx));
    }

    fn visit_array_length(&mut self, dest: Register, arr: Register) {
        self.emit(Bytecode::ArrayLength(dest, arr));
    }

    fn visit_load_array(&mut self, dest: Register, arr: Register, idx: Register) {
        self.emit(Bytecode::LoadArray(dest, arr, idx));
    }

    fn visit_store_array(&mut self, src: Register, arr: Register, idx: Register) {
        self.emit(Bytecode::StoreArray(src, arr, idx));
    }

    fn visit_string_length(&mut self, dest: Register, arr: Register) {
        self.emit(Bytecode::StringLength(dest, arr));
    }

    fn visit_load_string_uint8(&mut self, dest: Register, arr: Register, idx: Register) {
        self.emit(Bytecode::LoadStringUInt8(dest, arr, idx));
    }

    fn visit_ret(&mut self, opnd: Register) {
        self.emit(Bytecode::Ret(opnd));
    }
}
