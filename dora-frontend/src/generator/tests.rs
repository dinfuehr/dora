use std::collections::HashMap;
use std::mem;

use self::Bytecode::*;
use crate::generator::generate_fct_id;
use crate::program_emitter::Emitter;
use crate::sema::{ClassDefinitionId, FieldIndex, Sema, SemaCreationParams};
use crate::stdlib_lookup::{lookup_fct, resolve_path};
use crate::{check_program, emit_program};
use dora_bytecode::{
    self as bytecode, BytecodeFunction, BytecodeOffset, BytecodeType, BytecodeTypeArray,
    BytecodeVisitor, ClassId, ConstId, ConstPoolEntry, ConstPoolIdx, EnumId, FunctionId, GlobalId,
    Program, Register, StructId, TraitId,
};

fn positions(fct: &BytecodeFunction) -> Vec<(u32, u32)> {
    fct.locations()
        .iter()
        .map(|(bci, location)| (bci.0, location.line()))
        .collect()
}

fn sema(code: &'static str) -> Sema {
    let args: SemaCreationParams = SemaCreationParams::new().set_program_content(code);
    let mut sa = Sema::new(args);

    let result = check_program(&mut sa);
    if sa.diag.borrow().has_errors() {
        sa.diag.borrow_mut().dump(&sa, true);
    }
    assert!(!sa.diag.borrow().has_errors());
    assert!(result);

    sa
}

#[allow(unused)]
fn semac(code: &'static str) -> Program {
    let sa = sema(code);
    emit_program(sa)
}

fn bc(sa: &Sema, path: &str) -> (BytecodeFunction, Vec<Bytecode>) {
    let fct_id = lookup_fct(sa, path);
    let mut emitter = Emitter::new();
    let bc_fct = generate_fct_id(sa, &mut emitter, fct_id);
    let code = build(&bc_fct);

    (bc_fct, code)
}

#[test]
fn gen_generic_identity() {
    let sa = sema("fn f[T](x: T): T { x }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![Ret(r(0))];
    assert_eq!(expected, code);

    let sa = sema("fn f[T](x: T): T { let y = x; y }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![Mov(r(1), r(0)), Ret(r(1))];
    assert_eq!(expected, code);
}

#[test]
fn gen_generic_static_trait() {
    let sa = sema("trait Foo { static fn baz(); } fn f[T: Foo]() { T::baz() }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![InvokeGenericStatic(r(0), ConstPoolIdx(0)), Ret(r(0))];
    assert_eq!(expected, code);
}

#[test]
fn gen_generic_direct_trait() {
    let sa = sema("trait Foo { fn baz(); } fn f[T: Foo](obj: T) { obj.baz() }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        PushRegister(r(0)),
        InvokeGenericDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_load_field_uint8() {
    let sa = sema("class Foo { bar: UInt8 } fn f(a: Foo): UInt8 { return a.bar; }");
    let (fct, code) = bc(&sa, "<prog>::f");
    let (cls, field) = field_by_name(&sa, "<prog>::Foo", "bar");
    let expected = vec![LoadField(r(1), r(0), ConstPoolIdx(0)), Ret(r(1))];
    assert_eq!(expected, code);

    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Field(
            ClassId(cls.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty(),
            field.0 as u32
        )
    );
}

#[test]
fn gen_position_load_field_uint8() {
    let sa = sema("class Foo { bar: UInt8 } fn f(a: Foo): UInt8 { return a.bar; }");
    let (fct, _code) = bc(&sa, "<prog>::f");
    let result = positions(&fct);
    let expected = vec![(0, 1)];
    assert_eq!(expected, result);
}

#[test]
fn gen_store_field_uint8() {
    let sa = sema("class Foo{bar: UInt8} fn f(a: Foo, b: UInt8) { a.bar = b; }");
    let (fct, code) = bc(&sa, "<prog>::f");
    let (cls, field) = field_by_name(&sa, "<prog>::Foo", "bar");
    let expected = vec![StoreField(r(1), r(0), ConstPoolIdx(0)), Ret(r(2))];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Field(
            ClassId(cls.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty(),
            field.0 as u32
        )
    );
}

#[test]
fn gen_position_store_field_uint8() {
    let sa = sema("class Foo{bar: UInt8} fn f(a: Foo, b: UInt8) { a.bar = b; }");
    let (fct, _code) = bc(&sa, "<prog>::f");
    let result = positions(&fct);
    let expected = vec![(0, 1)];
    assert_eq!(expected, result);
}

#[test]
fn gen_add_int() {
    let sa = sema("fn f(): Int32 { return 1i32 + 2i32; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        ConstInt32(r(1), 1),
        ConstInt32(r(2), 2),
        Add(r(0), r(1), r(2)),
        Ret(r(0)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_add_float32() {
    let sa = sema("fn f(): Float32 { return 1f32 + 2f32; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        ConstFloat32(r(1), 1_f32),
        ConstFloat32(r(2), 2_f32),
        Add(r(0), r(1), r(2)),
        Ret(r(0)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_add_float64() {
    let sa = sema("fn f(a: Float64, b: Float64): Float64 { return a + b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![Add(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_id_int() {
    let sa = sema("fn f(a: Int32): Int32 { return a; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![Ret(r(0))];
    assert_eq!(expected, code);
}

#[test]
fn gen_id_ptr() {
    let sa = sema("class Object fn f(a: Object): Object { return a; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![Ret(r(0))];
    assert_eq!(expected, code);
}

#[test]
fn gen_ptr_is() {
    let sa = sema("class Object fn f(a: Object, b: Object): Bool { return a === b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestIdentity(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_ptr_is_not() {
    let sa = sema("class Object fn f(a: Object, b: Object): Bool { return a !== b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestIdentity(r(2), r(0), r(1)), Not(r(2), r(2)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_sub_int() {
    let sa = sema("fn f(a: Int32, b: Int32): Int32 { return a - b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![Sub(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_sub_float32() {
    let sa = sema("fn f(a: Float32, b: Float32): Float32 { return a - b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![Sub(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_sub_float64() {
    let sa = sema("fn f(a: Float64, b: Float64): Float64 { return a - b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![Sub(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_div_int() {
    let sa = sema("fn f(a: Int32, b: Int32): Int32 { return a / b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![Div(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_position_div_int() {
    let sa = sema("fn f(a: Int32, b: Int32): Int32 { return a / b; }");
    let (fct, _code) = bc(&sa, "<prog>::f");
    let result = positions(&fct);
    let expected = vec![(0, 1)];
    assert_eq!(expected, result);
}

#[test]
fn gen_div_float32() {
    let sa = sema("fn f(a: Float32, b: Float32): Float32 { return a / b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![Div(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_div_float64() {
    let sa = sema("fn f(a: Float64, b: Float64): Float64 { return a / b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![Div(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_mul_int() {
    let sa = sema("fn f(a: Int32, b: Int32): Int32 { return a * b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![Mul(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_mul_float32() {
    let sa = sema("fn f(a: Float32, b: Float32): Float32 { return a * b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![Mul(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_mul_float64() {
    let sa = sema("fn f(a: Float64, b: Float64): Float64 { return a * b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![Mul(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_stmt_var_init() {
    let sa = sema("fn f() { let x = 1i32; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![ConstInt32(r(1), 1), Mov(r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_generic_not() {
    let sa = sema("fn f[T: std::traits::Not](value: T): T { !value }");
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "std::traits::Not#not");
    let expected = vec![
        PushRegister(r(0)),
        InvokeGenericDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);

    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Generic(
            0,
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty(),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_stmt_let_tuple_pair() {
    let sa = sema("fn f(value: (Int32, Int32)): Int32 { let (x, y) = value; x+y }");
    let (fct, code) = bc(&sa, "<prog>::f");

    let tuple_ty = BytecodeType::Tuple(BytecodeTypeArray::new(vec![
        BytecodeType::Int32,
        BytecodeType::Int32,
    ]));
    let expected = vec![
        LoadTupleElement(r(3), r(0), ConstPoolIdx(0)),
        Mov(r(1), r(3)),
        LoadTupleElement(r(3), r(0), ConstPoolIdx(1)),
        Mov(r(2), r(3)),
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
}

#[test]
fn gen_stmt_let_tuple_nested_pair() {
    let sa = sema("fn f(value: (Int32, (Int32, Int32))): Int32 { let (x, (y, z)) = value; x+y+z }");
    let (fct, code) = bc(&sa, "<prog>::f");

    let nested_tuple_ty = BytecodeType::Tuple(BytecodeTypeArray::new(vec![
        BytecodeType::Int32,
        BytecodeType::Int32,
    ]));
    let tuple_ty = BytecodeType::Tuple(BytecodeTypeArray::new(vec![
        BytecodeType::Int32,
        nested_tuple_ty.clone(),
    ]));
    let expected = vec![
        LoadTupleElement(r(4), r(0), ConstPoolIdx(0)),
        Mov(r(1), r(4)),
        LoadTupleElement(r(5), r(0), ConstPoolIdx(1)),
        LoadTupleElement(r(4), r(5), ConstPoolIdx(2)),
        Mov(r(2), r(4)),
        LoadTupleElement(r(4), r(5), ConstPoolIdx(3)),
        Mov(r(3), r(4)),
        Add(r(6), r(1), r(2)),
        Add(r(4), r(6), r(3)),
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
        &ConstPoolEntry::TupleElement(nested_tuple_ty.clone(), 0)
    );

    assert_eq!(
        fct.const_pool(ConstPoolIdx(3)),
        &ConstPoolEntry::TupleElement(nested_tuple_ty, 1)
    );
}

#[test]
fn gen_stmt_let_tuple_nested_pair_any() {
    let sa = sema("fn f(value: (Int32, (Int32, Int32))): Int32 { let (x, (_, z)) = value; x+z }");
    let (fct, code) = bc(&sa, "<prog>::f");

    let nested_tuple_ty = BytecodeType::Tuple(BytecodeTypeArray::new(vec![
        BytecodeType::Int32,
        BytecodeType::Int32,
    ]));
    let tuple_ty = BytecodeType::Tuple(BytecodeTypeArray::new(vec![
        BytecodeType::Int32,
        nested_tuple_ty.clone(),
    ]));
    let expected = vec![
        LoadTupleElement(r(3), r(0), ConstPoolIdx(0)),
        Mov(r(1), r(3)),
        LoadTupleElement(r(4), r(0), ConstPoolIdx(1)),
        LoadTupleElement(r(3), r(4), ConstPoolIdx(2)),
        Mov(r(2), r(3)),
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

    assert_eq!(
        fct.const_pool(ConstPoolIdx(2)),
        &ConstPoolEntry::TupleElement(nested_tuple_ty, 1)
    );
}

#[test]
fn gen_stmt_let_unit() {
    let sa = sema("fn f(value: ()) { let () = value; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![Ret(r(0))];
    assert_eq!(expected, code);

    let sa = sema("fn f() { let x = (); }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![Ret(r(0))];
    assert_eq!(expected, code);

    let sa = sema("fn f(value: (Int32, (Int32, ()))): Int32 { let (x, (y, z)) = value; x+y }");
    let (fct, code) = bc(&sa, "<prog>::f");

    let inner_tuple_ty = BytecodeType::Tuple(BytecodeTypeArray::new(vec![
        BytecodeType::Int32,
        BytecodeType::Unit,
    ]));
    let tuple_ty = BytecodeType::Tuple(BytecodeTypeArray::new(vec![
        BytecodeType::Int32,
        inner_tuple_ty.clone(),
    ]));
    let expected = vec![
        LoadTupleElement(r(4), r(0), ConstPoolIdx(0)),
        Mov(r(1), r(4)),
        LoadTupleElement(r(5), r(0), ConstPoolIdx(1)),
        LoadTupleElement(r(4), r(5), ConstPoolIdx(2)),
        Mov(r(2), r(4)),
        LoadTupleElement(r(3), r(5), ConstPoolIdx(3)),
        Add(r(4), r(1), r(2)),
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

    let sa = sema("fn f(value: (Int32, (Int32, ()))): Int32 { let (x, (y, ())) = value; x+y }");
    let (fct, code) = bc(&sa, "<prog>::f");

    let nested_tuple_ty = BytecodeType::Tuple(BytecodeTypeArray::new(vec![
        BytecodeType::Int32,
        BytecodeType::Unit,
    ]));
    let tuple_ty = BytecodeType::Tuple(BytecodeTypeArray::new(vec![
        BytecodeType::Int32,
        nested_tuple_ty.clone(),
    ]));
    let expected = vec![
        LoadTupleElement(r(3), r(0), ConstPoolIdx(0)),
        Mov(r(1), r(3)),
        LoadTupleElement(r(4), r(0), ConstPoolIdx(1)),
        LoadTupleElement(r(3), r(4), ConstPoolIdx(2)),
        Mov(r(2), r(3)),
        LoadTupleElement(r(5), r(4), ConstPoolIdx(3)),
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

    assert_eq!(
        fct.const_pool(ConstPoolIdx(2)),
        &ConstPoolEntry::TupleElement(nested_tuple_ty, 0)
    );
}

#[test]
fn gen_stmt_while() {
    let sa = sema("fn f() { while true { 0; } }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        LoopStart,
        ConstTrue(r(0)),
        JumpIfFalse(r(0), 5),
        ConstInt64(r(1), 0),
        JumpLoop(0),
        Ret(r(2)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_stmt_if() {
    let sa = sema("fn f(a: Bool): Int32 { if a { return 1; } return 0; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        JumpIfFalse(r(0), 3),
        ConstInt32(r(2), 1),
        Ret(r(2)),
        ConstInt32(r(2), 0),
        Ret(r(2)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_stmt_if_else_with_return() {
    let sa = sema("fn f(a: Bool): Int32 { if a { return 1; } else { return 2; } }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        JumpIfFalse(r(0), 3),
        ConstInt32(r(2), 1),
        Ret(r(2)),
        ConstInt32(r(2), 2),
        Ret(r(2)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_stmt_if_else_without_return() {
    let sa = sema(
        "fn f(b: Bool): Bool {
        let mut a = b;
        if a { a = false; } else { a = true; }
        return a;
    }",
    );
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        Mov(r(1), r(0)),
        JumpIfFalse(r(1), 5),
        ConstFalse(r(3)),
        Mov(r(1), r(3)),
        Jump(7),
        ConstTrue(r(3)),
        Mov(r(1), r(3)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_stmt_break() {
    let sa = sema("fn f() { while true { break; } }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        LoopStart,
        ConstTrue(r(0)),
        JumpIfFalse(r(0), 5),
        Jump(5),
        JumpLoop(0),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_stmt_continue() {
    let sa = sema("fn f() { while true { continue; } }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        LoopStart,
        ConstTrue(r(0)),
        JumpIfFalse(r(0), 5),
        JumpLoop(0),
        JumpLoop(0),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_lit_char() {
    let sa = sema("fn f(): Char { return '1'; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![ConstChar(r(0), '1'), Ret(r(0))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_lit_int() {
    let sa = sema("fn f(): Int32 { return 1; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![ConstInt32(r(0), 1), Ret(r(0))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_lit_uint8() {
    let sa = sema("fn f(): UInt8 { return 1u8; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![ConstUInt8(r(0), 1), Ret(r(0))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_lit_int64() {
    let sa = sema("fn f(): Int64 { return 1i64; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![ConstInt64(r(0), 1), Ret(r(0))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_lit_float32() {
    let sa = sema("fn f(): Float32 { return 1f32; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![ConstFloat32(r(0), 1_f32), Ret(r(0))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_lit_float64() {
    let sa = sema("fn f(): Float64 { return 1f64; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![ConstFloat64(r(0), 1_f64), Ret(r(0))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_lit_string() {
    let sa = sema("fn f(): String { return \"z\"; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![ConstString(r(0), "z".to_string()), Ret(r(0))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_lit_string_duplicate() {
    let sa = sema("fn f() { let a = \"z\"; let b = \"z\"; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        ConstString(r(1), "z".to_string()),
        Mov(r(0), r(1)),
        ConstString(r(2), "z".to_string()),
        Mov(r(1), r(2)),
        Ret(r(3)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_lit_string_multiple() {
    let sa = sema("fn f() { let a = \"z\"; let b = \"y\"; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        ConstString(r(1), "z".to_string()),
        Mov(r(0), r(1)),
        ConstString(r(2), "y".to_string()),
        Mov(r(1), r(2)),
        Ret(r(3)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_lit_byte_zero() {
    let sa = sema("fn f(): UInt8 { return 0u8; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![ConstUInt8(r(0), 0), Ret(r(0))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_lit_int_zero() {
    let sa = sema("fn f(): Int32 { return 0; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![ConstInt32(r(0), 0), Ret(r(0))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_lit_int64_zero() {
    let sa = sema("fn f(): Int64 { return 0i64; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![ConstInt64(r(0), 0), Ret(r(0))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_lit_float32_zero() {
    let sa = sema("fn f(): Float32 { return 0f32; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![ConstFloat32(r(0), 0.0), Ret(r(0))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_lit_float64_zero() {
    let sa = sema("fn f(): Float64 { return 0f64; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![ConstFloat64(r(0), 0.0), Ret(r(0))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_or() {
    let sa = sema("fn f(a: Bool, b: Bool): Bool { return a || b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        Mov(r(2), r(0)),
        JumpIfTrue(r(2), 3),
        Mov(r(2), r(1)),
        Ret(r(2)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_and() {
    let sa = sema("fn f(a: Bool, b: Bool): Bool { return a && b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        Mov(r(2), r(0)),
        JumpIfFalse(r(2), 3),
        Mov(r(2), r(1)),
        Ret(r(2)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_int32_neg() {
    let sa = sema("fn f(a: Int32): Int32 { return -a; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![Neg(r(1), r(0)), Ret(r(1))];
    assert_eq!(expected, code);
}

#[test]
fn gen_int64_neg() {
    let sa = sema("fn f(a: Int64): Int64 { return -a; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![Neg(r(1), r(0)), Ret(r(1))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_not() {
    let sa = sema("fn f(a: Bool): Bool { return !a; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![Not(r(1), r(0)), Ret(r(1))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_mod() {
    let sa = sema("fn f(a: Int32, b: Int32): Int32 { return a % b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![Mod(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_position_mod_int32() {
    let sa = sema("fn f(a: Int32, b: Int32): Int32 { return a % b; }");
    let (fct, _code) = bc(&sa, "<prog>::f");
    let result = positions(&fct);
    let expected = vec![(0, 1)];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_bit_or() {
    let sa = sema("fn f(a: Int32, b: Int32): Int32 { return a | b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![Or(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_bit_and() {
    let sa = sema("fn f(a: Int32, b: Int32): Int32 { return a & b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![And(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_bit_xor() {
    let sa = sema("fn f(a: Int32, b: Int32): Int32 { return a ^ b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![Xor(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_bit_shiftl() {
    let sa = sema("fn f(a: Int32, b: Int32): Int32 { return a << b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![Shl(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_bit_shiftr() {
    let sa = sema("fn f(a: Int32, b: Int32): Int32 { return a >>> b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![Shr(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_bit_ashiftr() {
    let sa = sema("fn f(a: Int32, b: Int32): Int32 { return a >> b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![Sar(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_equal_bool() {
    let sa = sema("fn f(a: Bool, b: Bool): Bool { return a == b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestEq(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_notequal_bool() {
    let sa = sema("fn f(a: Bool, b: Bool): Bool { return a != b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestNe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_equal_uint8() {
    let sa = sema("fn f(a: UInt8, b: UInt8): Bool { return a == b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestEq(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_notequal_uint8() {
    let sa = sema("fn f(a: UInt8, b: UInt8): Bool { return a != b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestNe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_lessthan_uint8() {
    let sa = sema("fn f(a: UInt8, b: UInt8): Bool { return a < b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestLt(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_lessthanequal_uint8() {
    let sa = sema("fn f(a: UInt8, b: UInt8): Bool { return a <= b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestLe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_greaterthan_uint8() {
    let sa = sema("fn f(a: UInt8, b: UInt8): Bool { return a > b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestGt(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_greaterthanequal_uint8() {
    let sa = sema("fn f(a: UInt8, b: UInt8): Bool { return a >= b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestGe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_equal_char() {
    let sa = sema("fn f(a: Char, b: Char): Bool { return a == b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestEq(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_notequal_char() {
    let sa = sema("fn f(a: Char, b: Char): Bool { return a != b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestNe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_lessthan_char() {
    let sa = sema("fn f(a: Char, b: Char): Bool { return a < b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestLt(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_lessthanequal_char() {
    let sa = sema("fn f(a: Char, b: Char): Bool { return a <= b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestLe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_greaterthan_char() {
    let sa = sema("fn f(a: Char, b: Char): Bool { return a > b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestGt(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_greaterthanequal_char() {
    let sa = sema("fn f(a: Char, b: Char): Bool { return a >= b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestGe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_equal_enum() {
    let sa = sema(
        "fn f(a: Foo, b: Foo): Bool { return a == b; }
         enum Foo { A, B }",
    );
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestEq(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_notequal_enum() {
    let sa = sema(
        "fn f(a: Foo, b: Foo): Bool { return a != b; }
         enum Foo { A, B }",
    );
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestNe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_equal_int() {
    let sa = sema("fn f(a: Int32, b: Int32): Bool { return a == b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestEq(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_notequal_int() {
    let sa = sema("fn f(a: Int32, b: Int32): Bool { return a != b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestNe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_lessthan_int() {
    let sa = sema("fn f(a: Int32, b: Int32): Bool { return a < b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestLt(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_lessthanequal_int() {
    let sa = sema("fn f(a: Int32, b: Int32): Bool { return a <= b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestLe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_greaterthan_int() {
    let sa = sema("fn f(a: Int32, b: Int32): Bool { return a > b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestGt(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_greaterthanequal_int() {
    let sa = sema("fn f(a: Int32, b: Int32): Bool { return a >= b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestGe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_equal_float32() {
    let sa = sema("fn f(a: Float32, b: Float32): Bool { return a == b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestEq(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_notequal_float32() {
    let sa = sema("fn f(a: Float32, b: Float32): Bool { return a != b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestNe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_lessthan_float32() {
    let sa = sema("fn f(a: Float32, b: Float32): Bool { return a < b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestLt(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_lessthanequal_float32() {
    let sa = sema("fn f(a: Float32, b: Float32): Bool { return a <= b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestLe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_greaterthan_float32() {
    let sa = sema("fn f(a: Float32, b: Float32): Bool { return a > b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestGt(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_greaterthanequal_float32() {
    let sa = sema("fn f(a: Float32, b: Float32): Bool { return a >= b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestGe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_equal_float64() {
    let sa = sema("fn f(a: Float64, b: Float64): Bool { return a == b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestEq(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_notequal_float64() {
    let sa = sema("fn f(a: Float64, b: Float64): Bool { return a != b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestNe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_lessthan_float64() {
    let sa = sema("fn f(a: Float64, b: Float64): Bool { return a < b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestLt(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_lessthanequal_float64() {
    let sa = sema("fn f(a: Float64, b: Float64): Bool { return a <= b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestLe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_greaterthan_float64() {
    let sa = sema("fn f(a: Float64, b: Float64): Bool { return a > b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestGt(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_test_greaterthanequal_float64() {
    let sa = sema("fn f(a: Float64, b: Float64): Bool { return a >= b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestGe(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_path() {
    let sa = sema("fn f(): Int32 { let x = 1i32; return x; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![ConstInt32(r(1), 1), Mov(r(0), r(1)), Ret(r(0))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_assign() {
    let sa = sema("fn f() { let mut x = 1i32; x = 2i32; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        ConstInt32(r(1), 1),
        Mov(r(0), r(1)),
        ConstInt32(r(1), 2),
        Mov(r(0), r(1)),
        Ret(r(2)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_self() {
    let sa = sema("class Foo impl Foo { fn f(): Foo { return self; } }");
    let (_, result) = bc(&sa, "<prog>::Foo#f");
    let expected = vec![Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_self_assign() {
    let sa = sema("class Foo impl Foo { fn f() { let x = self; } }");
    let (_, result) = bc(&sa, "<prog>::Foo#f");
    let expected = vec![Mov(r(1), r(0)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_expr_return() {
    let sa = sema("fn f(): Int32 { return 1i32; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![ConstInt32(r(0), 1), Ret(r(0))];
    assert_eq!(expected, code);
}

#[test]
fn gen_expr_returnvoid() {
    let sa = sema("fn f() { }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![Ret(r(0))];
    assert_eq!(expected, code);
}

#[test]
fn gen_load_global() {
    let sa = sema("let a: Int32 = 0i32; fn f(): Int32 { return a; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let gid = resolve_path(&sa, "<prog>::a")
        .to_global()
        .expect("missing global");
    let expected = vec![
        LoadGlobal(r(0), GlobalId(gid.index().try_into().expect("overflow"))),
        Ret(r(0)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_store_global() {
    let sa = sema("let mut a: Bool = false; fn f(x: Bool) { a = x; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let gid = resolve_path(&sa, "<prog>::a")
        .to_global()
        .expect("missing global");
    let expected = vec![
        StoreGlobal(r(0), GlobalId(gid.index().try_into().expect("overflow"))),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_fct_call_void_with_0_args() {
    let sa = sema(
        "
            fn f() { g(); }
            fn g() { }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::g");
    let expected = vec![InvokeStatic(r(0), ConstPoolIdx(0)), Ret(r(0))];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_fct_call_int_with_0_args() {
    let sa = sema(
        "
            fn f(): Int32 { return g(); }
            fn g(): Int32 { return 1i32; }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");
    let fct_id = lookup_fct(&sa, "<prog>::g");
    let expected = vec![InvokeStatic(r(0), ConstPoolIdx(0)), Ret(r(0))];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_fct_call_int_with_0_args_and_unused_result() {
    let sa = sema(
        "
            fn f() { g(); }
            fn g(): Int32 { return 1i32; }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");
    let fct_id = lookup_fct(&sa, "<prog>::g");
    let expected = vec![InvokeStatic(r(0), ConstPoolIdx(0)), Ret(r(1))];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_fct_call_void_with_1_arg() {
    let sa = sema(
        "
            fn f() { g(1i32); }
            fn g(a: Int32) { }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::g");
    let expected = vec![
        ConstInt32(r(1), 1),
        PushRegister(r(1)),
        InvokeStatic(r(0), ConstPoolIdx(0)),
        Ret(r(0)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_fct_call_void_with_3_args() {
    let sa = sema(
        "
            fn f() { g(1i32, 2i32, 3i32); }
            fn g(a: Int32, b: Int32, c: Int32) { }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");
    let fct_id = lookup_fct(&sa, "<prog>::g");
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
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_fct_call_int_with_1_arg() {
    let sa = sema(
        "
            fn f(): Int32 { return g(1i32); }
            fn g(a: Int32): Int32 { return a; }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");
    let fct_id = lookup_fct(&sa, "<prog>::g");
    let expected = vec![
        ConstInt32(r(1), 1),
        PushRegister(r(1)),
        InvokeStatic(r(0), ConstPoolIdx(0)),
        Ret(r(0)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_fct_call_int_with_3_args() {
    let sa = sema(
        "
            fn f(): Int32 { return g(1i32, 2i32, 3i32); }
            fn g(a: Int32, b: Int32, c: Int32): Int32 { return 1i32; }
            ",
    );

    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::g");
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
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_void_check_correct_self() {
    let sa = sema(
        "
            fn f(i: Int32, foo: Foo) { foo.g(); }
            class Foo
            impl Foo {
                fn g() { }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");
    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
    let expected = vec![
        PushRegister(r(1)),
        InvokeDirect(r(2), ConstPoolIdx(0)),
        Ret(r(2)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_void_with_0_args() {
    let sa = sema(
        "
            fn f(foo: Foo) { foo.g(); }
            class Foo
            impl Foo {
                fn g() { }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");
    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_void_with_1_arg() {
    let sa = sema(
        "
            fn f(foo: Foo) { foo.g(1i32); }
            class Foo
            impl Foo {
                fn g(a: Int32) { }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");
    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
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
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_void_with_3_args() {
    let sa = sema(
        "
            fn f(foo: Foo) { foo.g(1i32, 2i32, 3i32); }
            class Foo
            impl Foo {
                fn g(a: Int32, b: Int32, c: Int32) { }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");
    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
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
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_bool_with_0_args() {
    let sa = sema(
        "
            fn f(foo: Foo): Bool { return foo.g(); }
            class Foo
            impl Foo {
                fn g(): Bool { return true; }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_bool_with_0_args_and_unused_result() {
    let sa = sema(
        "
            fn f(foo: Foo) { foo.g(); }
            class Foo
            impl Foo {
                fn g(): Bool { return true; }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(2)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_bool_with_1_arg() {
    let sa = sema(
        "
            fn f(foo: Foo): Bool { return foo.g(true); }
            class Foo
            impl Foo {
                fn g(a: Bool): Bool { return true; }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
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
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_bool_with_3_args() {
    let sa = sema(
        "
            fn f(foo: Foo): Bool { return foo.g(true, false, true); }
            class Foo
            impl Foo {
                fn g(a: Bool, b: Bool, c: Bool): Bool { return true; }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
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
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_byte_with_0_args() {
    let sa = sema(
        "
            fn f(foo: Foo): UInt8 { return foo.g(); }
            class Foo
            impl Foo {
                fn g(): UInt8 { return 1u8; }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_byte_with_0_args_and_unused_result() {
    let sa = sema(
        "
            fn f(foo: Foo) { foo.g(); }
            class Foo
            impl Foo {
                fn g(): UInt8 { return 1u8; }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(2)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_byte_with_1_arg() {
    let sa = sema(
        "
            fn f(foo: Foo): UInt8 { return foo.g(1u8); }
            class Foo
            impl Foo {
                fn g(a: UInt8): UInt8 { return 1u8; }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
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
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_byte_with_3_args() {
    let sa = sema(
        "
            fn f(foo: Foo): UInt8 { return foo.g(1u8, 2u8, 3u8); }
            class Foo
            impl Foo {
                fn g(a: UInt8, b: UInt8, c: UInt8): UInt8 { return 1u8; }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
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
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_char_with_0_args() {
    let sa = sema(
        "
            fn f(foo: Foo): Char { return foo.g(); }
            class Foo
            impl Foo {
                fn g(): Char { return '1'; }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_char_with_0_args_and_unused_result() {
    let sa = sema(
        "
            fn f(foo: Foo) { foo.g(); }
            class Foo
            impl Foo {
                fn g(): Char { return '1'; }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(2)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_char_with_1_arg() {
    let sa = sema(
        "
            fn f(foo: Foo): Char { return foo.g('1'); }
            class Foo
            impl Foo {
                fn g(a: Char): Char { return '1'; }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
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
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_char_with_3_args() {
    let sa = sema(
        "
            fn f(foo: Foo): Char { return foo.g('1', '2', '3'); }
            class Foo
            impl Foo {
                fn g(a: Char, b: Char, c: Char): Char { return '1'; }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
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
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_int_with_0_args() {
    let sa = sema(
        "
            fn f(foo: Foo): Int32 { return foo.g(); }
            class Foo
            impl Foo {
                fn g(): Int32 { return 1; }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_int_with_0_args_and_unused_result() {
    let sa = sema(
        "
            fn f(foo: Foo) { foo.g(); }
            class Foo
            impl Foo {
                fn g(): Int32 { return 1; }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(2)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_int_with_1_arg() {
    let sa = sema(
        "
            fn f(foo: Foo): Int32 { return foo.g(1i32); }
            class Foo
            impl Foo {
                fn g(a: Int32): Int32 { return 1i32; }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
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
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_int_with_3_args() {
    let sa = sema(
        "
            fn f(foo: Foo): Int32 { return foo.g(1i32, 2i32, 3i32); }
            class Foo
            impl Foo {
                fn g(a: Int32, b: Int32, c: Int32): Int32 { return 1i32; }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
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
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_int64_with_0_args() {
    let sa = sema(
        "
            fn f(foo: Foo): Int64 { return foo.g(); }
            class Foo
            impl Foo {
                fn g(): Int64 { return 1i64; }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_int64_with_0_args_and_unused_result() {
    let sa = sema(
        "
            fn f(foo: Foo) { foo.g(); }
            class Foo
            impl Foo {
                fn g(): Int64 { return 1i64; }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(2)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_int64_with_1_arg() {
    let sa = sema(
        "
            fn f(foo: Foo): Int64 { return foo.g(1i64); }
            class Foo
            impl Foo {
                fn g(a: Int64): Int64 { return 1i64; }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
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
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_int64_with_3_args() {
    let sa = sema(
        "
            fn f(foo: Foo): Int64 { return foo.g(1i64, 2i64, 3i64); }
            class Foo
            impl Foo {
                fn g(a: Int64, b: Int64, c: Int64): Int64 { return 1i64; }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
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
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_float32_with_0_args() {
    let sa = sema(
        "
            fn f(foo: Foo): Float32 { return foo.g(); }
            class Foo
            impl Foo {
                fn g(): Float32 { return 1f32; }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_float32_with_0_args_and_unused_result() {
    let sa = sema(
        "
            fn f(foo: Foo) { foo.g(); }
            class Foo
            impl Foo {
                fn g(): Float32 { return 1f32; }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(2)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_float32_with_1_arg() {
    let sa = sema(
        "
            fn f(foo: Foo): Float32 { return foo.g(1f32); }
            class Foo
            impl Foo {
                fn g(a: Float32): Float32 { return 1f32; }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
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
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_float32_with_3_args() {
    let sa = sema(
        "
            fn f(foo: Foo): Float32 { return foo.g(1f32, 2f32, 3f32); }
            class Foo
            impl Foo {
                fn g(a: Float32, b: Float32, c: Float32): Float32 { return 1f32; }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
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
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_float64_with_0_args() {
    let sa = sema(
        "
            fn f(foo: Foo): Float64 { return foo.g(); }
            class Foo
            impl Foo {
                fn g(): Float64 { return 1f64; }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_float64_with_0_args_and_unused_result() {
    let sa = sema(
        "
            fn f(foo: Foo) { foo.g(); }
            class Foo
            impl Foo {
                fn g(): Float64 { return 1f64; }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(2)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_float64_with_1_arg() {
    let sa = sema(
        "
            fn f(foo: Foo): Float64 { return foo.g(1f64); }
            class Foo
            impl Foo {
                fn g(a: Float64): Float64 { return 1f64; }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
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
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_float64_with_3_args() {
    let sa = sema(
        "
            fn f(foo: Foo): Float64 { return foo.g(1f64, 2f64, 3f64); }
            class Foo
            impl Foo {
                fn g(a: Float64, b: Float64, c: Float64): Float64 { return 1f64; }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
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
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_ptr_with_0_args() {
    let sa = sema(
        "
            fn f(foo: Foo): String { return foo.g(); }
            class Foo
            impl Foo {
                fn g(): String { return \"1\"; }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_ptr_with_0_args_and_unused_result() {
    let sa = sema(
        "
            fn f(foo: Foo) { foo.g(); }
            class Foo
            impl Foo {
                fn g(): String { return \"1\"; }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(2)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_ptr_with_1_arg() {
    let sa = sema(
        "
            fn f(foo: Foo): String { return foo.g(\"1\"); }
            class Foo
            impl Foo {
                fn g(a: String): String { return \"1\"; }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
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
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_method_call_ptr_with_3_args() {
    let sa = sema(
        "
            fn f(foo: Foo): String { return foo.g(\"1\", \"2\", \"3\"); }
            class Foo
            impl Foo {
                fn g(a: String, b: String, c: String): String { return \"1\"; }
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "<prog>::Foo#g");
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
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_new_struct() {
    let sa = sema(
        "
        struct Foo { f1: Int32, f2: Bool }
        fn f(): Foo { Foo(f1 = 10i32, f2 = false) }
    ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let struct_id = resolve_path(&sa, "<prog>::Foo")
        .to_struct()
        .expect("missing struct");
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
        &ConstPoolEntry::Struct(
            StructId(struct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );

    let sa = sema(
        "
        struct Foo[T] { f1: T, f2: Bool }
        fn f[T](val: T): Foo[T] { Foo[T](f1 = val, f2 = false) }
    ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let struct_id = resolve_path(&sa, "<prog>::Foo")
        .to_struct()
        .expect("missing struct");
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
            StructId(struct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::one(BytecodeType::TypeParam(0))
        )
    );
}

#[test]
fn gen_move_struct() {
    let sa = sema(
        "
        struct Foo { f1: Int32, f2: Bool }
        fn f(x: Foo): Foo { let y = x; y }
    ",
    );
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![Mov(r(1), r(0)), Ret(r(1))];
    assert_eq!(expected, code);
}

#[test]
fn gen_struct_field() {
    let sa = sema(
        "
        struct Foo { f1: Int32, f2: Bool }
        fn f(x: Foo): Int32 { x.f1 }
    ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let struct_id = resolve_path(&sa, "<prog>::Foo")
        .to_struct()
        .expect("missing struct");
    let expected = vec![LoadStructField(r(1), r(0), ConstPoolIdx(0)), Ret(r(1))];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::StructField(
            StructId(struct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty(),
            0
        )
    );

    let sa = sema(
        "
        struct Foo[T] { f1: T, f2: Bool }
        fn f(x: Foo[Int32]): Int32 { x.f1 }
    ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let struct_id = resolve_path(&sa, "<prog>::Foo")
        .to_struct()
        .expect("missing struct");
    let expected = vec![LoadStructField(r(1), r(0), ConstPoolIdx(0)), Ret(r(1))];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::StructField(
            StructId(struct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::one(BytecodeType::Int32),
            0
        )
    );
}

#[test]
fn gen_struct_array() {
    let sa = sema(
        "
        struct Foo { f1: Int32, f2: Bool }
        fn f(x: Array[Foo], idx: Int64): Foo { x(idx) }
    ",
    );
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![LoadArray(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);

    let sa = sema(
        "
        struct Foo { f1: Int32, f2: Bool }
        fn f(x: Array[Foo], idx: Int64, value: Foo) { x(idx) = value; }
    ",
    );
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![StoreArray(r(2), r(0), r(1)), Ret(r(3))];
    assert_eq!(expected, code);
}

#[test]
fn gen_new_enum() {
    let sa = sema(
        "
        enum Foo { A(Int32), B }
        fn f(): Foo { Foo::A(10i32) }
    ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let enum_id = resolve_path(&sa, "<prog>::Foo")
        .to_enum()
        .expect("enum expected");
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
            EnumId(enum_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty(),
            0
        )
    );

    let sa = sema(
        "
        enum Foo[T] { A(T), B }
        fn f(): Foo[Int32] { Foo[Int32]::A(10i32) }
    ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let enum_id = resolve_path(&sa, "<prog>::Foo")
        .to_enum()
        .expect("enum expected");
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
            EnumId(enum_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::one(BytecodeType::Int32),
            0
        )
    );

    let sa = sema(
        "
        enum Foo { A(Int32), B }
        fn f(): Foo { Foo::B }
    ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let enum_id = resolve_path(&sa, "<prog>::Foo")
        .to_enum()
        .expect("enum expected");
    let expected = vec![NewEnum(r(0), ConstPoolIdx(0)), Ret(r(0))];
    assert_eq!(expected, code);

    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::EnumVariant(
            EnumId(enum_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty(),
            1
        )
    );

    let sa = sema(
        "
        enum Foo[T] { A(T), B }
        fn f(): Foo[Int32] { Foo[Int32]::B }
    ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let enum_id = resolve_path(&sa, "<prog>::Foo")
        .to_enum()
        .expect("enum expected");
    let expected = vec![NewEnum(r(0), ConstPoolIdx(0)), Ret(r(0))];
    assert_eq!(expected, code);

    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::EnumVariant(
            EnumId(enum_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::one(BytecodeType::Int32),
            1
        )
    );

    let sa = sema(
        "
        enum Foo[T] { A(T), B }
        fn f(): Foo[Int32] { Foo::B[Int32] }
    ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let enum_id = resolve_path(&sa, "<prog>::Foo")
        .to_enum()
        .expect("enum expected");
    let expected = vec![NewEnum(r(0), ConstPoolIdx(0)), Ret(r(0))];
    assert_eq!(expected, code);

    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::EnumVariant(
            EnumId(enum_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::one(BytecodeType::Int32),
            1
        )
    );
}

#[test]
fn gen_new_object() {
    let sa = sema("class Object fn f(): Object { return Object(); }");
    let (fct, code) = bc(&sa, "<prog>::f");

    let cls_id = resolve_path(&sa, "<prog>::Object")
        .to_class()
        .expect("class expected");
    let expected = vec![NewObjectInitialized(r(0), ConstPoolIdx(0)), Ret(r(0))];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Class(
            ClassId(cls_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_new_object_initialized() {
    let sa = sema(
        "
        class Foo { a: Int64, b: Bool }
        fn f(a: Int64, b: Bool): Foo { return Foo(a, b); }",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let cls_id = resolve_path(&sa, "<prog>::Foo")
        .to_class()
        .expect("class expected");
    let expected = vec![
        PushRegister(r(0)),
        PushRegister(r(1)),
        NewObjectInitialized(r(2), ConstPoolIdx(0)),
        Ret(r(2)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Class(
            ClassId(cls_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_position_new_object() {
    let sa = sema("class Object fn f(): Object { return Object(); }");
    let (fct, _code) = bc(&sa, "<prog>::f");
    let result = positions(&fct);
    let expected = vec![(0, 1)];
    assert_eq!(expected, result);
}

#[test]
fn gen_new_array() {
    let sa = sema("fn f(): Array[Int32] { Array[Int32]::new(1i32, 2i32, 3i32) }");
    let (fct, code) = bc(&sa, "<prog>::f");

    let cls_id = resolve_path(&sa, "std::collections::Array")
        .to_class()
        .expect("class expected");
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
        &ConstPoolEntry::Class(
            ClassId(cls_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::one(BytecodeType::Int32)
        )
    );
}

#[test]
fn gen_array_length() {
    let sa = sema("fn f(a: Array[Int32]): Int64 { return a.size(); }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![ArrayLength(r(1), r(0)), Ret(r(1))];
    assert_eq!(expected, code);
}

#[test]
fn gen_position_array_length() {
    let sa = sema("fn f(a: Array[Int32]): Int64 { return a.size(); }");
    let (fct, _code) = bc(&sa, "<prog>::f");
    let result = positions(&fct);
    let expected = vec![(0, 1)];
    assert_eq!(expected, result);
}

#[test]
fn gen_array_length_effect() {
    let sa = sema("fn f(a: Array[Int32]) { a.size(); }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![ArrayLength(r(1), r(0)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_position_array_length_effect() {
    let sa = sema("fn f(a: Array[Int32]) { a.size(); }");
    let (fct, _code) = bc(&sa, "<prog>::f");
    let result = positions(&fct);
    let expected = vec![(0, 1)];
    assert_eq!(expected, result);
}

#[test]
fn gen_load_array_uint8() {
    let sa = sema("fn f(a: Array[UInt8]): UInt8 { return a(0); }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![ConstInt64(r(2), 0), LoadArray(r(1), r(0), r(2)), Ret(r(1))];
    assert_eq!(expected, code);
}

#[test]
fn gen_load_array_bool() {
    let sa = sema("fn f(a: Array[Bool], idx: Int64): Bool { return a(idx); }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![LoadArray(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_load_array_char() {
    let sa = sema("fn f(a: Array[Char], idx: Int64): Char { return a(idx); }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![LoadArray(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_load_array_int32() {
    let sa = sema("fn f(a: Array[Int32], idx: Int64): Int32 { return a(idx); }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![LoadArray(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_load_array_int64() {
    let sa = sema("fn f(a: Array[Int64], idx: Int64): Int64 { return a(idx); }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![LoadArray(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_load_array_float32() {
    let sa = sema("fn f(a: Array[Float32], idx: Int64): Float32 { return a(idx); }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![LoadArray(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_load_array_float64() {
    let sa = sema("fn f(a: Array[Float64], idx: Int64): Float64 { return a(idx); }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![LoadArray(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_load_array_ptr() {
    let sa = sema("class Object fn f(a: Array[Object], idx: Int64): Object { return a(idx); }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![LoadArray(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_position_load_array_bool() {
    let sa = sema("fn f(a: Array[Bool]): Bool { return a(0); }");
    let (fct, _code) = bc(&sa, "<prog>::f");
    let result = positions(&fct);
    let expected = vec![(3, 1)];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_load_array_char() {
    let sa = sema("fn f(a: Array[Char]): Char { return a(0); }");
    let (fct, _code) = bc(&sa, "<prog>::f");
    let result = positions(&fct);
    let expected = vec![(3, 1)];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_load_array_int32() {
    let sa = sema("fn f(a: Array[Int32]): Int32 { return a(0); }");
    let (fct, _code) = bc(&sa, "<prog>::f");
    let result = positions(&fct);
    let expected = vec![(3, 1)];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_load_array_int64() {
    let sa = sema("fn f(a: Array[Int64]): Int64 { return a(0); }");
    let (fct, _code) = bc(&sa, "<prog>::f");
    let result = positions(&fct);
    let expected = vec![(3, 1)];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_load_array_float32() {
    let sa = sema("fn f(a: Array[Float32]): Float32 { return a(0); }");
    let (fct, _code) = bc(&sa, "<prog>::f");
    let result = positions(&fct);
    let expected = vec![(3, 1)];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_load_array_float64() {
    let sa = sema("fn f(a: Array[Float64]): Float64 { return a(0); }");
    let (fct, _code) = bc(&sa, "<prog>::f");
    let result = positions(&fct);
    let expected = vec![(3, 1)];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_load_array_ptr() {
    let sa = sema("class Object fn f(a: Array[Object]): Object { return a(0); }");
    let (fct, _code) = bc(&sa, "<prog>::f");
    let result = positions(&fct);
    let expected = vec![(3, 1)];
    assert_eq!(expected, result);
}

#[test]
fn gen_store_array_uint8() {
    let sa = sema("fn f(a: Array[UInt8], b: UInt8) { a(0) = b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        ConstInt64(Register(2), 0),
        StoreArray(r(1), r(0), r(2)),
        Ret(r(3)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_store_array_bool() {
    let sa = sema("fn f(a: Array[Bool], b: Bool) { a(0) = b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        ConstInt64(Register(2), 0),
        StoreArray(r(1), r(0), r(2)),
        Ret(r(3)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_store_array_char() {
    let sa = sema("fn f(a: Array[Char], b: Char) { a(0) = b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        ConstInt64(Register(2), 0),
        StoreArray(r(1), r(0), r(2)),
        Ret(r(3)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_store_array_int32() {
    let sa = sema("fn f(a: Array[Int32], b: Int32) { a(0) = b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        ConstInt64(Register(2), 0),
        StoreArray(r(1), r(0), r(2)),
        Ret(r(3)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_store_array_int64() {
    let sa = sema("fn f(a: Array[Int64], b: Int64) { a(0) = b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        ConstInt64(Register(2), 0),
        StoreArray(r(1), r(0), r(2)),
        Ret(r(3)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_store_array_float32() {
    let sa = sema("fn f(a: Array[Float32], b: Float32) { a(0) = b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        ConstInt64(Register(2), 0),
        StoreArray(r(1), r(0), r(2)),
        Ret(r(3)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_store_array_float64() {
    let sa = sema("fn f(a: Array[Float64], b: Float64) { a(0) = b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        ConstInt64(Register(2), 0),
        StoreArray(r(1), r(0), r(2)),
        Ret(r(3)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_store_array_ptr() {
    let sa = sema("class Object fn f(a: Array[Object], b: Object) { a(0) = b; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        ConstInt64(Register(2), 0),
        StoreArray(r(1), r(0), r(2)),
        Ret(r(3)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_position_store_array_bool() {
    let sa = sema("fn f(a: Array[Bool], b: Bool) { a(0) = b; }");
    let (fct, _code) = bc(&sa, "<prog>::f");
    let result = positions(&fct);
    let expected = vec![(3, 1)];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_store_array_char() {
    let sa = sema("fn f(a: Array[Char], b: Char) { a(0) = b; }");
    let (fct, _code) = bc(&sa, "<prog>::f");
    let result = positions(&fct);
    let expected = vec![(3, 1)];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_store_array_int32() {
    let sa = sema("fn f(a: Array[Int32], b: Int32) { a(0) = b; }");
    let (fct, _code) = bc(&sa, "<prog>::f");
    let result = positions(&fct);
    let expected = vec![(3, 1)];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_store_array_int64() {
    let sa = sema("fn f(a: Array[Int64], b: Int64) { a(0) = b; }");
    let (fct, _code) = bc(&sa, "<prog>::f");
    let result = positions(&fct);
    let expected = vec![(3, 1)];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_store_array_float32() {
    let sa = sema("fn f(a: Array[Float32], b: Float32) { a(0) = b; }");
    let (fct, _code) = bc(&sa, "<prog>::f");
    let result = positions(&fct);
    let expected = vec![(3, 1)];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_store_array_float64() {
    let sa = sema("fn f(a: Array[Float64], b: Float64) { a(0) = b; }");
    let (fct, _code) = bc(&sa, "<prog>::f");
    let result = positions(&fct);
    let expected = vec![(3, 1)];
    assert_eq!(expected, result);
}

#[test]
fn gen_position_store_array_ptr() {
    let sa = sema("class Object fn f(a: Array[Object], b: Object) { a(0) = b; }");
    let (fct, _code) = bc(&sa, "<prog>::f");
    let result = positions(&fct);
    let expected = vec![(3, 1)];
    assert_eq!(expected, result);
}

#[test]
fn gen_new_object_with_multiple_args() {
    let sa = sema(
        "
            class Foo { a: Int32, b: Int32, c: Int32 }
            fn f(): Foo {
                return Foo(a = 1i32, b = 2i32, c = 3i32);
            }
            ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");
    let cls_id = resolve_path(&sa, "<prog>::Foo")
        .to_class()
        .expect("class expected");
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
        &ConstPoolEntry::Class(
            ClassId(cls_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_position_new_object_with_multiple_args() {
    let sa = sema(
        "
            class Foo { a: Int32, b: Int32, c: Int32 }
            fn f(): Foo {
                return Foo(a = 1i32, b = 2i32, c = 3i32);
            }",
    );
    let (fct, _code) = bc(&sa, "<prog>::f");
    let result = positions(&fct);
    let expected = vec![(15, 4)];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_for_bool() {
    let sa = sema(
        "trait MyId { fn f(): Self; }
            impl MyId for Bool { fn f(): Bool { return self; } }
            ",
    );
    let (_, result) = bc(&sa, "<prog>::MyId for std::primitives::Bool#f");
    let expected = vec![Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_for_uint8() {
    let sa = sema(
        "trait MyId { fn f(): Self; }
            impl MyId for UInt8 { fn f(): UInt8 { return self; } }
            ",
    );
    let (_, result) = bc(&sa, "<prog>::MyId for std::primitives::UInt8#f");
    let expected = vec![Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_for_int32() {
    let sa = sema(
        "trait MyId { fn f(): Self; }
            impl MyId for Int32 { fn f(): Int32 { return self; } }
            ",
    );
    let (_, result) = bc(&sa, "<prog>::MyId for std::primitives::Int32#f");
    let expected = vec![Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_for_int64() {
    let sa = sema(
        "trait MyId { fn f(): Self; }
            impl MyId for Int64 { fn f(): Int64 { return self; } }
            ",
    );
    let (_, result) = bc(&sa, "<prog>::MyId for std::primitives::Int64#f");
    let expected = vec![Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_for_float32() {
    let sa = sema(
        "trait MyId { fn f(): Self; }
            impl MyId for Float32 { fn f(): Float32 { return self; } }
            ",
    );
    let (_, result) = bc(&sa, "<prog>::MyId for std::primitives::Float32#f");
    let expected = vec![Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_for_float64() {
    let sa = sema(
        "trait MyId { fn f(): Self; }
            impl MyId for Float64 { fn f(): Float64 { return self; } }
            ",
    );
    let (_, result) = bc(&sa, "<prog>::MyId for std::primitives::Float64#f");
    let expected = vec![Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_for_string() {
    let sa = sema(
        "trait MyId { fn f(): Self; }
            impl MyId for String { fn f(): String { return self; } }
            ",
    );
    let (_, result) = bc(&sa, "<prog>::MyId for std::string::String#f");
    let expected = vec![Ret(r(0))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_assign_for_bool() {
    let sa = sema(
        "trait MyId { fn f(); }
            impl MyId for Bool { fn f() { let x = self; } }
            ",
    );
    let (_, result) = bc(&sa, "<prog>::MyId for std::primitives::Bool#f");
    let expected = vec![Mov(r(1), r(0)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_assign_for_uint8() {
    let sa = sema(
        "trait MyId { fn f(); }
            impl MyId for UInt8 { fn f() { let x = self; } }
            ",
    );
    let (_, result) = bc(&sa, "<prog>::MyId for std::primitives::UInt8#f");
    let expected = vec![Mov(r(1), r(0)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_assign_for_int32() {
    let sa = sema(
        "trait MyId { fn f(); }
            impl MyId for Int32 { fn f() { let x = self; } }
            ",
    );
    let (_, result) = bc(&sa, "<prog>::MyId for std::primitives::Int32#f");
    let expected = vec![Mov(r(1), r(0)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_assign_for_int64() {
    let sa = sema(
        "trait MyId { fn f(); }
            impl MyId for Int64 { fn f() { let x = self; } }
            ",
    );
    let (_, result) = bc(&sa, "<prog>::MyId for std::primitives::Int64#f");
    let expected = vec![Mov(r(1), r(0)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_assign_for_float32() {
    let sa = sema(
        "trait MyId { fn f(); }
            impl MyId for Float32 { fn f() { let x = self; } }
            ",
    );
    let (_, result) = bc(&sa, "<prog>::MyId for std::primitives::Float32#f");
    let expected = vec![Mov(r(1), r(0)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_assign_for_float64() {
    let sa = sema(
        "trait MyId { fn f(); }
            impl MyId for Float64 { fn f() { let x = self; } }
            ",
    );
    let (_, result) = bc(&sa, "<prog>::MyId for std::primitives::Float64#f");
    let expected = vec![Mov(r(1), r(0)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_self_assign_for_string() {
    let sa = sema(
        "trait MyId { fn f(); }
            impl MyId for String { fn f() { let x = self; } }
            ",
    );
    let (_, result) = bc(&sa, "<prog>::MyId for std::string::String#f");
    let expected = vec![Mov(r(1), r(0)), Ret(r(2))];
    assert_eq!(expected, result);
}

#[test]
fn gen_reinterpret_float32_as_int32() {
    let sa = sema("fn f(a: Float32): Int32 { a.asInt32() }");
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "std::primitives::Float32#asInt32");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_reinterpret_int32_as_float32() {
    let sa = sema("fn f(a: Int32): Float32 { a.asFloat32() }");
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "std::primitives::Int32#asFloat32");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_reinterpret_float64_as_int64() {
    let sa = sema("fn f(a: Float64): Int64 { a.asInt64() }");
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "std::primitives::Float64#asInt64");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_reinterpret_int64_as_float64() {
    let sa = sema("fn f(a: Int64): Float64 { a.asFloat64() }");
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "std::primitives::Int64#asFloat64");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_float32_is_nan() {
    let sa = sema("fn f(a: Float32): Bool { a.isNan() }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestNe(r(1), r(0), r(0)), Ret(r(1))];
    assert_eq!(expected, code);
}

#[test]
fn gen_float64_is_nan() {
    let sa = sema("fn f(a: Float64): Bool { a.isNan() }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![TestNe(r(1), r(0), r(0)), Ret(r(1))];
    assert_eq!(expected, code);
}

#[test]
fn gen_extend_int_to_int64() {
    let sa = sema("fn f(a: Int32): Int64 { a.toInt64() }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_cast_int64_to_int32() {
    let sa = sema("fn f(a: Int64): Int32 { a.toInt32() }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_convert_int32_to_float32() {
    let sa = sema("fn f(a: Int32): Float32 { a.toFloat32() }");
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "std::primitives::Int32#toFloat32");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_convert_int32_to_float64() {
    let sa = sema("fn f(a: Int32): Float64 { a.toFloat64() }");
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "std::primitives::Int32#toFloat64");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_convert_int64_to_float32() {
    let sa = sema("fn f(a: Int64): Float32 { a.toFloat32() }");
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "std::primitives::Int64#toFloat32");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_convert_int64_to_float64() {
    let sa = sema("fn f(a: Int64): Float64 { a.toFloat64() }");
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "std::primitives::Int64#toFloat64");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_truncate_float32_to_int32() {
    let sa = sema("fn f(a: Float32): Int32 { a.toInt32() }");
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "std::primitives::Float32#toInt32");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_truncate_float32_to_int64() {
    let sa = sema("fn f(a: Float32): Int64 { a.toInt64() }");
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "std::primitives::Float32#toInt64");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_truncate_float64_to_int32() {
    let sa = sema("fn f(a: Float64): Int32 { a.toInt32() }");
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "std::primitives::Float64#toInt32");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_truncate_float64_to_int64() {
    let sa = sema("fn f(a: Float64): Int64 { a.toInt64() }");
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "std::primitives::Float64#toInt64");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_enum_value() {
    let sa = sema("enum MyEnum { A, B } fn f(): MyEnum { MyEnum::A }");
    let (fct, code) = bc(&sa, "<prog>::f");

    let enum_id = resolve_path(&sa, "<prog>::MyEnum")
        .to_enum()
        .expect("enum expected");
    let expected = vec![NewEnum(r(0), ConstPoolIdx(0)), Ret(r(0))];
    assert_eq!(expected, code);

    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::EnumVariant(
            EnumId(enum_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty(),
            0
        )
    )
}

#[test]
fn gen_enum_mov_generic() {
    let sa = sema(
        "enum MyEnum { A(Int32), B }
        fn f(x: MyEnum): MyEnum {
            let tmp = x;
            tmp
        }",
    );
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![Mov(r(1), r(0)), Ret(r(1))];
    assert_eq!(expected, code);
}

#[test]
fn gen_unreachable() {
    let sa = sema("fn f(): Int32 { unreachable[Int32]() }");
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "std::unreachable");
    let expected = vec![InvokeStatic(r(0), ConstPoolIdx(0)), Ret(r(0))];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::one(BytecodeType::Int32)
        )
    );
}

#[test]
fn gen_enum_array() {
    let sa = sema(
        "enum MyEnum { A(Int32), B }
        fn f(arr: Array[MyEnum], idx: Int64): MyEnum {
            arr(idx)
        }",
    );
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![LoadArray(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);

    let sa = sema(
        "enum MyEnum { A(Int32), B }
        fn f(arr: Array[MyEnum], idx: Int64, value: MyEnum) {
            arr(idx) = value;
        }",
    );
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![StoreArray(r(2), r(0), r(1)), Ret(r(3))];
    assert_eq!(expected, code);
}

#[test]
fn gen_string_length() {
    let sa = sema("fn f(x: String): Int64 { x.size() }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![ArrayLength(r(1), r(0)), Ret(r(1))];
    assert_eq!(expected, code);
}

#[test]
fn gen_string_get_uint8() {
    let sa = sema("fn f(x: String, idx: Int64): UInt8 { x.getByte(idx) }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![LoadArray(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_array_get() {
    let sa = sema("fn f(x: Array[Float32], idx: Int64): Float32 { x(idx) }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![LoadArray(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_array_get_method() {
    let sa = sema(
        "use std::traits::IndexGet;
        fn f(x: Array[Float32], idx: Int64): Float32 { x.get(idx) }",
    );
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![LoadArray(r(2), r(0), r(1)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_array_set_method() {
    let sa = sema(
        "
        use std::traits::IndexSet;
        fn f(x: Array[Float32], idx: Int64, value: Float32) { x.set(idx, value); }",
    );
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![StoreArray(r(2), r(0), r(1)), Ret(r(3))];
    assert_eq!(expected, code);
}

#[test]
fn gen_string_concat() {
    let sa = sema("fn f(a: String, b: String): String { a + b }");
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "std::traits::Add for std::string::String#add");
    let expected = vec![
        PushRegister(r(0)),
        PushRegister(r(1)),
        InvokeDirect(r(2), ConstPoolIdx(0)),
        Ret(r(2)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_string_equals() {
    let sa = sema("fn f(a: String, b: String): Bool { a != b }");
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "std::traits::Equals for std::string::String#equals");
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
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_bool_to_string() {
    let sa = sema(
        "
        use std::string::Stringable;
        fn f(a: Bool): String { a.toString() }
    ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(
        &sa,
        "std::string::Stringable for std::primitives::Bool#toString",
    );
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_cmp_strings() {
    let sa = sema("fn f(a: String, b: String): Bool { a < b }");
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "std::traits::Comparable for std::string::String#cmp");
    let expected = vec![
        PushRegister(r(0)),
        PushRegister(r(1)),
        InvokeDirect(r(3), ConstPoolIdx(0)),
        PushRegister(r(3)),
        InvokeDirect(r(2), ConstPoolIdx(1)),
        Ret(r(2)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_extend_uint8() {
    let sa = sema("fn f(x: UInt8): Int32 { x.toInt32() }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);

    let sa = sema("fn f(x: UInt8): Int64 { x.toInt64() }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_extend_int() {
    let sa = sema("fn f(x: Int32): Int64 { x.toInt64() }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_cast_char() {
    let sa = sema("fn f(x: Char): Int32 { x.toInt32() }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);

    let sa = sema("fn f(x: Char): Int64 { x.toInt64() }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_cast_int() {
    let sa = sema("fn f(x: Int32): UInt8 { x.toUInt8() }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);

    let sa = sema("fn f(x: Int32): Char { x.toCharUnchecked() }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_cast_int64() {
    let sa = sema("fn f(x: Int64): UInt8 { x.toUInt8() }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);

    let sa = sema("fn f(x: Int64): Char { x.toCharUnchecked() }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);

    let sa = sema("fn f(x: Int64): Int32 { x.toInt32() }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_const_int32() {
    let sa = sema("const X: Int32 = 1i32; fn f(): Int32 { X }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![LoadConst(r(0), ConstId(0)), Ret(r(0))];
    assert_eq!(expected, code);
}

#[test]
fn gen_while_with_break() {
    let sa = sema("fn f(x: Bool) { while x { break; } }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        LoopStart,
        JumpIfFalse(r(0), 4),
        Jump(4),
        JumpLoop(0),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_vec_load() {
    let sa = sema("fn f(x: Vec[Int32], idx: Int64): Int32 { x(idx) }");
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "std::traits::IndexGet for std::collections::Vec#get");
    let expected = vec![
        PushRegister(r(0)),
        PushRegister(r(1)),
        InvokeDirect(r(2), ConstPoolIdx(0)),
        Ret(r(2)),
    ];
    assert_eq!(expected, code);
    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::one(BytecodeType::Int32)
        )
    );
}

#[test]
fn gen_vec_store() {
    let sa = sema("fn f(x: Vec[Int32], idx: Int64, value: Int32) { x(idx) = value; }");
    let (fct, code) = bc(&sa, "<prog>::f");

    let fct_id = lookup_fct(&sa, "std::traits::IndexSet for std::collections::Vec#set");
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
        &ConstPoolEntry::Fct(
            FunctionId(fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::one(BytecodeType::Int32)
        )
    );
}

#[test]
fn gen_byte_to_char() {
    let sa = sema("fn f(x: UInt8): Char { x.toChar() }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![
        PushRegister(r(0)),
        InvokeDirect(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_int32_min_value() {
    let sa = sema("fn f(): Int32 { -2147483648i32 }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![ConstInt32(r(0), -2147483648), Ret(r(0))];
    assert_eq!(expected, code);
}

#[test]
fn gen_int32_max_value() {
    let sa = sema("fn f(): Int32 { 2147483647i32 }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![ConstInt32(r(0), 2147483647), Ret(r(0))];
    assert_eq!(expected, code);
}

#[test]
fn gen_int64_min_value() {
    let sa = sema("fn f(): Int64 { -9223372036854775808i64 }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![ConstInt64(r(0), -9223372036854775808), Ret(r(0))];
    assert_eq!(expected, code);
}

#[test]
fn gen_int64_max_value() {
    let sa = sema("fn f(): Int64 { 9223372036854775807i64 }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![ConstInt64(r(0), 9223372036854775807), Ret(r(0))];
    assert_eq!(expected, code);
}

#[test]
fn gen_tuple_var() {
    let sa = sema("fn f() { let x = (1i32, 2i32); }");
    let (fct, code) = bc(&sa, "<prog>::f");

    let subtypes = vec![BytecodeType::Int32, BytecodeType::Int32];
    let expected = vec![
        ConstInt32(r(2), 1),
        ConstInt32(r(3), 2),
        PushRegister(r(2)),
        PushRegister(r(3)),
        NewTuple(r(1), ConstPoolIdx(2)),
        Mov(r(0), r(1)),
        Ret(r(4)),
    ];
    assert_eq!(expected, code);

    assert_eq!(
        fct.const_pool(ConstPoolIdx(2)),
        &ConstPoolEntry::Tuple(BytecodeTypeArray::new(subtypes))
    );
}

#[test]
fn gen_tuple_move() {
    let sa = sema("fn f(x: (Int32, Int32)) { let y = x; }");
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![Mov(r(1), r(0)), Ret(r(2))];
    assert_eq!(expected, code);
}

#[test]
fn gen_tuple_element() {
    let sa = sema("fn f(x: (Int32, Int32)): Int32 { x.0 }");
    let (fct, code) = bc(&sa, "<prog>::f");

    let tuple_ty = BytecodeType::Tuple(BytecodeTypeArray::new(vec![
        BytecodeType::Int32,
        BytecodeType::Int32,
    ]));
    let expected = vec![LoadTupleElement(r(1), r(0), ConstPoolIdx(0)), Ret(r(1))];
    assert_eq!(expected, code);

    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::TupleElement(tuple_ty, 0)
    );
}

#[test]
fn gen_trait_object() {
    let sa = sema(
        "
        trait Foo { fn bar(): Int32; }
        class Bar
        impl Foo for Bar {
            fn bar(): Int32 { 1i32 }
        }
        fn f(x: Bar): Foo { x as Foo }
    ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let trait_id = resolve_path(&sa, "<prog>::Foo")
        .to_trait()
        .expect("trait expected");
    let cls_id = resolve_path(&sa, "<prog>::Bar")
        .to_class()
        .expect("class expected");
    let object_ty = BytecodeType::Class(
        ClassId(cls_id.index().try_into().expect("overflow")),
        BytecodeTypeArray::empty(),
    );
    let expected = vec![NewTraitObject(r(1), ConstPoolIdx(0), r(0)), Ret(r(1))];
    assert_eq!(expected, code);

    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::TraitObject {
            trait_ty: BytecodeType::TraitObject(
                TraitId(trait_id.index().try_into().expect("overflow")),
                BytecodeTypeArray::empty(),
                BytecodeTypeArray::empty()
            ),
            actual_object_ty: object_ty
        }
    );
}

#[test]
fn gen_trait_object_copy() {
    let sa = sema(
        "
        trait Foo { fn bar(): Int32; }
        fn f(x: Foo): Foo { let y = x; y }
    ",
    );
    let (_, code) = bc(&sa, "<prog>::f");
    let expected = vec![Mov(r(1), r(0)), Ret(r(1))];
    assert_eq!(expected, code);
}

#[test]
fn gen_trait_object_method_call() {
    let sa = sema(
        "
        trait Foo { fn bar(): Int32; }
        fn f(x: Foo): Int32 { x.bar() }
    ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let trait_id = resolve_path(&sa, "<prog>::Foo")
        .to_trait()
        .expect("trait expected");
    let fct_id = lookup_fct(&sa, "<prog>::Foo#bar");
    let expected = vec![
        PushRegister(r(0)),
        InvokeVirtual(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);

    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::TraitObjectMethod(
            BytecodeType::TraitObject(
                TraitId(trait_id.index().try_into().expect("overflow")),
                BytecodeTypeArray::empty(),
                BytecodeTypeArray::empty()
            ),
            FunctionId(fct_id.index().try_into().expect("overflow")),
        )
    );
}

#[test]
fn gen_new_lambda() {
    let sa = sema(
        "
        fn f(): (): Int32 {
            ||: Int32 { 12 }
        }
    ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let expected = vec![NewLambda(r(0), ConstPoolIdx(0)), Ret(r(0))];
    assert_eq!(expected, code);

    assert!(fct.const_pool(ConstPoolIdx(0)).is_fct());
}

#[test]
fn gen_context_allocated_var() {
    let sa = sema(
        "
        fn f(): (): Int64 {
            let mut x = 10;
            x = 11;
            let y = x;
            ||: Int64 { x }
        }
    ",
    );
    let (_, code) = bc(&sa, "<prog>::f");

    let expected = vec![
        NewObject(r(0), ConstPoolIdx(0)),
        ConstInt64(r(1), 10),
        StoreField(r(1), r(0), ConstPoolIdx(2)),
        ConstInt64(r(1), 11),
        StoreField(r(1), r(0), ConstPoolIdx(4)),
        LoadField(r(3), r(0), ConstPoolIdx(5)),
        Mov(r(1), r(3)),
        PushRegister(r(0)),
        NewLambda(r(4), ConstPoolIdx(6)),
        Ret(r(4)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_invoke_lambda() {
    let sa = sema(
        "
        fn f(x: (): Int32): Int32 {
            x()
        }
    ",
    );
    let (_, code) = bc(&sa, "<prog>::f");

    let expected = vec![
        PushRegister(r(0)),
        InvokeLambda(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);

    let sa = sema(
        "
        fn f(x: (): ()) {
            x()
        }
    ",
    );
    let (_, code) = bc(&sa, "<prog>::f");

    let expected = vec![
        PushRegister(r(0)),
        InvokeLambda(r(1), ConstPoolIdx(0)),
        Ret(r(1)),
    ];
    assert_eq!(expected, code);
}

#[test]
fn gen_comparable_trait() {
    let sa = sema(
        "
        use std::traits::{Comparable, Ordering};
        class X
        impl Comparable for X {
            fn cmp(rhs: X): Ordering { Ordering::Less }
        }
        fn f(a: X, b: X): Bool {
            a > b
        }
    ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let expected = vec![
        PushRegister(r(0)),
        PushRegister(r(1)),
        InvokeDirect(r(3), ConstPoolIdx(0)),
        PushRegister(r(3)),
        InvokeDirect(r(2), ConstPoolIdx(1)),
        Ret(r(2)),
    ];
    assert_eq!(expected, code);

    let cmp_fct_id = lookup_fct(&sa, "std::traits::Comparable for <prog>::X#cmp");

    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Fct(
            FunctionId(cmp_fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty()
        )
    );

    assert_eq!(
        fct.const_pool(ConstPoolIdx(1)),
        &ConstPoolEntry::Fct(
            FunctionId(
                sa.known
                    .functions
                    .ordering_is_gt()
                    .index()
                    .try_into()
                    .expect("overflow")
            ),
            BytecodeTypeArray::empty()
        )
    );
}

#[test]
fn gen_comparable_trait_generic() {
    let sa = sema(
        "
        fn f[T: std::traits::Comparable](a: T, b: T): Bool {
            a < b
        }
    ",
    );
    let (fct, code) = bc(&sa, "<prog>::f");

    let expected = vec![
        PushRegister(r(0)),
        PushRegister(r(1)),
        InvokeGenericDirect(r(3), ConstPoolIdx(0)),
        PushRegister(r(3)),
        InvokeDirect(r(2), ConstPoolIdx(1)),
        Ret(r(2)),
    ];
    assert_eq!(expected, code);

    let trait_id = sa.known.traits.comparable();
    let trait_ = sa.trait_(trait_id);
    let name = sa.interner.intern("cmp");
    let cmp_fct_id = trait_.get_method(name, false).expect("missing fct");

    assert_eq!(
        fct.const_pool(ConstPoolIdx(0)),
        &ConstPoolEntry::Generic(
            0,
            FunctionId(cmp_fct_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty(),
            BytecodeTypeArray::empty()
        )
    );

    assert_eq!(
        fct.const_pool(ConstPoolIdx(1)),
        &ConstPoolEntry::Fct(
            FunctionId(
                sa.known
                    .functions
                    .ordering_is_lt()
                    .index()
                    .try_into()
                    .expect("overflow")
            ),
            BytecodeTypeArray::empty()
        )
    );
}

pub fn field_by_name(
    sa: &Sema,
    class_name: &'static str,
    field_name: &'static str,
) -> (ClassDefinitionId, FieldIndex) {
    let cls_id = resolve_path(sa, class_name)
        .to_class()
        .expect("class expected");
    let cls = &sa.classes[cls_id];
    let field_name = sa.interner.intern(field_name);
    let field_id = cls.field_by_name(sa, field_name);

    (cls_id, field_id)
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
    LoadEnumVariant(Register, Register, ConstPoolIdx),

    LoadField(Register, Register, ConstPoolIdx),
    StoreField(Register, Register, ConstPoolIdx),

    LoadGlobal(Register, GlobalId),
    StoreGlobal(Register, GlobalId),
    LoadConst(Register, ConstId),

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

    Ret(Register),
}

pub(crate) fn build(bc: &BytecodeFunction) -> Vec<Bytecode> {
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
                Bytecode::Jump(target) => *target = idx,
                Bytecode::JumpIfFalse(_, target) => *target = idx,
                Bytecode::JumpIfTrue(_, target) => *target = idx,
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

    fn visit_load_enum_variant(&mut self, dest: Register, src: Register, idx: ConstPoolIdx) {
        self.emit(Bytecode::LoadEnumVariant(dest, src, idx));
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

    fn visit_load_global(&mut self, dest: Register, global_id: GlobalId) {
        self.emit(Bytecode::LoadGlobal(dest, global_id));
    }

    fn visit_store_global(&mut self, src: Register, global_id: GlobalId) {
        self.emit(Bytecode::StoreGlobal(src, global_id));
    }

    fn visit_load_const(&mut self, dest: Register, const_id: dora_bytecode::ConstId) {
        self.emit(Bytecode::LoadConst(dest, const_id))
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
    fn visit_jump_if_true(&mut self, opnd: Register, offset: u32) {
        let offset = BytecodeOffset(self.pc.to_u32() + offset);
        self.jumps.push((self.next_idx - 1, offset));
        self.emit(Bytecode::JumpIfTrue(opnd, 0));
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
    fn visit_new_array(&mut self, dest: Register, length: Register, idx: ConstPoolIdx) {
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
    fn visit_new_trait_object(&mut self, dest: Register, src: Register, idx: ConstPoolIdx) {
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

    fn visit_ret(&mut self, opnd: Register) {
        self.emit(Bytecode::Ret(opnd));
    }
}
