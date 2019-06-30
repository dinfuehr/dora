use std::collections::hash_map::HashMap;
use std::convert::From;
use std::fmt;
use std::mem;

use crate::mem as cratemem;

use crate::bytecode::opcode::Bytecode;
use crate::class::{ClassDefId, FieldId};
use crate::ty::{BuiltinType, MachineMode};
use crate::vm::{FctId, GlobalId};

#[derive(Copy, Clone, PartialEq, Debug)]
pub struct Register(pub usize);

impl Register {
    pub fn invalid() -> Register {
        Register(usize::max_value())
    }

    pub fn zero() -> Register {
        Register(0)
    }

    pub fn is_invalid(&self) -> bool {
        self.0 == usize::max_value()
    }

    pub fn offset(&self, value: usize) -> Register {
        Register(self.0 + value)
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "r{}", self.0)
    }
}

#[derive(Copy, Clone, PartialEq, Debug, Eq, Hash)]
pub struct Label(pub usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct BytecodeIdx(pub usize);

impl BytecodeIdx {
    fn invalid() -> BytecodeIdx {
        BytecodeIdx(usize::max_value())
    }

    fn is_invalid(&self) -> bool {
        self.0 == usize::max_value()
    }
}

impl fmt::Display for BytecodeIdx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "bc#{}", self.0)
    }
}

#[derive(Copy, Clone)]
pub enum BytecodeType {
    Bool,
    Byte,
    Char,
    Int,
    Long,
    Float,
    Double,
    Ptr,
}

impl BytecodeType {
    pub fn size(&self) -> i32 {
        match self {
            BytecodeType::Bool => 1,
            BytecodeType::Byte => 1,
            BytecodeType::Char => 4,
            BytecodeType::Int => 4,
            BytecodeType::Long => 8,
            BytecodeType::Float => 4,
            BytecodeType::Double => 8,
            BytecodeType::Ptr => cratemem::ptr_width(),
        }
    }

    pub fn mode(&self) -> MachineMode {
        match self {
            BytecodeType::Bool => MachineMode::Int8,
            BytecodeType::Byte => MachineMode::Int8,
            BytecodeType::Char => MachineMode::Int32,
            BytecodeType::Int => MachineMode::Int32,
            BytecodeType::Long => MachineMode::Int64,
            BytecodeType::Float => MachineMode::Float32,
            BytecodeType::Double => MachineMode::Float64,
            BytecodeType::Ptr => MachineMode::Ptr,
        }
    }
}

impl From<BuiltinType> for BytecodeType {
    fn from(ty: BuiltinType) -> Self {
        match ty {
            BuiltinType::Bool => BytecodeType::Bool,
            BuiltinType::Byte => BytecodeType::Byte,
            BuiltinType::Char => BytecodeType::Char,
            BuiltinType::Int => BytecodeType::Int,
            BuiltinType::Long => BytecodeType::Long,
            BuiltinType::Float => BytecodeType::Float,
            BuiltinType::Double => BytecodeType::Double,
            BuiltinType::Class(_, _) => BytecodeType::Ptr,
            _ => panic!("BuiltinType cannot converted to BytecodeType"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct StrConstPoolIdx(pub usize);

impl fmt::Display for StrConstPoolIdx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "str_idx#{}", self.0)
    }
}

pub struct BytecodeGenerator {
    code: Vec<Bytecode>,
    labels: Vec<Option<BytecodeIdx>>,
    unresolved_jumps: Vec<(BytecodeIdx, Label)>,
    registers: Vec<BytecodeType>,
    string_pool_map: HashMap<String, StrConstPoolIdx>,
}

impl BytecodeGenerator {
    pub fn new() -> BytecodeGenerator {
        BytecodeGenerator {
            code: Vec::new(),
            labels: Vec::new(),
            unresolved_jumps: Vec::new(),
            registers: Vec::new(),
            string_pool_map: HashMap::new(),
        }
    }

    pub fn add_register(&mut self, ty: BytecodeType) -> Register {
        self.registers.push(ty);
        Register(self.registers.len() - 1)
    }

    pub fn add_register_chain(&mut self, types: &[BytecodeType]) -> Register {
        assert!(types.len() > 0);
        let start = Register(self.registers.len());

        for &ty in types {
            self.registers.push(ty);
        }

        start
    }

    pub fn create_label(&mut self) -> Label {
        self.labels.push(None);
        Label(self.labels.len() - 1)
    }

    pub fn define_label(&mut self) -> Label {
        let dest = BytecodeIdx(self.code.len());
        self.labels.push(Some(dest));
        Label(self.labels.len() - 1)
    }

    pub fn bind_label(&mut self, lbl: Label) {
        assert!(self.labels[lbl.0].is_none(), "bind label twice");
        let dest = BytecodeIdx(self.code.len());
        self.labels[lbl.0] = Some(dest);
    }

    fn dest_label(&self, lbl: Label) -> Option<BytecodeIdx> {
        self.labels[lbl.0]
    }

    fn pc(&self) -> BytecodeIdx {
        BytecodeIdx(self.code.len())
    }

    pub fn emit_add_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::AddInt(dest, lhs, rhs));
    }

    pub fn emit_add_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::AddLong(dest, lhs, rhs));
    }

    pub fn emit_add_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::AddFloat(dest, lhs, rhs));
    }

    pub fn emit_add_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::AddDouble(dest, lhs, rhs));
    }

    pub fn emit_and_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::AndInt(dest, lhs, rhs));
    }

    pub fn emit_or_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::OrInt(dest, lhs, rhs));
    }

    pub fn emit_xor_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::XorInt(dest, lhs, rhs));
    }

    pub fn emit_div_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::DivInt(dest, lhs, rhs));
    }

    pub fn emit_load_field_bool(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.code
            .push(Bytecode::LoadFieldBool(dest, obj, cls, field));
    }

    pub fn emit_load_field_byte(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.code
            .push(Bytecode::LoadFieldByte(dest, obj, cls, field));
    }

    pub fn emit_load_field_char(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.code
            .push(Bytecode::LoadFieldChar(dest, obj, cls, field));
    }

    pub fn emit_load_field_int(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.code
            .push(Bytecode::LoadFieldInt(dest, obj, cls, field));
    }

    pub fn emit_load_field_long(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.code
            .push(Bytecode::LoadFieldLong(dest, obj, cls, field));
    }

    pub fn emit_load_field_float(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.code
            .push(Bytecode::LoadFieldFloat(dest, obj, cls, field));
    }

    pub fn emit_load_field_double(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.code
            .push(Bytecode::LoadFieldDouble(dest, obj, cls, field));
    }

    pub fn emit_load_field_ptr(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.code
            .push(Bytecode::LoadFieldPtr(dest, obj, cls, field));
    }

    pub fn emit_const_nil(&mut self, dest: Register) {
        self.code.push(Bytecode::ConstNil(dest));
    }

    pub fn emit_const_char(&mut self, dest: Register, value: char) {
        self.code.push(Bytecode::ConstChar(dest, value));
    }

    pub fn emit_const_byte(&mut self, dest: Register, value: u8) {
        self.code.push(Bytecode::ConstByte(dest, value));
    }

    pub fn emit_const_int(&mut self, dest: Register, value: u32) {
        self.code.push(Bytecode::ConstInt(dest, value));
    }

    pub fn emit_const_long(&mut self, dest: Register, value: u64) {
        self.code.push(Bytecode::ConstLong(dest, value));
    }

    pub fn emit_const_float(&mut self, dest: Register, value: f32) {
        self.code.push(Bytecode::ConstFloat(dest, value));
    }

    pub fn emit_const_double(&mut self, dest: Register, value: f64) {
        self.code.push(Bytecode::ConstDouble(dest, value));
    }

    pub fn add_string_const_pool(&mut self, value: String) -> StrConstPoolIdx {
        match self.string_pool_map.get(&value) {
            Some(index) => index.clone(),
            None => {
                let index = StrConstPoolIdx(self.string_pool_map.len());
                self.string_pool_map.insert(value.clone(), index);
                index
            }
        }
    }

    pub fn emit_const_string(&mut self, dest: Register, index: StrConstPoolIdx) {
        self.code.push(Bytecode::ConstString(dest, index));
    }

    pub fn emit_const_zero_byte(&mut self, dest: Register) {
        self.code.push(Bytecode::ConstZeroByte(dest));
    }

    pub fn emit_const_zero_int(&mut self, dest: Register) {
        self.code.push(Bytecode::ConstZeroInt(dest));
    }

    pub fn emit_const_zero_long(&mut self, dest: Register) {
        self.code.push(Bytecode::ConstZeroLong(dest));
    }

    pub fn emit_const_zero_float(&mut self, dest: Register) {
        self.code.push(Bytecode::ConstZeroFloat(dest));
    }

    pub fn emit_const_zero_double(&mut self, dest: Register) {
        self.code.push(Bytecode::ConstZeroDouble(dest));
    }

    pub fn emit_const_true(&mut self, dest: Register) {
        self.code.push(Bytecode::ConstTrue(dest));
    }

    pub fn emit_const_false(&mut self, dest: Register) {
        self.code.push(Bytecode::ConstFalse(dest));
    }

    pub fn emit_not_bool(&mut self, dest: Register, src: Register) {
        self.code.push(Bytecode::NotBool(dest, src));
    }

    pub fn emit_jump_if_false(&mut self, opnd: Register, lbl: Label) {
        if let Some(idx) = self.dest_label(lbl) {
            self.code.push(Bytecode::JumpIfFalse(opnd, idx));
        } else {
            self.unresolved_jumps.push((self.pc(), lbl));
            self.code
                .push(Bytecode::JumpIfFalse(opnd, BytecodeIdx::invalid()));
        }
    }

    pub fn emit_jump_if_true(&mut self, opnd: Register, lbl: Label) {
        if let Some(idx) = self.dest_label(lbl) {
            self.code.push(Bytecode::JumpIfTrue(opnd, idx));
        } else {
            self.unresolved_jumps.push((self.pc(), lbl));
            self.code
                .push(Bytecode::JumpIfTrue(opnd, BytecodeIdx::invalid()));
        }
    }

    pub fn emit_jump(&mut self, lbl: Label) {
        if let Some(idx) = self.dest_label(lbl) {
            self.code.push(Bytecode::Jump(idx));
        } else {
            self.unresolved_jumps.push((self.pc(), lbl));
            self.code.push(Bytecode::Jump(BytecodeIdx::invalid()));
        }
    }

    pub fn emit_mod_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::ModInt(dest, lhs, rhs));
    }

    pub fn emit_mul_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::MulInt(dest, lhs, rhs));
    }

    pub fn emit_neg_int(&mut self, dest: Register, src: Register) {
        self.code.push(Bytecode::NegInt(dest, src));
    }

    pub fn emit_neg_long(&mut self, dest: Register, src: Register) {
        self.code.push(Bytecode::NegLong(dest, src));
    }

    pub fn emit_shl_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::ShlInt(dest, lhs, rhs));
    }

    pub fn emit_shr_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::ShrInt(dest, lhs, rhs));
    }

    pub fn emit_sar_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::SarInt(dest, lhs, rhs));
    }

    pub fn emit_sub_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::SubInt(dest, lhs, rhs));
    }

    pub fn emit_mov_bool(&mut self, dest: Register, src: Register) {
        self.code.push(Bytecode::MovBool(dest, src));
    }

    pub fn emit_mov_byte(&mut self, dest: Register, src: Register) {
        self.code.push(Bytecode::MovByte(dest, src));
    }

    pub fn emit_mov_char(&mut self, dest: Register, src: Register) {
        self.code.push(Bytecode::MovChar(dest, src));
    }

    pub fn emit_mov_int(&mut self, dest: Register, src: Register) {
        self.code.push(Bytecode::MovInt(dest, src));
    }

    pub fn emit_mov_long(&mut self, dest: Register, src: Register) {
        self.code.push(Bytecode::MovLong(dest, src));
    }

    pub fn emit_mov_float(&mut self, dest: Register, src: Register) {
        self.code.push(Bytecode::MovFloat(dest, src));
    }

    pub fn emit_mov_double(&mut self, dest: Register, src: Register) {
        self.code.push(Bytecode::MovDouble(dest, src));
    }

    pub fn emit_mov_ptr(&mut self, dest: Register, src: Register) {
        self.code.push(Bytecode::MovPtr(dest, src));
    }

    pub fn emit_mov_self(&mut self, dest: Register, src: Register) {
        self.code.push(Bytecode::MovPtr(dest, src));
    }

    pub fn emit_ret_bool(&mut self, src: Register) {
        self.code.push(Bytecode::RetBool(src));
    }

    pub fn emit_ret_byte(&mut self, src: Register) {
        self.code.push(Bytecode::RetByte(src));
    }

    pub fn emit_ret_char(&mut self, src: Register) {
        self.code.push(Bytecode::RetChar(src));
    }

    pub fn emit_ret_int(&mut self, src: Register) {
        self.code.push(Bytecode::RetInt(src));
    }

    pub fn emit_ret_long(&mut self, src: Register) {
        self.code.push(Bytecode::RetLong(src));
    }

    pub fn emit_ret_float(&mut self, src: Register) {
        self.code.push(Bytecode::RetFloat(src));
    }

    pub fn emit_ret_double(&mut self, src: Register) {
        self.code.push(Bytecode::RetDouble(src));
    }

    pub fn emit_ret_ptr(&mut self, src: Register) {
        self.code.push(Bytecode::RetPtr(src));
    }

    pub fn emit_ret_void(&mut self) {
        self.code.push(Bytecode::RetVoid);
    }

    pub fn emit_test_eq_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::TestEqInt(dest, lhs, rhs));
    }

    pub fn emit_test_eq_ptr(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::TestEqPtr(dest, lhs, rhs));
    }

    pub fn emit_test_ne_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::TestNeInt(dest, lhs, rhs));
    }

    pub fn emit_test_ne_ptr(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::TestNePtr(dest, lhs, rhs));
    }

    pub fn emit_test_gt_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::TestGtInt(dest, lhs, rhs));
    }

    pub fn emit_test_ge_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::TestGeInt(dest, lhs, rhs));
    }

    pub fn emit_test_lt_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::TestLtInt(dest, lhs, rhs));
    }

    pub fn emit_test_le_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::TestLeInt(dest, lhs, rhs));
    }

    pub fn emit_load_global_bool(&mut self, dest: Register, gid: GlobalId) {
        self.code.push(Bytecode::LoadGlobalBool(dest, gid));
    }

    pub fn emit_load_global_byte(&mut self, dest: Register, gid: GlobalId) {
        self.code.push(Bytecode::LoadGlobalByte(dest, gid));
    }

    pub fn emit_load_global_char(&mut self, dest: Register, gid: GlobalId) {
        self.code.push(Bytecode::LoadGlobalChar(dest, gid));
    }

    pub fn emit_load_global_int(&mut self, dest: Register, gid: GlobalId) {
        self.code.push(Bytecode::LoadGlobalInt(dest, gid));
    }

    pub fn emit_load_global_long(&mut self, dest: Register, gid: GlobalId) {
        self.code.push(Bytecode::LoadGlobalLong(dest, gid));
    }

    pub fn emit_load_global_float(&mut self, dest: Register, gid: GlobalId) {
        self.code.push(Bytecode::LoadGlobalFloat(dest, gid));
    }

    pub fn emit_load_global_double(&mut self, dest: Register, gid: GlobalId) {
        self.code.push(Bytecode::LoadGlobalDouble(dest, gid));
    }

    pub fn emit_load_global_ptr(&mut self, dest: Register, gid: GlobalId) {
        self.code.push(Bytecode::LoadGlobalPtr(dest, gid));
    }

    pub fn emit_invoke_direct_void(&mut self, fid: FctId, start: Register, num: usize) {
        self.code.push(Bytecode::InvokeDirectVoid(fid, start, num));
    }

    pub fn emit_invoke_direct_bool(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.code
            .push(Bytecode::InvokeDirectBool(dest, fid, start, num));
    }

    pub fn emit_invoke_direct_byte(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.code
            .push(Bytecode::InvokeDirectByte(dest, fid, start, num));
    }

    pub fn emit_invoke_direct_char(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.code
            .push(Bytecode::InvokeDirectChar(dest, fid, start, num));
    }

    pub fn emit_invoke_direct_int(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.code
            .push(Bytecode::InvokeDirectInt(dest, fid, start, num));
    }

    pub fn emit_invoke_direct_long(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.code
            .push(Bytecode::InvokeDirectLong(dest, fid, start, num));
    }

    pub fn emit_invoke_direct_float(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.code
            .push(Bytecode::InvokeDirectFloat(dest, fid, start, num));
    }

    pub fn emit_invoke_direct_double(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.code
            .push(Bytecode::InvokeDirectDouble(dest, fid, start, num));
    }

    pub fn emit_invoke_direct_ptr(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.code
            .push(Bytecode::InvokeDirectPtr(dest, fid, start, num));
    }

    pub fn emit_invoke_virtual_void(&mut self, fid: FctId, start: Register, num: usize) {
        self.code.push(Bytecode::InvokeVirtualVoid(fid, start, num));
    }

    pub fn emit_invoke_virtual_bool(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.code
            .push(Bytecode::InvokeVirtualBool(dest, fid, start, num));
    }

    pub fn emit_invoke_virtual_byte(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.code
            .push(Bytecode::InvokeVirtualByte(dest, fid, start, num));
    }

    pub fn emit_invoke_virtual_char(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.code
            .push(Bytecode::InvokeVirtualChar(dest, fid, start, num));
    }

    pub fn emit_invoke_virtual_int(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.code
            .push(Bytecode::InvokeVirtualInt(dest, fid, start, num));
    }

    pub fn emit_invoke_virtual_long(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.code
            .push(Bytecode::InvokeVirtualLong(dest, fid, start, num));
    }

    pub fn emit_invoke_virtual_float(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.code
            .push(Bytecode::InvokeVirtualFloat(dest, fid, start, num));
    }

    pub fn emit_invoke_virtual_double(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.code
            .push(Bytecode::InvokeVirtualDouble(dest, fid, start, num));
    }

    pub fn emit_invoke_virtual_ptr(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.code
            .push(Bytecode::InvokeVirtualPtr(dest, fid, start, num));
    }

    pub fn emit_invoke_static_void(&mut self, fid: FctId, start: Register, num: usize) {
        self.code.push(Bytecode::InvokeStaticVoid(fid, start, num));
    }

    pub fn emit_invoke_static_bool(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.code
            .push(Bytecode::InvokeStaticBool(dest, fid, start, num));
    }

    pub fn emit_invoke_static_byte(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.code
            .push(Bytecode::InvokeStaticByte(dest, fid, start, num));
    }

    pub fn emit_invoke_static_char(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.code
            .push(Bytecode::InvokeStaticChar(dest, fid, start, num));
    }

    pub fn emit_invoke_static_int(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.code
            .push(Bytecode::InvokeStaticInt(dest, fid, start, num));
    }

    pub fn emit_invoke_static_long(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.code
            .push(Bytecode::InvokeStaticLong(dest, fid, start, num));
    }

    pub fn emit_invoke_static_float(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.code
            .push(Bytecode::InvokeStaticFloat(dest, fid, start, num));
    }

    pub fn emit_invoke_static_double(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.code
            .push(Bytecode::InvokeStaticDouble(dest, fid, start, num));
    }

    pub fn emit_invoke_static_ptr(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.code
            .push(Bytecode::InvokeStaticPtr(dest, fid, start, num));
    }

    pub fn emit_new_object(&mut self, dest: Register, cls_id: ClassDefId) {
        self.code.push(Bytecode::NewObject(dest, cls_id));
    }

    pub fn generate(mut self) -> BytecodeFunction {
        self.resolve_forward_jumps();

        BytecodeFunction {
            code: self.code,
            offset: generate_offset(&self.registers),
            registers: self.registers,
            string_pool: generate_string_pool(self.string_pool_map),
        }
    }

    fn resolve_forward_jumps(&mut self) {
        let unresolved_jumps = mem::replace(&mut self.unresolved_jumps, Vec::new());

        for (idx, lbl) in unresolved_jumps {
            let lbl_dest = self.dest_label(lbl).expect("label unresolved");
            let op = &mut self.code[idx.0];

            match op {
                Bytecode::JumpIfFalse(_, ref mut dest) => {
                    assert!(dest.is_invalid());
                    *dest = lbl_dest;
                }

                Bytecode::JumpIfTrue(_, ref mut dest) => {
                    assert!(dest.is_invalid());
                    *dest = lbl_dest;
                }

                Bytecode::Jump(ref mut dest) => {
                    assert!(dest.is_invalid());
                    *dest = lbl_dest;
                }

                _ => unreachable!(),
            }
        }
    }
}

fn generate_string_pool(map: HashMap<String, StrConstPoolIdx>) -> Vec<String> {
    let mut pool: Vec<String> = vec![String::new(); map.len()];
    for (string_value, StrConstPoolIdx(index)) in map {
        pool[index] = string_value;
    }

    pool
}

fn generate_offset(registers: &Vec<BytecodeType>) -> Vec<i32> {
    let mut offset: Vec<i32> = vec![0; registers.len()];
    let mut stacksize: i32 = 0;
    for (index, ty) in registers.iter().enumerate() {
        offset[index] = cratemem::align_i32(stacksize - ty.size(), ty.size());
        stacksize = offset[index];
    }

    offset
}

pub struct BytecodeFunction {
    code: Vec<Bytecode>,
    registers: Vec<BytecodeType>,
    string_pool: Vec<String>,
    offset: Vec<i32>,
}

impl BytecodeFunction {
    pub fn code(&self) -> &[Bytecode] {
        &self.code
    }

    pub fn string_pool(&self) -> &[String] {
        &self.string_pool
    }

    pub fn registers(&self) -> &[BytecodeType] {
        &self.registers
    }

    pub fn register(&self, register: Register) -> BytecodeType {
        *self.registers.get(register.0).expect("register not found")
    }

    pub fn offset(&self, register: Register) -> i32 {
        *self.offset.get(register.0).expect("offset not found")
    }

    pub fn stacksize(&self) -> i32 {
        match self.offset.last() {
            None => 0,
            Some(stacksize) => cratemem::align_i32(-(*stacksize), 16),
        }
    }

    pub fn dump(&self) {
        let mut btidx = 0;
        for btcode in self.code.iter() {
            match btcode {
                Bytecode::AddInt(dest, lhs, rhs) => {
                    println!("{}: {} <-int {} + {}", btidx, dest, lhs, rhs)
                }
                Bytecode::AddLong(dest, lhs, rhs) => {
                    println!("{}: {} <-long {} + {}", btidx, dest, lhs, rhs)
                }
                Bytecode::AddFloat(dest, lhs, rhs) => {
                    println!("{}: {} <-float {} + {}", btidx, dest, lhs, rhs)
                }
                Bytecode::AddDouble(dest, lhs, rhs) => {
                    println!("{}: {} <-double {} + {}", btidx, dest, lhs, rhs)
                }
                Bytecode::AndInt(dest, lhs, rhs) => {
                    println!("{}: {} <-int {} & {}", btidx, dest, lhs, rhs)
                }
                Bytecode::OrInt(dest, lhs, rhs) => {
                    println!("{}: {} <-int {} | {}", btidx, dest, lhs, rhs)
                }
                Bytecode::XorInt(dest, lhs, rhs) => {
                    println!("{}: {} <-int {} ^ {}", btidx, dest, lhs, rhs)
                }
                Bytecode::DivInt(dest, lhs, rhs) => {
                    println!("{}: {} <-int {} / {}", btidx, dest, lhs, rhs)
                }
                Bytecode::ConstNil(dest) => println!("{}: {} <- nil", btidx, dest),
                Bytecode::ConstTrue(dest) => println!("{}: {} <- true", btidx, dest),
                Bytecode::ConstFalse(dest) => println!("{}: {} <- false", btidx, dest),
                Bytecode::ConstZeroByte(dest) => println!("{}: {} <-byte 0", btidx, dest),
                Bytecode::ConstZeroInt(dest) => println!("{}: {} <-int 0", btidx, dest),
                Bytecode::ConstZeroLong(dest) => println!("{}: {} <-long 0", btidx, dest),
                Bytecode::ConstZeroFloat(dest) => println!("{}: {} <-float 0", btidx, dest),
                Bytecode::ConstZeroDouble(dest) => println!("{}: {} <-double 0", btidx, dest),
                Bytecode::ConstChar(dest, val) => println!("{}: {} <-char {}", btidx, dest, val),
                Bytecode::ConstByte(dest, val) => println!("{}: {} <-byte {}", btidx, dest, val),
                Bytecode::ConstInt(dest, val) => println!("{}: {} <-int {}", btidx, dest, val),
                Bytecode::ConstLong(dest, val) => println!("{}: {} <-long {}", btidx, dest, val),
                Bytecode::ConstFloat(dest, val) => println!("{}: {} <-float {}", btidx, dest, val),
                Bytecode::ConstDouble(dest, val) => {
                    println!("{}: {} <-double {}", btidx, dest, val)
                }
                Bytecode::ConstString(dest, index) => {
                    println!("{}: {} <-string {}", btidx, dest, index)
                }
                Bytecode::NotBool(dest, src) => println!("{}: {} <-bool {}", btidx, dest, src),
                Bytecode::JumpIfFalse(opnd, target) => {
                    println!("{}: if {}=false goto {}", btidx, opnd, target)
                }
                Bytecode::JumpIfTrue(opnd, target) => {
                    println!("{}: if {} goto {}", btidx, opnd, target)
                }
                Bytecode::Jump(dest) => println!("{}: Jump bc#{}", btidx, dest),
                Bytecode::ModInt(dest, lhs, rhs) => {
                    println!("{}: {} <-int {} % {}", btidx, dest, lhs, rhs)
                }
                Bytecode::MulInt(dest, lhs, rhs) => {
                    println!("{}: {} <-int {} * {}", btidx, dest, lhs, rhs)
                }
                Bytecode::NegInt(dest, src) => println!("{}: {} <-int -{}", btidx, dest, src),
                Bytecode::NegLong(dest, src) => println!("{}: {} <-long -{}", btidx, dest, src),
                Bytecode::ShlInt(dest, lhs, rhs) => {
                    println!("{}: {} <-int {} << {}", btidx, dest, lhs, rhs)
                }
                Bytecode::SarInt(dest, lhs, rhs) => {
                    println!("{}: {} <-int {} >> {}", btidx, dest, lhs, rhs)
                }
                Bytecode::ShrInt(dest, lhs, rhs) => {
                    println!("{}: {} <-int {} >>> {}", btidx, dest, lhs, rhs)
                }
                Bytecode::SubInt(dest, lhs, rhs) => {
                    println!("{}: {} <-int {} - {}", btidx, dest, lhs, rhs)
                }

                Bytecode::LoadFieldBool(dest, obj, cls, field) => {
                    println!("{}: {} <-bool {} {:?}.{:?}", btidx, dest, obj, cls, field);
                }
                Bytecode::LoadFieldByte(dest, obj, cls, field) => {
                    println!("{}: {} <-byte {} {:?}.{:?}", btidx, dest, obj, cls, field);
                }
                Bytecode::LoadFieldChar(dest, obj, cls, field) => {
                    println!("{}: {} <-char {} {:?}.{:?}", btidx, dest, obj, cls, field);
                }
                Bytecode::LoadFieldInt(dest, obj, cls, field) => {
                    println!("{}: {} <-int {} {:?}.{:?}", btidx, dest, obj, cls, field);
                }
                Bytecode::LoadFieldLong(dest, obj, cls, field) => {
                    println!("{}: {} <-long {} {:?}.{:?}", btidx, dest, obj, cls, field);
                }
                Bytecode::LoadFieldFloat(dest, obj, cls, field) => {
                    println!("{}: {} <-float {} {:?}.{:?}", btidx, dest, obj, cls, field);
                }
                Bytecode::LoadFieldDouble(dest, obj, cls, field) => {
                    println!("{}: {} <-double {} {:?}.{:?}", btidx, dest, obj, cls, field);
                }
                Bytecode::LoadFieldPtr(dest, obj, cls, field) => {
                    println!("{}: {} <-ptr {} {:?}.{:?}", btidx, dest, obj, cls, field);
                }
                Bytecode::RetBool(res) => println!("{}: Ret.Bool {}", btidx, res),
                Bytecode::RetByte(res) => println!("{}: Ret.Byte {}", btidx, res),
                Bytecode::RetChar(res) => println!("{}: Ret.Char {}", btidx, res),
                Bytecode::RetInt(res) => println!("{}: Ret.Int {}", btidx, res),
                Bytecode::RetLong(res) => println!("{}: Ret.Long {}", btidx, res),
                Bytecode::RetFloat(res) => println!("{}: Ret.Float {}", btidx, res),
                Bytecode::RetDouble(res) => println!("{}: Ret.Double {}", btidx, res),
                Bytecode::RetPtr(res) => println!("{}: Ret.Ptr {}", btidx, res),
                Bytecode::RetVoid => println!("{}: Ret", btidx),
                Bytecode::TestEqInt(dest, lhs, rhs) => {
                    println!("{}: {} <- {} =.int {}", btidx, dest, lhs, rhs)
                }
                Bytecode::TestEqPtr(dest, lhs, rhs) => {
                    println!("{}: {} <- {} === {}", btidx, dest, lhs, rhs)
                }
                Bytecode::TestNeInt(dest, lhs, rhs) => {
                    println!("{}: {} <- {} !=.int {}", btidx, dest, lhs, rhs)
                }
                Bytecode::TestNePtr(dest, lhs, rhs) => {
                    println!("{}: {} <- {} !== {}", btidx, dest, lhs, rhs)
                }
                Bytecode::TestGtInt(dest, lhs, rhs) => {
                    println!("{}: {} <- {} >.int {}", btidx, dest, lhs, rhs)
                }
                Bytecode::TestGeInt(dest, lhs, rhs) => {
                    println!("{}: {} <- {} <=.int {}", btidx, dest, lhs, rhs)
                }
                Bytecode::TestLtInt(dest, lhs, rhs) => {
                    println!("{}: {} <- {} >.int {}", btidx, dest, lhs, rhs)
                }
                Bytecode::TestLeInt(dest, lhs, rhs) => {
                    println!("{}: {} <- {} >=.int {}", btidx, dest, lhs, rhs)
                }
                Bytecode::LoadGlobalBool(dest, gid) => {
                    println!("{}: {} <-bool global {:?}", btidx, dest, gid)
                }
                Bytecode::LoadGlobalByte(dest, gid) => {
                    println!("{}: {} <-byte global {:?}", btidx, dest, gid)
                }
                Bytecode::LoadGlobalChar(dest, gid) => {
                    println!("{}: {} <-char global {:?}", btidx, dest, gid)
                }
                Bytecode::LoadGlobalInt(dest, gid) => {
                    println!("{}: {} <-int global {:?}", btidx, dest, gid)
                }
                Bytecode::LoadGlobalLong(dest, gid) => {
                    println!("{}: {} <-long global {:?}", btidx, dest, gid)
                }
                Bytecode::LoadGlobalFloat(dest, gid) => {
                    println!("{}: {} <-float global {:?}", btidx, dest, gid)
                }
                Bytecode::LoadGlobalDouble(dest, gid) => {
                    println!("{}: {} <-double global {:?}", btidx, dest, gid)
                }
                Bytecode::LoadGlobalPtr(dest, gid) => {
                    println!("{}: {} <-ptr global {:?}", btidx, dest, gid)
                }
                Bytecode::MovBool(dest, src) => println!("{}: {} <-bool {}", btidx, dest, src),
                Bytecode::MovByte(dest, src) => println!("{}: {} <-byte {}", btidx, dest, src),
                Bytecode::MovChar(dest, src) => println!("{}: {} <-char {}", btidx, dest, src),
                Bytecode::MovInt(dest, src) => println!("{}: {} <-int {}", btidx, dest, src),
                Bytecode::MovLong(dest, src) => println!("{}: {} <-long {}", btidx, dest, src),
                Bytecode::MovFloat(dest, src) => println!("{}: {} <-float {}", btidx, dest, src),
                Bytecode::MovDouble(dest, src) => println!("{}: {} <-double {}", btidx, dest, src),
                Bytecode::MovPtr(dest, src) => println!("{}: {} <-ptr {}", btidx, dest, src),
                Bytecode::InvokeDirectVoid(fct_id, start, num) => {
                    println!("{}: invoke direct {:?} {} {}", btidx, fct_id, start, num);
                }
                Bytecode::InvokeDirectBool(dest, fct_id, start, num) => {
                    println!(
                        "{}: {} <-bool invoke direct {:?} {} {}",
                        btidx, dest, fct_id, start, num
                    );
                }
                Bytecode::InvokeDirectByte(dest, fct_id, start, num) => {
                    println!(
                        "{}: {} <-byte invoke direct {:?} {} {}",
                        btidx, dest, fct_id, start, num
                    );
                }
                Bytecode::InvokeDirectChar(dest, fct_id, start, num) => {
                    println!(
                        "{}: {} <-char invoke direct {:?} {} {}",
                        btidx, dest, fct_id, start, num
                    );
                }
                Bytecode::InvokeDirectInt(dest, fct_id, start, num) => {
                    println!(
                        "{}: {} <-int invoke direct {:?} {} {}",
                        btidx, dest, fct_id, start, num
                    );
                }
                Bytecode::InvokeDirectLong(dest, fct_id, start, num) => {
                    println!(
                        "{}: {} <-long invoke direct {:?} {} {}",
                        btidx, dest, fct_id, start, num
                    );
                }
                Bytecode::InvokeDirectFloat(dest, fct_id, start, num) => {
                    println!(
                        "{}: {} <-float invoke direct {:?} {} {}",
                        btidx, dest, fct_id, start, num
                    );
                }
                Bytecode::InvokeDirectDouble(dest, fct_id, start, num) => {
                    println!(
                        "{}: {} <-double invoke direct {:?} {} {}",
                        btidx, dest, fct_id, start, num
                    );
                }
                Bytecode::InvokeDirectPtr(dest, fct_id, start, num) => {
                    println!(
                        "{}: {} <-ptr invoke direct {:?} {} {}",
                        btidx, dest, fct_id, start, num
                    );
                }
                Bytecode::InvokeVirtualVoid(fct_id, start, num) => {
                    println!("{}: invoke virtual {:?} {} {}", btidx, fct_id, start, num);
                }
                Bytecode::InvokeVirtualBool(dest, fct_id, start, num) => {
                    println!(
                        "{}: {} <-bool invoke virtual {:?} {} {}",
                        btidx, dest, fct_id, start, num
                    );
                }
                Bytecode::InvokeVirtualByte(dest, fct_id, start, num) => {
                    println!(
                        "{}: {} <-byte invoke virtual {:?} {} {}",
                        btidx, dest, fct_id, start, num
                    );
                }
                Bytecode::InvokeVirtualChar(dest, fct_id, start, num) => {
                    println!(
                        "{}: {} <-char invoke virtual {:?} {} {}",
                        btidx, dest, fct_id, start, num
                    );
                }
                Bytecode::InvokeVirtualInt(dest, fct_id, start, num) => {
                    println!(
                        "{}: {} <-int invoke virtual {:?} {} {}",
                        btidx, dest, fct_id, start, num
                    );
                }
                Bytecode::InvokeVirtualLong(dest, fct_id, start, num) => {
                    println!(
                        "{}: {} <-long invoke virtual {:?} {} {}",
                        btidx, dest, fct_id, start, num
                    );
                }
                Bytecode::InvokeVirtualFloat(dest, fct_id, start, num) => {
                    println!(
                        "{}: {} <-float invoke virtual {:?} {} {}",
                        btidx, dest, fct_id, start, num
                    );
                }
                Bytecode::InvokeVirtualDouble(dest, fct_id, start, num) => {
                    println!(
                        "{}: {} <-double invoke virtual {:?} {} {}",
                        btidx, dest, fct_id, start, num
                    );
                }
                Bytecode::InvokeVirtualPtr(dest, fct_id, start, num) => {
                    println!(
                        "{}: {} <-ptr invoke virtual {:?} {} {}",
                        btidx, dest, fct_id, start, num
                    );
                }
                Bytecode::InvokeStaticVoid(fct_id, start, num) => {
                    println!("{}: invoke static {:?} {} {}", btidx, fct_id, start, num);
                }
                Bytecode::InvokeStaticBool(dest, fct_id, start, num) => {
                    println!(
                        "{}: {} <-bool invoke static {:?} {} {}",
                        btidx, dest, fct_id, start, num
                    );
                }
                Bytecode::InvokeStaticByte(dest, fct_id, start, num) => {
                    println!(
                        "{}: {} <-byte invoke static {:?} {} {}",
                        btidx, dest, fct_id, start, num
                    );
                }
                Bytecode::InvokeStaticChar(dest, fct_id, start, num) => {
                    println!(
                        "{}: {} <-char invoke static {:?} {} {}",
                        btidx, dest, fct_id, start, num
                    );
                }
                Bytecode::InvokeStaticInt(dest, fct_id, start, num) => {
                    println!(
                        "{}: {} <-int invoke static {:?} {} {}",
                        btidx, dest, fct_id, start, num
                    );
                }
                Bytecode::InvokeStaticLong(dest, fct_id, start, num) => {
                    println!(
                        "{}: {} <-long invoke static {:?} {} {}",
                        btidx, dest, fct_id, start, num
                    );
                }
                Bytecode::InvokeStaticFloat(dest, fct_id, start, num) => {
                    println!(
                        "{}: {} <-float invoke static {:?} {} {}",
                        btidx, dest, fct_id, start, num
                    );
                }
                Bytecode::InvokeStaticDouble(dest, fct_id, start, num) => {
                    println!(
                        "{}: {} <-double invoke static {:?} {} {}",
                        btidx, dest, fct_id, start, num
                    );
                }
                Bytecode::InvokeStaticPtr(dest, fct_id, start, num) => {
                    println!(
                        "{}: {} <-ptr invoke static {:?} {} {}",
                        btidx, dest, fct_id, start, num
                    );
                }
                Bytecode::NewObject(dest, cls_id) => {
                    println!("{}: {} <- new {:?}", btidx, dest, cls_id);
                }
            }
            btidx = btidx + 1;
        }
    }
}
