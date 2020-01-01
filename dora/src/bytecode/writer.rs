use num_traits::cast::FromPrimitive;

use std::mem;

use crate::bytecode::{
    Bytecode, BytecodeIdx, BytecodeInst, BytecodeOffset, BytecodeType, Register,
};
use crate::mem::align_i32;
use crate::vm::{ClassDefId, FctId, FieldId, GlobalId};

#[derive(Copy, Clone, PartialEq, Debug, Eq, Hash)]
pub struct Label(pub usize);

pub struct BytecodeWriter {
    code: Vec<Bytecode>,
    data: Vec<u8>,

    labels: Vec<Option<BytecodeIdx>>,
    unresolved_jumps: Vec<(BytecodeIdx, Label)>,

    label_offsets: Vec<Option<BytecodeOffset>>,
    unresolved_jump_offsets: Vec<(BytecodeOffset, BytecodeOffset, Label)>,
    unresolved_jump_consts: Vec<(BytecodeOffset, ConstPoolIdx, Label)>,

    registers: Vec<BytecodeType>,
    const_pool: Vec<ConstPoolEntry>,
}

impl BytecodeWriter {
    pub fn new() -> BytecodeWriter {
        BytecodeWriter {
            code: Vec::new(),
            data: Vec::new(),

            labels: Vec::new(),
            unresolved_jumps: Vec::new(),

            label_offsets: Vec::new(),
            unresolved_jump_offsets: Vec::new(),
            unresolved_jump_consts: Vec::new(),

            registers: Vec::new(),
            const_pool: Vec::new(),
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
        self.label_offsets.push(None);
        Label(self.label_offsets.len() - 1)
    }

    pub fn define_label(&mut self) -> Label {
        let idx = BytecodeIdx(self.code.len());
        self.labels.push(Some(idx));

        let offset = BytecodeOffset(self.data.len() as u32);
        self.label_offsets.push(Some(offset));

        Label(self.label_offsets.len() - 1)
    }

    pub fn bind_label(&mut self, lbl: Label) {
        assert!(self.labels[lbl.0].is_none(), "bind label twice");
        assert!(self.label_offsets[lbl.0].is_none(), "bind label twice");

        self.labels[lbl.0] = Some(self.pc());
        self.label_offsets[lbl.0] = Some(self.offset());
    }

    fn dest_label(&self, lbl: Label) -> Option<BytecodeIdx> {
        self.labels[lbl.0]
    }

    fn lookup_label(&self, lbl: Label) -> Option<BytecodeOffset> {
        self.label_offsets[lbl.0]
    }

    fn pc(&self) -> BytecodeIdx {
        BytecodeIdx(self.code.len())
    }

    fn offset(&self) -> BytecodeOffset {
        BytecodeOffset(self.data.len() as u32)
    }

    pub fn emit_add_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::AddInt(dest, lhs, rhs));
        self.emit_reg3(BytecodeInst::AddInt, dest, lhs, rhs);
    }

    pub fn emit_add_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::AddLong(dest, lhs, rhs));
        self.emit_reg3(BytecodeInst::AddLong, dest, lhs, rhs);
    }

    pub fn emit_add_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::AddFloat(dest, lhs, rhs));
        self.emit_reg3(BytecodeInst::AddFloat, dest, lhs, rhs);
    }

    pub fn emit_add_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::AddDouble(dest, lhs, rhs));
        self.emit_reg3(BytecodeInst::AddDouble, dest, lhs, rhs);
    }

    pub fn emit_and_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::AndInt(dest, lhs, rhs));
        self.emit_reg3(BytecodeInst::AndInt, dest, lhs, rhs);
    }

    pub fn emit_or_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::OrInt(dest, lhs, rhs));
        self.emit_reg3(BytecodeInst::OrInt, dest, lhs, rhs);
    }

    pub fn emit_xor_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::XorInt(dest, lhs, rhs));
        self.emit_reg3(BytecodeInst::XorInt, dest, lhs, rhs);
    }

    pub fn emit_div_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::DivInt(dest, lhs, rhs));
        self.emit_reg3(BytecodeInst::DivInt, dest, lhs, rhs);
    }

    pub fn emit_div_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::DivFloat(dest, lhs, rhs));
        self.emit_reg3(BytecodeInst::DivFloat, dest, lhs, rhs);
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
        self.emit_load_field(BytecodeInst::LoadFieldBool, dest, obj, cls, field);
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
        self.emit_load_field(BytecodeInst::LoadFieldByte, dest, obj, cls, field);
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
        self.emit_load_field(BytecodeInst::LoadFieldChar, dest, obj, cls, field);
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
        self.emit_load_field(BytecodeInst::LoadFieldInt, dest, obj, cls, field);
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
        self.emit_load_field(BytecodeInst::LoadFieldLong, dest, obj, cls, field);
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
        self.emit_load_field(BytecodeInst::LoadFieldFloat, dest, obj, cls, field);
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
        self.emit_load_field(BytecodeInst::LoadFieldDouble, dest, obj, cls, field);
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
        self.emit_load_field(BytecodeInst::LoadFieldPtr, dest, obj, cls, field);
    }

    pub fn emit_const_nil(&mut self, dest: Register) {
        self.code.push(Bytecode::ConstNil(dest));
        self.emit_reg1(BytecodeInst::ConstNil, dest);
    }

    pub fn emit_const_char(&mut self, dest: Register, value: char) {
        self.code.push(Bytecode::ConstChar(dest, value));
        let idx = self.add_const(ConstPoolEntry::Char(value));
        self.emit_reg1_idx(BytecodeInst::ConstChar, dest, idx);
    }

    pub fn emit_const_byte(&mut self, dest: Register, value: u8) {
        self.code.push(Bytecode::ConstByte(dest, value));
        self.emit_reg1_byte(BytecodeInst::ConstByte, dest, value);
    }

    pub fn emit_const_int(&mut self, dest: Register, value: i32) {
        self.code.push(Bytecode::ConstInt(dest, value));
        let idx = self.add_const(ConstPoolEntry::Int(value as i32));
        self.emit_reg1_idx(BytecodeInst::ConstInt, dest, idx);
    }

    pub fn emit_const_long(&mut self, dest: Register, value: i64) {
        self.code.push(Bytecode::ConstLong(dest, value));
        let idx = self.add_const(ConstPoolEntry::Long(value as i64));
        self.emit_reg1_idx(BytecodeInst::ConstLong, dest, idx);
    }

    pub fn emit_const_float(&mut self, dest: Register, value: f32) {
        self.code.push(Bytecode::ConstFloat(dest, value));
        let idx = self.add_const(ConstPoolEntry::Float32(value));
        self.emit_reg1_idx(BytecodeInst::ConstFloat, dest, idx);
    }

    pub fn emit_const_double(&mut self, dest: Register, value: f64) {
        self.code.push(Bytecode::ConstDouble(dest, value));
        let idx = self.add_const(ConstPoolEntry::Float64(value));
        self.emit_reg1_idx(BytecodeInst::ConstDouble, dest, idx);
    }

    pub fn emit_const_string(&mut self, dest: Register, value: String) {
        self.code.push(Bytecode::ConstString(dest, value.clone()));
        let idx = self.add_const(ConstPoolEntry::String(value));
        self.emit_reg1_idx(BytecodeInst::ConstString, dest, idx);
    }

    pub fn emit_const_zero_byte(&mut self, dest: Register) {
        self.code.push(Bytecode::ConstZeroByte(dest));
        self.emit_reg1(BytecodeInst::ConstZeroByte, dest);
    }

    pub fn emit_const_zero_int(&mut self, dest: Register) {
        self.code.push(Bytecode::ConstZeroInt(dest));
        self.emit_reg1(BytecodeInst::ConstZeroInt, dest);
    }

    pub fn emit_const_zero_long(&mut self, dest: Register) {
        self.code.push(Bytecode::ConstZeroLong(dest));
        self.emit_reg1(BytecodeInst::ConstZeroLong, dest);
    }

    pub fn emit_const_zero_float(&mut self, dest: Register) {
        self.code.push(Bytecode::ConstZeroFloat(dest));
        self.emit_reg1(BytecodeInst::ConstZeroFloat, dest);
    }

    pub fn emit_const_zero_double(&mut self, dest: Register) {
        self.code.push(Bytecode::ConstZeroDouble(dest));
        self.emit_reg1(BytecodeInst::ConstZeroDouble, dest);
    }

    pub fn emit_const_true(&mut self, dest: Register) {
        self.code.push(Bytecode::ConstTrue(dest));
        self.emit_reg1(BytecodeInst::ConstTrue, dest);
    }

    pub fn emit_const_false(&mut self, dest: Register) {
        self.code.push(Bytecode::ConstFalse(dest));
        self.emit_reg1(BytecodeInst::ConstFalse, dest);
    }

    pub fn emit_not_bool(&mut self, dest: Register, src: Register) {
        self.code.push(Bytecode::NotBool(dest, src));
        self.emit_reg2(BytecodeInst::NotBool, dest, src);
    }

    pub fn emit_jump_if_false(&mut self, opnd: Register, lbl: Label) {
        assert!(self.lookup_label(lbl).is_none());

        self.unresolved_jumps.push((self.pc(), lbl));
        self.code
            .push(Bytecode::JumpIfFalse(opnd, BytecodeIdx::invalid()));

        self.emit_jmp_forward(
            BytecodeInst::JumpIfFalse,
            BytecodeInst::JumpIfFalseConst,
            Some(opnd),
            lbl,
        );
    }

    pub fn emit_jump_if_true(&mut self, opnd: Register, lbl: Label) {
        assert!(self.lookup_label(lbl).is_none());

        self.unresolved_jumps.push((self.pc(), lbl));
        self.code
            .push(Bytecode::JumpIfTrue(opnd, BytecodeIdx::invalid()));

        self.emit_jmp_forward(
            BytecodeInst::JumpIfTrue,
            BytecodeInst::JumpIfTrueConst,
            Some(opnd),
            lbl,
        );
    }

    pub fn emit_jump_loop(&mut self, lbl: Label) {
        if let Some(idx) = self.dest_label(lbl) {
            self.code.push(Bytecode::Jump(idx));
        } else {
            panic!("label not bound");
        }

        let offset = self.lookup_label(lbl).expect("label not bound");
        assert!(offset.to_usize() <= self.data.len());
        let distance = (self.data.len() - offset.to_usize()) as u32;
        self.emit_jmp(BytecodeInst::JumpLoop, distance);
    }

    pub fn emit_jump(&mut self, lbl: Label) {
        assert!(self.lookup_label(lbl).is_none());
        self.unresolved_jumps.push((self.pc(), lbl));
        self.code.push(Bytecode::Jump(BytecodeIdx::invalid()));

        self.emit_jmp_forward(BytecodeInst::Jump, BytecodeInst::JumpConst, None, lbl);
    }

    pub fn emit_mod_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::ModInt(dest, lhs, rhs));
        self.emit_reg3(BytecodeInst::ModInt, dest, lhs, rhs);
    }

    pub fn emit_mul_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::MulInt(dest, lhs, rhs));
        self.emit_reg3(BytecodeInst::MulInt, dest, lhs, rhs);
    }

    pub fn emit_mul_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::MulFloat(dest, lhs, rhs));
        self.emit_reg3(BytecodeInst::MulFloat, dest, lhs, rhs);
    }

    pub fn emit_neg_int(&mut self, dest: Register, src: Register) {
        self.code.push(Bytecode::NegInt(dest, src));
        self.emit_reg2(BytecodeInst::NegInt, dest, src);
    }

    pub fn emit_neg_long(&mut self, dest: Register, src: Register) {
        self.code.push(Bytecode::NegLong(dest, src));
        self.emit_reg2(BytecodeInst::NegLong, dest, src);
    }

    pub fn emit_shl_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::ShlInt(dest, lhs, rhs));
        self.emit_reg3(BytecodeInst::ShlInt, dest, lhs, rhs);
    }

    pub fn emit_shr_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::ShrInt(dest, lhs, rhs));
        self.emit_reg3(BytecodeInst::ShrInt, dest, lhs, rhs);
    }

    pub fn emit_sar_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::SarInt(dest, lhs, rhs));
        self.emit_reg3(BytecodeInst::SarInt, dest, lhs, rhs);
    }

    pub fn emit_sub_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::SubInt(dest, lhs, rhs));
        self.emit_reg3(BytecodeInst::SubInt, dest, lhs, rhs);
    }

    pub fn emit_sub_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::SubFloat(dest, lhs, rhs));
        self.emit_reg3(BytecodeInst::SubFloat, dest, lhs, rhs);
    }

    pub fn emit_mov_bool(&mut self, dest: Register, src: Register) {
        self.code.push(Bytecode::MovBool(dest, src));
        self.emit_reg2(BytecodeInst::MovBool, dest, src);
    }

    pub fn emit_mov_byte(&mut self, dest: Register, src: Register) {
        self.code.push(Bytecode::MovByte(dest, src));
        self.emit_reg2(BytecodeInst::MovByte, dest, src);
    }

    pub fn emit_mov_char(&mut self, dest: Register, src: Register) {
        self.code.push(Bytecode::MovChar(dest, src));
        self.emit_reg2(BytecodeInst::MovChar, dest, src);
    }

    pub fn emit_mov_int(&mut self, dest: Register, src: Register) {
        self.code.push(Bytecode::MovInt(dest, src));
        self.emit_reg2(BytecodeInst::MovInt, dest, src);
    }

    pub fn emit_mov_long(&mut self, dest: Register, src: Register) {
        self.code.push(Bytecode::MovLong(dest, src));
        self.emit_reg2(BytecodeInst::MovLong, dest, src);
    }

    pub fn emit_mov_float(&mut self, dest: Register, src: Register) {
        self.code.push(Bytecode::MovFloat(dest, src));
        self.emit_reg2(BytecodeInst::MovFloat, dest, src);
    }

    pub fn emit_mov_double(&mut self, dest: Register, src: Register) {
        self.code.push(Bytecode::MovDouble(dest, src));
        self.emit_reg2(BytecodeInst::MovDouble, dest, src);
    }

    pub fn emit_mov_ptr(&mut self, dest: Register, src: Register) {
        self.code.push(Bytecode::MovPtr(dest, src));
        self.emit_reg2(BytecodeInst::MovPtr, dest, src);
    }

    pub fn emit_ret_bool(&mut self, src: Register) {
        self.code.push(Bytecode::RetBool(src));
        self.emit_reg1(BytecodeInst::RetBool, src);
    }

    pub fn emit_ret_byte(&mut self, src: Register) {
        self.code.push(Bytecode::RetByte(src));
        self.emit_reg1(BytecodeInst::RetByte, src);
    }

    pub fn emit_ret_char(&mut self, src: Register) {
        self.code.push(Bytecode::RetChar(src));
        self.emit_reg1(BytecodeInst::RetChar, src);
    }

    pub fn emit_ret_int(&mut self, src: Register) {
        self.code.push(Bytecode::RetInt(src));
        self.emit_reg1(BytecodeInst::RetInt, src);
    }

    pub fn emit_ret_long(&mut self, src: Register) {
        self.code.push(Bytecode::RetLong(src));
        self.emit_reg1(BytecodeInst::RetLong, src);
    }

    pub fn emit_ret_float(&mut self, src: Register) {
        self.code.push(Bytecode::RetFloat(src));
        self.emit_reg1(BytecodeInst::RetFloat, src);
    }

    pub fn emit_ret_double(&mut self, src: Register) {
        self.code.push(Bytecode::RetDouble(src));
        self.emit_reg1(BytecodeInst::RetDouble, src);
    }

    pub fn emit_ret_ptr(&mut self, src: Register) {
        self.code.push(Bytecode::RetPtr(src));
        self.emit_reg1(BytecodeInst::RetPtr, src);
    }

    pub fn emit_ret_void(&mut self) {
        self.code.push(Bytecode::RetVoid);
        self.emit_op(BytecodeInst::RetVoid);
    }

    pub fn emit_test_eq_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::TestEqInt(dest, lhs, rhs));
        self.emit_reg3(BytecodeInst::TestEqInt, dest, lhs, rhs);
    }

    pub fn emit_test_eq_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::TestEqFloat(dest, lhs, rhs));
        self.emit_reg3(BytecodeInst::TestEqFloat, dest, lhs, rhs);
    }

    pub fn emit_test_eq_ptr(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::TestEqPtr(dest, lhs, rhs));
        self.emit_reg3(BytecodeInst::TestEqPtr, dest, lhs, rhs);
    }

    pub fn emit_test_ne_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::TestNeInt(dest, lhs, rhs));
        self.emit_reg3(BytecodeInst::TestNeInt, dest, lhs, rhs);
    }

    pub fn emit_test_ne_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::TestNeFloat(dest, lhs, rhs));
        self.emit_reg3(BytecodeInst::TestNeFloat, dest, lhs, rhs);
    }

    pub fn emit_test_ne_ptr(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::TestNePtr(dest, lhs, rhs));
        self.emit_reg3(BytecodeInst::TestNePtr, dest, lhs, rhs);
    }

    pub fn emit_test_gt_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::TestGtInt(dest, lhs, rhs));
        self.emit_reg3(BytecodeInst::TestGtInt, dest, lhs, rhs);
    }

    pub fn emit_test_gt_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::TestGtFloat(dest, lhs, rhs));
        self.emit_reg3(BytecodeInst::TestGtFloat, dest, lhs, rhs);
    }

    pub fn emit_test_ge_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::TestGeInt(dest, lhs, rhs));
        self.emit_reg3(BytecodeInst::TestGeInt, dest, lhs, rhs);
    }

    pub fn emit_test_ge_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::TestGeFloat(dest, lhs, rhs));
        self.emit_reg3(BytecodeInst::TestGeFloat, dest, lhs, rhs);
    }

    pub fn emit_test_lt_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::TestLtInt(dest, lhs, rhs));
        self.emit_reg3(BytecodeInst::TestLtInt, dest, lhs, rhs);
    }

    pub fn emit_test_lt_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::TestLtFloat(dest, lhs, rhs));
        self.emit_reg3(BytecodeInst::TestLtFloat, dest, lhs, rhs);
    }

    pub fn emit_test_le_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::TestLeInt(dest, lhs, rhs));
        self.emit_reg3(BytecodeInst::TestLeInt, dest, lhs, rhs);
    }

    pub fn emit_test_le_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.code.push(Bytecode::TestLeFloat(dest, lhs, rhs));
        self.emit_reg3(BytecodeInst::TestLeFloat, dest, lhs, rhs);
    }

    pub fn emit_load_global_bool(&mut self, dest: Register, gid: GlobalId) {
        self.code.push(Bytecode::LoadGlobalBool(dest, gid));
        self.emit_load_global(BytecodeInst::LoadGlobalBool, dest, gid);
    }

    pub fn emit_load_global_byte(&mut self, dest: Register, gid: GlobalId) {
        self.code.push(Bytecode::LoadGlobalByte(dest, gid));
        self.emit_load_global(BytecodeInst::LoadGlobalByte, dest, gid);
    }

    pub fn emit_load_global_char(&mut self, dest: Register, gid: GlobalId) {
        self.code.push(Bytecode::LoadGlobalChar(dest, gid));
        self.emit_load_global(BytecodeInst::LoadGlobalChar, dest, gid);
    }

    pub fn emit_load_global_int(&mut self, dest: Register, gid: GlobalId) {
        self.code.push(Bytecode::LoadGlobalInt(dest, gid));
        self.emit_load_global(BytecodeInst::LoadGlobalInt, dest, gid);
    }

    pub fn emit_load_global_long(&mut self, dest: Register, gid: GlobalId) {
        self.code.push(Bytecode::LoadGlobalLong(dest, gid));
        self.emit_load_global(BytecodeInst::LoadGlobalLong, dest, gid);
    }

    pub fn emit_load_global_float(&mut self, dest: Register, gid: GlobalId) {
        self.code.push(Bytecode::LoadGlobalFloat(dest, gid));
        self.emit_load_global(BytecodeInst::LoadGlobalFloat, dest, gid);
    }

    pub fn emit_load_global_double(&mut self, dest: Register, gid: GlobalId) {
        self.code.push(Bytecode::LoadGlobalDouble(dest, gid));
        self.emit_load_global(BytecodeInst::LoadGlobalDouble, dest, gid);
    }

    pub fn emit_load_global_ptr(&mut self, dest: Register, gid: GlobalId) {
        self.code.push(Bytecode::LoadGlobalPtr(dest, gid));
        self.emit_load_global(BytecodeInst::LoadGlobalPtr, dest, gid);
    }

    pub fn emit_invoke_direct_void(&mut self, fid: FctId, start: Register, num: usize) {
        self.code.push(Bytecode::InvokeDirectVoid(fid, start, num));
        self.emit_fct_void(BytecodeInst::InvokeDirectVoid, fid, start, num);
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
        self.emit_fct(BytecodeInst::InvokeDirectBool, dest, fid, start, num);
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
        self.emit_fct(BytecodeInst::InvokeDirectByte, dest, fid, start, num);
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
        self.emit_fct(BytecodeInst::InvokeDirectChar, dest, fid, start, num);
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
        self.emit_fct(BytecodeInst::InvokeDirectInt, dest, fid, start, num);
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
        self.emit_fct(BytecodeInst::InvokeDirectLong, dest, fid, start, num);
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
        self.emit_fct(BytecodeInst::InvokeDirectFloat, dest, fid, start, num);
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
        self.emit_fct(BytecodeInst::InvokeDirectDouble, dest, fid, start, num);
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
        self.emit_fct(BytecodeInst::InvokeDirectPtr, dest, fid, start, num);
    }

    pub fn emit_invoke_virtual_void(&mut self, fid: FctId, start: Register, num: usize) {
        self.code.push(Bytecode::InvokeVirtualVoid(fid, start, num));
        self.emit_fct_void(BytecodeInst::InvokeVirtualVoid, fid, start, num);
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
        self.emit_fct(BytecodeInst::InvokeVirtualBool, dest, fid, start, num);
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
        self.emit_fct(BytecodeInst::InvokeVirtualByte, dest, fid, start, num);
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
        self.emit_fct(BytecodeInst::InvokeVirtualChar, dest, fid, start, num);
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
        self.emit_fct(BytecodeInst::InvokeVirtualInt, dest, fid, start, num);
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
        self.emit_fct(BytecodeInst::InvokeVirtualLong, dest, fid, start, num);
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
        self.emit_fct(BytecodeInst::InvokeVirtualFloat, dest, fid, start, num);
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
        self.emit_fct(BytecodeInst::InvokeVirtualDouble, dest, fid, start, num);
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
        self.emit_fct(BytecodeInst::InvokeVirtualPtr, dest, fid, start, num);
    }

    pub fn emit_invoke_static_void(&mut self, fid: FctId, start: Register, num: usize) {
        self.code.push(Bytecode::InvokeStaticVoid(fid, start, num));
        self.emit_fct_void(BytecodeInst::InvokeStaticVoid, fid, start, num);
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
        self.emit_fct(BytecodeInst::InvokeStaticBool, dest, fid, start, num);
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
        self.emit_fct(BytecodeInst::InvokeStaticByte, dest, fid, start, num);
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
        self.emit_fct(BytecodeInst::InvokeStaticChar, dest, fid, start, num);
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
        self.emit_fct(BytecodeInst::InvokeStaticInt, dest, fid, start, num);
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
        self.emit_fct(BytecodeInst::InvokeStaticLong, dest, fid, start, num);
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
        self.emit_fct(BytecodeInst::InvokeStaticFloat, dest, fid, start, num);
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
        self.emit_fct(BytecodeInst::InvokeStaticDouble, dest, fid, start, num);
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
        self.emit_fct(BytecodeInst::InvokeStaticPtr, dest, fid, start, num);
    }

    pub fn emit_throw(&mut self, opnd: Register) {
        self.code.push(Bytecode::Throw(opnd));
        self.emit_reg1(BytecodeInst::Throw, opnd);
    }

    pub fn emit_new_object(&mut self, dest: Register, cls_id: ClassDefId) {
        self.code.push(Bytecode::NewObject(dest, cls_id));
        self.emit_new(BytecodeInst::NewObject, dest, cls_id);
    }

    pub fn generate(mut self) -> BytecodeFunction {
        self.resolve_forward_jumps();

        BytecodeFunction {
            code: self.code,
            data: self.data,
            offset: generate_offset(&self.registers),
            registers: self.registers,
            const_pool: self.const_pool,
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

        let unresolved_jumps = mem::replace(&mut self.unresolved_jump_offsets, Vec::new());

        for (start, address, label) in unresolved_jumps {
            let label = self.lookup_label(label).expect("label not bound");
            assert!(start.to_usize() < label.to_usize());
            let distance = (label.to_usize() - start.to_usize()) as u32;
            self.patch_u32(address, distance as u32);
        }

        let unresolved_jumps = mem::replace(&mut self.unresolved_jump_consts, Vec::new());

        for (start, const_idx, label) in unresolved_jumps {
            let target = self.lookup_label(label).expect("label not bound");
            assert!(start.to_usize() < target.to_usize());
            let distance = (target.to_usize() - start.to_usize()) as u32;
            let inst = self.bytecode_at(start);

            if inst != BytecodeInst::Wide && fits_u8(distance) {
                let inst_imm = match inst {
                    BytecodeInst::JumpIfFalseConst => BytecodeInst::JumpIfFalse,
                    BytecodeInst::JumpIfTrueConst => BytecodeInst::JumpIfTrue,
                    BytecodeInst::JumpConst => BytecodeInst::Jump,
                    _ => unreachable!(),
                };

                let jump_target = if inst == BytecodeInst::JumpConst {
                    1
                } else {
                    2
                };

                self.data[start.to_usize()] = inst_imm as u8;
                self.data[start.to_usize() + jump_target] = distance as u8;
            } else {
                self.patch_const(const_idx, ConstPoolEntry::Int(distance as i32));
            }
        }
    }

    fn emit_reg3(&mut self, inst: BytecodeInst, r1: Register, r2: Register, r3: Register) {
        let values = [
            inst as u32,
            r1.to_usize() as u32,
            r2.to_usize() as u32,
            r3.to_usize() as u32,
        ];
        self.emit_values(&values);
    }

    fn emit_reg2(&mut self, inst: BytecodeInst, r1: Register, r2: Register) {
        let values = [inst as u32, r1.to_usize() as u32, r2.to_usize() as u32];
        self.emit_values(&values);
    }

    fn emit_reg1(&mut self, inst: BytecodeInst, r1: Register) {
        let values = [inst as u32, r1.to_usize() as u32];
        self.emit_values(&values);
    }

    fn emit_reg1_idx(&mut self, inst: BytecodeInst, r1: Register, idx: ConstPoolIdx) {
        let values = [inst as u32, r1.to_usize() as u32, idx.to_usize() as u32];
        self.emit_values(&values);
    }

    fn emit_reg1_byte(&mut self, inst: BytecodeInst, r1: Register, value: u8) {
        let values = [inst as u32, r1.to_usize() as u32];
        self.emit_values(&values);
        self.emit_u8(value);
    }

    fn add_const(&mut self, value: ConstPoolEntry) -> ConstPoolIdx {
        let idx = self.const_pool.len();
        self.const_pool.push(value);
        idx.into()
    }

    fn emit_new(&mut self, inst: BytecodeInst, r1: Register, cid: ClassDefId) {
        let values = [inst as u32, r1.to_usize() as u32, cid.to_usize() as u32];
        self.emit_values(&values);
    }

    fn emit_fct_void(&mut self, inst: BytecodeInst, fid: FctId, r1: Register, cnt: usize) {
        let values = [
            inst as u32,
            fid.to_usize() as u32,
            r1.to_usize() as u32,
            cnt as u32,
        ];
        self.emit_values(&values);
    }

    fn emit_fct(&mut self, inst: BytecodeInst, r1: Register, fid: FctId, r2: Register, cnt: usize) {
        let values = [
            inst as u32,
            r1.to_usize() as u32,
            fid.to_usize() as u32,
            r2.to_usize() as u32,
            cnt as u32,
        ];
        self.emit_values(&values);
    }

    fn emit_load_field(
        &mut self,
        inst: BytecodeInst,
        r1: Register,
        r2: Register,
        cid: ClassDefId,
        fid: FieldId,
    ) {
        let values = [
            inst as u32,
            r1.to_usize() as u32,
            r2.to_usize() as u32,
            cid.to_usize() as u32,
            fid.to_usize() as u32,
        ];
        self.emit_values(&values);
    }

    fn emit_load_global(&mut self, inst: BytecodeInst, r1: Register, gid: GlobalId) {
        let values = [inst as u32, r1.to_usize() as u32, gid.to_usize() as u32];
        self.emit_values(&values);
    }

    fn emit_op(&mut self, inst: BytecodeInst) {
        let values = [inst as u32];
        self.emit_values(&values);
    }

    fn emit_values(&mut self, values: &[u32]) {
        if is_wide(values) {
            self.emit_wide();
            for &value in values {
                self.emit_u32(value);
            }
        } else {
            for &value in values {
                self.emit_u8(value as u8);
            }
        }
    }

    fn emit_wide(&mut self) {
        self.data.push(BytecodeInst::Wide as u8);
    }

    fn emit_u8(&mut self, value: u8) {
        self.data.push(value);
    }

    fn emit_cond_jmp(&mut self, inst: BytecodeInst, cond: Register, offset: i32) {
        if !fits_u8(inst as u32) || !fits_u8(cond.to_usize() as u32) || !fits_i8(offset) {
            self.emit_wide();
            self.emit_u32(inst as u32);
            self.emit_u32(cond.to_usize() as u32);
            self.emit_u32(offset as u32);
        } else {
            self.emit_u8(inst as u8);
            self.emit_u8(cond.to_usize() as u8);
            self.emit_u8(offset as u8);
        }
    }

    fn emit_jmp_forward(
        &mut self,
        inst: BytecodeInst,
        inst_const: BytecodeInst,
        cond: Option<Register>,
        lbl: Label,
    ) {
        debug_assert!(fits_u8(inst as u32));
        debug_assert!(fits_u8(inst_const as u32));
        let start = self.offset();

        if (cond.is_some() && !fits_u8(cond.unwrap().to_usize() as u32))
            || !fits_u8(self.const_pool.len() as u32)
        {
            self.emit_wide();
            self.emit_u32(inst as u32);
            if let Some(cond) = cond {
                self.emit_u32(cond.to_usize() as u32);
            }
            let address = self.offset();
            self.emit_u32(0);
            self.unresolved_jump_offsets.push((start, address, lbl));
        } else {
            self.emit_u8(inst_const as u8);
            if let Some(cond) = cond {
                self.emit_u8(cond.to_usize() as u8);
            }
            let idx = self.add_const(ConstPoolEntry::Int(0));
            self.emit_u8(idx.to_usize() as u8);
            self.unresolved_jump_consts.push((start, idx, lbl));
        }
    }

    fn emit_jmp(&mut self, inst: BytecodeInst, offset: u32) {
        self.emit_values(&[inst as u32, offset]);
    }

    fn bytecode_at(&self, offset: BytecodeOffset) -> BytecodeInst {
        FromPrimitive::from_u8(self.data[offset.to_usize()]).expect("unknown bytecode")
    }

    fn emit_u32(&mut self, value: u32) {
        self.data.push((value & 0xFF) as u8);
        self.data.push(((value >> 8) & 0xFF) as u8);
        self.data.push(((value >> 16) & 0xFF) as u8);
        self.data.push(((value >> 24) & 0xFF) as u8);
    }

    fn patch_u32(&mut self, offset: BytecodeOffset, value: u32) {
        let offset = offset.to_usize();
        self.data[offset] = (value & 0xFF) as u8;
        self.data[offset + 1] = ((value >> 8) & 0xFF) as u8;
        self.data[offset + 2] = ((value >> 16) & 0xFF) as u8;
        self.data[offset + 3] = ((value >> 24) & 0xFF) as u8;
    }

    fn patch_const(&mut self, idx: ConstPoolIdx, entry: ConstPoolEntry) {
        self.const_pool[idx.to_usize()] = entry;
    }
}

fn generate_offset(registers: &Vec<BytecodeType>) -> Vec<i32> {
    let mut offset: Vec<i32> = vec![0; registers.len()];
    let mut stacksize: i32 = 0;
    for (index, ty) in registers.iter().enumerate() {
        stacksize = align_i32(stacksize + ty.size(), ty.size());
        offset[index] = -stacksize;
    }

    offset
}

pub struct BytecodeFunction {
    code: Vec<Bytecode>,
    data: Vec<u8>,
    registers: Vec<BytecodeType>,
    const_pool: Vec<ConstPoolEntry>,
    offset: Vec<i32>,
}

impl BytecodeFunction {
    pub fn code(&self) -> &[Bytecode] {
        &self.code
    }

    pub fn data(&self) -> &[u8] {
        &self.data
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
            Some(stacksize) => align_i32(-(*stacksize), 16),
        }
    }

    pub fn const_pool(&self, idx: ConstPoolIdx) -> &ConstPoolEntry {
        &self.const_pool[idx.to_usize()]
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
                Bytecode::DivFloat(dest, lhs, rhs) => {
                    println!("{}: {} <-float {} / {}", btidx, dest, lhs, rhs)
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
                Bytecode::MulFloat(dest, lhs, rhs) => {
                    println!("{}: {} <-float {} * {}", btidx, dest, lhs, rhs)
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
                Bytecode::SubFloat(dest, lhs, rhs) => {
                    println!("{}: {} <-float {} - {}", btidx, dest, lhs, rhs)
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
                Bytecode::TestEqFloat(dest, lhs, rhs) => {
                    println!("{}: {} <- {} =.float {}", btidx, dest, lhs, rhs)
                }
                Bytecode::TestEqPtr(dest, lhs, rhs) => {
                    println!("{}: {} <- {} === {}", btidx, dest, lhs, rhs)
                }
                Bytecode::TestNeInt(dest, lhs, rhs) => {
                    println!("{}: {} <- {} !=.int {}", btidx, dest, lhs, rhs)
                }
                Bytecode::TestNeFloat(dest, lhs, rhs) => {
                    println!("{}: {} <- {} !=.float {}", btidx, dest, lhs, rhs)
                }
                Bytecode::TestNePtr(dest, lhs, rhs) => {
                    println!("{}: {} <- {} !== {}", btidx, dest, lhs, rhs)
                }
                Bytecode::TestGtInt(dest, lhs, rhs) => {
                    println!("{}: {} <- {} >.int {}", btidx, dest, lhs, rhs)
                }
                Bytecode::TestGtFloat(dest, lhs, rhs) => {
                    println!("{}: {} <- {} >.float {}", btidx, dest, lhs, rhs)
                }
                Bytecode::TestGeInt(dest, lhs, rhs) => {
                    println!("{}: {} <- {} >=.int {}", btidx, dest, lhs, rhs)
                }
                Bytecode::TestGeFloat(dest, lhs, rhs) => {
                    println!("{}: {} <- {} >=.float {}", btidx, dest, lhs, rhs)
                }
                Bytecode::TestLtInt(dest, lhs, rhs) => {
                    println!("{}: {} <- {} <.int {}", btidx, dest, lhs, rhs)
                }
                Bytecode::TestLtFloat(dest, lhs, rhs) => {
                    println!("{}: {} <- {} <.float {}", btidx, dest, lhs, rhs)
                }
                Bytecode::TestLeInt(dest, lhs, rhs) => {
                    println!("{}: {} <- {} <=.int {}", btidx, dest, lhs, rhs)
                }
                Bytecode::TestLeFloat(dest, lhs, rhs) => {
                    println!("{}: {} <- {} <=.float {}", btidx, dest, lhs, rhs)
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
                Bytecode::Throw(exception) => {
                    println!("{}: throw {}", btidx, exception);
                }
                Bytecode::NewObject(dest, cls_id) => {
                    println!("{}: {} <- new {:?}", btidx, dest, cls_id);
                }
            }
            btidx = btidx + 1;
        }
    }
}

fn is_wide(values: &[u32]) -> bool {
    values.iter().any(|&val| val > u8::max_value() as u32)
}

fn fits_u8(value: u32) -> bool {
    value <= u8::max_value() as u32
}

fn fits_i8(value: i32) -> bool {
    i8::min_value() as i32 <= value && value <= i8::max_value() as i32
}

pub enum ConstPoolEntry {
    String(String),
    Float32(f32),
    Float64(f64),
    Int(i32),
    Long(i64),
    Char(char),
}

impl ConstPoolEntry {
    pub fn to_string(&self) -> Option<&str> {
        match self {
            ConstPoolEntry::String(ref value) => Some(value),
            _ => None,
        }
    }

    pub fn to_float(&self) -> Option<f32> {
        match self {
            ConstPoolEntry::Float32(value) => Some(*value),
            _ => None,
        }
    }

    pub fn to_double(&self) -> Option<f64> {
        match self {
            ConstPoolEntry::Float64(value) => Some(*value),
            _ => None,
        }
    }

    pub fn to_int(&self) -> Option<i32> {
        match self {
            ConstPoolEntry::Int(value) => Some(*value),
            _ => None,
        }
    }

    pub fn to_long(&self) -> Option<i64> {
        match self {
            ConstPoolEntry::Long(value) => Some(*value),
            _ => None,
        }
    }

    pub fn to_char(&self) -> Option<char> {
        match self {
            ConstPoolEntry::Char(value) => Some(*value),
            _ => None,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct ConstPoolIdx(pub usize);

impl ConstPoolIdx {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

impl From<usize> for ConstPoolIdx {
    fn from(value: usize) -> Self {
        ConstPoolIdx(value)
    }
}
