use num_traits::cast::FromPrimitive;

use std::mem;

use crate::bytecode::{BytecodeOffset, BytecodeOpcode, BytecodeType, Register};
use crate::mem::align_i32;
use crate::vm::{ClassDefId, FctId, FieldId, GlobalId};

#[derive(Copy, Clone, PartialEq, Debug, Eq, Hash)]
pub struct Label(pub usize);

pub struct BytecodeWriter {
    data: Vec<u8>,

    label_offsets: Vec<Option<BytecodeOffset>>,
    unresolved_jump_offsets: Vec<(BytecodeOffset, BytecodeOffset, Label)>,
    unresolved_jump_consts: Vec<(BytecodeOffset, ConstPoolIdx, Label)>,

    registers: Vec<BytecodeType>,
    const_pool: Vec<ConstPoolEntry>,
}

impl BytecodeWriter {
    pub fn new() -> BytecodeWriter {
        BytecodeWriter {
            data: Vec::new(),

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
        self.label_offsets.push(None);
        Label(self.label_offsets.len() - 1)
    }

    pub fn define_label(&mut self) -> Label {
        let offset = BytecodeOffset(self.data.len() as u32);
        self.label_offsets.push(Some(offset));

        Label(self.label_offsets.len() - 1)
    }

    pub fn bind_label(&mut self, lbl: Label) {
        assert!(self.label_offsets[lbl.0].is_none(), "bind label twice");

        self.label_offsets[lbl.0] = Some(self.offset());
    }

    fn lookup_label(&self, lbl: Label) -> Option<BytecodeOffset> {
        self.label_offsets[lbl.0]
    }

    fn offset(&self) -> BytecodeOffset {
        BytecodeOffset(self.data.len() as u32)
    }

    pub fn emit_add_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::AddInt, dest, lhs, rhs);
    }

    pub fn emit_add_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::AddLong, dest, lhs, rhs);
    }

    pub fn emit_add_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::AddFloat, dest, lhs, rhs);
    }

    pub fn emit_add_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::AddDouble, dest, lhs, rhs);
    }

    pub fn emit_and_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::AndInt, dest, lhs, rhs);
    }

    pub fn emit_or_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::OrInt, dest, lhs, rhs);
    }

    pub fn emit_xor_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::XorInt, dest, lhs, rhs);
    }

    pub fn emit_div_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::DivInt, dest, lhs, rhs);
    }

    pub fn emit_div_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::DivFloat, dest, lhs, rhs);
    }

    pub fn emit_load_field_bool(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_load_field(BytecodeOpcode::LoadFieldBool, dest, obj, cls, field);
    }

    pub fn emit_load_field_byte(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_load_field(BytecodeOpcode::LoadFieldByte, dest, obj, cls, field);
    }

    pub fn emit_load_field_char(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_load_field(BytecodeOpcode::LoadFieldChar, dest, obj, cls, field);
    }

    pub fn emit_load_field_int(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_load_field(BytecodeOpcode::LoadFieldInt, dest, obj, cls, field);
    }

    pub fn emit_load_field_long(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_load_field(BytecodeOpcode::LoadFieldLong, dest, obj, cls, field);
    }

    pub fn emit_load_field_float(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_load_field(BytecodeOpcode::LoadFieldFloat, dest, obj, cls, field);
    }

    pub fn emit_load_field_double(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_load_field(BytecodeOpcode::LoadFieldDouble, dest, obj, cls, field);
    }

    pub fn emit_load_field_ptr(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_load_field(BytecodeOpcode::LoadFieldPtr, dest, obj, cls, field);
    }

    pub fn emit_const_nil(&mut self, dest: Register) {
        self.emit_reg1(BytecodeOpcode::ConstNil, dest);
    }

    pub fn emit_const_char(&mut self, dest: Register, value: char) {
        let idx = self.add_const(ConstPoolEntry::Char(value));
        self.emit_reg1_idx(BytecodeOpcode::ConstChar, dest, idx);
    }

    pub fn emit_const_byte(&mut self, dest: Register, value: u8) {
        self.emit_reg1_byte(BytecodeOpcode::ConstByte, dest, value);
    }

    pub fn emit_const_int(&mut self, dest: Register, value: i32) {
        let idx = self.add_const(ConstPoolEntry::Int(value as i32));
        self.emit_reg1_idx(BytecodeOpcode::ConstInt, dest, idx);
    }

    pub fn emit_const_long(&mut self, dest: Register, value: i64) {
        let idx = self.add_const(ConstPoolEntry::Long(value as i64));
        self.emit_reg1_idx(BytecodeOpcode::ConstLong, dest, idx);
    }

    pub fn emit_const_float(&mut self, dest: Register, value: f32) {
        let idx = self.add_const(ConstPoolEntry::Float32(value));
        self.emit_reg1_idx(BytecodeOpcode::ConstFloat, dest, idx);
    }

    pub fn emit_const_double(&mut self, dest: Register, value: f64) {
        let idx = self.add_const(ConstPoolEntry::Float64(value));
        self.emit_reg1_idx(BytecodeOpcode::ConstDouble, dest, idx);
    }

    pub fn emit_const_string(&mut self, dest: Register, value: String) {
        let idx = self.add_const(ConstPoolEntry::String(value));
        self.emit_reg1_idx(BytecodeOpcode::ConstString, dest, idx);
    }

    pub fn emit_const_zero_byte(&mut self, dest: Register) {
        self.emit_reg1(BytecodeOpcode::ConstZeroByte, dest);
    }

    pub fn emit_const_zero_int(&mut self, dest: Register) {
        self.emit_reg1(BytecodeOpcode::ConstZeroInt, dest);
    }

    pub fn emit_const_zero_long(&mut self, dest: Register) {
        self.emit_reg1(BytecodeOpcode::ConstZeroLong, dest);
    }

    pub fn emit_const_zero_float(&mut self, dest: Register) {
        self.emit_reg1(BytecodeOpcode::ConstZeroFloat, dest);
    }

    pub fn emit_const_zero_double(&mut self, dest: Register) {
        self.emit_reg1(BytecodeOpcode::ConstZeroDouble, dest);
    }

    pub fn emit_const_true(&mut self, dest: Register) {
        self.emit_reg1(BytecodeOpcode::ConstTrue, dest);
    }

    pub fn emit_const_false(&mut self, dest: Register) {
        self.emit_reg1(BytecodeOpcode::ConstFalse, dest);
    }

    pub fn emit_not_bool(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::NotBool, dest, src);
    }

    pub fn emit_jump_if_false(&mut self, opnd: Register, lbl: Label) {
        assert!(self.lookup_label(lbl).is_none());

        self.emit_jmp_forward(
            BytecodeOpcode::JumpIfFalse,
            BytecodeOpcode::JumpIfFalseConst,
            Some(opnd),
            lbl,
        );
    }

    pub fn emit_jump_if_true(&mut self, opnd: Register, lbl: Label) {
        assert!(self.lookup_label(lbl).is_none());

        self.emit_jmp_forward(
            BytecodeOpcode::JumpIfTrue,
            BytecodeOpcode::JumpIfTrueConst,
            Some(opnd),
            lbl,
        );
    }

    pub fn emit_jump_loop(&mut self, lbl: Label) {
        let offset = self.lookup_label(lbl).expect("label not bound");
        assert!(offset.to_usize() <= self.data.len());
        let distance = (self.data.len() - offset.to_usize()) as u32;
        self.emit_jmp(BytecodeOpcode::JumpLoop, distance);
    }

    pub fn emit_jump(&mut self, lbl: Label) {
        assert!(self.lookup_label(lbl).is_none());
        self.emit_jmp_forward(BytecodeOpcode::Jump, BytecodeOpcode::JumpConst, None, lbl);
    }

    pub fn emit_mod_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::ModInt, dest, lhs, rhs);
    }

    pub fn emit_mul_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::MulInt, dest, lhs, rhs);
    }

    pub fn emit_mul_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::MulFloat, dest, lhs, rhs);
    }

    pub fn emit_neg_int(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::NegInt, dest, src);
    }

    pub fn emit_neg_long(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::NegLong, dest, src);
    }

    pub fn emit_shl_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::ShlInt, dest, lhs, rhs);
    }

    pub fn emit_shr_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::ShrInt, dest, lhs, rhs);
    }

    pub fn emit_sar_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::SarInt, dest, lhs, rhs);
    }

    pub fn emit_sub_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::SubInt, dest, lhs, rhs);
    }

    pub fn emit_sub_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::SubFloat, dest, lhs, rhs);
    }

    pub fn emit_mov_bool(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::MovBool, dest, src);
    }

    pub fn emit_mov_byte(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::MovByte, dest, src);
    }

    pub fn emit_mov_char(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::MovChar, dest, src);
    }

    pub fn emit_mov_int(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::MovInt, dest, src);
    }

    pub fn emit_mov_long(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::MovLong, dest, src);
    }

    pub fn emit_mov_float(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::MovFloat, dest, src);
    }

    pub fn emit_mov_double(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::MovDouble, dest, src);
    }

    pub fn emit_mov_ptr(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::MovPtr, dest, src);
    }

    pub fn emit_ret_bool(&mut self, src: Register) {
        self.emit_reg1(BytecodeOpcode::RetBool, src);
    }

    pub fn emit_ret_byte(&mut self, src: Register) {
        self.emit_reg1(BytecodeOpcode::RetByte, src);
    }

    pub fn emit_ret_char(&mut self, src: Register) {
        self.emit_reg1(BytecodeOpcode::RetChar, src);
    }

    pub fn emit_ret_int(&mut self, src: Register) {
        self.emit_reg1(BytecodeOpcode::RetInt, src);
    }

    pub fn emit_ret_long(&mut self, src: Register) {
        self.emit_reg1(BytecodeOpcode::RetLong, src);
    }

    pub fn emit_ret_float(&mut self, src: Register) {
        self.emit_reg1(BytecodeOpcode::RetFloat, src);
    }

    pub fn emit_ret_double(&mut self, src: Register) {
        self.emit_reg1(BytecodeOpcode::RetDouble, src);
    }

    pub fn emit_ret_ptr(&mut self, src: Register) {
        self.emit_reg1(BytecodeOpcode::RetPtr, src);
    }

    pub fn emit_ret_void(&mut self) {
        self.emit_op(BytecodeOpcode::RetVoid);
    }

    pub fn emit_test_eq_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestEqInt, dest, lhs, rhs);
    }

    pub fn emit_test_eq_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestEqFloat, dest, lhs, rhs);
    }

    pub fn emit_test_eq_ptr(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestEqPtr, dest, lhs, rhs);
    }

    pub fn emit_test_ne_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestNeInt, dest, lhs, rhs);
    }

    pub fn emit_test_ne_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestNeFloat, dest, lhs, rhs);
    }

    pub fn emit_test_ne_ptr(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestNePtr, dest, lhs, rhs);
    }

    pub fn emit_test_gt_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestGtInt, dest, lhs, rhs);
    }

    pub fn emit_test_gt_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestGtFloat, dest, lhs, rhs);
    }

    pub fn emit_test_ge_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestGeInt, dest, lhs, rhs);
    }

    pub fn emit_test_ge_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestGeFloat, dest, lhs, rhs);
    }

    pub fn emit_test_lt_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestLtInt, dest, lhs, rhs);
    }

    pub fn emit_test_lt_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestLtFloat, dest, lhs, rhs);
    }

    pub fn emit_test_le_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestLeInt, dest, lhs, rhs);
    }

    pub fn emit_test_le_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestLeFloat, dest, lhs, rhs);
    }

    pub fn emit_load_global_bool(&mut self, dest: Register, gid: GlobalId) {
        self.emit_load_global(BytecodeOpcode::LoadGlobalBool, dest, gid);
    }

    pub fn emit_load_global_byte(&mut self, dest: Register, gid: GlobalId) {
        self.emit_load_global(BytecodeOpcode::LoadGlobalByte, dest, gid);
    }

    pub fn emit_load_global_char(&mut self, dest: Register, gid: GlobalId) {
        self.emit_load_global(BytecodeOpcode::LoadGlobalChar, dest, gid);
    }

    pub fn emit_load_global_int(&mut self, dest: Register, gid: GlobalId) {
        self.emit_load_global(BytecodeOpcode::LoadGlobalInt, dest, gid);
    }

    pub fn emit_load_global_long(&mut self, dest: Register, gid: GlobalId) {
        self.emit_load_global(BytecodeOpcode::LoadGlobalLong, dest, gid);
    }

    pub fn emit_load_global_float(&mut self, dest: Register, gid: GlobalId) {
        self.emit_load_global(BytecodeOpcode::LoadGlobalFloat, dest, gid);
    }

    pub fn emit_load_global_double(&mut self, dest: Register, gid: GlobalId) {
        self.emit_load_global(BytecodeOpcode::LoadGlobalDouble, dest, gid);
    }

    pub fn emit_load_global_ptr(&mut self, dest: Register, gid: GlobalId) {
        self.emit_load_global(BytecodeOpcode::LoadGlobalPtr, dest, gid);
    }

    pub fn emit_invoke_direct_void(&mut self, fid: FctId, start: Register, num: usize) {
        self.emit_fct_void(BytecodeOpcode::InvokeDirectVoid, fid, start, num);
    }

    pub fn emit_invoke_direct_bool(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.emit_fct(BytecodeOpcode::InvokeDirectBool, dest, fid, start, num);
    }

    pub fn emit_invoke_direct_byte(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.emit_fct(BytecodeOpcode::InvokeDirectByte, dest, fid, start, num);
    }

    pub fn emit_invoke_direct_char(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.emit_fct(BytecodeOpcode::InvokeDirectChar, dest, fid, start, num);
    }

    pub fn emit_invoke_direct_int(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.emit_fct(BytecodeOpcode::InvokeDirectInt, dest, fid, start, num);
    }

    pub fn emit_invoke_direct_long(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.emit_fct(BytecodeOpcode::InvokeDirectLong, dest, fid, start, num);
    }

    pub fn emit_invoke_direct_float(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.emit_fct(BytecodeOpcode::InvokeDirectFloat, dest, fid, start, num);
    }

    pub fn emit_invoke_direct_double(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.emit_fct(BytecodeOpcode::InvokeDirectDouble, dest, fid, start, num);
    }

    pub fn emit_invoke_direct_ptr(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.emit_fct(BytecodeOpcode::InvokeDirectPtr, dest, fid, start, num);
    }

    pub fn emit_invoke_virtual_void(&mut self, fid: FctId, start: Register, num: usize) {
        self.emit_fct_void(BytecodeOpcode::InvokeVirtualVoid, fid, start, num);
    }

    pub fn emit_invoke_virtual_bool(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.emit_fct(BytecodeOpcode::InvokeVirtualBool, dest, fid, start, num);
    }

    pub fn emit_invoke_virtual_byte(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.emit_fct(BytecodeOpcode::InvokeVirtualByte, dest, fid, start, num);
    }

    pub fn emit_invoke_virtual_char(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.emit_fct(BytecodeOpcode::InvokeVirtualChar, dest, fid, start, num);
    }

    pub fn emit_invoke_virtual_int(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.emit_fct(BytecodeOpcode::InvokeVirtualInt, dest, fid, start, num);
    }

    pub fn emit_invoke_virtual_long(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.emit_fct(BytecodeOpcode::InvokeVirtualLong, dest, fid, start, num);
    }

    pub fn emit_invoke_virtual_float(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.emit_fct(BytecodeOpcode::InvokeVirtualFloat, dest, fid, start, num);
    }

    pub fn emit_invoke_virtual_double(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.emit_fct(BytecodeOpcode::InvokeVirtualDouble, dest, fid, start, num);
    }

    pub fn emit_invoke_virtual_ptr(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.emit_fct(BytecodeOpcode::InvokeVirtualPtr, dest, fid, start, num);
    }

    pub fn emit_invoke_static_void(&mut self, fid: FctId, start: Register, num: usize) {
        self.emit_fct_void(BytecodeOpcode::InvokeStaticVoid, fid, start, num);
    }

    pub fn emit_invoke_static_bool(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.emit_fct(BytecodeOpcode::InvokeStaticBool, dest, fid, start, num);
    }

    pub fn emit_invoke_static_byte(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.emit_fct(BytecodeOpcode::InvokeStaticByte, dest, fid, start, num);
    }

    pub fn emit_invoke_static_char(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.emit_fct(BytecodeOpcode::InvokeStaticChar, dest, fid, start, num);
    }

    pub fn emit_invoke_static_int(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.emit_fct(BytecodeOpcode::InvokeStaticInt, dest, fid, start, num);
    }

    pub fn emit_invoke_static_long(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.emit_fct(BytecodeOpcode::InvokeStaticLong, dest, fid, start, num);
    }

    pub fn emit_invoke_static_float(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.emit_fct(BytecodeOpcode::InvokeStaticFloat, dest, fid, start, num);
    }

    pub fn emit_invoke_static_double(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.emit_fct(BytecodeOpcode::InvokeStaticDouble, dest, fid, start, num);
    }

    pub fn emit_invoke_static_ptr(
        &mut self,
        dest: Register,
        fid: FctId,
        start: Register,
        num: usize,
    ) {
        self.emit_fct(BytecodeOpcode::InvokeStaticPtr, dest, fid, start, num);
    }

    pub fn emit_throw(&mut self, opnd: Register) {
        self.emit_reg1(BytecodeOpcode::Throw, opnd);
    }

    pub fn emit_new_object(&mut self, dest: Register, cls_id: ClassDefId) {
        self.emit_new(BytecodeOpcode::NewObject, dest, cls_id);
    }

    pub fn generate(mut self) -> BytecodeFunction {
        self.resolve_forward_jumps();

        BytecodeFunction {
            data: self.data,
            offset: generate_offset(&self.registers),
            registers: self.registers,
            const_pool: self.const_pool,
        }
    }

    fn resolve_forward_jumps(&mut self) {
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

            if inst != BytecodeOpcode::Wide && fits_u8(distance) {
                let inst_imm = match inst {
                    BytecodeOpcode::JumpIfFalseConst => BytecodeOpcode::JumpIfFalse,
                    BytecodeOpcode::JumpIfTrueConst => BytecodeOpcode::JumpIfTrue,
                    BytecodeOpcode::JumpConst => BytecodeOpcode::Jump,
                    _ => unreachable!(),
                };

                let jump_target = if inst == BytecodeOpcode::JumpConst {
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

    fn emit_reg3(&mut self, inst: BytecodeOpcode, r1: Register, r2: Register, r3: Register) {
        let values = [
            inst as u32,
            r1.to_usize() as u32,
            r2.to_usize() as u32,
            r3.to_usize() as u32,
        ];
        self.emit_values(&values);
    }

    fn emit_reg2(&mut self, inst: BytecodeOpcode, r1: Register, r2: Register) {
        let values = [inst as u32, r1.to_usize() as u32, r2.to_usize() as u32];
        self.emit_values(&values);
    }

    fn emit_reg1(&mut self, inst: BytecodeOpcode, r1: Register) {
        let values = [inst as u32, r1.to_usize() as u32];
        self.emit_values(&values);
    }

    fn emit_reg1_idx(&mut self, inst: BytecodeOpcode, r1: Register, idx: ConstPoolIdx) {
        let values = [inst as u32, r1.to_usize() as u32, idx.to_usize() as u32];
        self.emit_values(&values);
    }

    fn emit_reg1_byte(&mut self, inst: BytecodeOpcode, r1: Register, value: u8) {
        let values = [inst as u32, r1.to_usize() as u32];
        self.emit_values(&values);
        self.emit_u8(value);
    }

    fn add_const(&mut self, value: ConstPoolEntry) -> ConstPoolIdx {
        let idx = self.const_pool.len();
        self.const_pool.push(value);
        idx.into()
    }

    fn emit_new(&mut self, inst: BytecodeOpcode, r1: Register, cid: ClassDefId) {
        let values = [inst as u32, r1.to_usize() as u32, cid.to_usize() as u32];
        self.emit_values(&values);
    }

    fn emit_fct_void(&mut self, inst: BytecodeOpcode, fid: FctId, r1: Register, cnt: usize) {
        let values = [
            inst as u32,
            fid.to_usize() as u32,
            r1.to_usize() as u32,
            cnt as u32,
        ];
        self.emit_values(&values);
    }

    fn emit_fct(
        &mut self,
        inst: BytecodeOpcode,
        r1: Register,
        fid: FctId,
        r2: Register,
        cnt: usize,
    ) {
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
        inst: BytecodeOpcode,
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

    fn emit_load_global(&mut self, inst: BytecodeOpcode, r1: Register, gid: GlobalId) {
        let values = [inst as u32, r1.to_usize() as u32, gid.to_usize() as u32];
        self.emit_values(&values);
    }

    fn emit_op(&mut self, inst: BytecodeOpcode) {
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
        self.data.push(BytecodeOpcode::Wide as u8);
    }

    fn emit_u8(&mut self, value: u8) {
        self.data.push(value);
    }

    fn emit_cond_jmp(&mut self, inst: BytecodeOpcode, cond: Register, offset: i32) {
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
        inst: BytecodeOpcode,
        inst_const: BytecodeOpcode,
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

    fn emit_jmp(&mut self, inst: BytecodeOpcode, offset: u32) {
        self.emit_values(&[inst as u32, offset]);
    }

    fn bytecode_at(&self, offset: BytecodeOffset) -> BytecodeOpcode {
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
    data: Vec<u8>,
    registers: Vec<BytecodeType>,
    const_pool: Vec<ConstPoolEntry>,
    offset: Vec<i32>,
}

impl BytecodeFunction {
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
