use num_traits::cast::FromPrimitive;

use std::mem;

use crate::bytecode::{
    BytecodeFunction, BytecodeOffset, BytecodeOpcode, BytecodeType, ConstPoolEntry, ConstPoolIdx,
    Register,
};
use crate::vm::{ClassDefId, FctDefId, FieldId, GlobalId, TupleId};

use dora_parser::lexer::position::Position;

#[derive(Copy, Clone, PartialEq, Debug, Eq, Hash)]
pub struct Label(pub usize);

pub struct BytecodeWriter {
    code: Vec<u8>,
    arguments: u32,

    label_offsets: Vec<Option<BytecodeOffset>>,
    unresolved_jump_offsets: Vec<(BytecodeOffset, BytecodeOffset, Label)>,
    unresolved_jump_consts: Vec<(BytecodeOffset, ConstPoolIdx, Label)>,

    registers: Vec<BytecodeType>,
    const_pool: Vec<ConstPoolEntry>,

    positions: Vec<(u32, Position)>,
    position: Option<Position>,
}

impl BytecodeWriter {
    pub fn new() -> BytecodeWriter {
        BytecodeWriter {
            code: Vec::new(),
            arguments: 0,

            label_offsets: Vec::new(),
            unresolved_jump_offsets: Vec::new(),
            unresolved_jump_consts: Vec::new(),

            registers: Vec::new(),
            const_pool: Vec::new(),

            positions: Vec::new(),
            position: None,
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
        let offset = BytecodeOffset(self.code.len() as u32);
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
        BytecodeOffset(self.code.len() as u32)
    }

    pub fn set_arguments(&mut self, arguments: u32) {
        self.arguments = arguments;
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

    pub fn emit_and_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::AndLong, dest, lhs, rhs);
    }

    pub fn emit_or_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::OrInt, dest, lhs, rhs);
    }

    pub fn emit_or_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::OrLong, dest, lhs, rhs);
    }

    pub fn emit_xor_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::XorInt, dest, lhs, rhs);
    }

    pub fn emit_xor_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::XorLong, dest, lhs, rhs);
    }

    pub fn emit_div_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::DivInt, dest, lhs, rhs);
    }

    pub fn emit_div_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::DivLong, dest, lhs, rhs);
    }

    pub fn emit_div_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::DivFloat, dest, lhs, rhs);
    }

    pub fn emit_div_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::DivDouble, dest, lhs, rhs);
    }

    pub fn emit_load_field_bool(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_access_field(BytecodeOpcode::LoadFieldBool, dest, obj, cls, field);
    }

    pub fn emit_load_field_byte(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_access_field(BytecodeOpcode::LoadFieldByte, dest, obj, cls, field);
    }

    pub fn emit_load_field_char(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_access_field(BytecodeOpcode::LoadFieldChar, dest, obj, cls, field);
    }

    pub fn emit_load_field_int(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_access_field(BytecodeOpcode::LoadFieldInt, dest, obj, cls, field);
    }

    pub fn emit_load_field_long(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_access_field(BytecodeOpcode::LoadFieldLong, dest, obj, cls, field);
    }

    pub fn emit_load_field_float(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_access_field(BytecodeOpcode::LoadFieldFloat, dest, obj, cls, field);
    }

    pub fn emit_load_field_double(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_access_field(BytecodeOpcode::LoadFieldDouble, dest, obj, cls, field);
    }

    pub fn emit_load_field_ptr(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_access_field(BytecodeOpcode::LoadFieldPtr, dest, obj, cls, field);
    }

    pub fn emit_store_field_bool(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_access_field(BytecodeOpcode::StoreFieldBool, src, obj, cls, field);
    }

    pub fn emit_store_field_byte(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_access_field(BytecodeOpcode::StoreFieldByte, src, obj, cls, field);
    }

    pub fn emit_store_field_char(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_access_field(BytecodeOpcode::StoreFieldChar, src, obj, cls, field);
    }

    pub fn emit_store_field_int(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_access_field(BytecodeOpcode::StoreFieldInt, src, obj, cls, field);
    }

    pub fn emit_store_field_long(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_access_field(BytecodeOpcode::StoreFieldLong, src, obj, cls, field);
    }

    pub fn emit_store_field_float(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_access_field(BytecodeOpcode::StoreFieldFloat, src, obj, cls, field);
    }

    pub fn emit_store_field_double(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_access_field(BytecodeOpcode::StoreFieldDouble, src, obj, cls, field);
    }

    pub fn emit_store_field_ptr(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_access_field(BytecodeOpcode::StoreFieldPtr, src, obj, cls, field);
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
        let idx = self.add_const(ConstPoolEntry::Float(value));
        self.emit_reg1_idx(BytecodeOpcode::ConstFloat, dest, idx);
    }

    pub fn emit_const_double(&mut self, dest: Register, value: f64) {
        let idx = self.add_const(ConstPoolEntry::Double(value));
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

    pub fn emit_not_int(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::NotInt, dest, src);
    }

    pub fn emit_not_long(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::NotLong, dest, src);
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
        assert!(offset.to_usize() <= self.code.len());
        let distance = (self.code.len() - offset.to_usize()) as u32;
        self.emit_jmp(BytecodeOpcode::JumpLoop, distance);
    }

    pub fn emit_jump(&mut self, lbl: Label) {
        assert!(self.lookup_label(lbl).is_none());
        self.emit_jmp_forward(BytecodeOpcode::Jump, BytecodeOpcode::JumpConst, None, lbl);
    }

    pub fn emit_mod_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::ModInt, dest, lhs, rhs);
    }

    pub fn emit_mod_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::ModLong, dest, lhs, rhs);
    }

    pub fn emit_mul_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::MulInt, dest, lhs, rhs);
    }

    pub fn emit_mul_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::MulLong, dest, lhs, rhs);
    }

    pub fn emit_mul_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::MulFloat, dest, lhs, rhs);
    }

    pub fn emit_mul_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::MulDouble, dest, lhs, rhs);
    }

    pub fn emit_neg_int(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::NegInt, dest, src);
    }

    pub fn emit_neg_long(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::NegLong, dest, src);
    }

    pub fn emit_neg_float(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::NegFloat, dest, src);
    }

    pub fn emit_neg_double(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::NegDouble, dest, src);
    }

    pub fn emit_shl_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::ShlInt, dest, lhs, rhs);
    }

    pub fn emit_shl_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::ShlLong, dest, lhs, rhs);
    }

    pub fn emit_shr_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::ShrInt, dest, lhs, rhs);
    }

    pub fn emit_shr_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::ShrLong, dest, lhs, rhs);
    }

    pub fn emit_sar_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::SarInt, dest, lhs, rhs);
    }

    pub fn emit_sar_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::SarLong, dest, lhs, rhs);
    }

    pub fn emit_rol_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::RolInt, dest, lhs, rhs);
    }

    pub fn emit_rol_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::RolLong, dest, lhs, rhs);
    }

    pub fn emit_ror_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::RorInt, dest, lhs, rhs);
    }

    pub fn emit_ror_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::RorLong, dest, lhs, rhs);
    }

    pub fn emit_reinterpret_float_as_int(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::ReinterpretFloatAsInt, dest, src);
    }

    pub fn emit_reinterpret_int_as_float(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::ReinterpretIntAsFloat, dest, src);
    }

    pub fn emit_reinterpret_double_as_long(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::ReinterpretDoubleAsLong, dest, src);
    }

    pub fn emit_reinterpret_long_as_double(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::ReinterpretLongAsDouble, dest, src);
    }

    pub fn emit_extend_byte_to_char(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::ExtendByteToChar, dest, src);
    }
    pub fn emit_extend_byte_to_int(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::ExtendByteToInt, dest, src);
    }
    pub fn emit_extend_byte_to_long(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::ExtendByteToLong, dest, src);
    }
    pub fn emit_extend_char_to_long(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::ExtendCharToLong, dest, src);
    }
    pub fn emit_extend_int_to_long(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::ExtendIntToLong, dest, src);
    }

    pub fn emit_cast_char_to_int(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::CastCharToInt, dest, src);
    }
    pub fn emit_cast_int_to_byte(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::CastIntToByte, dest, src);
    }
    pub fn emit_cast_int_to_char(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::CastIntToChar, dest, src);
    }
    pub fn emit_cast_long_to_byte(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::CastLongToByte, dest, src);
    }
    pub fn emit_cast_long_to_char(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::CastLongToChar, dest, src);
    }
    pub fn emit_cast_long_to_int(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::CastLongToInt, dest, src);
    }

    pub fn emit_convert_int_to_float(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::ConvertIntToFloat, dest, src);
    }
    pub fn emit_convert_int_to_double(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::ConvertIntToDouble, dest, src);
    }
    pub fn emit_convert_long_to_float(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::ConvertLongToFloat, dest, src);
    }
    pub fn emit_convert_long_to_double(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::ConvertLongToDouble, dest, src);
    }

    pub fn emit_truncate_float_to_int(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::TruncateFloatToInt, dest, src);
    }
    pub fn emit_truncate_float_to_long(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::TruncateFloatToLong, dest, src);
    }
    pub fn emit_truncate_double_to_int(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::TruncateDoubleToInt, dest, src);
    }
    pub fn emit_truncate_double_to_long(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::TruncateDoubleToLong, dest, src);
    }

    pub fn emit_instance_of(&mut self, dest: Register, src: Register, cls_id: ClassDefId) {
        self.emit_reg2_cls(BytecodeOpcode::InstanceOf, dest, src, cls_id);
    }

    pub fn emit_checked_cast(&mut self, src: Register, cls_id: ClassDefId) {
        self.emit_reg1_cls(BytecodeOpcode::CheckedCast, src, cls_id);
    }

    pub fn emit_sub_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::SubInt, dest, lhs, rhs);
    }

    pub fn emit_sub_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::SubLong, dest, lhs, rhs);
    }

    pub fn emit_sub_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::SubFloat, dest, lhs, rhs);
    }

    pub fn emit_sub_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::SubDouble, dest, lhs, rhs);
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

    pub fn emit_mov_tuple(&mut self, dest: Register, src: Register, tuple_id: TupleId) {
        self.emit_reg2_tuple(BytecodeOpcode::MovPtr, dest, src, tuple_id);
    }

    pub fn emit_load_tuple_element(
        &mut self,
        dest: Register,
        src: Register,
        tuple_id: TupleId,
        element: u32,
    ) {
        self.emit_access_tuple(
            BytecodeOpcode::LoadTupleElement,
            dest,
            src,
            tuple_id,
            element,
        );
    }

    pub fn emit_store_tuple_element(
        &mut self,
        src: Register,
        dest: Register,
        tuple_id: TupleId,
        element: u32,
    ) {
        self.emit_access_tuple(
            BytecodeOpcode::LoadTupleElement,
            dest,
            src,
            tuple_id,
            element,
        );
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

    pub fn emit_test_eq_bool(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestEqBool, dest, lhs, rhs);
    }

    pub fn emit_test_eq_byte(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestEqByte, dest, lhs, rhs);
    }

    pub fn emit_test_eq_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestEqChar, dest, lhs, rhs);
    }

    pub fn emit_test_eq_enum(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestEqEnum, dest, lhs, rhs);
    }

    pub fn emit_test_eq_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestEqInt, dest, lhs, rhs);
    }

    pub fn emit_test_eq_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestEqLong, dest, lhs, rhs);
    }

    pub fn emit_test_eq_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestEqFloat, dest, lhs, rhs);
    }

    pub fn emit_test_eq_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestEqDouble, dest, lhs, rhs);
    }

    pub fn emit_test_eq_ptr(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestEqPtr, dest, lhs, rhs);
    }

    pub fn emit_test_ne_bool(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestNeBool, dest, lhs, rhs);
    }

    pub fn emit_test_ne_byte(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestNeByte, dest, lhs, rhs);
    }

    pub fn emit_test_ne_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestNeChar, dest, lhs, rhs);
    }

    pub fn emit_test_ne_enum(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestNeEnum, dest, lhs, rhs);
    }

    pub fn emit_test_ne_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestNeInt, dest, lhs, rhs);
    }

    pub fn emit_test_ne_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestNeLong, dest, lhs, rhs);
    }

    pub fn emit_test_ne_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestNeFloat, dest, lhs, rhs);
    }

    pub fn emit_test_ne_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestNeDouble, dest, lhs, rhs);
    }

    pub fn emit_test_ne_ptr(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestNePtr, dest, lhs, rhs);
    }

    pub fn emit_test_gt_byte(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestGtByte, dest, lhs, rhs);
    }

    pub fn emit_test_gt_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestGtChar, dest, lhs, rhs);
    }

    pub fn emit_test_gt_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestGtInt, dest, lhs, rhs);
    }

    pub fn emit_test_gt_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestGtLong, dest, lhs, rhs);
    }

    pub fn emit_test_gt_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestGtFloat, dest, lhs, rhs);
    }

    pub fn emit_test_gt_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestGtDouble, dest, lhs, rhs);
    }

    pub fn emit_test_ge_byte(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestGeByte, dest, lhs, rhs);
    }

    pub fn emit_test_ge_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestGeChar, dest, lhs, rhs);
    }

    pub fn emit_test_ge_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestGeInt, dest, lhs, rhs);
    }

    pub fn emit_test_ge_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestGeLong, dest, lhs, rhs);
    }

    pub fn emit_test_ge_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestGeFloat, dest, lhs, rhs);
    }

    pub fn emit_test_ge_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestGeDouble, dest, lhs, rhs);
    }

    pub fn emit_test_lt_byte(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestLtByte, dest, lhs, rhs);
    }

    pub fn emit_test_lt_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestLtChar, dest, lhs, rhs);
    }

    pub fn emit_test_lt_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestLtInt, dest, lhs, rhs);
    }

    pub fn emit_test_lt_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestLtLong, dest, lhs, rhs);
    }

    pub fn emit_test_lt_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestLtFloat, dest, lhs, rhs);
    }

    pub fn emit_test_lt_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestLtDouble, dest, lhs, rhs);
    }

    pub fn emit_test_le_byte(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestLeByte, dest, lhs, rhs);
    }

    pub fn emit_test_le_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestLeChar, dest, lhs, rhs);
    }

    pub fn emit_test_le_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestLeInt, dest, lhs, rhs);
    }

    pub fn emit_test_le_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestLeLong, dest, lhs, rhs);
    }

    pub fn emit_test_le_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestLeFloat, dest, lhs, rhs);
    }

    pub fn emit_test_le_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestLeDouble, dest, lhs, rhs);
    }

    pub fn emit_assert(&mut self, value: Register) {
        self.emit_reg1(BytecodeOpcode::Assert, value);
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

    pub fn emit_store_global_bool(&mut self, src: Register, gid: GlobalId) {
        self.emit_store_global(BytecodeOpcode::StoreGlobalBool, src, gid);
    }

    pub fn emit_store_global_byte(&mut self, src: Register, gid: GlobalId) {
        self.emit_store_global(BytecodeOpcode::StoreGlobalByte, src, gid);
    }

    pub fn emit_store_global_char(&mut self, src: Register, gid: GlobalId) {
        self.emit_store_global(BytecodeOpcode::StoreGlobalChar, src, gid);
    }

    pub fn emit_store_global_int(&mut self, src: Register, gid: GlobalId) {
        self.emit_store_global(BytecodeOpcode::StoreGlobalInt, src, gid);
    }

    pub fn emit_store_global_long(&mut self, src: Register, gid: GlobalId) {
        self.emit_store_global(BytecodeOpcode::StoreGlobalLong, src, gid);
    }

    pub fn emit_store_global_float(&mut self, src: Register, gid: GlobalId) {
        self.emit_store_global(BytecodeOpcode::StoreGlobalFloat, src, gid);
    }

    pub fn emit_store_global_double(&mut self, src: Register, gid: GlobalId) {
        self.emit_store_global(BytecodeOpcode::StoreGlobalDouble, src, gid);
    }

    pub fn emit_store_global_ptr(&mut self, src: Register, gid: GlobalId) {
        self.emit_store_global(BytecodeOpcode::StoreGlobalPtr, src, gid);
    }

    pub fn emit_push_register(&mut self, src: Register) {
        self.emit_reg1(BytecodeOpcode::PushRegister, src);
    }

    pub fn emit_invoke_direct_void(&mut self, fid: FctDefId, num: usize) {
        self.emit_fct_void(BytecodeOpcode::InvokeDirectVoid, fid, num);
    }

    pub fn emit_invoke_direct_bool(&mut self, dest: Register, fid: FctDefId, num: usize) {
        self.emit_fct(BytecodeOpcode::InvokeDirectBool, dest, fid, num);
    }

    pub fn emit_invoke_direct_byte(&mut self, dest: Register, fid: FctDefId, num: usize) {
        self.emit_fct(BytecodeOpcode::InvokeDirectByte, dest, fid, num);
    }

    pub fn emit_invoke_direct_char(&mut self, dest: Register, fid: FctDefId, num: usize) {
        self.emit_fct(BytecodeOpcode::InvokeDirectChar, dest, fid, num);
    }

    pub fn emit_invoke_direct_int(&mut self, dest: Register, fid: FctDefId, num: usize) {
        self.emit_fct(BytecodeOpcode::InvokeDirectInt, dest, fid, num);
    }

    pub fn emit_invoke_direct_long(&mut self, dest: Register, fid: FctDefId, num: usize) {
        self.emit_fct(BytecodeOpcode::InvokeDirectLong, dest, fid, num);
    }

    pub fn emit_invoke_direct_float(&mut self, dest: Register, fid: FctDefId, num: usize) {
        self.emit_fct(BytecodeOpcode::InvokeDirectFloat, dest, fid, num);
    }

    pub fn emit_invoke_direct_double(&mut self, dest: Register, fid: FctDefId, num: usize) {
        self.emit_fct(BytecodeOpcode::InvokeDirectDouble, dest, fid, num);
    }

    pub fn emit_invoke_direct_ptr(&mut self, dest: Register, fid: FctDefId, num: usize) {
        self.emit_fct(BytecodeOpcode::InvokeDirectPtr, dest, fid, num);
    }

    pub fn emit_invoke_virtual_void(&mut self, fid: FctDefId, num: usize) {
        self.emit_fct_void(BytecodeOpcode::InvokeVirtualVoid, fid, num);
    }

    pub fn emit_invoke_virtual_bool(&mut self, dest: Register, fid: FctDefId, num: usize) {
        self.emit_fct(BytecodeOpcode::InvokeVirtualBool, dest, fid, num);
    }

    pub fn emit_invoke_virtual_byte(&mut self, dest: Register, fid: FctDefId, num: usize) {
        self.emit_fct(BytecodeOpcode::InvokeVirtualByte, dest, fid, num);
    }

    pub fn emit_invoke_virtual_char(&mut self, dest: Register, fid: FctDefId, num: usize) {
        self.emit_fct(BytecodeOpcode::InvokeVirtualChar, dest, fid, num);
    }

    pub fn emit_invoke_virtual_int(&mut self, dest: Register, fid: FctDefId, num: usize) {
        self.emit_fct(BytecodeOpcode::InvokeVirtualInt, dest, fid, num);
    }

    pub fn emit_invoke_virtual_long(&mut self, dest: Register, fid: FctDefId, num: usize) {
        self.emit_fct(BytecodeOpcode::InvokeVirtualLong, dest, fid, num);
    }

    pub fn emit_invoke_virtual_float(&mut self, dest: Register, fid: FctDefId, num: usize) {
        self.emit_fct(BytecodeOpcode::InvokeVirtualFloat, dest, fid, num);
    }

    pub fn emit_invoke_virtual_double(&mut self, dest: Register, fid: FctDefId, num: usize) {
        self.emit_fct(BytecodeOpcode::InvokeVirtualDouble, dest, fid, num);
    }

    pub fn emit_invoke_virtual_ptr(&mut self, dest: Register, fid: FctDefId, num: usize) {
        self.emit_fct(BytecodeOpcode::InvokeVirtualPtr, dest, fid, num);
    }

    pub fn emit_invoke_static_void(&mut self, fid: FctDefId, num: usize) {
        self.emit_fct_void(BytecodeOpcode::InvokeStaticVoid, fid, num);
    }

    pub fn emit_invoke_static_bool(&mut self, dest: Register, fid: FctDefId, num: usize) {
        self.emit_fct(BytecodeOpcode::InvokeStaticBool, dest, fid, num);
    }

    pub fn emit_invoke_static_byte(&mut self, dest: Register, fid: FctDefId, num: usize) {
        self.emit_fct(BytecodeOpcode::InvokeStaticByte, dest, fid, num);
    }

    pub fn emit_invoke_static_char(&mut self, dest: Register, fid: FctDefId, num: usize) {
        self.emit_fct(BytecodeOpcode::InvokeStaticChar, dest, fid, num);
    }

    pub fn emit_invoke_static_int(&mut self, dest: Register, fid: FctDefId, num: usize) {
        self.emit_fct(BytecodeOpcode::InvokeStaticInt, dest, fid, num);
    }

    pub fn emit_invoke_static_long(&mut self, dest: Register, fid: FctDefId, num: usize) {
        self.emit_fct(BytecodeOpcode::InvokeStaticLong, dest, fid, num);
    }

    pub fn emit_invoke_static_float(&mut self, dest: Register, fid: FctDefId, num: usize) {
        self.emit_fct(BytecodeOpcode::InvokeStaticFloat, dest, fid, num);
    }

    pub fn emit_invoke_static_double(&mut self, dest: Register, fid: FctDefId, num: usize) {
        self.emit_fct(BytecodeOpcode::InvokeStaticDouble, dest, fid, num);
    }

    pub fn emit_invoke_static_ptr(&mut self, dest: Register, fid: FctDefId, num: usize) {
        self.emit_fct(BytecodeOpcode::InvokeStaticPtr, dest, fid, num);
    }

    pub fn emit_new_object(&mut self, dest: Register, cls_id: ClassDefId) {
        self.emit_new(BytecodeOpcode::NewObject, dest, cls_id);
    }
    pub fn emit_new_array(&mut self, dest: Register, cls_id: ClassDefId, length: Register) {
        self.emit_new_arr(BytecodeOpcode::NewArray, dest, cls_id, length);
    }

    pub fn emit_nil_check(&mut self, obj: Register) {
        self.emit_reg1(BytecodeOpcode::NilCheck, obj);
    }

    pub fn emit_array_length(&mut self, dest: Register, array: Register) {
        self.emit_reg2(BytecodeOpcode::ArrayLength, dest, array);
    }
    pub fn emit_array_bound_check(&mut self, arr: Register, idx: Register) {
        self.emit_reg2(BytecodeOpcode::ArrayBoundCheck, arr, idx);
    }

    pub fn emit_store_array_byte(&mut self, src: Register, array: Register, idx: Register) {
        self.emit_reg3(BytecodeOpcode::StoreArrayByte, src, array, idx);
    }
    pub fn emit_store_array_bool(&mut self, src: Register, array: Register, index: Register) {
        self.emit_reg3(BytecodeOpcode::StoreArrayBool, src, array, index);
    }
    pub fn emit_store_array_char(&mut self, src: Register, array: Register, index: Register) {
        self.emit_reg3(BytecodeOpcode::StoreArrayChar, src, array, index);
    }
    pub fn emit_store_array_int(&mut self, src: Register, array: Register, index: Register) {
        self.emit_reg3(BytecodeOpcode::StoreArrayInt, src, array, index);
    }
    pub fn emit_store_array_long(&mut self, src: Register, array: Register, index: Register) {
        self.emit_reg3(BytecodeOpcode::StoreArrayLong, src, array, index);
    }
    pub fn emit_store_array_float(&mut self, src: Register, array: Register, index: Register) {
        self.emit_reg3(BytecodeOpcode::StoreArrayFloat, src, array, index);
    }
    pub fn emit_store_array_double(&mut self, src: Register, array: Register, index: Register) {
        self.emit_reg3(BytecodeOpcode::StoreArrayDouble, src, array, index);
    }
    pub fn emit_store_array_ptr(&mut self, src: Register, array: Register, index: Register) {
        self.emit_reg3(BytecodeOpcode::StoreArrayPtr, src, array, index);
    }

    pub fn emit_load_array_byte(&mut self, dest: Register, array: Register, idx: Register) {
        self.emit_reg3(BytecodeOpcode::LoadArrayByte, dest, array, idx);
    }
    pub fn emit_load_array_bool(&mut self, dest: Register, array: Register, index: Register) {
        self.emit_reg3(BytecodeOpcode::LoadArrayBool, dest, array, index);
    }
    pub fn emit_load_array_char(&mut self, dest: Register, array: Register, index: Register) {
        self.emit_reg3(BytecodeOpcode::LoadArrayChar, dest, array, index);
    }
    pub fn emit_load_array_int(&mut self, dest: Register, array: Register, index: Register) {
        self.emit_reg3(BytecodeOpcode::LoadArrayInt, dest, array, index);
    }
    pub fn emit_load_array_long(&mut self, dest: Register, array: Register, index: Register) {
        self.emit_reg3(BytecodeOpcode::LoadArrayLong, dest, array, index);
    }
    pub fn emit_load_array_float(&mut self, dest: Register, array: Register, index: Register) {
        self.emit_reg3(BytecodeOpcode::LoadArrayFloat, dest, array, index);
    }
    pub fn emit_load_array_double(&mut self, dest: Register, array: Register, index: Register) {
        self.emit_reg3(BytecodeOpcode::LoadArrayDouble, dest, array, index);
    }
    pub fn emit_load_array_ptr(&mut self, dest: Register, array: Register, index: Register) {
        self.emit_reg3(BytecodeOpcode::LoadArrayPtr, dest, array, index);
    }

    pub fn generate(mut self) -> BytecodeFunction {
        self.resolve_forward_jumps();

        BytecodeFunction::new(
            self.code,
            self.const_pool,
            self.registers,
            self.arguments,
            self.positions,
        )
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

                self.code[start.to_usize()] = inst_imm as u8;
                self.code[start.to_usize() + jump_target] = distance as u8;
            } else {
                self.patch_const(const_idx, ConstPoolEntry::Int(distance as i32));
            }
        }
    }

    fn emit_reg3(&mut self, inst: BytecodeOpcode, r1: Register, r2: Register, r3: Register) {
        let values = [
            r1.to_usize() as u32,
            r2.to_usize() as u32,
            r3.to_usize() as u32,
        ];
        self.emit_values(inst, &values);
    }

    fn emit_reg2(&mut self, inst: BytecodeOpcode, r1: Register, r2: Register) {
        let values = [r1.to_usize() as u32, r2.to_usize() as u32];
        self.emit_values(inst, &values);
    }

    fn emit_reg2_tuple(
        &mut self,
        inst: BytecodeOpcode,
        r1: Register,
        r2: Register,
        tuple_id: TupleId,
    ) {
        let values = [
            r1.to_usize() as u32,
            r2.to_usize() as u32,
            tuple_id.to_usize() as u32,
        ];
        self.emit_values(inst, &values);
    }

    fn emit_reg2_cls(
        &mut self,
        inst: BytecodeOpcode,
        r1: Register,
        r2: Register,
        cls_id: ClassDefId,
    ) {
        let values = [
            r1.to_usize() as u32,
            r2.to_usize() as u32,
            cls_id.to_usize() as u32,
        ];
        self.emit_values(inst, &values);
    }

    fn emit_reg1(&mut self, inst: BytecodeOpcode, r1: Register) {
        let values = [r1.to_usize() as u32];
        self.emit_values(inst, &values);
    }

    fn emit_reg1_cls(&mut self, inst: BytecodeOpcode, r1: Register, cls_id: ClassDefId) {
        let values = [r1.to_usize() as u32, cls_id.to_usize() as u32];
        self.emit_values(inst, &values);
    }

    fn emit_reg1_idx(&mut self, inst: BytecodeOpcode, r1: Register, idx: ConstPoolIdx) {
        let values = [r1.to_usize() as u32, idx.to_usize() as u32];
        self.emit_values(inst, &values);
    }

    fn emit_reg1_byte(&mut self, inst: BytecodeOpcode, r1: Register, value: u8) {
        let values = [r1.to_usize() as u32];
        self.emit_values(inst, &values);
        self.emit_u8(value);
    }

    fn add_const(&mut self, value: ConstPoolEntry) -> ConstPoolIdx {
        let idx = self.const_pool.len();
        self.const_pool.push(value);
        idx.into()
    }

    fn emit_new(&mut self, inst: BytecodeOpcode, r1: Register, cid: ClassDefId) {
        let values = [r1.to_usize() as u32, cid.to_usize() as u32];
        self.emit_values(inst, &values);
    }

    fn emit_new_arr(&mut self, inst: BytecodeOpcode, r1: Register, cid: ClassDefId, lth: Register) {
        let values = [
            r1.to_usize() as u32,
            cid.to_usize() as u32,
            lth.to_usize() as u32,
        ];
        self.emit_values(inst, &values);
    }

    fn emit_fct_void(&mut self, inst: BytecodeOpcode, fid: FctDefId, cnt: usize) {
        let values = [fid.to_usize() as u32, cnt as u32];
        self.emit_values(inst, &values);
    }

    fn emit_fct(&mut self, inst: BytecodeOpcode, r1: Register, fid: FctDefId, cnt: usize) {
        let values = [r1.to_usize() as u32, fid.to_usize() as u32, cnt as u32];
        self.emit_values(inst, &values);
    }

    fn emit_access_field(
        &mut self,
        inst: BytecodeOpcode,
        r1: Register,
        r2: Register,
        cid: ClassDefId,
        fid: FieldId,
    ) {
        let values = [
            r1.to_usize() as u32,
            r2.to_usize() as u32,
            cid.to_usize() as u32,
            fid.to_usize() as u32,
        ];
        self.emit_values(inst, &values);
    }

    fn emit_access_tuple(
        &mut self,
        inst: BytecodeOpcode,
        r1: Register,
        r2: Register,
        tuple_id: TupleId,
        element: u32,
    ) {
        let values = [
            r1.to_usize() as u32,
            r2.to_usize() as u32,
            tuple_id.to_usize() as u32,
            element,
        ];
        self.emit_values(inst, &values);
    }

    fn emit_load_global(&mut self, inst: BytecodeOpcode, r1: Register, gid: GlobalId) {
        let values = [r1.to_usize() as u32, gid.to_usize() as u32];
        self.emit_values(inst, &values);
    }

    fn emit_store_global(&mut self, inst: BytecodeOpcode, r1: Register, gid: GlobalId) {
        let values = [r1.to_usize() as u32, gid.to_usize() as u32];
        self.emit_values(inst, &values);
    }

    pub fn set_position(&mut self, pos: Position) {
        self.position = Some(pos);
    }

    fn emit_op(&mut self, inst: BytecodeOpcode) {
        let values = [];
        self.emit_values(inst, &values);
    }

    fn emit_position(&mut self) {
        let offset = self.code.len() as u32;
        if self.position.is_none() {
            return;
        }

        let position = self.position.unwrap();
        let last_position = self.positions.last().map(|(_, p)| p);

        if let Some(last_position) = last_position {
            if *last_position == position {
                return;
            }
        }

        self.positions.push((offset, position));
        self.position = None;
    }

    fn emit_values(&mut self, op: BytecodeOpcode, values: &[u32]) {
        if op.need_position() {
            self.emit_position();
        }

        let is_wide = op as u32 > u8::max_value() as u32
            || values.iter().any(|&val| val > u8::max_value() as u32);

        if is_wide {
            self.emit_wide();
            self.emit_u32(op as u32);
            for &value in values {
                self.emit_u32(value);
            }
        } else {
            self.emit_u8(op as u8);
            for &value in values {
                self.emit_u8(value as u8);
            }
        }
    }

    fn emit_wide(&mut self) {
        self.code.push(BytecodeOpcode::Wide as u8);
    }

    fn emit_u8(&mut self, value: u8) {
        self.code.push(value);
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
        self.emit_values(inst, &[offset]);
    }

    fn bytecode_at(&self, offset: BytecodeOffset) -> BytecodeOpcode {
        FromPrimitive::from_u8(self.code[offset.to_usize()]).expect("unknown bytecode")
    }

    fn emit_u32(&mut self, value: u32) {
        self.code.push((value & 0xFF) as u8);
        self.code.push(((value >> 8) & 0xFF) as u8);
        self.code.push(((value >> 16) & 0xFF) as u8);
        self.code.push(((value >> 24) & 0xFF) as u8);
    }

    fn patch_u32(&mut self, offset: BytecodeOffset, value: u32) {
        let offset = offset.to_usize();
        self.code[offset] = (value & 0xFF) as u8;
        self.code[offset + 1] = ((value >> 8) & 0xFF) as u8;
        self.code[offset + 2] = ((value >> 16) & 0xFF) as u8;
        self.code[offset + 3] = ((value >> 24) & 0xFF) as u8;
    }

    fn patch_const(&mut self, idx: ConstPoolIdx, entry: ConstPoolEntry) {
        self.const_pool[idx.to_usize()] = entry;
    }
}

fn fits_u8(value: u32) -> bool {
    value <= u8::max_value() as u32
}
