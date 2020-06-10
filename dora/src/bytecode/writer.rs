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

    pub fn emit_add_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::AddInt32, dest, lhs, rhs);
    }

    pub fn emit_add_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::AddInt64, dest, lhs, rhs);
    }

    pub fn emit_add_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::AddFloat32, dest, lhs, rhs);
    }

    pub fn emit_add_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::AddFloat64, dest, lhs, rhs);
    }

    pub fn emit_and_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::AndInt32, dest, lhs, rhs);
    }

    pub fn emit_and_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::AndInt64, dest, lhs, rhs);
    }

    pub fn emit_or_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::OrInt32, dest, lhs, rhs);
    }

    pub fn emit_or_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::OrInt64, dest, lhs, rhs);
    }

    pub fn emit_xor_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::XorInt32, dest, lhs, rhs);
    }

    pub fn emit_xor_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::XorInt64, dest, lhs, rhs);
    }

    pub fn emit_div_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::DivInt32, dest, lhs, rhs);
    }

    pub fn emit_div_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::DivInt64, dest, lhs, rhs);
    }

    pub fn emit_div_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::DivFloat32, dest, lhs, rhs);
    }

    pub fn emit_div_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::DivFloat64, dest, lhs, rhs);
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

    pub fn emit_load_field_uint8(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_access_field(BytecodeOpcode::LoadFieldUInt8, dest, obj, cls, field);
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

    pub fn emit_load_field_int32(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_access_field(BytecodeOpcode::LoadFieldInt32, dest, obj, cls, field);
    }

    pub fn emit_load_field_int64(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_access_field(BytecodeOpcode::LoadFieldInt64, dest, obj, cls, field);
    }

    pub fn emit_load_field_float32(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_access_field(BytecodeOpcode::LoadFieldFloat32, dest, obj, cls, field);
    }

    pub fn emit_load_field_float64(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_access_field(BytecodeOpcode::LoadFieldFloat64, dest, obj, cls, field);
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

    pub fn emit_load_field_tuple(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_access_field(BytecodeOpcode::LoadFieldTuple, dest, obj, cls, field);
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

    pub fn emit_store_field_uint8(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_access_field(BytecodeOpcode::StoreFieldUInt8, src, obj, cls, field);
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

    pub fn emit_store_field_int32(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_access_field(BytecodeOpcode::StoreFieldInt32, src, obj, cls, field);
    }

    pub fn emit_store_field_int64(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_access_field(BytecodeOpcode::StoreFieldInt64, src, obj, cls, field);
    }

    pub fn emit_store_field_float32(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_access_field(BytecodeOpcode::StoreFieldFloat32, src, obj, cls, field);
    }

    pub fn emit_store_field_float64(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_access_field(BytecodeOpcode::StoreFieldFloat64, src, obj, cls, field);
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

    pub fn emit_store_field_tuple(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_access_field(BytecodeOpcode::StoreFieldTuple, src, obj, cls, field);
    }

    pub fn emit_const_nil(&mut self, dest: Register) {
        self.emit_reg1(BytecodeOpcode::ConstNil, dest);
    }

    pub fn emit_const_char(&mut self, dest: Register, value: char) {
        let idx = self.add_const(ConstPoolEntry::Char(value));
        self.emit_reg1_idx(BytecodeOpcode::ConstChar, dest, idx);
    }

    pub fn emit_const_uint8(&mut self, dest: Register, value: u8) {
        self.emit_reg1_uint8(BytecodeOpcode::ConstUInt8, dest, value);
    }

    pub fn emit_const_int32(&mut self, dest: Register, value: i32) {
        let idx = self.add_const(ConstPoolEntry::Int32(value as i32));
        self.emit_reg1_idx(BytecodeOpcode::ConstInt32, dest, idx);
    }

    pub fn emit_const_int64(&mut self, dest: Register, value: i64) {
        let idx = self.add_const(ConstPoolEntry::Int64(value as i64));
        self.emit_reg1_idx(BytecodeOpcode::ConstInt64, dest, idx);
    }

    pub fn emit_const_float32(&mut self, dest: Register, value: f32) {
        let idx = self.add_const(ConstPoolEntry::Float32(value));
        self.emit_reg1_idx(BytecodeOpcode::ConstFloat32, dest, idx);
    }

    pub fn emit_const_float64(&mut self, dest: Register, value: f64) {
        let idx = self.add_const(ConstPoolEntry::Float64(value));
        self.emit_reg1_idx(BytecodeOpcode::ConstFloat64, dest, idx);
    }

    pub fn emit_const_string(&mut self, dest: Register, value: String) {
        let idx = self.add_const(ConstPoolEntry::String(value));
        self.emit_reg1_idx(BytecodeOpcode::ConstString, dest, idx);
    }

    pub fn emit_const_zero_uint8(&mut self, dest: Register) {
        self.emit_reg1(BytecodeOpcode::ConstZeroUInt8, dest);
    }

    pub fn emit_const_zero_int32(&mut self, dest: Register) {
        self.emit_reg1(BytecodeOpcode::ConstZeroInt32, dest);
    }

    pub fn emit_const_zero_int64(&mut self, dest: Register) {
        self.emit_reg1(BytecodeOpcode::ConstZeroInt64, dest);
    }

    pub fn emit_const_zero_float32(&mut self, dest: Register) {
        self.emit_reg1(BytecodeOpcode::ConstZeroFloat32, dest);
    }

    pub fn emit_const_zero_float64(&mut self, dest: Register) {
        self.emit_reg1(BytecodeOpcode::ConstZeroFloat64, dest);
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

    pub fn emit_not_int32(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::NotInt32, dest, src);
    }

    pub fn emit_not_int64(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::NotInt64, dest, src);
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

    pub fn emit_mod_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::ModInt32, dest, lhs, rhs);
    }

    pub fn emit_mod_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::ModInt64, dest, lhs, rhs);
    }

    pub fn emit_mul_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::MulInt32, dest, lhs, rhs);
    }

    pub fn emit_mul_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::MulInt64, dest, lhs, rhs);
    }

    pub fn emit_mul_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::MulFloat32, dest, lhs, rhs);
    }

    pub fn emit_mul_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::MulFloat64, dest, lhs, rhs);
    }

    pub fn emit_neg_int32(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::NegInt32, dest, src);
    }

    pub fn emit_neg_int64(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::NegInt64, dest, src);
    }

    pub fn emit_neg_float32(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::NegFloat32, dest, src);
    }

    pub fn emit_neg_float64(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::NegFloat64, dest, src);
    }

    pub fn emit_shl_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::ShlInt32, dest, lhs, rhs);
    }

    pub fn emit_shl_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::ShlInt64, dest, lhs, rhs);
    }

    pub fn emit_shr_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::ShrInt32, dest, lhs, rhs);
    }

    pub fn emit_shr_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::ShrInt64, dest, lhs, rhs);
    }

    pub fn emit_sar_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::SarInt32, dest, lhs, rhs);
    }

    pub fn emit_sar_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::SarInt64, dest, lhs, rhs);
    }

    pub fn emit_rol_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::RolInt32, dest, lhs, rhs);
    }

    pub fn emit_rol_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::RolInt64, dest, lhs, rhs);
    }

    pub fn emit_ror_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::RorInt32, dest, lhs, rhs);
    }

    pub fn emit_ror_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::RorInt64, dest, lhs, rhs);
    }

    pub fn emit_reinterpret_float32_as_int32(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::ReinterpretFloat32AsInt32, dest, src);
    }

    pub fn emit_reinterpret_int32_as_float32(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::ReinterpretInt32AsFloat32, dest, src);
    }

    pub fn emit_reinterpret_float64_as_int64(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::ReinterpretFloat64AsInt64, dest, src);
    }

    pub fn emit_reinterpret_int64_as_float64(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::ReinterpretInt64AsFloat64, dest, src);
    }

    pub fn emit_extend_byte_to_char(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::ExtendUInt8ToChar, dest, src);
    }
    pub fn emit_extend_byte_to_int32(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::ExtendUInt8ToInt32, dest, src);
    }
    pub fn emit_extend_byte_to_int64(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::ExtendUInt8ToInt64, dest, src);
    }
    pub fn emit_extend_char_to_int64(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::ExtendCharToInt64, dest, src);
    }
    pub fn emit_extend_int32_to_int64(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::ExtendInt32ToInt64, dest, src);
    }

    pub fn emit_cast_char_to_int32(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::CastCharToInt32, dest, src);
    }
    pub fn emit_cast_int32_to_uint8(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::CastInt32ToUInt8, dest, src);
    }
    pub fn emit_cast_int32_to_char(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::CastInt32ToChar, dest, src);
    }
    pub fn emit_cast_int64_to_uint8(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::CastInt64ToUInt8, dest, src);
    }
    pub fn emit_cast_int64_to_char(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::CastInt64ToChar, dest, src);
    }
    pub fn emit_cast_int64_to_int32(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::CastInt64ToInt32, dest, src);
    }

    pub fn emit_convert_int32_to_float32(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::ConvertInt32ToFloat32, dest, src);
    }
    pub fn emit_convert_int32_to_float64(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::ConvertInt32ToFloat64, dest, src);
    }
    pub fn emit_convert_int64_to_float32(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::ConvertInt64ToFloat32, dest, src);
    }
    pub fn emit_convert_int64_to_float64(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::ConvertInt64ToFloat64, dest, src);
    }

    pub fn emit_truncate_float32_to_int32(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::TruncateFloat32ToInt32, dest, src);
    }
    pub fn emit_truncate_float32_to_int64(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::TruncateFloat32ToInt64, dest, src);
    }
    pub fn emit_truncate_float64_to_int32(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::TruncateFloat64ToInt32, dest, src);
    }
    pub fn emit_truncate_float64_to_int64(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::TruncateFloat64ToInt64, dest, src);
    }

    pub fn emit_promote_float32_to_float64(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::PromoteFloat32ToFloat64, dest, src);
    }
    pub fn emit_demote_float64_to_float32(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::DemoteFloat64ToFloat32, dest, src);
    }

    pub fn emit_instance_of(&mut self, dest: Register, src: Register, cls_id: ClassDefId) {
        self.emit_reg2_cls(BytecodeOpcode::InstanceOf, dest, src, cls_id);
    }

    pub fn emit_checked_cast(&mut self, src: Register, cls_id: ClassDefId) {
        self.emit_reg1_cls(BytecodeOpcode::CheckedCast, src, cls_id);
    }

    pub fn emit_sub_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::SubInt32, dest, lhs, rhs);
    }

    pub fn emit_sub_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::SubInt64, dest, lhs, rhs);
    }

    pub fn emit_sub_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::SubFloat32, dest, lhs, rhs);
    }

    pub fn emit_sub_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::SubFloat64, dest, lhs, rhs);
    }

    pub fn emit_mov_bool(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::MovBool, dest, src);
    }

    pub fn emit_mov_uint8(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::MovUInt8, dest, src);
    }

    pub fn emit_mov_char(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::MovChar, dest, src);
    }

    pub fn emit_mov_int32(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::MovInt32, dest, src);
    }

    pub fn emit_mov_int64(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::MovInt64, dest, src);
    }

    pub fn emit_mov_float32(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::MovFloat32, dest, src);
    }

    pub fn emit_mov_float64(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::MovFloat64, dest, src);
    }

    pub fn emit_mov_ptr(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::MovPtr, dest, src);
    }

    pub fn emit_mov_tuple(&mut self, dest: Register, src: Register, tuple_id: TupleId) {
        self.emit_reg2_tuple(BytecodeOpcode::MovTuple, dest, src, tuple_id);
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

    pub fn emit_ret_bool(&mut self, src: Register) {
        self.emit_reg1(BytecodeOpcode::RetBool, src);
    }

    pub fn emit_ret_uint8(&mut self, src: Register) {
        self.emit_reg1(BytecodeOpcode::RetUInt8, src);
    }

    pub fn emit_ret_char(&mut self, src: Register) {
        self.emit_reg1(BytecodeOpcode::RetChar, src);
    }

    pub fn emit_ret_int32(&mut self, src: Register) {
        self.emit_reg1(BytecodeOpcode::RetInt32, src);
    }

    pub fn emit_ret_int64(&mut self, src: Register) {
        self.emit_reg1(BytecodeOpcode::RetInt64, src);
    }

    pub fn emit_ret_float32(&mut self, src: Register) {
        self.emit_reg1(BytecodeOpcode::RetFloat32, src);
    }

    pub fn emit_ret_float64(&mut self, src: Register) {
        self.emit_reg1(BytecodeOpcode::RetFloat64, src);
    }

    pub fn emit_ret_ptr(&mut self, src: Register) {
        self.emit_reg1(BytecodeOpcode::RetPtr, src);
    }

    pub fn emit_ret_tuple(&mut self, src: Register) {
        self.emit_reg1(BytecodeOpcode::RetTuple, src);
    }

    pub fn emit_ret_void(&mut self) {
        self.emit_op(BytecodeOpcode::RetVoid);
    }

    pub fn emit_test_eq_bool(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestEqBool, dest, lhs, rhs);
    }

    pub fn emit_test_eq_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestEqUInt8, dest, lhs, rhs);
    }

    pub fn emit_test_eq_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestEqChar, dest, lhs, rhs);
    }

    pub fn emit_test_eq_enum(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestEqEnum, dest, lhs, rhs);
    }

    pub fn emit_test_eq_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestEqInt32, dest, lhs, rhs);
    }

    pub fn emit_test_eq_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestEqInt64, dest, lhs, rhs);
    }

    pub fn emit_test_eq_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestEqFloat32, dest, lhs, rhs);
    }

    pub fn emit_test_eq_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestEqFloat64, dest, lhs, rhs);
    }

    pub fn emit_test_eq_ptr(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestEqPtr, dest, lhs, rhs);
    }

    pub fn emit_test_ne_bool(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestNeBool, dest, lhs, rhs);
    }

    pub fn emit_test_ne_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestNeUInt8, dest, lhs, rhs);
    }

    pub fn emit_test_ne_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestNeChar, dest, lhs, rhs);
    }

    pub fn emit_test_ne_enum(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestNeEnum, dest, lhs, rhs);
    }

    pub fn emit_test_ne_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestNeInt32, dest, lhs, rhs);
    }

    pub fn emit_test_ne_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestNeInt64, dest, lhs, rhs);
    }

    pub fn emit_test_ne_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestNeFloat32, dest, lhs, rhs);
    }

    pub fn emit_test_ne_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestNeFloat64, dest, lhs, rhs);
    }

    pub fn emit_test_ne_ptr(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestNePtr, dest, lhs, rhs);
    }

    pub fn emit_test_gt_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestGtUInt8, dest, lhs, rhs);
    }

    pub fn emit_test_gt_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestGtChar, dest, lhs, rhs);
    }

    pub fn emit_test_gt_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestGtInt32, dest, lhs, rhs);
    }

    pub fn emit_test_gt_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestGtInt64, dest, lhs, rhs);
    }

    pub fn emit_test_gt_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestGtFloat32, dest, lhs, rhs);
    }

    pub fn emit_test_gt_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestGtFloat64, dest, lhs, rhs);
    }

    pub fn emit_test_ge_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestGeUInt8, dest, lhs, rhs);
    }

    pub fn emit_test_ge_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestGeChar, dest, lhs, rhs);
    }

    pub fn emit_test_ge_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestGeInt32, dest, lhs, rhs);
    }

    pub fn emit_test_ge_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestGeInt64, dest, lhs, rhs);
    }

    pub fn emit_test_ge_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestGeFloat32, dest, lhs, rhs);
    }

    pub fn emit_test_ge_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestGeFloat64, dest, lhs, rhs);
    }

    pub fn emit_test_lt_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestLtUInt8, dest, lhs, rhs);
    }

    pub fn emit_test_lt_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestLtChar, dest, lhs, rhs);
    }

    pub fn emit_test_lt_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestLtInt32, dest, lhs, rhs);
    }

    pub fn emit_test_lt_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestLtInt64, dest, lhs, rhs);
    }

    pub fn emit_test_lt_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestLtFloat32, dest, lhs, rhs);
    }

    pub fn emit_test_lt_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestLtFloat64, dest, lhs, rhs);
    }

    pub fn emit_test_le_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestLeUInt8, dest, lhs, rhs);
    }

    pub fn emit_test_le_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestLeChar, dest, lhs, rhs);
    }

    pub fn emit_test_le_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestLeInt32, dest, lhs, rhs);
    }

    pub fn emit_test_le_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestLeInt64, dest, lhs, rhs);
    }

    pub fn emit_test_le_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestLeFloat32, dest, lhs, rhs);
    }

    pub fn emit_test_le_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestLeFloat64, dest, lhs, rhs);
    }

    pub fn emit_assert(&mut self, value: Register) {
        self.emit_reg1(BytecodeOpcode::Assert, value);
    }

    pub fn emit_load_global_bool(&mut self, dest: Register, gid: GlobalId) {
        self.emit_load_global(BytecodeOpcode::LoadGlobalBool, dest, gid);
    }

    pub fn emit_load_global_uint8(&mut self, dest: Register, gid: GlobalId) {
        self.emit_load_global(BytecodeOpcode::LoadGlobalUInt8, dest, gid);
    }

    pub fn emit_load_global_char(&mut self, dest: Register, gid: GlobalId) {
        self.emit_load_global(BytecodeOpcode::LoadGlobalChar, dest, gid);
    }

    pub fn emit_load_global_int32(&mut self, dest: Register, gid: GlobalId) {
        self.emit_load_global(BytecodeOpcode::LoadGlobalInt32, dest, gid);
    }

    pub fn emit_load_global_int64(&mut self, dest: Register, gid: GlobalId) {
        self.emit_load_global(BytecodeOpcode::LoadGlobalInt64, dest, gid);
    }

    pub fn emit_load_global_float32(&mut self, dest: Register, gid: GlobalId) {
        self.emit_load_global(BytecodeOpcode::LoadGlobalFloat32, dest, gid);
    }

    pub fn emit_load_global_float64(&mut self, dest: Register, gid: GlobalId) {
        self.emit_load_global(BytecodeOpcode::LoadGlobalFloat64, dest, gid);
    }

    pub fn emit_load_global_ptr(&mut self, dest: Register, gid: GlobalId) {
        self.emit_load_global(BytecodeOpcode::LoadGlobalPtr, dest, gid);
    }

    pub fn emit_load_global_tuple(&mut self, dest: Register, gid: GlobalId) {
        self.emit_load_global(BytecodeOpcode::LoadGlobalTuple, dest, gid);
    }

    pub fn emit_store_global_bool(&mut self, src: Register, gid: GlobalId) {
        self.emit_store_global(BytecodeOpcode::StoreGlobalBool, src, gid);
    }

    pub fn emit_store_global_uint8(&mut self, src: Register, gid: GlobalId) {
        self.emit_store_global(BytecodeOpcode::StoreGlobalUInt8, src, gid);
    }

    pub fn emit_store_global_char(&mut self, src: Register, gid: GlobalId) {
        self.emit_store_global(BytecodeOpcode::StoreGlobalChar, src, gid);
    }

    pub fn emit_store_global_int32(&mut self, src: Register, gid: GlobalId) {
        self.emit_store_global(BytecodeOpcode::StoreGlobalInt32, src, gid);
    }

    pub fn emit_store_global_int64(&mut self, src: Register, gid: GlobalId) {
        self.emit_store_global(BytecodeOpcode::StoreGlobalInt64, src, gid);
    }

    pub fn emit_store_global_float32(&mut self, src: Register, gid: GlobalId) {
        self.emit_store_global(BytecodeOpcode::StoreGlobalFloat32, src, gid);
    }

    pub fn emit_store_global_float64(&mut self, src: Register, gid: GlobalId) {
        self.emit_store_global(BytecodeOpcode::StoreGlobalFloat64, src, gid);
    }

    pub fn emit_store_global_ptr(&mut self, src: Register, gid: GlobalId) {
        self.emit_store_global(BytecodeOpcode::StoreGlobalPtr, src, gid);
    }

    pub fn emit_store_global_tuple(&mut self, src: Register, gid: GlobalId) {
        self.emit_store_global(BytecodeOpcode::StoreGlobalTuple, src, gid);
    }

    pub fn emit_push_register(&mut self, src: Register) {
        self.emit_reg1(BytecodeOpcode::PushRegister, src);
    }

    pub fn emit_invoke_direct_void(&mut self, fid: FctDefId) {
        self.emit_fct_void(BytecodeOpcode::InvokeDirectVoid, fid);
    }

    pub fn emit_invoke_direct_bool(&mut self, dest: Register, fid: FctDefId) {
        self.emit_fct(BytecodeOpcode::InvokeDirectBool, dest, fid);
    }

    pub fn emit_invoke_direct_uint8(&mut self, dest: Register, fid: FctDefId) {
        self.emit_fct(BytecodeOpcode::InvokeDirectUInt8, dest, fid);
    }

    pub fn emit_invoke_direct_char(&mut self, dest: Register, fid: FctDefId) {
        self.emit_fct(BytecodeOpcode::InvokeDirectChar, dest, fid);
    }

    pub fn emit_invoke_direct_int32(&mut self, dest: Register, fid: FctDefId) {
        self.emit_fct(BytecodeOpcode::InvokeDirectInt32, dest, fid);
    }

    pub fn emit_invoke_direct_int64(&mut self, dest: Register, fid: FctDefId) {
        self.emit_fct(BytecodeOpcode::InvokeDirectInt64, dest, fid);
    }

    pub fn emit_invoke_direct_float32(&mut self, dest: Register, fid: FctDefId) {
        self.emit_fct(BytecodeOpcode::InvokeDirectFloat32, dest, fid);
    }

    pub fn emit_invoke_direct_float64(&mut self, dest: Register, fid: FctDefId) {
        self.emit_fct(BytecodeOpcode::InvokeDirectFloat64, dest, fid);
    }

    pub fn emit_invoke_direct_ptr(&mut self, dest: Register, fid: FctDefId) {
        self.emit_fct(BytecodeOpcode::InvokeDirectPtr, dest, fid);
    }

    pub fn emit_invoke_direct_tuple(&mut self, dest: Register, fid: FctDefId) {
        self.emit_fct(BytecodeOpcode::InvokeDirectTuple, dest, fid);
    }

    pub fn emit_invoke_virtual_void(&mut self, fid: FctDefId) {
        self.emit_fct_void(BytecodeOpcode::InvokeVirtualVoid, fid);
    }

    pub fn emit_invoke_virtual_bool(&mut self, dest: Register, fid: FctDefId) {
        self.emit_fct(BytecodeOpcode::InvokeVirtualBool, dest, fid);
    }

    pub fn emit_invoke_virtual_uint8(&mut self, dest: Register, fid: FctDefId) {
        self.emit_fct(BytecodeOpcode::InvokeVirtualUInt8, dest, fid);
    }

    pub fn emit_invoke_virtual_char(&mut self, dest: Register, fid: FctDefId) {
        self.emit_fct(BytecodeOpcode::InvokeVirtualChar, dest, fid);
    }

    pub fn emit_invoke_virtual_int32(&mut self, dest: Register, fid: FctDefId) {
        self.emit_fct(BytecodeOpcode::InvokeVirtualInt32, dest, fid);
    }

    pub fn emit_invoke_virtual_int64(&mut self, dest: Register, fid: FctDefId) {
        self.emit_fct(BytecodeOpcode::InvokeVirtualInt64, dest, fid);
    }

    pub fn emit_invoke_virtual_float32(&mut self, dest: Register, fid: FctDefId) {
        self.emit_fct(BytecodeOpcode::InvokeVirtualFloat32, dest, fid);
    }

    pub fn emit_invoke_virtual_float64(&mut self, dest: Register, fid: FctDefId) {
        self.emit_fct(BytecodeOpcode::InvokeVirtualFloat64, dest, fid);
    }

    pub fn emit_invoke_virtual_ptr(&mut self, dest: Register, fid: FctDefId) {
        self.emit_fct(BytecodeOpcode::InvokeVirtualPtr, dest, fid);
    }

    pub fn emit_invoke_virtual_tuple(&mut self, dest: Register, fid: FctDefId) {
        self.emit_fct(BytecodeOpcode::InvokeVirtualTuple, dest, fid);
    }

    pub fn emit_invoke_static_void(&mut self, fid: FctDefId) {
        self.emit_fct_void(BytecodeOpcode::InvokeStaticVoid, fid);
    }

    pub fn emit_invoke_static_bool(&mut self, dest: Register, fid: FctDefId) {
        self.emit_fct(BytecodeOpcode::InvokeStaticBool, dest, fid);
    }

    pub fn emit_invoke_static_uint8(&mut self, dest: Register, fid: FctDefId) {
        self.emit_fct(BytecodeOpcode::InvokeStaticUInt8, dest, fid);
    }

    pub fn emit_invoke_static_char(&mut self, dest: Register, fid: FctDefId) {
        self.emit_fct(BytecodeOpcode::InvokeStaticChar, dest, fid);
    }

    pub fn emit_invoke_static_int32(&mut self, dest: Register, fid: FctDefId) {
        self.emit_fct(BytecodeOpcode::InvokeStaticInt32, dest, fid);
    }

    pub fn emit_invoke_static_int64(&mut self, dest: Register, fid: FctDefId) {
        self.emit_fct(BytecodeOpcode::InvokeStaticInt64, dest, fid);
    }

    pub fn emit_invoke_static_float32(&mut self, dest: Register, fid: FctDefId) {
        self.emit_fct(BytecodeOpcode::InvokeStaticFloat32, dest, fid);
    }

    pub fn emit_invoke_static_float64(&mut self, dest: Register, fid: FctDefId) {
        self.emit_fct(BytecodeOpcode::InvokeStaticFloat64, dest, fid);
    }

    pub fn emit_invoke_static_ptr(&mut self, dest: Register, fid: FctDefId) {
        self.emit_fct(BytecodeOpcode::InvokeStaticPtr, dest, fid);
    }

    pub fn emit_invoke_static_tuple(&mut self, dest: Register, fid: FctDefId) {
        self.emit_fct(BytecodeOpcode::InvokeStaticTuple, dest, fid);
    }

    pub fn emit_new_object(&mut self, dest: Register, cls_id: ClassDefId) {
        self.emit_new(BytecodeOpcode::NewObject, dest, cls_id);
    }
    pub fn emit_new_array(&mut self, dest: Register, cls_id: ClassDefId, length: Register) {
        self.emit_new_arr(BytecodeOpcode::NewArray, dest, cls_id, length);
    }
    pub fn emit_new_tuple(&mut self, dest: Register, tuple_id: TupleId) {
        let values = [dest.to_usize() as u32, tuple_id.to_usize() as u32];
        self.emit_values(BytecodeOpcode::NewTuple, &values);
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

    pub fn emit_store_array_uint8(&mut self, src: Register, array: Register, idx: Register) {
        self.emit_reg3(BytecodeOpcode::StoreArrayUInt8, src, array, idx);
    }
    pub fn emit_store_array_bool(&mut self, src: Register, array: Register, index: Register) {
        self.emit_reg3(BytecodeOpcode::StoreArrayBool, src, array, index);
    }
    pub fn emit_store_array_char(&mut self, src: Register, array: Register, index: Register) {
        self.emit_reg3(BytecodeOpcode::StoreArrayChar, src, array, index);
    }
    pub fn emit_store_array_int32(&mut self, src: Register, array: Register, index: Register) {
        self.emit_reg3(BytecodeOpcode::StoreArrayInt32, src, array, index);
    }
    pub fn emit_store_array_int64(&mut self, src: Register, array: Register, index: Register) {
        self.emit_reg3(BytecodeOpcode::StoreArrayInt64, src, array, index);
    }
    pub fn emit_store_array_float32(&mut self, src: Register, array: Register, index: Register) {
        self.emit_reg3(BytecodeOpcode::StoreArrayFloat32, src, array, index);
    }
    pub fn emit_store_array_float64(&mut self, src: Register, array: Register, index: Register) {
        self.emit_reg3(BytecodeOpcode::StoreArrayFloat64, src, array, index);
    }
    pub fn emit_store_array_ptr(&mut self, src: Register, array: Register, index: Register) {
        self.emit_reg3(BytecodeOpcode::StoreArrayPtr, src, array, index);
    }
    pub fn emit_store_array_tuple(&mut self, src: Register, array: Register, index: Register) {
        self.emit_reg3(BytecodeOpcode::StoreArrayTuple, src, array, index);
    }

    pub fn emit_load_array_uint8(&mut self, dest: Register, array: Register, idx: Register) {
        self.emit_reg3(BytecodeOpcode::LoadArrayUInt8, dest, array, idx);
    }
    pub fn emit_load_array_bool(&mut self, dest: Register, array: Register, index: Register) {
        self.emit_reg3(BytecodeOpcode::LoadArrayBool, dest, array, index);
    }
    pub fn emit_load_array_char(&mut self, dest: Register, array: Register, index: Register) {
        self.emit_reg3(BytecodeOpcode::LoadArrayChar, dest, array, index);
    }
    pub fn emit_load_array_int32(&mut self, dest: Register, array: Register, index: Register) {
        self.emit_reg3(BytecodeOpcode::LoadArrayInt32, dest, array, index);
    }
    pub fn emit_load_array_int64(&mut self, dest: Register, array: Register, index: Register) {
        self.emit_reg3(BytecodeOpcode::LoadArrayInt64, dest, array, index);
    }
    pub fn emit_load_array_float32(&mut self, dest: Register, array: Register, index: Register) {
        self.emit_reg3(BytecodeOpcode::LoadArrayFloat32, dest, array, index);
    }
    pub fn emit_load_array_float64(&mut self, dest: Register, array: Register, index: Register) {
        self.emit_reg3(BytecodeOpcode::LoadArrayFloat64, dest, array, index);
    }
    pub fn emit_load_array_ptr(&mut self, dest: Register, array: Register, index: Register) {
        self.emit_reg3(BytecodeOpcode::LoadArrayPtr, dest, array, index);
    }
    pub fn emit_load_array_tuple(&mut self, dest: Register, array: Register, index: Register) {
        self.emit_reg3(BytecodeOpcode::LoadArrayTuple, dest, array, index);
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
                self.patch_const(const_idx, ConstPoolEntry::Int32(distance as i32));
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

    fn emit_reg1_uint8(&mut self, inst: BytecodeOpcode, r1: Register, value: u8) {
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

    fn emit_fct_void(&mut self, inst: BytecodeOpcode, fid: FctDefId) {
        let values = [fid.to_usize() as u32];
        self.emit_values(inst, &values);
    }

    fn emit_fct(&mut self, inst: BytecodeOpcode, r1: Register, fid: FctDefId) {
        let values = [r1.to_usize() as u32, fid.to_usize() as u32];
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

        let is_wide = values.iter().any(|&val| val > u8::max_value() as u32);

        if is_wide {
            self.emit_wide();
            self.emit_opcode(op as u32);
            for &value in values {
                self.emit_u32(value);
            }
        } else {
            self.emit_opcode(op as u32);
            for &value in values {
                self.emit_u8(value as u8);
            }
        }
    }

    fn emit_opcode(&mut self, code: u32) {
        if code >= 255 {
            self.emit_u8(255);
            assert!(code < 512);
            self.emit_u8((code - 255) as u8);
        } else {
            self.emit_u8(code as u8);
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
            self.emit_opcode(inst as u32);
            if let Some(cond) = cond {
                self.emit_u32(cond.to_usize() as u32);
            }
            let address = self.offset();
            self.emit_u32(0);
            self.unresolved_jump_offsets.push((start, address, lbl));
        } else {
            self.emit_opcode(inst_const as u32);
            if let Some(cond) = cond {
                self.emit_u8(cond.to_usize() as u8);
            }
            let idx = self.add_const(ConstPoolEntry::Int32(0));
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
