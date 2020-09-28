use std::collections::{HashMap, HashSet};

use dora_parser::lexer::position::Position;

use crate::bytecode::{
    dump, BytecodeFunction, BytecodeType, BytecodeWriter, ConstPoolEntry, ConstPoolIdx, Label,
    Register,
};
use crate::driver::cmd::Args;
use crate::ty::{TypeList, TypeListId};
use crate::vm::{ClassId, EnumId, FctId, FieldId, GlobalId, TupleId, VM};

pub struct BytecodeBuilder {
    writer: BytecodeWriter,
    registers: Registers,
    clear_regs: bool,
}

impl BytecodeBuilder {
    pub fn new(args: &Args) -> BytecodeBuilder {
        BytecodeBuilder {
            writer: BytecodeWriter::new(),
            registers: Registers::new(),
            clear_regs: args.flag_clear_regs,
        }
    }

    pub fn create_label(&mut self) -> Label {
        self.writer.create_label()
    }

    pub fn define_label(&mut self) -> Label {
        self.writer.define_label()
    }

    pub fn bind_label(&mut self, lbl: Label) {
        self.writer.bind_label(lbl)
    }

    pub fn set_arguments(&mut self, arguments: u32) {
        self.writer.set_arguments(arguments)
    }

    pub fn add_const(&mut self, entry: ConstPoolEntry) -> ConstPoolIdx {
        self.writer.add_const(entry)
    }

    pub fn add_const_fct(&mut self, id: FctId) -> ConstPoolIdx {
        self.writer
            .add_const(ConstPoolEntry::Fct(id, TypeList::empty()))
    }

    pub fn add_const_enum(&mut self, id: EnumId, type_params: TypeList) -> ConstPoolIdx {
        self.writer.add_const(ConstPoolEntry::Enum(id, type_params))
    }

    pub fn add_const_fct_types(&mut self, id: FctId, type_params: TypeList) -> ConstPoolIdx {
        self.writer.add_const(ConstPoolEntry::Fct(id, type_params))
    }

    pub fn add_const_generic(
        &mut self,
        id: TypeListId,
        fct_id: FctId,
        type_params: TypeList,
    ) -> ConstPoolIdx {
        self.writer
            .add_const(ConstPoolEntry::Generic(id, fct_id, type_params))
    }

    pub fn add_const_field_types(
        &mut self,
        cls_id: ClassId,
        type_params: TypeList,
        field_id: FieldId,
    ) -> ConstPoolIdx {
        self.writer
            .add_const(ConstPoolEntry::Field(cls_id, type_params, field_id))
    }

    pub fn add_const_cls(&mut self, id: ClassId) -> ConstPoolIdx {
        self.writer
            .add_const(ConstPoolEntry::Class(id, TypeList::empty()))
    }

    pub fn add_const_cls_types(&mut self, id: ClassId, type_params: TypeList) -> ConstPoolIdx {
        self.writer
            .add_const(ConstPoolEntry::Class(id, type_params))
    }

    pub fn emit_add_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_add_int32(dest, lhs, rhs);
    }

    pub fn emit_add_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_add_int64(dest, lhs, rhs);
    }

    pub fn emit_add_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_add_float32(dest, lhs, rhs);
    }

    pub fn emit_add_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_add_float64(dest, lhs, rhs);
    }

    pub fn emit_and_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_and_int32(dest, lhs, rhs);
    }

    pub fn emit_and_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_and_int64(dest, lhs, rhs);
    }

    pub fn emit_or_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_or_int32(dest, lhs, rhs);
    }

    pub fn emit_or_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_or_int64(dest, lhs, rhs);
    }

    pub fn emit_xor_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_xor_int32(dest, lhs, rhs);
    }

    pub fn emit_xor_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_xor_int64(dest, lhs, rhs);
    }

    pub fn emit_div_int32(&mut self, dest: Register, lhs: Register, rhs: Register, pos: Position) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.set_position(pos);
        self.writer.emit_div_int32(dest, lhs, rhs);
    }

    pub fn emit_div_int64(&mut self, dest: Register, lhs: Register, rhs: Register, pos: Position) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.set_position(pos);
        self.writer.emit_div_int64(dest, lhs, rhs);
    }

    pub fn emit_div_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_div_float32(dest, lhs, rhs);
    }

    pub fn emit_div_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_div_float64(dest, lhs, rhs);
    }

    pub fn emit_load_field(
        &mut self,
        dest: Register,
        obj: Register,
        field_idx: ConstPoolIdx,
        pos: Position,
    ) {
        assert!(self.def(dest) && self.used(obj));
        self.writer.set_position(pos);
        self.writer.emit_load_field(dest, obj, field_idx);
    }

    pub fn emit_store_field(
        &mut self,
        src: Register,
        obj: Register,
        field_idx: ConstPoolIdx,
        pos: Position,
    ) {
        assert!(self.used(src) && self.used(obj));
        self.writer.set_position(pos);
        self.writer.emit_store_field(src, obj, field_idx);
    }

    pub fn emit_const_nil(&mut self, dest: Register) {
        assert!(self.def(dest));
        self.writer.emit_const_nil(dest);
    }

    pub fn emit_const_char(&mut self, dest: Register, value: char) {
        assert!(self.def(dest));
        self.writer.emit_const_char(dest, value);
    }

    pub fn emit_const_uint8(&mut self, dest: Register, value: u8) {
        assert!(self.def(dest));
        self.writer.emit_const_uint8(dest, value);
    }

    pub fn emit_const_int32(&mut self, dest: Register, value: i32) {
        assert!(self.def(dest));
        self.writer.emit_const_int32(dest, value);
    }

    pub fn emit_const_int64(&mut self, dest: Register, value: i64) {
        assert!(self.def(dest));
        self.writer.emit_const_int64(dest, value);
    }

    pub fn emit_const_float32(&mut self, dest: Register, value: f32) {
        assert!(self.def(dest));
        self.writer.emit_const_float32(dest, value);
    }

    pub fn emit_const_float64(&mut self, dest: Register, value: f64) {
        assert!(self.def(dest));
        self.writer.emit_const_float64(dest, value);
    }

    pub fn emit_const_string(&mut self, dest: Register, value: String) {
        assert!(self.def(dest));
        self.writer.emit_const_string(dest, value);
    }

    pub fn emit_const_zero_uint8(&mut self, dest: Register) {
        assert!(self.def(dest));
        self.writer.emit_const_zero_uint8(dest);
    }

    pub fn emit_const_zero_int32(&mut self, dest: Register) {
        assert!(self.def(dest));
        self.writer.emit_const_zero_int32(dest);
    }

    pub fn emit_const_zero_int64(&mut self, dest: Register) {
        assert!(self.def(dest));
        self.writer.emit_const_zero_int64(dest);
    }

    pub fn emit_const_zero_float32(&mut self, dest: Register) {
        assert!(self.def(dest));
        self.writer.emit_const_zero_float32(dest);
    }

    pub fn emit_const_zero_float64(&mut self, dest: Register) {
        assert!(self.def(dest));
        self.writer.emit_const_zero_float64(dest);
    }

    pub fn emit_const_true(&mut self, dest: Register) {
        assert!(self.def(dest));
        self.writer.emit_const_true(dest);
    }

    pub fn emit_const_false(&mut self, dest: Register) {
        assert!(self.def(dest));
        self.writer.emit_const_false(dest);
    }

    pub fn emit_const_generic_default(&mut self, dest: Register) {
        assert!(self.def(dest));
        self.writer.emit_const_generic_default(dest);
    }

    pub fn emit_not_bool(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_not_bool(dest, src);
    }

    pub fn emit_not_int32(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_not_int32(dest, src);
    }

    pub fn emit_not_int64(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_not_int64(dest, src);
    }

    pub fn emit_jump_if_false(&mut self, opnd: Register, lbl: Label) {
        assert!(self.used(opnd));
        self.writer.emit_jump_if_false(opnd, lbl);
    }

    pub fn emit_jump_if_true(&mut self, opnd: Register, lbl: Label) {
        assert!(self.used(opnd));
        self.writer.emit_jump_if_true(opnd, lbl);
    }

    pub fn emit_jump_loop(&mut self, lbl: Label) {
        self.writer.emit_jump_loop(lbl);
    }

    pub fn emit_loop_start(&mut self) {
        self.writer.emit_loop_start();
    }

    pub fn emit_jump(&mut self, lbl: Label) {
        self.writer.emit_jump(lbl);
    }

    pub fn emit_mod_int32(&mut self, dest: Register, lhs: Register, rhs: Register, pos: Position) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.set_position(pos);
        self.writer.emit_mod_int32(dest, lhs, rhs);
    }

    pub fn emit_mod_int64(&mut self, dest: Register, lhs: Register, rhs: Register, pos: Position) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.set_position(pos);
        self.writer.emit_mod_int64(dest, lhs, rhs);
    }

    pub fn emit_mul_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_mul_int32(dest, lhs, rhs);
    }

    pub fn emit_mul_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_mul_int64(dest, lhs, rhs);
    }

    pub fn emit_mul_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_mul_float32(dest, lhs, rhs);
    }

    pub fn emit_mul_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_mul_float64(dest, lhs, rhs);
    }

    pub fn emit_neg_int32(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_neg_int32(dest, src);
    }

    pub fn emit_neg_int64(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_neg_int64(dest, src);
    }

    pub fn emit_neg_float32(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_neg_float32(dest, src);
    }

    pub fn emit_neg_float64(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_neg_float64(dest, src);
    }

    pub fn emit_shl_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_shl_int32(dest, lhs, rhs);
    }

    pub fn emit_shl_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_shl_int64(dest, lhs, rhs);
    }

    pub fn emit_shr_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_shr_int32(dest, lhs, rhs);
    }

    pub fn emit_shr_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_shr_int64(dest, lhs, rhs);
    }

    pub fn emit_sar_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_sar_int32(dest, lhs, rhs);
    }

    pub fn emit_sar_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_sar_int64(dest, lhs, rhs);
    }

    pub fn emit_rol_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_rol_int32(dest, lhs, rhs);
    }

    pub fn emit_rol_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_rol_int64(dest, lhs, rhs);
    }

    pub fn emit_ror_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_ror_int32(dest, lhs, rhs);
    }

    pub fn emit_ror_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_ror_int64(dest, lhs, rhs);
    }

    pub fn emit_reinterpret_float32_as_int32(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_reinterpret_float32_as_int32(dest, src);
    }

    pub fn emit_reinterpret_int32_as_float32(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_reinterpret_int32_as_float32(dest, src);
    }

    pub fn emit_reinterpret_float64_as_int64(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_reinterpret_float64_as_int64(dest, src);
    }

    pub fn emit_reinterpret_int64_as_float64(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_reinterpret_int64_as_float64(dest, src);
    }

    pub fn emit_extend_byte_to_char(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_extend_byte_to_char(dest, src);
    }
    pub fn emit_extend_byte_to_int32(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_extend_byte_to_int32(dest, src);
    }
    pub fn emit_extend_byte_to_int64(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_extend_byte_to_int64(dest, src);
    }
    pub fn emit_extend_char_to_int64(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_extend_char_to_int64(dest, src);
    }
    pub fn emit_extend_int32_to_int64(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_extend_int32_to_int64(dest, src);
    }

    pub fn emit_cast_char_to_int32(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_cast_char_to_int32(dest, src);
    }
    pub fn emit_cast_int32_to_uint8(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_cast_int32_to_uint8(dest, src);
    }
    pub fn emit_cast_int32_to_char(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_cast_int32_to_char(dest, src);
    }
    pub fn emit_cast_int64_to_uint8(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_cast_int64_to_uint8(dest, src);
    }
    pub fn emit_cast_int64_to_char(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_cast_int64_to_char(dest, src);
    }
    pub fn emit_cast_int64_to_int32(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_cast_int64_to_int32(dest, src);
    }

    pub fn emit_convert_int32_to_float32(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_convert_int32_to_float32(dest, src);
    }
    pub fn emit_convert_int32_to_float64(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_convert_int32_to_float64(dest, src);
    }
    pub fn emit_convert_int64_to_float32(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_convert_int64_to_float32(dest, src);
    }
    pub fn emit_convert_int64_to_float64(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_convert_int64_to_float64(dest, src);
    }

    pub fn emit_truncate_float32_to_int32(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_truncate_float32_to_int32(dest, src);
    }
    pub fn emit_truncate_float32_to_int64(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_truncate_float32_to_int64(dest, src);
    }
    pub fn emit_truncate_float64_to_int32(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_truncate_float64_to_int32(dest, src);
    }
    pub fn emit_truncate_float64_to_int64(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_truncate_float64_to_int64(dest, src);
    }

    pub fn emit_promote_float32_to_float64(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_promote_float32_to_float64(dest, src);
    }
    pub fn emit_demote_float64_to_float32(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_demote_float64_to_float32(dest, src);
    }

    pub fn emit_instance_of(&mut self, dest: Register, src: Register, cls_idx: ConstPoolIdx) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_instance_of(dest, src, cls_idx);
    }

    pub fn emit_checked_cast(&mut self, src: Register, cls_idx: ConstPoolIdx, pos: Position) {
        assert!(self.used(src));
        self.writer.set_position(pos);
        self.writer.emit_checked_cast(src, cls_idx);
    }

    pub fn emit_sub_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_sub_int32(dest, lhs, rhs);
    }

    pub fn emit_sub_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_sub_int64(dest, lhs, rhs);
    }

    pub fn emit_sub_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_sub_float32(dest, lhs, rhs);
    }

    pub fn emit_sub_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_sub_float64(dest, lhs, rhs);
    }

    pub fn emit_mov_bool(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_mov_bool(dest, src);
    }

    pub fn emit_mov_uint8(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_mov_uint8(dest, src);
    }

    pub fn emit_mov_char(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_mov_char(dest, src);
    }

    pub fn emit_mov_int32(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_mov_int32(dest, src);
    }

    pub fn emit_mov_int64(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_mov_int64(dest, src);
    }

    pub fn emit_mov_float32(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_mov_float32(dest, src);
    }

    pub fn emit_mov_float64(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_mov_float64(dest, src);
    }

    pub fn emit_mov_ptr(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_mov_ptr(dest, src);
    }

    pub fn emit_mov_tuple(&mut self, dest: Register, src: Register, tuple_id: TupleId) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_mov_tuple(dest, src, tuple_id);
    }

    pub fn emit_mov_generic(&mut self, dest: Register, src: Register) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_mov_generic(dest, src);
    }

    pub fn emit_mov_enum(&mut self, dest: Register, src: Register, idx: ConstPoolIdx) {
        assert!(self.def(dest) && self.used(src));
        self.writer.emit_mov_enum(dest, src, idx);
    }

    pub fn emit_load_tuple_element(
        &mut self,
        dest: Register,
        src: Register,
        tuple_id: TupleId,
        element: u32,
    ) {
        assert!(self.def(dest) && self.used(src));
        self.writer
            .emit_load_tuple_element(dest, src, tuple_id, element);
    }

    pub fn emit_ret(&mut self, src: Register) {
        assert!(self.used(src));
        self.writer.emit_ret(src);
    }

    pub fn emit_ret_void(&mut self) {
        self.writer.emit_ret_void();
    }

    pub fn emit_test_eq_bool(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_eq_bool(dest, lhs, rhs);
    }

    pub fn emit_test_eq_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_eq_uint8(dest, lhs, rhs);
    }

    pub fn emit_test_eq_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_eq_char(dest, lhs, rhs);
    }

    pub fn emit_test_eq_enum(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_eq_enum(dest, lhs, rhs);
    }

    pub fn emit_test_eq_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_eq_int32(dest, lhs, rhs);
    }

    pub fn emit_test_eq_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_eq_int64(dest, lhs, rhs);
    }

    pub fn emit_test_eq_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_eq_float32(dest, lhs, rhs);
    }

    pub fn emit_test_eq_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_eq_float64(dest, lhs, rhs);
    }

    pub fn emit_test_eq_ptr(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_eq_ptr(dest, lhs, rhs);
    }

    pub fn emit_test_eq_generic(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_eq_generic(dest, lhs, rhs);
    }

    pub fn emit_test_ne_bool(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_ne_bool(dest, lhs, rhs);
    }

    pub fn emit_test_ne_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_ne_uint8(dest, lhs, rhs);
    }

    pub fn emit_test_ne_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_ne_char(dest, lhs, rhs);
    }

    pub fn emit_test_ne_enum(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_ne_enum(dest, lhs, rhs);
    }

    pub fn emit_test_ne_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_ne_int32(dest, lhs, rhs);
    }

    pub fn emit_test_ne_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_ne_int64(dest, lhs, rhs);
    }

    pub fn emit_test_ne_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_ne_float32(dest, lhs, rhs);
    }

    pub fn emit_test_ne_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_ne_float64(dest, lhs, rhs);
    }

    pub fn emit_test_ne_ptr(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_ne_ptr(dest, lhs, rhs);
    }

    pub fn emit_test_ne_generic(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_ne_generic(dest, lhs, rhs);
    }

    pub fn emit_test_gt_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_gt_uint8(dest, lhs, rhs);
    }

    pub fn emit_test_gt_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_gt_char(dest, lhs, rhs);
    }

    pub fn emit_test_gt_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_gt_int32(dest, lhs, rhs);
    }

    pub fn emit_test_gt_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_gt_int64(dest, lhs, rhs);
    }

    pub fn emit_test_gt_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_gt_float32(dest, lhs, rhs);
    }

    pub fn emit_test_gt_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_gt_float64(dest, lhs, rhs);
    }

    pub fn emit_test_ge_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_ge_uint8(dest, lhs, rhs);
    }

    pub fn emit_test_ge_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_ge_char(dest, lhs, rhs);
    }

    pub fn emit_test_ge_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_ge_int32(dest, lhs, rhs);
    }

    pub fn emit_test_ge_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_ge_int64(dest, lhs, rhs);
    }

    pub fn emit_test_ge_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_ge_float32(dest, lhs, rhs);
    }

    pub fn emit_test_ge_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_ge_float64(dest, lhs, rhs);
    }

    pub fn emit_test_lt_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_lt_uint8(dest, lhs, rhs);
    }

    pub fn emit_test_lt_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_lt_char(dest, lhs, rhs);
    }

    pub fn emit_test_lt_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_lt_int32(dest, lhs, rhs);
    }

    pub fn emit_test_lt_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_lt_int64(dest, lhs, rhs);
    }

    pub fn emit_test_lt_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_lt_float32(dest, lhs, rhs);
    }

    pub fn emit_test_lt_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_lt_float64(dest, lhs, rhs);
    }

    pub fn emit_test_le_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_le_uint8(dest, lhs, rhs);
    }

    pub fn emit_test_le_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_le_char(dest, lhs, rhs);
    }

    pub fn emit_test_le_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_le_int32(dest, lhs, rhs);
    }

    pub fn emit_test_le_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_le_int64(dest, lhs, rhs);
    }

    pub fn emit_test_le_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_le_float32(dest, lhs, rhs);
    }

    pub fn emit_test_le_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert!(self.def(dest) && self.used(lhs) && self.used(rhs));
        self.writer.emit_test_le_float64(dest, lhs, rhs);
    }

    pub fn emit_assert(&mut self, value: Register, pos: Position) {
        assert!(self.used(value));
        self.writer.set_position(pos);
        self.writer.emit_assert(value);
    }

    pub fn emit_load_global(&mut self, dest: Register, gid: GlobalId) {
        assert!(self.def(dest));
        self.writer.emit_load_global(dest, gid);
    }

    pub fn emit_store_global(&mut self, src: Register, gid: GlobalId) {
        assert!(self.used(src));
        self.writer.emit_store_global(src, gid);
    }

    pub fn emit_push_register(&mut self, src: Register) {
        assert!(self.used(src));
        self.writer.emit_push_register(src);
    }

    pub fn emit_invoke_direct_void(&mut self, fid: ConstPoolIdx, pos: Position) {
        self.writer.set_position(pos);
        self.writer.emit_invoke_direct_void(fid);
    }

    pub fn emit_invoke_direct(&mut self, dest: Register, fid: ConstPoolIdx, pos: Position) {
        assert!(self.def(dest));
        self.writer.set_position(pos);
        self.writer.emit_invoke_direct(dest, fid);
    }

    pub fn emit_invoke_virtual_void(&mut self, idx: ConstPoolIdx, pos: Position) {
        self.writer.set_position(pos);
        self.writer.emit_invoke_virtual_void(idx);
    }

    pub fn emit_invoke_virtual(&mut self, dest: Register, idx: ConstPoolIdx, pos: Position) {
        assert!(self.def(dest));
        self.writer.set_position(pos);
        self.writer.emit_invoke_virtual(dest, idx);
    }

    pub fn emit_invoke_static_void(&mut self, idx: ConstPoolIdx, pos: Position) {
        self.writer.set_position(pos);
        self.writer.emit_invoke_static_void(idx);
    }

    pub fn emit_invoke_static(&mut self, dest: Register, idx: ConstPoolIdx, pos: Position) {
        assert!(self.def(dest));
        self.writer.set_position(pos);
        self.writer.emit_invoke_static(dest, idx);
    }

    pub fn emit_invoke_generic_static_void(&mut self, idx: ConstPoolIdx, pos: Position) {
        self.writer.set_position(pos);
        self.writer.emit_invoke_generic_static_void(idx);
    }

    pub fn emit_invoke_generic_static(&mut self, dest: Register, idx: ConstPoolIdx, pos: Position) {
        assert!(self.def(dest));
        self.writer.set_position(pos);
        self.writer.emit_invoke_generic_static(dest, idx);
    }

    pub fn emit_invoke_generic_direct_void(&mut self, idx: ConstPoolIdx, pos: Position) {
        self.writer.set_position(pos);
        self.writer.emit_invoke_generic_direct_void(idx);
    }

    pub fn emit_invoke_generic_direct(&mut self, dest: Register, idx: ConstPoolIdx, pos: Position) {
        assert!(self.def(dest));
        self.writer.set_position(pos);
        self.writer.emit_invoke_generic_direct(dest, idx);
    }

    pub fn emit_new_object(&mut self, dest: Register, idx: ConstPoolIdx, pos: Position) {
        assert!(self.def(dest));
        self.writer.set_position(pos);
        self.writer.emit_new_object(dest, idx);
    }
    pub fn emit_new_array(
        &mut self,
        dest: Register,
        cls_idx: ConstPoolIdx,
        length: Register,
        pos: Position,
    ) {
        assert!(self.def(dest));
        self.writer.set_position(pos);
        self.writer.emit_new_array(dest, cls_idx, length);
    }
    pub fn emit_new_tuple(&mut self, dest: Register, tuple_id: TupleId) {
        assert!(self.def(dest));
        self.writer.emit_new_tuple(dest, tuple_id);
    }

    pub fn emit_nil_check(&mut self, obj: Register, pos: Position) {
        assert!(self.used(obj));
        self.writer.set_position(pos);
        self.writer.emit_nil_check(obj);
    }

    pub fn emit_array_length(&mut self, dest: Register, array: Register, pos: Position) {
        assert!(self.def(dest) && self.used(array));
        self.writer.set_position(pos);
        self.writer.emit_array_length(dest, array);
    }
    pub fn emit_array_bound_check(&mut self, arr: Register, idx: Register, pos: Position) {
        assert!(self.used(arr) && self.used(idx));
        self.writer.set_position(pos);
        self.writer.emit_array_bound_check(arr, idx);
    }

    pub fn emit_store_array_uint8(
        &mut self,
        src: Register,
        array: Register,
        index: Register,
        pos: Position,
    ) {
        assert!(self.used(src) && self.used(array) && self.used(index));
        self.writer.set_position(pos);
        self.writer.emit_store_array_uint8(src, array, index);
    }
    pub fn emit_store_array_bool(
        &mut self,
        src: Register,
        array: Register,
        index: Register,
        pos: Position,
    ) {
        assert!(self.used(src) && self.used(array) && self.used(index));
        self.writer.set_position(pos);
        self.writer.emit_store_array_bool(src, array, index);
    }
    pub fn emit_store_array_char(
        &mut self,
        src: Register,
        array: Register,
        index: Register,
        pos: Position,
    ) {
        assert!(self.used(src) && self.used(array) && self.used(index));
        self.writer.set_position(pos);
        self.writer.emit_store_array_char(src, array, index);
    }
    pub fn emit_store_array_int32(
        &mut self,
        src: Register,
        array: Register,
        index: Register,
        pos: Position,
    ) {
        assert!(self.used(src) && self.used(array) && self.used(index));
        self.writer.set_position(pos);
        self.writer.emit_store_array_int32(src, array, index);
    }
    pub fn emit_store_array_int64(
        &mut self,
        src: Register,
        array: Register,
        index: Register,
        pos: Position,
    ) {
        assert!(self.used(src) && self.used(array) && self.used(index));
        self.writer.set_position(pos);
        self.writer.emit_store_array_int64(src, array, index);
    }
    pub fn emit_store_array_float32(
        &mut self,
        src: Register,
        array: Register,
        index: Register,
        pos: Position,
    ) {
        assert!(self.used(src) && self.used(array) && self.used(index));
        self.writer.set_position(pos);
        self.writer.emit_store_array_float32(src, array, index);
    }
    pub fn emit_store_array_float64(
        &mut self,
        src: Register,
        array: Register,
        index: Register,
        pos: Position,
    ) {
        assert!(self.used(src) && self.used(array) && self.used(index));
        self.writer.set_position(pos);
        self.writer.emit_store_array_float64(src, array, index);
    }
    pub fn emit_store_array_ptr(
        &mut self,
        src: Register,
        array: Register,
        index: Register,
        pos: Position,
    ) {
        assert!(self.used(src) && self.used(array) && self.used(index));
        self.writer.set_position(pos);
        self.writer.emit_store_array_ptr(src, array, index);
    }
    pub fn emit_store_array_tuple(
        &mut self,
        src: Register,
        array: Register,
        index: Register,
        pos: Position,
    ) {
        assert!(self.used(src) && self.used(array) && self.used(index));
        self.writer.set_position(pos);
        self.writer.emit_store_array_tuple(src, array, index);
    }
    pub fn emit_store_array_generic(
        &mut self,
        src: Register,
        array: Register,
        index: Register,
        pos: Position,
    ) {
        assert!(self.used(src) && self.used(array) && self.used(index));
        self.writer.set_position(pos);
        self.writer.emit_store_array_generic(src, array, index);
    }
    pub fn emit_store_array_enum(
        &mut self,
        src: Register,
        array: Register,
        index: Register,
        enum_idx: ConstPoolIdx,
        pos: Position,
    ) {
        assert!(self.used(src) && self.used(array) && self.used(index));
        self.writer.set_position(pos);
        self.writer
            .emit_store_array_enum(src, array, index, enum_idx);
    }

    pub fn emit_load_array_uint8(
        &mut self,
        dest: Register,
        array: Register,
        index: Register,
        pos: Position,
    ) {
        assert!(self.def(dest) && self.used(array) && self.used(index));
        self.writer.set_position(pos);
        self.writer.emit_load_array_uint8(dest, array, index);
    }
    pub fn emit_load_array_bool(
        &mut self,
        dest: Register,
        array: Register,
        index: Register,
        pos: Position,
    ) {
        assert!(self.def(dest) && self.used(array) && self.used(index));
        self.writer.set_position(pos);
        self.writer.emit_load_array_bool(dest, array, index);
    }
    pub fn emit_load_array_char(
        &mut self,
        dest: Register,
        array: Register,
        index: Register,
        pos: Position,
    ) {
        assert!(self.def(dest) && self.used(array) && self.used(index));
        self.writer.set_position(pos);
        self.writer.emit_load_array_char(dest, array, index);
    }
    pub fn emit_load_array_int32(
        &mut self,
        dest: Register,
        array: Register,
        index: Register,
        pos: Position,
    ) {
        assert!(self.def(dest) && self.used(array) && self.used(index));
        self.writer.set_position(pos);
        self.writer.emit_load_array_int32(dest, array, index);
    }
    pub fn emit_load_array_int64(
        &mut self,
        dest: Register,
        array: Register,
        index: Register,
        pos: Position,
    ) {
        assert!(self.def(dest) && self.used(array) && self.used(index));
        self.writer.set_position(pos);
        self.writer.emit_load_array_int64(dest, array, index);
    }
    pub fn emit_load_array_float32(
        &mut self,
        dest: Register,
        array: Register,
        index: Register,
        pos: Position,
    ) {
        assert!(self.def(dest) && self.used(array) && self.used(index));
        self.writer.set_position(pos);
        self.writer.emit_load_array_float32(dest, array, index);
    }
    pub fn emit_load_array_float64(
        &mut self,
        dest: Register,
        array: Register,
        index: Register,
        pos: Position,
    ) {
        assert!(self.def(dest) && self.used(array) && self.used(index));
        self.writer.set_position(pos);
        self.writer.emit_load_array_float64(dest, array, index);
    }
    pub fn emit_load_array_ptr(
        &mut self,
        dest: Register,
        array: Register,
        index: Register,
        pos: Position,
    ) {
        assert!(self.def(dest) && self.used(array) && self.used(index));
        self.writer.set_position(pos);
        self.writer.emit_load_array_ptr(dest, array, index);
    }
    pub fn emit_load_array_tuple(
        &mut self,
        dest: Register,
        array: Register,
        index: Register,
        pos: Position,
    ) {
        assert!(self.def(dest) && self.used(array) && self.used(index));
        self.writer.set_position(pos);
        self.writer.emit_load_array_tuple(dest, array, index);
    }
    pub fn emit_load_array_generic(
        &mut self,
        dest: Register,
        array: Register,
        index: Register,
        pos: Position,
    ) {
        assert!(self.def(dest) && self.used(array) && self.used(index));
        self.writer.set_position(pos);
        self.writer.emit_load_array_generic(dest, array, index);
    }
    pub fn emit_load_array_enum(
        &mut self,
        dest: Register,
        array: Register,
        index: Register,
        enum_idx: ConstPoolIdx,
        pos: Position,
    ) {
        assert!(self.def(dest) && self.used(array) && self.used(index));
        self.writer.set_position(pos);
        self.writer
            .emit_load_array_enum(dest, array, index, enum_idx);
    }

    pub fn generate(self, vm: &VM) -> BytecodeFunction {
        for reg in &self.registers.used {
            println!("used reg {}", reg);
        }

        let used = self.registers.used();

        let bc = self.writer.generate_with_registers(self.registers.all());

        if used {
            dump(vm, None, &bc);
            panic!("all registers should be freed.");
        }

        bc
    }

    pub fn push_scope(&mut self) {
        self.registers.push_scope();
    }

    pub fn pop_scope(&mut self) {
        self.registers.pop_scope();
    }

    pub fn alloc_var(&mut self, ty: BytecodeType) -> Register {
        self.registers.alloc_var(ty)
    }

    pub fn alloc_temp(&mut self, ty: BytecodeType) -> Register {
        self.registers.alloc_temp(ty)
    }

    pub fn free_if_temp(&mut self, reg: Register) {
        if self.registers.free_if_temp(reg) {
            self.clear_if_ptr(reg);
        }
    }

    pub fn free_temp(&mut self, reg: Register) {
        self.registers.free_temp(reg);
        self.clear_if_ptr(reg);
    }

    fn clear_if_ptr(&mut self, reg: Register) {
        if !self.clear_regs {
            return;
        }

        if self.registers.all[reg.0].is_ptr() {
            self.writer.emit_const_nil(reg);
        }
    }

    fn used(&self, reg: Register) -> bool {
        self.registers.used.contains(&reg)
    }

    fn def(&self, reg: Register) -> bool {
        self.registers.used.contains(&reg)
    }
}

struct Registers {
    all: Vec<BytecodeType>,
    scopes: Vec<RegisterScope>,
    used: HashSet<Register>,
    temps: HashSet<Register>,
    unused: HashMap<BytecodeType, Vec<Register>>,
}

impl Registers {
    fn new() -> Registers {
        Registers {
            all: Vec::new(),
            scopes: Vec::new(),
            used: HashSet::new(),
            temps: HashSet::new(),
            unused: HashMap::new(),
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(RegisterScope::new());
    }

    fn pop_scope(&mut self) {
        let scope = self.scopes.pop().expect("missing scope");

        for reg in scope.0 {
            let ty = self.all[reg.0].clone();
            self.unused.entry(ty).or_insert(Vec::new()).push(reg);
            assert!(self.used.remove(&reg));
        }
    }

    fn alloc_var(&mut self, ty: BytecodeType) -> Register {
        let reg = self.alloc_internal(ty);
        assert!(self.scopes.last_mut().expect("missing scope").0.insert(reg));
        assert!(self.used.insert(reg));
        reg
    }

    fn alloc_temp(&mut self, ty: BytecodeType) -> Register {
        let reg = self.alloc_internal(ty);
        assert!(self.temps.insert(reg));
        assert!(self.used.insert(reg));
        reg
    }

    fn free_temp(&mut self, reg: Register) {
        assert!(self.temps.remove(&reg));
        let ty = self.all[reg.0].clone();
        self.unused.entry(ty).or_insert(Vec::new()).push(reg);
        assert!(self.used.remove(&reg));
    }

    fn free_if_temp(&mut self, reg: Register) -> bool {
        if self.temps.contains(&reg) {
            self.free_temp(reg);
            true
        } else {
            false
        }
    }

    fn alloc_internal(&mut self, ty: BytecodeType) -> Register {
        if let Some(regs) = self.unused.get_mut(&ty) {
            if let Some(reg) = regs.pop() {
                return reg;
            }
        }

        self.new_register(ty)
    }

    fn new_register(&mut self, ty: BytecodeType) -> Register {
        self.all.push(ty);
        Register(self.all.len() - 1)
    }

    fn used(&self) -> bool {
        !self.scopes.is_empty() || !self.used.is_empty()
    }

    fn all(self) -> Vec<BytecodeType> {
        self.all
    }
}

struct RegisterScope(HashSet<Register>);

impl RegisterScope {
    fn new() -> RegisterScope {
        RegisterScope(HashSet::new())
    }
}
