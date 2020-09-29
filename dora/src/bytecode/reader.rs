use num_traits::cast::FromPrimitive;

use crate::bytecode::{BytecodeOffset, BytecodeOpcode, ConstPoolIdx, Register};
use crate::vm::{ClassDefId, FieldId, GlobalId, TupleId};

pub fn read<T: BytecodeVisitor>(data: &[u8], visitor: &mut T) {
    BytecodeReader::new(data, visitor).read();
}

struct BytecodeReader<'a, T: BytecodeVisitor> {
    data: &'a [u8],
    pos: usize,
    visitor: &'a mut T,
}

impl<'a, T> BytecodeReader<'a, T>
where
    T: BytecodeVisitor,
{
    fn new(data: &'a [u8], visitor: &'a mut T) -> BytecodeReader<'a, T> {
        BytecodeReader {
            data: data,
            pos: 0,
            visitor: visitor,
        }
    }

    fn read(&mut self) {
        while self.pos < self.data.len() {
            self.visitor
                .visit_instruction(BytecodeOffset(self.pos as u32));
            let wide = self.read_operand_width();
            let opcode = self.read_opcode();
            self.read_instruction(wide, opcode)
        }
    }

    fn read_instruction(&mut self, wide: bool, opcode: u32) {
        let inst: BytecodeOpcode = FromPrimitive::from_u32(opcode).expect("illegal opcode");

        match inst {
            BytecodeOpcode::Wide => unreachable!(),

            BytecodeOpcode::AddInt32 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_add_int32(dest, lhs, rhs);
            }
            BytecodeOpcode::AddInt64 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_add_int64(dest, lhs, rhs);
            }
            BytecodeOpcode::AddFloat32 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_add_float32(dest, lhs, rhs);
            }
            BytecodeOpcode::AddFloat64 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_add_float64(dest, lhs, rhs);
            }
            BytecodeOpcode::SubInt32 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_sub_int32(dest, lhs, rhs);
            }
            BytecodeOpcode::SubInt64 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_sub_int64(dest, lhs, rhs);
            }
            BytecodeOpcode::SubFloat32 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_sub_float32(dest, lhs, rhs);
            }
            BytecodeOpcode::SubFloat64 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_sub_float64(dest, lhs, rhs);
            }
            BytecodeOpcode::NegInt32 => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_neg_int32(dest, src);
            }
            BytecodeOpcode::NegInt64 => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_neg_int64(dest, src);
            }
            BytecodeOpcode::NegFloat32 => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_neg_float32(dest, src);
            }
            BytecodeOpcode::NegFloat64 => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_neg_float64(dest, src);
            }
            BytecodeOpcode::MulInt32 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_mul_int32(dest, lhs, rhs);
            }
            BytecodeOpcode::MulInt64 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_mul_int64(dest, lhs, rhs);
            }
            BytecodeOpcode::MulFloat32 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_mul_float32(dest, lhs, rhs);
            }
            BytecodeOpcode::MulFloat64 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_mul_float64(dest, lhs, rhs);
            }
            BytecodeOpcode::DivInt32 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_div_int32(dest, lhs, rhs);
            }
            BytecodeOpcode::DivInt64 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_div_int64(dest, lhs, rhs);
            }
            BytecodeOpcode::DivFloat32 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_div_float32(dest, lhs, rhs);
            }
            BytecodeOpcode::DivFloat64 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_div_float64(dest, lhs, rhs);
            }

            BytecodeOpcode::ModInt32 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_mod_int32(dest, lhs, rhs);
            }
            BytecodeOpcode::ModInt64 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_mod_int64(dest, lhs, rhs);
            }

            BytecodeOpcode::AndInt32 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_and_int32(dest, lhs, rhs);
            }
            BytecodeOpcode::AndInt64 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_and_int64(dest, lhs, rhs);
            }

            BytecodeOpcode::OrInt32 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_or_int32(dest, lhs, rhs);
            }
            BytecodeOpcode::OrInt64 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_or_int64(dest, lhs, rhs);
            }

            BytecodeOpcode::XorInt32 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_xor_int32(dest, lhs, rhs);
            }
            BytecodeOpcode::XorInt64 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_xor_int64(dest, lhs, rhs);
            }

            BytecodeOpcode::NotBool => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_not_bool(dest, src);
            }
            BytecodeOpcode::NotInt32 => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_not_int32(dest, src);
            }
            BytecodeOpcode::NotInt64 => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_not_int64(dest, src);
            }

            BytecodeOpcode::ShlInt32 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_shl_int32(dest, lhs, rhs);
            }
            BytecodeOpcode::ShrInt32 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_shr_int32(dest, lhs, rhs);
            }
            BytecodeOpcode::SarInt32 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_sar_int32(dest, lhs, rhs);
            }

            BytecodeOpcode::ShlInt64 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_shl_int64(dest, lhs, rhs);
            }
            BytecodeOpcode::ShrInt64 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_shr_int64(dest, lhs, rhs);
            }
            BytecodeOpcode::SarInt64 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_sar_int64(dest, lhs, rhs);
            }

            BytecodeOpcode::RolInt32 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_rol_int32(dest, lhs, rhs);
            }
            BytecodeOpcode::RorInt32 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_ror_int32(dest, lhs, rhs);
            }

            BytecodeOpcode::RolInt64 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_rol_int64(dest, lhs, rhs);
            }
            BytecodeOpcode::RorInt64 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_ror_int64(dest, lhs, rhs);
            }

            BytecodeOpcode::ReinterpretFloat32AsInt32 => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_reinterpret_float32_as_int32(dest, src);
            }
            BytecodeOpcode::ReinterpretInt32AsFloat32 => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_reinterpret_int32_as_float32(dest, src);
            }
            BytecodeOpcode::ReinterpretFloat64AsInt64 => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_reinterpret_float64_as_int64(dest, src);
            }
            BytecodeOpcode::ReinterpretInt64AsFloat64 => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_reinterpret_int64_as_float64(dest, src);
            }

            BytecodeOpcode::ExtendUInt8ToChar => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_extend_byte_to_char(dest, src);
            }
            BytecodeOpcode::ExtendUInt8ToInt32 => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_extend_byte_to_int32(dest, src);
            }
            BytecodeOpcode::ExtendUInt8ToInt64 => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_extend_byte_to_int64(dest, src);
            }
            BytecodeOpcode::ExtendInt32ToInt64 => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_extend_int32_to_int64(dest, src);
            }
            BytecodeOpcode::ExtendCharToInt64 => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_extend_char_to_int64(dest, src);
            }
            BytecodeOpcode::CastCharToInt32 => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_cast_char_to_int32(dest, src);
            }
            BytecodeOpcode::CastInt32ToUInt8 => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_cast_int32_to_uint8(dest, src);
            }
            BytecodeOpcode::CastInt32ToChar => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_cast_int32_to_char(dest, src);
            }
            BytecodeOpcode::CastInt64ToUInt8 => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_cast_int64_to_uint8(dest, src);
            }
            BytecodeOpcode::CastInt64ToChar => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_cast_int64_to_char(dest, src);
            }
            BytecodeOpcode::CastInt64ToInt32 => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_cast_int64_to_int32(dest, src);
            }

            BytecodeOpcode::InstanceOf => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                let cls_id = self.read_const_pool_idx(wide);
                self.visitor.visit_instance_of(dest, src, cls_id);
            }
            BytecodeOpcode::CheckedCast => {
                let src = self.read_register(wide);
                let cls_id = self.read_const_pool_idx(wide);
                self.visitor.visit_checked_cast(src, cls_id);
            }

            BytecodeOpcode::MovBool => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_mov_bool(dest, src);
            }
            BytecodeOpcode::MovUInt8 => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_mov_uint8(dest, src);
            }
            BytecodeOpcode::MovChar => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_mov_char(dest, src);
            }
            BytecodeOpcode::MovInt32 => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_mov_int32(dest, src);
            }
            BytecodeOpcode::MovInt64 => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_mov_int64(dest, src);
            }
            BytecodeOpcode::MovFloat32 => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_mov_float32(dest, src);
            }
            BytecodeOpcode::MovFloat64 => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_mov_float64(dest, src);
            }
            BytecodeOpcode::MovPtr => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_mov_ptr(dest, src);
            }
            BytecodeOpcode::MovTuple => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                let tuple = self.read_tuple(wide);
                self.visitor.visit_mov_tuple(dest, src, tuple);
            }
            BytecodeOpcode::MovGeneric => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_mov_generic(dest, src);
            }
            BytecodeOpcode::MovEnum => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                let idx = self.read_const_pool_idx(wide);
                self.visitor.visit_mov_enum(dest, src, idx);
            }

            BytecodeOpcode::LoadTupleElement => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                let tuple = self.read_tuple(wide);
                let element = self.read_index(wide);
                self.visitor
                    .visit_load_tuple_element(dest, src, tuple, element);
            }

            BytecodeOpcode::StoreTupleElement => {
                let src = self.read_register(wide);
                let dest = self.read_register(wide);
                let tuple = self.read_tuple(wide);
                let element = self.read_index(wide);
                self.visitor
                    .visit_store_tuple_element(src, dest, tuple, element);
            }

            BytecodeOpcode::LoadField => {
                let dest = self.read_register(wide);
                let obj = self.read_register(wide);
                let field = self.read_const_pool_idx(wide);
                self.visitor.visit_load_field(dest, obj, field);
            }

            BytecodeOpcode::StoreField => {
                let src = self.read_register(wide);
                let obj = self.read_register(wide);
                let field = self.read_const_pool_idx(wide);
                self.visitor.visit_store_field(src, obj, field);
            }

            BytecodeOpcode::LoadGlobal => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_load_global(dest, glob);
            }

            BytecodeOpcode::StoreGlobal => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_store_global(dest, glob);
            }

            BytecodeOpcode::PushRegister => {
                let src = self.read_register(wide);
                self.visitor.visit_push_register(src);
            }

            BytecodeOpcode::ConstNil => {
                let dest = self.read_register(wide);
                self.visitor.visit_const_nil(dest);
            }
            BytecodeOpcode::ConstTrue => {
                let dest = self.read_register(wide);
                self.visitor.visit_const_true(dest);
            }
            BytecodeOpcode::ConstFalse => {
                let dest = self.read_register(wide);
                self.visitor.visit_const_false(dest);
            }
            BytecodeOpcode::ConstZeroUInt8 => {
                let dest = self.read_register(wide);
                self.visitor.visit_const_zero_uint8(dest);
            }
            BytecodeOpcode::ConstZeroChar => {
                let dest = self.read_register(wide);
                self.visitor.visit_const_zero_char(dest);
            }
            BytecodeOpcode::ConstZeroInt32 => {
                let dest = self.read_register(wide);
                self.visitor.visit_const_zero_int32(dest);
            }
            BytecodeOpcode::ConstZeroInt64 => {
                let dest = self.read_register(wide);
                self.visitor.visit_const_zero_int64(dest);
            }
            BytecodeOpcode::ConstZeroFloat32 => {
                let dest = self.read_register(wide);
                self.visitor.visit_const_zero_float32(dest);
            }
            BytecodeOpcode::ConstZeroFloat64 => {
                let dest = self.read_register(wide);
                self.visitor.visit_const_zero_float64(dest);
            }
            BytecodeOpcode::ConstChar => {
                let dest = self.read_register(wide);
                let idx = self.read_const_pool_idx(wide);
                self.visitor.visit_const_char(dest, idx);
            }
            BytecodeOpcode::ConstUInt8 => {
                let dest = self.read_register(wide);
                let value = self.read_byte();
                self.visitor.visit_const_uint8(dest, value as u8);
            }
            BytecodeOpcode::ConstInt32 => {
                let dest = self.read_register(wide);
                let idx = self.read_const_pool_idx(wide);
                self.visitor.visit_const_int32(dest, idx);
            }
            BytecodeOpcode::ConstInt64 => {
                let dest = self.read_register(wide);
                let idx = self.read_const_pool_idx(wide);
                self.visitor.visit_const_int64(dest, idx);
            }
            BytecodeOpcode::ConstFloat32 => {
                let dest = self.read_register(wide);
                let idx = self.read_const_pool_idx(wide);
                self.visitor.visit_const_float32(dest, idx);
            }
            BytecodeOpcode::ConstFloat64 => {
                let dest = self.read_register(wide);
                let idx = self.read_const_pool_idx(wide);
                self.visitor.visit_const_float64(dest, idx);
            }
            BytecodeOpcode::ConstString => {
                let dest = self.read_register(wide);
                let idx = self.read_const_pool_idx(wide);
                self.visitor.visit_const_string(dest, idx);
            }

            BytecodeOpcode::TestEqPtr => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_eq_ptr(dest, lhs, rhs);
            }
            BytecodeOpcode::TestNePtr => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ne_ptr(dest, lhs, rhs);
            }

            BytecodeOpcode::TestEqGeneric => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_eq_generic(dest, lhs, rhs);
            }
            BytecodeOpcode::TestNeGeneric => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ne_generic(dest, lhs, rhs);
            }

            BytecodeOpcode::TestEqBool => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_eq_bool(dest, lhs, rhs);
            }
            BytecodeOpcode::TestNeBool => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ne_bool(dest, lhs, rhs);
            }
            BytecodeOpcode::TestEqUInt8 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_eq_uint8(dest, lhs, rhs);
            }
            BytecodeOpcode::TestNeUInt8 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ne_uint8(dest, lhs, rhs);
            }
            BytecodeOpcode::TestGtUInt8 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_gt_uint8(dest, lhs, rhs);
            }
            BytecodeOpcode::TestGeUInt8 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ge_uint8(dest, lhs, rhs);
            }
            BytecodeOpcode::TestLtUInt8 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_lt_uint8(dest, lhs, rhs);
            }
            BytecodeOpcode::TestLeUInt8 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_le_uint8(dest, lhs, rhs);
            }
            BytecodeOpcode::TestEqChar => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_eq_char(dest, lhs, rhs);
            }
            BytecodeOpcode::TestNeChar => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ne_char(dest, lhs, rhs);
            }
            BytecodeOpcode::TestGtChar => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_gt_char(dest, lhs, rhs);
            }
            BytecodeOpcode::TestGeChar => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ge_char(dest, lhs, rhs);
            }
            BytecodeOpcode::TestLtChar => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_lt_char(dest, lhs, rhs);
            }
            BytecodeOpcode::TestLeChar => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_le_char(dest, lhs, rhs);
            }
            BytecodeOpcode::TestEqEnum => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_eq_enum(dest, lhs, rhs);
            }
            BytecodeOpcode::TestNeEnum => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ne_enum(dest, lhs, rhs);
            }
            BytecodeOpcode::TestEqInt64 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_eq_int64(dest, lhs, rhs);
            }
            BytecodeOpcode::TestNeInt64 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ne_int64(dest, lhs, rhs);
            }
            BytecodeOpcode::TestGtInt64 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_gt_int64(dest, lhs, rhs);
            }
            BytecodeOpcode::TestGeInt64 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ge_int64(dest, lhs, rhs);
            }
            BytecodeOpcode::TestLtInt64 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_lt_int64(dest, lhs, rhs);
            }
            BytecodeOpcode::TestLeInt64 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_le_int64(dest, lhs, rhs);
            }
            BytecodeOpcode::TestEqInt32 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_eq_int32(dest, lhs, rhs);
            }
            BytecodeOpcode::TestNeInt32 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ne_int32(dest, lhs, rhs);
            }
            BytecodeOpcode::TestGtInt32 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_gt_int32(dest, lhs, rhs);
            }
            BytecodeOpcode::TestGeInt32 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ge_int32(dest, lhs, rhs);
            }
            BytecodeOpcode::TestLtInt32 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_lt_int32(dest, lhs, rhs);
            }
            BytecodeOpcode::TestLeInt32 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_le_int32(dest, lhs, rhs);
            }

            BytecodeOpcode::TestEqFloat32 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_eq_float32(dest, lhs, rhs);
            }
            BytecodeOpcode::TestNeFloat32 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ne_float32(dest, lhs, rhs);
            }
            BytecodeOpcode::TestGtFloat32 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_gt_float32(dest, lhs, rhs);
            }
            BytecodeOpcode::TestGeFloat32 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ge_float32(dest, lhs, rhs);
            }
            BytecodeOpcode::TestLtFloat32 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_lt_float32(dest, lhs, rhs);
            }
            BytecodeOpcode::TestLeFloat32 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_le_float32(dest, lhs, rhs);
            }

            BytecodeOpcode::TestEqFloat64 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_eq_float64(dest, lhs, rhs);
            }
            BytecodeOpcode::TestNeFloat64 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ne_float64(dest, lhs, rhs);
            }
            BytecodeOpcode::TestGtFloat64 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_gt_float64(dest, lhs, rhs);
            }
            BytecodeOpcode::TestGeFloat64 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ge_float64(dest, lhs, rhs);
            }
            BytecodeOpcode::TestLtFloat64 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_lt_float64(dest, lhs, rhs);
            }
            BytecodeOpcode::TestLeFloat64 => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_le_float64(dest, lhs, rhs);
            }
            BytecodeOpcode::Assert => {
                let value = self.read_register(wide);
                self.visitor.visit_assert(value);
            }

            BytecodeOpcode::JumpLoop => {
                let offset = self.read_offset(wide);
                self.visitor.visit_jump_loop(offset);
            }
            BytecodeOpcode::LoopStart => {
                self.visitor.visit_loop_start();
            }
            BytecodeOpcode::JumpIfFalse => {
                let opnd = self.read_register(wide);
                let offset = self.read_offset(wide);
                self.visitor.visit_jump_if_false(opnd, offset);
            }
            BytecodeOpcode::JumpIfFalseConst => {
                let opnd = self.read_register(wide);
                let idx = self.read_const_pool_idx(wide);
                self.visitor.visit_jump_if_false_const(opnd, idx);
            }
            BytecodeOpcode::JumpIfTrue => {
                let opnd = self.read_register(wide);
                let offset = self.read_offset(wide);
                self.visitor.visit_jump_if_true(opnd, offset);
            }
            BytecodeOpcode::JumpIfTrueConst => {
                let opnd = self.read_register(wide);
                let idx = self.read_const_pool_idx(wide);
                self.visitor.visit_jump_if_true_const(opnd, idx);
            }
            BytecodeOpcode::Jump => {
                let offset = self.read_offset(wide);
                self.visitor.visit_jump(offset);
            }
            BytecodeOpcode::JumpConst => {
                let idx = self.read_const_pool_idx(wide);
                self.visitor.visit_jump_const(idx);
            }

            BytecodeOpcode::InvokeDirectVoid => {
                let fct = self.read_const_pool_idx(wide);
                self.visitor.visit_invoke_direct_void(fct);
            }
            BytecodeOpcode::InvokeDirect => {
                let dest = self.read_register(wide);
                let fct = self.read_const_pool_idx(wide);
                self.visitor.visit_invoke_direct(dest, fct);
            }

            BytecodeOpcode::InvokeVirtualVoid => {
                let fct = self.read_const_pool_idx(wide);
                self.visitor.visit_invoke_virtual_void(fct);
            }
            BytecodeOpcode::InvokeVirtual => {
                let dest = self.read_register(wide);
                let fct = self.read_const_pool_idx(wide);
                self.visitor.visit_invoke_virtual(dest, fct);
            }

            BytecodeOpcode::InvokeStaticVoid => {
                let fct = self.read_const_pool_idx(wide);
                self.visitor.visit_invoke_static_void(fct);
            }
            BytecodeOpcode::InvokeStatic => {
                let dest = self.read_register(wide);
                let fct = self.read_const_pool_idx(wide);
                self.visitor.visit_invoke_static(dest, fct);
            }

            BytecodeOpcode::InvokeGenericStaticVoid => {
                let fct = self.read_const_pool_idx(wide);
                self.visitor.visit_invoke_generic_static_void(fct);
            }
            BytecodeOpcode::InvokeGenericStatic => {
                let dest = self.read_register(wide);
                let fct = self.read_const_pool_idx(wide);
                self.visitor.visit_invoke_generic_static(dest, fct);
            }

            BytecodeOpcode::InvokeGenericDirectVoid => {
                let fct = self.read_const_pool_idx(wide);
                self.visitor.visit_invoke_generic_direct_void(fct);
            }
            BytecodeOpcode::InvokeGenericDirect => {
                let dest = self.read_register(wide);
                let fct = self.read_const_pool_idx(wide);
                self.visitor.visit_invoke_generic_direct(dest, fct);
            }

            BytecodeOpcode::NewObject => {
                let dest = self.read_register(wide);
                let cls = self.read_const_pool_idx(wide);
                self.visitor.visit_new_object(dest, cls);
            }
            BytecodeOpcode::NewArray => {
                let dest = self.read_register(wide);
                let cls = self.read_const_pool_idx(wide);
                let length = self.read_register(wide);
                self.visitor.visit_new_array(dest, cls, length);
            }
            BytecodeOpcode::NewTuple => {
                let dest = self.read_register(wide);
                let tuple = self.read_tuple(wide);
                self.visitor.visit_new_tuple(dest, tuple);
            }

            BytecodeOpcode::NilCheck => {
                let obj = self.read_register(wide);
                self.visitor.visit_nil_check(obj);
            }

            BytecodeOpcode::ArrayLength => {
                let dest = self.read_register(wide);
                let array = self.read_register(wide);
                self.visitor.visit_array_length(dest, array);
            }
            BytecodeOpcode::ArrayBoundCheck => {
                let array = self.read_register(wide);
                let index = self.read_register(wide);
                self.visitor.visit_array_bound_check(array, index);
            }

            BytecodeOpcode::LoadArrayBool => {
                let dest = self.read_register(wide);
                let array = self.read_register(wide);
                let index = self.read_register(wide);
                self.visitor.visit_load_array_bool(dest, array, index);
            }
            BytecodeOpcode::LoadArrayUInt8 => {
                let dest = self.read_register(wide);
                let array = self.read_register(wide);
                let index = self.read_register(wide);
                self.visitor.visit_load_array_uint8(dest, array, index);
            }
            BytecodeOpcode::LoadArrayChar => {
                let dest = self.read_register(wide);
                let array = self.read_register(wide);
                let index = self.read_register(wide);
                self.visitor.visit_load_array_char(dest, array, index);
            }
            BytecodeOpcode::LoadArrayInt32 => {
                let dest = self.read_register(wide);
                let array = self.read_register(wide);
                let index = self.read_register(wide);
                self.visitor.visit_load_array_int32(dest, array, index);
            }
            BytecodeOpcode::LoadArrayInt64 => {
                let dest = self.read_register(wide);
                let array = self.read_register(wide);
                let index = self.read_register(wide);
                self.visitor.visit_load_array_int64(dest, array, index);
            }
            BytecodeOpcode::LoadArrayFloat32 => {
                let dest = self.read_register(wide);
                let array = self.read_register(wide);
                let index = self.read_register(wide);
                self.visitor.visit_load_array_float32(dest, array, index);
            }
            BytecodeOpcode::LoadArrayFloat64 => {
                let dest = self.read_register(wide);
                let array = self.read_register(wide);
                let index = self.read_register(wide);
                self.visitor.visit_load_array_float64(dest, array, index);
            }
            BytecodeOpcode::LoadArrayPtr => {
                let dest = self.read_register(wide);
                let array = self.read_register(wide);
                let index = self.read_register(wide);
                self.visitor.visit_load_array_ptr(dest, array, index);
            }
            BytecodeOpcode::LoadArrayTuple => {
                let dest = self.read_register(wide);
                let array = self.read_register(wide);
                let index = self.read_register(wide);
                self.visitor.visit_load_array_tuple(dest, array, index);
            }
            BytecodeOpcode::LoadArrayGeneric => {
                let dest = self.read_register(wide);
                let array = self.read_register(wide);
                let index = self.read_register(wide);
                self.visitor.visit_load_array_generic(dest, array, index);
            }
            BytecodeOpcode::LoadArrayEnum => {
                let dest = self.read_register(wide);
                let array = self.read_register(wide);
                let index = self.read_register(wide);
                let enum_idx = self.read_const_pool_idx(wide);
                self.visitor
                    .visit_load_array_enum(dest, array, index, enum_idx);
            }

            BytecodeOpcode::StoreArrayBool => {
                let src = self.read_register(wide);
                let array = self.read_register(wide);
                let index = self.read_register(wide);
                self.visitor.visit_store_array_bool(src, array, index);
            }
            BytecodeOpcode::StoreArrayUInt8 => {
                let src = self.read_register(wide);
                let array = self.read_register(wide);
                let index = self.read_register(wide);
                self.visitor.visit_store_array_uint8(src, array, index);
            }
            BytecodeOpcode::StoreArrayChar => {
                let src = self.read_register(wide);
                let array = self.read_register(wide);
                let index = self.read_register(wide);
                self.visitor.visit_store_array_char(src, array, index);
            }
            BytecodeOpcode::StoreArrayInt32 => {
                let src = self.read_register(wide);
                let array = self.read_register(wide);
                let index = self.read_register(wide);
                self.visitor.visit_store_array_int32(src, array, index);
            }
            BytecodeOpcode::StoreArrayInt64 => {
                let src = self.read_register(wide);
                let array = self.read_register(wide);
                let index = self.read_register(wide);
                self.visitor.visit_store_array_int64(src, array, index);
            }
            BytecodeOpcode::StoreArrayFloat32 => {
                let src = self.read_register(wide);
                let array = self.read_register(wide);
                let index = self.read_register(wide);
                self.visitor.visit_store_array_float32(src, array, index);
            }
            BytecodeOpcode::StoreArrayFloat64 => {
                let src = self.read_register(wide);
                let array = self.read_register(wide);
                let index = self.read_register(wide);
                self.visitor.visit_store_array_float64(src, array, index);
            }
            BytecodeOpcode::StoreArrayPtr => {
                let src = self.read_register(wide);
                let array = self.read_register(wide);
                let index = self.read_register(wide);
                self.visitor.visit_store_array_ptr(src, array, index);
            }
            BytecodeOpcode::StoreArrayTuple => {
                let src = self.read_register(wide);
                let array = self.read_register(wide);
                let index = self.read_register(wide);
                self.visitor.visit_store_array_tuple(src, array, index);
            }
            BytecodeOpcode::StoreArrayGeneric => {
                let src = self.read_register(wide);
                let array = self.read_register(wide);
                let index = self.read_register(wide);
                self.visitor.visit_store_array_tuple(src, array, index);
            }
            BytecodeOpcode::StoreArrayEnum => {
                let dest = self.read_register(wide);
                let array = self.read_register(wide);
                let index = self.read_register(wide);
                let enum_idx = self.read_const_pool_idx(wide);
                self.visitor
                    .visit_store_array_enum(dest, array, index, enum_idx);
            }

            BytecodeOpcode::RetVoid => {
                self.visitor.visit_ret_void();
            }
            BytecodeOpcode::Ret => {
                let opnd = self.read_register(wide);
                self.visitor.visit_ret(opnd);
            }
        }
    }

    fn read_register(&mut self, wide: bool) -> Register {
        Register(self.read_index(wide) as usize)
    }

    fn read_class(&mut self, wide: bool) -> ClassDefId {
        (self.read_index(wide) as usize).into()
    }

    fn read_field(&mut self, wide: bool) -> FieldId {
        (self.read_index(wide) as usize).into()
    }

    fn read_tuple(&mut self, wide: bool) -> TupleId {
        self.read_index(wide).into()
    }

    fn read_global(&mut self, wide: bool) -> GlobalId {
        self.read_index(wide).into()
    }

    fn read_opcode(&mut self) -> u32 {
        let first = self.read_byte();

        if first == 255 {
            let second = self.read_byte();
            255 + second
        } else {
            first
        }
    }

    fn read_const_pool_idx(&mut self, wide: bool) -> ConstPoolIdx {
        (self.read_index(wide) as usize).into()
    }

    fn read_offset(&mut self, wide: bool) -> u32 {
        self.read_index(wide)
    }

    fn read_index(&mut self, wide: bool) -> u32 {
        if wide {
            self.read_wide()
        } else {
            self.read_byte()
        }
    }

    fn read_operand_width(&mut self) -> bool {
        if self.data[self.pos] as u32 == BytecodeOpcode::Wide as u32 {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    fn read_byte(&mut self) -> u32 {
        let value = self.data[self.pos];
        self.pos += 1;
        value as u32
    }

    fn read_wide(&mut self) -> u32 {
        let v1 = self.read_byte();
        let v2 = self.read_byte();
        let v3 = self.read_byte();
        let v4 = self.read_byte();

        (v4 << 24) | (v3 << 16) | (v2 << 8) | v1
    }
}

pub trait BytecodeVisitor {
    fn visit_instruction(&mut self, _offset: BytecodeOffset) {}

    fn visit_add_int32(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_add_int64(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_add_float32(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_add_float64(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_sub_int32(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_sub_int64(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_sub_float32(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_sub_float64(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_neg_int32(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_neg_int64(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_neg_float32(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_neg_float64(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }

    fn visit_mul_int32(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_mul_int64(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_mul_float32(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_mul_float64(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_div_int32(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_div_int64(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_div_float32(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_div_float64(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_mod_int32(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_mod_int64(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_and_int32(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_and_int64(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_or_int32(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_or_int64(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_xor_int32(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_xor_int64(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_not_bool(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_not_int32(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_not_int64(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }

    fn visit_shl_int32(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_shr_int32(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_sar_int32(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_shl_int64(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_shr_int64(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_sar_int64(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_rol_int32(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_ror_int32(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_rol_int64(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_ror_int64(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_reinterpret_float32_as_int32(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_reinterpret_int32_as_float32(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_reinterpret_float64_as_int64(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_reinterpret_int64_as_float64(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }

    fn visit_extend_byte_to_char(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_extend_byte_to_int32(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_extend_byte_to_int64(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_extend_int32_to_int64(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_extend_char_to_int64(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_cast_char_to_int32(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_cast_int32_to_uint8(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_cast_int32_to_char(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_cast_int64_to_uint8(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_cast_int64_to_char(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_cast_int64_to_int32(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }

    fn visit_truncate_float32_to_int32(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_truncate_float32_to_int64(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_truncate_float64_to_int32(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_truncate_float64_to_int64(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }

    fn visit_instance_of(&mut self, _dest: Register, _src: Register, _cls_id: ConstPoolIdx) {
        unimplemented!();
    }

    fn visit_checked_cast(&mut self, _src: Register, _cls_id: ConstPoolIdx) {
        unimplemented!();
    }

    fn visit_mov_bool(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_mov_uint8(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_mov_char(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_mov_int32(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_mov_int64(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_mov_float32(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_mov_float64(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_mov_ptr(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_mov_tuple(&mut self, _dest: Register, _src: Register, _tuple_id: TupleId) {
        unimplemented!();
    }
    fn visit_mov_generic(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_mov_enum(&mut self, _dest: Register, _src: Register, _idx: ConstPoolIdx) {
        unimplemented!();
    }

    fn visit_load_tuple_element(
        &mut self,
        _dest: Register,
        _src: Register,
        _tuple_id: TupleId,
        _element: u32,
    ) {
        unimplemented!();
    }

    fn visit_store_tuple_element(
        &mut self,
        _src: Register,
        _dest: Register,
        _tuple_id: TupleId,
        _element: u32,
    ) {
        unimplemented!();
    }

    fn visit_load_field(&mut self, _dest: Register, _obj: Register, _field: ConstPoolIdx) {
        unimplemented!();
    }

    fn visit_store_field(&mut self, _src: Register, _obj: Register, _field: ConstPoolIdx) {
        unimplemented!();
    }

    fn visit_load_global(&mut self, _dest: Register, _glob: GlobalId) {
        unimplemented!();
    }

    fn visit_store_global(&mut self, _src: Register, _glob: GlobalId) {
        unimplemented!();
    }

    fn visit_push_register(&mut self, _src: Register) {
        unimplemented!();
    }

    fn visit_const_nil(&mut self, _dest: Register) {
        unimplemented!();
    }
    fn visit_const_true(&mut self, _dest: Register) {
        unimplemented!();
    }
    fn visit_const_false(&mut self, _dest: Register) {
        unimplemented!();
    }
    fn visit_const_zero_uint8(&mut self, _dest: Register) {
        unimplemented!();
    }
    fn visit_const_zero_char(&mut self, _dest: Register) {
        unimplemented!();
    }
    fn visit_const_zero_int32(&mut self, _dest: Register) {
        unimplemented!();
    }
    fn visit_const_zero_int64(&mut self, _dest: Register) {
        unimplemented!();
    }
    fn visit_const_zero_float32(&mut self, _dest: Register) {
        unimplemented!();
    }
    fn visit_const_zero_float64(&mut self, _dest: Register) {
        unimplemented!();
    }
    fn visit_const_char(&mut self, _dest: Register, _value: ConstPoolIdx) {
        unimplemented!();
    }
    fn visit_const_uint8(&mut self, _dest: Register, _value: u8) {
        unimplemented!();
    }
    fn visit_const_int32(&mut self, _dest: Register, _value: ConstPoolIdx) {
        unimplemented!();
    }
    fn visit_const_int64(&mut self, _dest: Register, _value: ConstPoolIdx) {
        unimplemented!();
    }
    fn visit_const_float32(&mut self, _dest: Register, _value: ConstPoolIdx) {
        unimplemented!();
    }
    fn visit_const_float64(&mut self, _dest: Register, _value: ConstPoolIdx) {
        unimplemented!();
    }
    fn visit_const_string(&mut self, _dest: Register, _value: ConstPoolIdx) {
        unimplemented!();
    }

    fn visit_test_eq_ptr(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_ne_ptr(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_test_eq_generic(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_ne_generic(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_test_eq_bool(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_ne_bool(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_eq_uint8(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_ne_uint8(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_gt_uint8(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_ge_uint8(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_lt_uint8(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_le_uint8(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_eq_char(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_ne_char(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_gt_char(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_ge_char(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_lt_char(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_le_char(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_eq_enum(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_ne_enum(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_test_eq_int32(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_ne_int32(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_gt_int32(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_ge_int32(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_lt_int32(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_le_int32(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_test_eq_int64(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_ne_int64(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_gt_int64(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_ge_int64(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_lt_int64(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_le_int64(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_test_eq_float32(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_ne_float32(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_gt_float32(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_ge_float32(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_lt_float32(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_le_float32(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_test_eq_float64(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_ne_float64(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_gt_float64(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_ge_float64(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_lt_float64(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_le_float64(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_assert(&mut self, _value: Register) {
        unimplemented!();
    }

    fn visit_jump_if_false(&mut self, _opnd: Register, _offset: u32) {
        unimplemented!();
    }
    fn visit_jump_if_false_const(&mut self, _opnd: Register, _idx: ConstPoolIdx) {
        unimplemented!();
    }
    fn visit_jump_if_true(&mut self, _opnd: Register, _offset: u32) {
        unimplemented!();
    }
    fn visit_jump_if_true_const(&mut self, _opnd: Register, _idx: ConstPoolIdx) {
        unimplemented!();
    }
    fn visit_jump_loop(&mut self, _offset: u32) {
        unimplemented!();
    }
    fn visit_loop_start(&mut self) {
        unimplemented!();
    }
    fn visit_jump(&mut self, _offset: u32) {
        unimplemented!();
    }
    fn visit_jump_const(&mut self, _idx: ConstPoolIdx) {
        unimplemented!();
    }

    fn visit_invoke_direct_void(&mut self, _fct: ConstPoolIdx) {
        unimplemented!();
    }
    fn visit_invoke_direct(&mut self, _dest: Register, _fct: ConstPoolIdx) {
        unimplemented!();
    }

    fn visit_invoke_virtual_void(&mut self, _fct: ConstPoolIdx) {
        unimplemented!();
    }
    fn visit_invoke_virtual(&mut self, _dest: Register, _fct: ConstPoolIdx) {
        unimplemented!();
    }

    fn visit_invoke_static_void(&mut self, _fct: ConstPoolIdx) {
        unimplemented!();
    }
    fn visit_invoke_static(&mut self, _dest: Register, _fct: ConstPoolIdx) {
        unimplemented!();
    }

    fn visit_invoke_generic_static_void(&mut self, _fct: ConstPoolIdx) {
        unimplemented!();
    }
    fn visit_invoke_generic_static(&mut self, _dest: Register, _fct: ConstPoolIdx) {
        unimplemented!();
    }

    fn visit_invoke_generic_direct_void(&mut self, _fct: ConstPoolIdx) {
        unimplemented!();
    }
    fn visit_invoke_generic_direct(&mut self, _dest: Register, _fct: ConstPoolIdx) {
        unimplemented!();
    }

    fn visit_new_object(&mut self, _dest: Register, _cls: ConstPoolIdx) {
        unimplemented!();
    }
    fn visit_new_array(&mut self, _dest: Register, _cls: ConstPoolIdx, _length: Register) {
        unimplemented!();
    }
    fn visit_new_tuple(&mut self, _dest: Register, _tuple: TupleId) {
        unimplemented!();
    }

    fn visit_nil_check(&mut self, _obj: Register) {
        unimplemented!();
    }

    fn visit_array_length(&mut self, _dest: Register, _arr: Register) {
        unimplemented!();
    }
    fn visit_array_bound_check(&mut self, _arr: Register, _idx: Register) {
        unimplemented!();
    }

    fn visit_load_array_bool(&mut self, _dest: Register, _arr: Register, _idx: Register) {
        unimplemented!();
    }
    fn visit_load_array_uint8(&mut self, _dest: Register, _arr: Register, _idx: Register) {
        unimplemented!();
    }
    fn visit_load_array_char(&mut self, _dest: Register, _arr: Register, _idx: Register) {
        unimplemented!();
    }
    fn visit_load_array_int32(&mut self, _dest: Register, _arr: Register, _idx: Register) {
        unimplemented!();
    }
    fn visit_load_array_int64(&mut self, _dest: Register, _arr: Register, _idx: Register) {
        unimplemented!();
    }
    fn visit_load_array_float32(&mut self, _dest: Register, _arr: Register, _idx: Register) {
        unimplemented!();
    }
    fn visit_load_array_float64(&mut self, _dest: Register, _arr: Register, _idx: Register) {
        unimplemented!();
    }
    fn visit_load_array_ptr(&mut self, _dest: Register, _arr: Register, _idx: Register) {
        unimplemented!();
    }
    fn visit_load_array_tuple(&mut self, _dest: Register, _arr: Register, _idx: Register) {
        unimplemented!();
    }
    fn visit_load_array_generic(&mut self, _dest: Register, _arr: Register, _idx: Register) {
        unimplemented!();
    }
    fn visit_load_array_enum(
        &mut self,
        _dest: Register,
        _arr: Register,
        _idx: Register,
        _enum_idx: ConstPoolIdx,
    ) {
        unimplemented!();
    }

    fn visit_store_array_bool(&mut self, _src: Register, _arr: Register, _idx: Register) {
        unimplemented!();
    }
    fn visit_store_array_uint8(&mut self, _src: Register, _arr: Register, _idx: Register) {
        unimplemented!();
    }
    fn visit_store_array_char(&mut self, _src: Register, _arr: Register, _idx: Register) {
        unimplemented!();
    }
    fn visit_store_array_int32(&mut self, _src: Register, _arr: Register, _idx: Register) {
        unimplemented!();
    }
    fn visit_store_array_int64(&mut self, _src: Register, _arr: Register, _idx: Register) {
        unimplemented!();
    }
    fn visit_store_array_float32(&mut self, _src: Register, _arr: Register, _idx: Register) {
        unimplemented!();
    }
    fn visit_store_array_float64(&mut self, _src: Register, _arr: Register, _idx: Register) {
        unimplemented!();
    }
    fn visit_store_array_ptr(&mut self, _src: Register, _arr: Register, _idx: Register) {
        unimplemented!();
    }
    fn visit_store_array_tuple(&mut self, _src: Register, _arr: Register, _idx: Register) {
        unimplemented!();
    }
    fn visit_store_array_generic(&mut self, _src: Register, _arr: Register, _idx: Register) {
        unimplemented!();
    }
    fn visit_store_array_enum(
        &mut self,
        _dest: Register,
        _arr: Register,
        _idx: Register,
        _enum_idx: ConstPoolIdx,
    ) {
        unimplemented!();
    }

    fn visit_ret_void(&mut self) {
        unimplemented!();
    }
    fn visit_ret(&mut self, _opnd: Register) {
        unimplemented!();
    }
}
