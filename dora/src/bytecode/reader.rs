use num_traits::cast::FromPrimitive;

use crate::bytecode::{BytecodeOffset, BytecodeOpcode, ConstPoolIdx, Register};
use crate::vm::{ClassDefId, FctId, FieldId, GlobalId};

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
            let opcode = self.read_opcode(wide);
            self.read_instruction(wide, opcode)
        }
    }

    fn read_instruction(&mut self, wide: bool, opcode: u32) {
        let inst: BytecodeOpcode = FromPrimitive::from_u32(opcode).expect("illegal opcode");

        match inst {
            BytecodeOpcode::Wide => unreachable!(),

            BytecodeOpcode::AddInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_add_int(dest, lhs, rhs);
            }
            BytecodeOpcode::AddLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_add_long(dest, lhs, rhs);
            }
            BytecodeOpcode::AddFloat => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_add_float(dest, lhs, rhs);
            }
            BytecodeOpcode::AddDouble => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_add_double(dest, lhs, rhs);
            }
            BytecodeOpcode::SubInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_sub_int(dest, lhs, rhs);
            }
            BytecodeOpcode::SubLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_sub_long(dest, lhs, rhs);
            }
            BytecodeOpcode::SubFloat => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_sub_float(dest, lhs, rhs);
            }
            BytecodeOpcode::SubDouble => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_sub_double(dest, lhs, rhs);
            }
            BytecodeOpcode::NegInt => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_neg_int(dest, src);
            }
            BytecodeOpcode::NegLong => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_neg_long(dest, src);
            }
            BytecodeOpcode::NegFloat => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_neg_float(dest, src);
            }
            BytecodeOpcode::NegDouble => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_neg_double(dest, src);
            }
            BytecodeOpcode::MulInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_mul_int(dest, lhs, rhs);
            }
            BytecodeOpcode::MulLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_mul_long(dest, lhs, rhs);
            }
            BytecodeOpcode::MulFloat => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_mul_float(dest, lhs, rhs);
            }
            BytecodeOpcode::MulDouble => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_mul_double(dest, lhs, rhs);
            }
            BytecodeOpcode::DivInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_div_int(dest, lhs, rhs);
            }
            BytecodeOpcode::DivLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_div_long(dest, lhs, rhs);
            }
            BytecodeOpcode::DivFloat => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_div_float(dest, lhs, rhs);
            }
            BytecodeOpcode::DivDouble => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_div_double(dest, lhs, rhs);
            }

            BytecodeOpcode::ModInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_mod_int(dest, lhs, rhs);
            }
            BytecodeOpcode::ModLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_mod_long(dest, lhs, rhs);
            }

            BytecodeOpcode::AndInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_and_int(dest, lhs, rhs);
            }
            BytecodeOpcode::AndLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_and_long(dest, lhs, rhs);
            }

            BytecodeOpcode::OrInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_or_int(dest, lhs, rhs);
            }
            BytecodeOpcode::OrLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_or_long(dest, lhs, rhs);
            }

            BytecodeOpcode::XorInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_xor_int(dest, lhs, rhs);
            }
            BytecodeOpcode::XorLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_xor_long(dest, lhs, rhs);
            }

            BytecodeOpcode::NotBool => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_not_bool(dest, src);
            }
            BytecodeOpcode::NotInt => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_not_int(dest, src);
            }
            BytecodeOpcode::NotLong => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_not_long(dest, src);
            }

            BytecodeOpcode::ShlInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_shl_int(dest, lhs, rhs);
            }
            BytecodeOpcode::ShrInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_shr_int(dest, lhs, rhs);
            }
            BytecodeOpcode::SarInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_sar_int(dest, lhs, rhs);
            }

            BytecodeOpcode::ShlLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_shl_long(dest, lhs, rhs);
            }
            BytecodeOpcode::ShrLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_shr_long(dest, lhs, rhs);
            }
            BytecodeOpcode::SarLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_sar_long(dest, lhs, rhs);
            }

            BytecodeOpcode::RotateLeftInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_rotate_left_int(dest, lhs, rhs);
            }
            BytecodeOpcode::RotateRightInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_rotate_right_int(dest, lhs, rhs);
            }

            BytecodeOpcode::MovBool => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_mov_bool(dest, src);
            }
            BytecodeOpcode::MovByte => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_mov_byte(dest, src);
            }
            BytecodeOpcode::MovChar => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_mov_char(dest, src);
            }
            BytecodeOpcode::MovInt => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_mov_int(dest, src);
            }
            BytecodeOpcode::MovLong => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_mov_long(dest, src);
            }
            BytecodeOpcode::MovFloat => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_mov_float(dest, src);
            }
            BytecodeOpcode::MovDouble => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_mov_double(dest, src);
            }
            BytecodeOpcode::MovPtr => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_mov_ptr(dest, src);
            }

            BytecodeOpcode::LoadFieldBool => {
                let dest = self.read_register(wide);
                let obj = self.read_register(wide);
                let cls = self.read_class(wide);
                let field = self.read_field(wide);
                self.visitor.visit_load_field_bool(dest, obj, cls, field);
            }
            BytecodeOpcode::LoadFieldByte => {
                let dest = self.read_register(wide);
                let obj = self.read_register(wide);
                let cls = self.read_class(wide);
                let field = self.read_field(wide);
                self.visitor.visit_load_field_byte(dest, obj, cls, field);
            }
            BytecodeOpcode::LoadFieldChar => {
                let dest = self.read_register(wide);
                let obj = self.read_register(wide);
                let cls = self.read_class(wide);
                let field = self.read_field(wide);
                self.visitor.visit_load_field_char(dest, obj, cls, field);
            }
            BytecodeOpcode::LoadFieldInt => {
                let dest = self.read_register(wide);
                let obj = self.read_register(wide);
                let cls = self.read_class(wide);
                let field = self.read_field(wide);
                self.visitor.visit_load_field_int(dest, obj, cls, field);
            }
            BytecodeOpcode::LoadFieldLong => {
                let dest = self.read_register(wide);
                let obj = self.read_register(wide);
                let cls = self.read_class(wide);
                let field = self.read_field(wide);
                self.visitor.visit_load_field_long(dest, obj, cls, field);
            }
            BytecodeOpcode::LoadFieldFloat => {
                let dest = self.read_register(wide);
                let obj = self.read_register(wide);
                let cls = self.read_class(wide);
                let field = self.read_field(wide);
                self.visitor.visit_load_field_float(dest, obj, cls, field);
            }
            BytecodeOpcode::LoadFieldDouble => {
                let dest = self.read_register(wide);
                let obj = self.read_register(wide);
                let cls = self.read_class(wide);
                let field = self.read_field(wide);
                self.visitor.visit_load_field_double(dest, obj, cls, field);
            }
            BytecodeOpcode::LoadFieldPtr => {
                let dest = self.read_register(wide);
                let obj = self.read_register(wide);
                let cls = self.read_class(wide);
                let field = self.read_field(wide);
                self.visitor.visit_load_field_ptr(dest, obj, cls, field);
            }

            BytecodeOpcode::StoreFieldBool => {
                let src = self.read_register(wide);
                let obj = self.read_register(wide);
                let cls = self.read_class(wide);
                let field = self.read_field(wide);
                self.visitor.visit_store_field_bool(src, obj, cls, field);
            }
            BytecodeOpcode::StoreFieldByte => {
                let src = self.read_register(wide);
                let obj = self.read_register(wide);
                let cls = self.read_class(wide);
                let field = self.read_field(wide);
                self.visitor.visit_store_field_byte(src, obj, cls, field);
            }
            BytecodeOpcode::StoreFieldChar => {
                let src = self.read_register(wide);
                let obj = self.read_register(wide);
                let cls = self.read_class(wide);
                let field = self.read_field(wide);
                self.visitor.visit_store_field_char(src, obj, cls, field);
            }
            BytecodeOpcode::StoreFieldInt => {
                let src = self.read_register(wide);
                let obj = self.read_register(wide);
                let cls = self.read_class(wide);
                let field = self.read_field(wide);
                self.visitor.visit_store_field_int(src, obj, cls, field);
            }
            BytecodeOpcode::StoreFieldLong => {
                let src = self.read_register(wide);
                let obj = self.read_register(wide);
                let cls = self.read_class(wide);
                let field = self.read_field(wide);
                self.visitor.visit_store_field_long(src, obj, cls, field);
            }
            BytecodeOpcode::StoreFieldFloat => {
                let src = self.read_register(wide);
                let obj = self.read_register(wide);
                let cls = self.read_class(wide);
                let field = self.read_field(wide);
                self.visitor.visit_store_field_float(src, obj, cls, field);
            }
            BytecodeOpcode::StoreFieldDouble => {
                let src = self.read_register(wide);
                let obj = self.read_register(wide);
                let cls = self.read_class(wide);
                let field = self.read_field(wide);
                self.visitor.visit_store_field_double(src, obj, cls, field);
            }
            BytecodeOpcode::StoreFieldPtr => {
                let src = self.read_register(wide);
                let obj = self.read_register(wide);
                let cls = self.read_class(wide);
                let field = self.read_field(wide);
                self.visitor.visit_store_field_ptr(src, obj, cls, field);
            }

            BytecodeOpcode::LoadGlobalBool => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_load_global_bool(dest, glob);
            }
            BytecodeOpcode::LoadGlobalByte => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_load_global_byte(dest, glob);
            }
            BytecodeOpcode::LoadGlobalChar => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_load_global_char(dest, glob);
            }
            BytecodeOpcode::LoadGlobalInt => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_load_global_int(dest, glob);
            }
            BytecodeOpcode::LoadGlobalLong => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_load_global_long(dest, glob);
            }
            BytecodeOpcode::LoadGlobalFloat => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_load_global_float(dest, glob);
            }
            BytecodeOpcode::LoadGlobalDouble => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_load_global_double(dest, glob);
            }
            BytecodeOpcode::LoadGlobalPtr => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_load_global_ptr(dest, glob);
            }

            BytecodeOpcode::StoreGlobalBool => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_store_global_bool(dest, glob);
            }
            BytecodeOpcode::StoreGlobalByte => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_store_global_byte(dest, glob);
            }
            BytecodeOpcode::StoreGlobalChar => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_store_global_char(dest, glob);
            }
            BytecodeOpcode::StoreGlobalInt => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_store_global_int(dest, glob);
            }
            BytecodeOpcode::StoreGlobalLong => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_store_global_long(dest, glob);
            }
            BytecodeOpcode::StoreGlobalFloat => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_store_global_float(dest, glob);
            }
            BytecodeOpcode::StoreGlobalDouble => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_store_global_double(dest, glob);
            }
            BytecodeOpcode::StoreGlobalPtr => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_store_global_ptr(dest, glob);
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
            BytecodeOpcode::ConstZeroByte => {
                let dest = self.read_register(wide);
                self.visitor.visit_const_zero_byte(dest);
            }
            BytecodeOpcode::ConstZeroChar => {
                let dest = self.read_register(wide);
                self.visitor.visit_const_zero_char(dest);
            }
            BytecodeOpcode::ConstZeroInt => {
                let dest = self.read_register(wide);
                self.visitor.visit_const_zero_int(dest);
            }
            BytecodeOpcode::ConstZeroLong => {
                let dest = self.read_register(wide);
                self.visitor.visit_const_zero_long(dest);
            }
            BytecodeOpcode::ConstZeroFloat => {
                let dest = self.read_register(wide);
                self.visitor.visit_const_zero_float(dest);
            }
            BytecodeOpcode::ConstZeroDouble => {
                let dest = self.read_register(wide);
                self.visitor.visit_const_zero_double(dest);
            }
            BytecodeOpcode::ConstChar => {
                let dest = self.read_register(wide);
                let idx = self.read_const_pool_idx(wide);
                self.visitor.visit_const_char(dest, idx);
            }
            BytecodeOpcode::ConstByte => {
                let dest = self.read_register(wide);
                let value = self.read_byte();
                self.visitor.visit_const_byte(dest, value as u8);
            }
            BytecodeOpcode::ConstInt => {
                let dest = self.read_register(wide);
                let idx = self.read_const_pool_idx(wide);
                self.visitor.visit_const_int(dest, idx);
            }
            BytecodeOpcode::ConstLong => {
                let dest = self.read_register(wide);
                let idx = self.read_const_pool_idx(wide);
                self.visitor.visit_const_long(dest, idx);
            }
            BytecodeOpcode::ConstFloat => {
                let dest = self.read_register(wide);
                let idx = self.read_const_pool_idx(wide);
                self.visitor.visit_const_float(dest, idx);
            }
            BytecodeOpcode::ConstDouble => {
                let dest = self.read_register(wide);
                let idx = self.read_const_pool_idx(wide);
                self.visitor.visit_const_double(dest, idx);
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
            BytecodeOpcode::TestEqByte => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_eq_byte(dest, lhs, rhs);
            }
            BytecodeOpcode::TestNeByte => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ne_byte(dest, lhs, rhs);
            }
            BytecodeOpcode::TestGtByte => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_gt_byte(dest, lhs, rhs);
            }
            BytecodeOpcode::TestGeByte => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ge_byte(dest, lhs, rhs);
            }
            BytecodeOpcode::TestLtByte => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_lt_byte(dest, lhs, rhs);
            }
            BytecodeOpcode::TestLeByte => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_le_byte(dest, lhs, rhs);
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
            BytecodeOpcode::TestEqInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_eq_int(dest, lhs, rhs);
            }
            BytecodeOpcode::TestNeInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ne_int(dest, lhs, rhs);
            }
            BytecodeOpcode::TestGtInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_gt_int(dest, lhs, rhs);
            }
            BytecodeOpcode::TestGeInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ge_int(dest, lhs, rhs);
            }
            BytecodeOpcode::TestLtInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_lt_int(dest, lhs, rhs);
            }
            BytecodeOpcode::TestLeInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_le_int(dest, lhs, rhs);
            }
            BytecodeOpcode::TestEqLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_eq_long(dest, lhs, rhs);
            }
            BytecodeOpcode::TestNeLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ne_long(dest, lhs, rhs);
            }
            BytecodeOpcode::TestGtLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_gt_long(dest, lhs, rhs);
            }
            BytecodeOpcode::TestGeLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ge_long(dest, lhs, rhs);
            }
            BytecodeOpcode::TestLtLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_lt_long(dest, lhs, rhs);
            }
            BytecodeOpcode::TestLeLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_le_long(dest, lhs, rhs);
            }

            BytecodeOpcode::TestEqFloat => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_eq_float(dest, lhs, rhs);
            }
            BytecodeOpcode::TestNeFloat => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ne_float(dest, lhs, rhs);
            }
            BytecodeOpcode::TestGtFloat => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_gt_float(dest, lhs, rhs);
            }
            BytecodeOpcode::TestGeFloat => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ge_float(dest, lhs, rhs);
            }
            BytecodeOpcode::TestLtFloat => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_lt_float(dest, lhs, rhs);
            }
            BytecodeOpcode::TestLeFloat => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_le_float(dest, lhs, rhs);
            }

            BytecodeOpcode::TestEqDouble => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_eq_double(dest, lhs, rhs);
            }
            BytecodeOpcode::TestNeDouble => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ne_double(dest, lhs, rhs);
            }
            BytecodeOpcode::TestGtDouble => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_gt_double(dest, lhs, rhs);
            }
            BytecodeOpcode::TestGeDouble => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ge_double(dest, lhs, rhs);
            }
            BytecodeOpcode::TestLtDouble => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_lt_double(dest, lhs, rhs);
            }
            BytecodeOpcode::TestLeDouble => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_le_double(dest, lhs, rhs);
            }

            BytecodeOpcode::JumpLoop => {
                let offset = self.read_offset(wide);
                self.visitor.visit_jump_loop(offset);
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
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor.visit_invoke_direct_void(fct, start, count);
            }
            BytecodeOpcode::InvokeDirectBool => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_direct_bool(dest, fct, start, count);
            }
            BytecodeOpcode::InvokeDirectByte => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_direct_byte(dest, fct, start, count);
            }
            BytecodeOpcode::InvokeDirectChar => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_direct_char(dest, fct, start, count);
            }
            BytecodeOpcode::InvokeDirectInt => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_direct_int(dest, fct, start, count);
            }
            BytecodeOpcode::InvokeDirectLong => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_direct_long(dest, fct, start, count);
            }
            BytecodeOpcode::InvokeDirectFloat => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_direct_float(dest, fct, start, count);
            }
            BytecodeOpcode::InvokeDirectDouble => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_direct_double(dest, fct, start, count);
            }
            BytecodeOpcode::InvokeDirectPtr => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_direct_ptr(dest, fct, start, count);
            }

            BytecodeOpcode::InvokeVirtualVoid => {
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor.visit_invoke_virtual_void(fct, start, count);
            }
            BytecodeOpcode::InvokeVirtualBool => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_virtual_bool(dest, fct, start, count);
            }
            BytecodeOpcode::InvokeVirtualByte => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_virtual_byte(dest, fct, start, count);
            }
            BytecodeOpcode::InvokeVirtualChar => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_virtual_char(dest, fct, start, count);
            }
            BytecodeOpcode::InvokeVirtualInt => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_virtual_int(dest, fct, start, count);
            }
            BytecodeOpcode::InvokeVirtualLong => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_virtual_long(dest, fct, start, count);
            }
            BytecodeOpcode::InvokeVirtualFloat => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_virtual_float(dest, fct, start, count);
            }
            BytecodeOpcode::InvokeVirtualDouble => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_virtual_double(dest, fct, start, count);
            }
            BytecodeOpcode::InvokeVirtualPtr => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_virtual_ptr(dest, fct, start, count);
            }

            BytecodeOpcode::InvokeStaticVoid => {
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor.visit_invoke_static_void(fct, start, count);
            }
            BytecodeOpcode::InvokeStaticBool => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_static_bool(dest, fct, start, count);
            }
            BytecodeOpcode::InvokeStaticByte => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_static_byte(dest, fct, start, count);
            }
            BytecodeOpcode::InvokeStaticChar => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_static_char(dest, fct, start, count);
            }
            BytecodeOpcode::InvokeStaticInt => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_static_int(dest, fct, start, count);
            }
            BytecodeOpcode::InvokeStaticLong => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_static_long(dest, fct, start, count);
            }
            BytecodeOpcode::InvokeStaticFloat => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_static_float(dest, fct, start, count);
            }
            BytecodeOpcode::InvokeStaticDouble => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_static_double(dest, fct, start, count);
            }
            BytecodeOpcode::InvokeStaticPtr => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_static_ptr(dest, fct, start, count);
            }

            BytecodeOpcode::NewObject => {
                let dest = self.read_register(wide);
                let cls = self.read_class(wide);
                self.visitor.visit_new_object(dest, cls);
            }

            BytecodeOpcode::Throw => {
                let opnd = self.read_register(wide);
                self.visitor.visit_throw(opnd);
            }

            BytecodeOpcode::RetVoid => {
                self.visitor.visit_ret_void();
            }
            BytecodeOpcode::RetBool => {
                let opnd = self.read_register(wide);
                self.visitor.visit_ret_bool(opnd);
            }
            BytecodeOpcode::RetByte => {
                let opnd = self.read_register(wide);
                self.visitor.visit_ret_byte(opnd);
            }
            BytecodeOpcode::RetChar => {
                let opnd = self.read_register(wide);
                self.visitor.visit_ret_char(opnd);
            }
            BytecodeOpcode::RetInt => {
                let opnd = self.read_register(wide);
                self.visitor.visit_ret_int(opnd);
            }
            BytecodeOpcode::RetLong => {
                let opnd = self.read_register(wide);
                self.visitor.visit_ret_long(opnd);
            }
            BytecodeOpcode::RetFloat => {
                let opnd = self.read_register(wide);
                self.visitor.visit_ret_float(opnd);
            }
            BytecodeOpcode::RetDouble => {
                let opnd = self.read_register(wide);
                self.visitor.visit_ret_double(opnd);
            }
            BytecodeOpcode::RetPtr => {
                let opnd = self.read_register(wide);
                self.visitor.visit_ret_ptr(opnd);
            }
        }
    }

    fn read_register(&mut self, wide: bool) -> Register {
        Register(self.read_index(wide) as usize)
    }

    fn read_fct(&mut self, wide: bool) -> FctId {
        (self.read_index(wide) as usize).into()
    }

    fn read_class(&mut self, wide: bool) -> ClassDefId {
        (self.read_index(wide) as usize).into()
    }

    fn read_field(&mut self, wide: bool) -> FieldId {
        (self.read_index(wide) as usize).into()
    }

    fn read_global(&mut self, wide: bool) -> GlobalId {
        self.read_index(wide).into()
    }

    fn read_opcode(&mut self, wide: bool) -> u32 {
        self.read_index(wide)
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

    fn visit_add_int(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_add_long(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_add_float(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_add_double(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_sub_int(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_sub_long(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_sub_float(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_sub_double(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_neg_int(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_neg_long(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_neg_float(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_neg_double(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }

    fn visit_mul_int(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_mul_long(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_mul_float(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_mul_double(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_div_int(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_div_long(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_div_float(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_div_double(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_mod_int(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_mod_long(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_and_int(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_and_long(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_or_int(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_or_long(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_xor_int(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_xor_long(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_not_bool(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_not_int(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_not_long(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }

    fn visit_shl_int(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_shr_int(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_sar_int(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_shl_long(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_shr_long(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_sar_long(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_rotate_left_int(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_rotate_right_int(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_mov_bool(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_mov_byte(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_mov_char(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_mov_int(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_mov_long(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_mov_float(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_mov_double(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_mov_ptr(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }

    fn visit_load_field_bool(
        &mut self,
        _dest: Register,
        _obj: Register,
        _cls: ClassDefId,
        _field: FieldId,
    ) {
        unimplemented!();
    }
    fn visit_load_field_byte(
        &mut self,
        _dest: Register,
        _obj: Register,
        _cls: ClassDefId,
        _field: FieldId,
    ) {
        unimplemented!();
    }
    fn visit_load_field_char(
        &mut self,
        _dest: Register,
        _obj: Register,
        _cls: ClassDefId,
        _field: FieldId,
    ) {
        unimplemented!();
    }
    fn visit_load_field_int(
        &mut self,
        _dest: Register,
        _obj: Register,
        _cls: ClassDefId,
        _field: FieldId,
    ) {
        unimplemented!();
    }
    fn visit_load_field_long(
        &mut self,
        _dest: Register,
        _obj: Register,
        _cls: ClassDefId,
        _field: FieldId,
    ) {
        unimplemented!();
    }
    fn visit_load_field_float(
        &mut self,
        _dest: Register,
        _obj: Register,
        _cls: ClassDefId,
        _field: FieldId,
    ) {
        unimplemented!();
    }
    fn visit_load_field_double(
        &mut self,
        _dest: Register,
        _obj: Register,
        _cls: ClassDefId,
        _field: FieldId,
    ) {
        unimplemented!();
    }
    fn visit_load_field_ptr(
        &mut self,
        _dest: Register,
        _obj: Register,
        _cls: ClassDefId,
        _field: FieldId,
    ) {
        unimplemented!();
    }

    fn visit_store_field_bool(
        &mut self,
        _src: Register,
        _obj: Register,
        _cls: ClassDefId,
        _field: FieldId,
    ) {
        unimplemented!();
    }
    fn visit_store_field_byte(
        &mut self,
        _src: Register,
        _obj: Register,
        _cls: ClassDefId,
        _field: FieldId,
    ) {
        unimplemented!();
    }
    fn visit_store_field_char(
        &mut self,
        _src: Register,
        _obj: Register,
        _cls: ClassDefId,
        _field: FieldId,
    ) {
        unimplemented!();
    }
    fn visit_store_field_int(
        &mut self,
        _src: Register,
        _obj: Register,
        _cls: ClassDefId,
        _field: FieldId,
    ) {
        unimplemented!();
    }
    fn visit_store_field_long(
        &mut self,
        _src: Register,
        _obj: Register,
        _cls: ClassDefId,
        _field: FieldId,
    ) {
        unimplemented!();
    }
    fn visit_store_field_float(
        &mut self,
        _src: Register,
        _obj: Register,
        _cls: ClassDefId,
        _field: FieldId,
    ) {
        unimplemented!();
    }
    fn visit_store_field_double(
        &mut self,
        _src: Register,
        _obj: Register,
        _cls: ClassDefId,
        _field: FieldId,
    ) {
        unimplemented!();
    }
    fn visit_store_field_ptr(
        &mut self,
        _src: Register,
        _obj: Register,
        _cls: ClassDefId,
        _field: FieldId,
    ) {
        unimplemented!();
    }

    fn visit_load_global_bool(&mut self, _dest: Register, _glob: GlobalId) {
        unimplemented!();
    }
    fn visit_load_global_byte(&mut self, _dest: Register, _glob: GlobalId) {
        unimplemented!();
    }
    fn visit_load_global_char(&mut self, _dest: Register, _glob: GlobalId) {
        unimplemented!();
    }
    fn visit_load_global_int(&mut self, _dest: Register, _glob: GlobalId) {
        unimplemented!();
    }
    fn visit_load_global_long(&mut self, _dest: Register, _glob: GlobalId) {
        unimplemented!();
    }
    fn visit_load_global_float(&mut self, _dest: Register, _glob: GlobalId) {
        unimplemented!();
    }
    fn visit_load_global_double(&mut self, _dest: Register, _glob: GlobalId) {
        unimplemented!();
    }
    fn visit_load_global_ptr(&mut self, _dest: Register, _glob: GlobalId) {
        unimplemented!();
    }

    fn visit_store_global_bool(&mut self, _src: Register, _glob: GlobalId) {
        unimplemented!();
    }
    fn visit_store_global_byte(&mut self, _src: Register, _glob: GlobalId) {
        unimplemented!();
    }
    fn visit_store_global_char(&mut self, _src: Register, _glob: GlobalId) {
        unimplemented!();
    }
    fn visit_store_global_int(&mut self, _src: Register, _glob: GlobalId) {
        unimplemented!();
    }
    fn visit_store_global_long(&mut self, _src: Register, _glob: GlobalId) {
        unimplemented!();
    }
    fn visit_store_global_float(&mut self, _src: Register, _glob: GlobalId) {
        unimplemented!();
    }
    fn visit_store_global_double(&mut self, _src: Register, _glob: GlobalId) {
        unimplemented!();
    }
    fn visit_store_global_ptr(&mut self, _src: Register, _glob: GlobalId) {
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
    fn visit_const_zero_byte(&mut self, _dest: Register) {
        unimplemented!();
    }
    fn visit_const_zero_char(&mut self, _dest: Register) {
        unimplemented!();
    }
    fn visit_const_zero_int(&mut self, _dest: Register) {
        unimplemented!();
    }
    fn visit_const_zero_long(&mut self, _dest: Register) {
        unimplemented!();
    }
    fn visit_const_zero_float(&mut self, _dest: Register) {
        unimplemented!();
    }
    fn visit_const_zero_double(&mut self, _dest: Register) {
        unimplemented!();
    }
    fn visit_const_char(&mut self, _dest: Register, _value: ConstPoolIdx) {
        unimplemented!();
    }
    fn visit_const_byte(&mut self, _dest: Register, _value: u8) {
        unimplemented!();
    }
    fn visit_const_int(&mut self, _dest: Register, _value: ConstPoolIdx) {
        unimplemented!();
    }
    fn visit_const_long(&mut self, _dest: Register, _value: ConstPoolIdx) {
        unimplemented!();
    }
    fn visit_const_float(&mut self, _dest: Register, _value: ConstPoolIdx) {
        unimplemented!();
    }
    fn visit_const_double(&mut self, _dest: Register, _value: ConstPoolIdx) {
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
    fn visit_test_eq_bool(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_ne_bool(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_eq_byte(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_ne_byte(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_gt_byte(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_ge_byte(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_lt_byte(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_le_byte(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
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
    fn visit_test_eq_int(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_ne_int(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_gt_int(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_ge_int(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_lt_int(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_le_int(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_test_eq_long(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_ne_long(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_gt_long(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_ge_long(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_lt_long(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_le_long(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_test_eq_float(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_ne_float(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_gt_float(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_ge_float(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_lt_float(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_le_float(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_test_eq_double(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_ne_double(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_gt_double(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_ge_double(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_lt_double(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_le_double(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
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
    fn visit_jump(&mut self, _offset: u32) {
        unimplemented!();
    }
    fn visit_jump_const(&mut self, _idx: ConstPoolIdx) {
        unimplemented!();
    }

    fn visit_invoke_direct_void(&mut self, _fct: FctId, _start: Register, _count: u32) {
        unimplemented!();
    }
    fn visit_invoke_direct_bool(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_direct_byte(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_direct_char(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_direct_int(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_direct_long(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_direct_float(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_direct_double(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_direct_ptr(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }

    fn visit_invoke_virtual_void(&mut self, _fct: FctId, _start: Register, _count: u32) {
        unimplemented!();
    }
    fn visit_invoke_virtual_bool(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_virtual_byte(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_virtual_char(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_virtual_int(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_virtual_long(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_virtual_float(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_virtual_double(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_virtual_ptr(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }

    fn visit_invoke_static_void(&mut self, _fct: FctId, _start: Register, _count: u32) {
        unimplemented!();
    }
    fn visit_invoke_static_bool(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_static_byte(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_static_char(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_static_int(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_static_long(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_static_float(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_static_double(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }
    fn visit_invoke_static_ptr(
        &mut self,
        _dest: Register,
        _fct: FctId,
        _start: Register,
        _count: u32,
    ) {
        unimplemented!();
    }

    fn visit_new_object(&mut self, _dest: Register, _cls: ClassDefId) {
        unimplemented!();
    }
    fn visit_throw(&mut self, _opnd: Register) {
        unimplemented!();
    }

    fn visit_ret_void(&mut self) {
        unimplemented!();
    }
    fn visit_ret_bool(&mut self, _opnd: Register) {
        unimplemented!();
    }
    fn visit_ret_byte(&mut self, _opnd: Register) {
        unimplemented!();
    }
    fn visit_ret_char(&mut self, _opnd: Register) {
        unimplemented!();
    }
    fn visit_ret_int(&mut self, _opnd: Register) {
        unimplemented!();
    }
    fn visit_ret_long(&mut self, _opnd: Register) {
        unimplemented!();
    }
    fn visit_ret_float(&mut self, _opnd: Register) {
        unimplemented!();
    }
    fn visit_ret_double(&mut self, _opnd: Register) {
        unimplemented!();
    }
    fn visit_ret_ptr(&mut self, _opnd: Register) {
        unimplemented!();
    }
}
