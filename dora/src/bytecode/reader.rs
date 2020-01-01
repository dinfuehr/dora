use num_traits::cast::FromPrimitive;

use crate::bytecode::{BytecodeInst, ConstPoolIdx, Register};
use crate::vm::{ClassDefId, FctId, FieldId, GlobalId};

pub struct BytecodeReader<'a, T: BytecodeVisitor> {
    data: &'a [u8],
    pos: usize,
    visitor: &'a mut T,
}

impl<'a, T> BytecodeReader<'a, T>
where
    T: BytecodeVisitor,
{
    pub fn new(data: &'a [u8], visitor: &'a mut T) -> BytecodeReader<'a, T> {
        BytecodeReader {
            data: data,
            pos: 0,
            visitor: visitor,
        }
    }

    pub fn read(&mut self) {
        while self.pos < self.data.len() {
            let wide = self.read_operand_width();
            let opcode = self.read_opcode(wide);
            self.read_instruction(wide, opcode)
        }
    }

    fn read_instruction(&mut self, wide: bool, opcode: u32) {
        let inst: BytecodeInst = FromPrimitive::from_u32(opcode).expect("illegal opcode");

        match inst {
            BytecodeInst::Wide => unreachable!(),

            BytecodeInst::AddInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_add_int(dest, lhs, rhs);
            }
            BytecodeInst::AddLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_add_long(dest, lhs, rhs);
            }
            BytecodeInst::AddFloat => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_add_float(dest, lhs, rhs);
            }
            BytecodeInst::AddDouble => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_add_double(dest, lhs, rhs);
            }
            BytecodeInst::SubInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_sub_int(dest, lhs, rhs);
            }
            BytecodeInst::SubLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_sub_long(dest, lhs, rhs);
            }
            BytecodeInst::SubFloat => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_sub_float(dest, lhs, rhs);
            }
            BytecodeInst::SubDouble => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_sub_double(dest, lhs, rhs);
            }
            BytecodeInst::NegInt => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_neg_int(dest, src);
            }
            BytecodeInst::NegLong => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_neg_long(dest, src);
            }
            BytecodeInst::NegFloat => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_neg_float(dest, src);
            }
            BytecodeInst::NegDouble => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_neg_double(dest, src);
            }
            BytecodeInst::MulInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_sub_int(dest, lhs, rhs);
            }
            BytecodeInst::MulLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_sub_long(dest, lhs, rhs);
            }
            BytecodeInst::MulFloat => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_sub_float(dest, lhs, rhs);
            }
            BytecodeInst::MulDouble => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_sub_double(dest, lhs, rhs);
            }
            BytecodeInst::DivInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_div_int(dest, lhs, rhs);
            }
            BytecodeInst::DivLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_div_long(dest, lhs, rhs);
            }
            BytecodeInst::DivFloat => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_div_float(dest, lhs, rhs);
            }
            BytecodeInst::DivDouble => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_div_double(dest, lhs, rhs);
            }

            BytecodeInst::ModInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_mod_int(dest, lhs, rhs);
            }
            BytecodeInst::ModLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_mod_long(dest, lhs, rhs);
            }

            BytecodeInst::AndInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_and_int(dest, lhs, rhs);
            }
            BytecodeInst::AndLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_and_long(dest, lhs, rhs);
            }

            BytecodeInst::OrInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_or_int(dest, lhs, rhs);
            }
            BytecodeInst::OrLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_or_long(dest, lhs, rhs);
            }

            BytecodeInst::XorInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_xor_int(dest, lhs, rhs);
            }
            BytecodeInst::XorLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_xor_long(dest, lhs, rhs);
            }

            BytecodeInst::NotBool => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_not_bool(dest, src);
            }
            BytecodeInst::NotInt => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_not_int(dest, src);
            }
            BytecodeInst::NotLong => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_not_long(dest, src);
            }

            BytecodeInst::ShlInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_shl_int(dest, lhs, rhs);
            }
            BytecodeInst::ShrInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_shr_int(dest, lhs, rhs);
            }
            BytecodeInst::SarInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_sar_int(dest, lhs, rhs);
            }

            BytecodeInst::ShlLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_shl_long(dest, lhs, rhs);
            }
            BytecodeInst::ShrLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_shr_long(dest, lhs, rhs);
            }
            BytecodeInst::SarLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_sar_long(dest, lhs, rhs);
            }

            BytecodeInst::MovBool => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_mov_bool(dest, src);
            }
            BytecodeInst::MovByte => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_mov_byte(dest, src);
            }
            BytecodeInst::MovChar => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_mov_char(dest, src);
            }
            BytecodeInst::MovInt => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_mov_int(dest, src);
            }
            BytecodeInst::MovLong => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_mov_long(dest, src);
            }
            BytecodeInst::MovFloat => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_mov_float(dest, src);
            }
            BytecodeInst::MovDouble => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_mov_double(dest, src);
            }
            BytecodeInst::MovPtr => {
                let dest = self.read_register(wide);
                let src = self.read_register(wide);
                self.visitor.visit_mov_ptr(dest, src);
            }

            BytecodeInst::LoadFieldBool => {
                let dest = self.read_register(wide);
                let obj = self.read_register(wide);
                let cls = self.read_class(wide);
                let field = self.read_field(wide);
                self.visitor.visit_load_field_bool(dest, obj, cls, field);
            }
            BytecodeInst::LoadFieldByte => {
                let dest = self.read_register(wide);
                let obj = self.read_register(wide);
                let cls = self.read_class(wide);
                let field = self.read_field(wide);
                self.visitor.visit_load_field_byte(dest, obj, cls, field);
            }
            BytecodeInst::LoadFieldChar => {
                let dest = self.read_register(wide);
                let obj = self.read_register(wide);
                let cls = self.read_class(wide);
                let field = self.read_field(wide);
                self.visitor.visit_load_field_char(dest, obj, cls, field);
            }
            BytecodeInst::LoadFieldInt => {
                let dest = self.read_register(wide);
                let obj = self.read_register(wide);
                let cls = self.read_class(wide);
                let field = self.read_field(wide);
                self.visitor.visit_load_field_int(dest, obj, cls, field);
            }
            BytecodeInst::LoadFieldLong => {
                let dest = self.read_register(wide);
                let obj = self.read_register(wide);
                let cls = self.read_class(wide);
                let field = self.read_field(wide);
                self.visitor.visit_load_field_long(dest, obj, cls, field);
            }
            BytecodeInst::LoadFieldFloat => {
                let dest = self.read_register(wide);
                let obj = self.read_register(wide);
                let cls = self.read_class(wide);
                let field = self.read_field(wide);
                self.visitor.visit_load_field_float(dest, obj, cls, field);
            }
            BytecodeInst::LoadFieldDouble => {
                let dest = self.read_register(wide);
                let obj = self.read_register(wide);
                let cls = self.read_class(wide);
                let field = self.read_field(wide);
                self.visitor.visit_load_field_double(dest, obj, cls, field);
            }
            BytecodeInst::LoadFieldPtr => {
                let dest = self.read_register(wide);
                let obj = self.read_register(wide);
                let cls = self.read_class(wide);
                let field = self.read_field(wide);
                self.visitor.visit_load_field_ptr(dest, obj, cls, field);
            }

            BytecodeInst::StoreFieldBool => {
                let src = self.read_register(wide);
                let obj = self.read_register(wide);
                let cls = self.read_class(wide);
                let field = self.read_field(wide);
                self.visitor.visit_store_field_bool(src, obj, cls, field);
            }
            BytecodeInst::StoreFieldByte => {
                let src = self.read_register(wide);
                let obj = self.read_register(wide);
                let cls = self.read_class(wide);
                let field = self.read_field(wide);
                self.visitor.visit_store_field_byte(src, obj, cls, field);
            }
            BytecodeInst::StoreFieldChar => {
                let src = self.read_register(wide);
                let obj = self.read_register(wide);
                let cls = self.read_class(wide);
                let field = self.read_field(wide);
                self.visitor.visit_store_field_char(src, obj, cls, field);
            }
            BytecodeInst::StoreFieldInt => {
                let src = self.read_register(wide);
                let obj = self.read_register(wide);
                let cls = self.read_class(wide);
                let field = self.read_field(wide);
                self.visitor.visit_store_field_int(src, obj, cls, field);
            }
            BytecodeInst::StoreFieldLong => {
                let src = self.read_register(wide);
                let obj = self.read_register(wide);
                let cls = self.read_class(wide);
                let field = self.read_field(wide);
                self.visitor.visit_store_field_long(src, obj, cls, field);
            }
            BytecodeInst::StoreFieldFloat => {
                let src = self.read_register(wide);
                let obj = self.read_register(wide);
                let cls = self.read_class(wide);
                let field = self.read_field(wide);
                self.visitor.visit_store_field_float(src, obj, cls, field);
            }
            BytecodeInst::StoreFieldDouble => {
                let src = self.read_register(wide);
                let obj = self.read_register(wide);
                let cls = self.read_class(wide);
                let field = self.read_field(wide);
                self.visitor.visit_store_field_double(src, obj, cls, field);
            }
            BytecodeInst::StoreFieldPtr => {
                let src = self.read_register(wide);
                let obj = self.read_register(wide);
                let cls = self.read_class(wide);
                let field = self.read_field(wide);
                self.visitor.visit_store_field_ptr(src, obj, cls, field);
            }

            BytecodeInst::LoadGlobalBool => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_load_global_bool(dest, glob);
            }
            BytecodeInst::LoadGlobalByte => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_load_global_byte(dest, glob);
            }
            BytecodeInst::LoadGlobalChar => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_load_global_char(dest, glob);
            }
            BytecodeInst::LoadGlobalInt => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_load_global_int(dest, glob);
            }
            BytecodeInst::LoadGlobalLong => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_load_global_long(dest, glob);
            }
            BytecodeInst::LoadGlobalFloat => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_load_global_float(dest, glob);
            }
            BytecodeInst::LoadGlobalDouble => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_load_global_double(dest, glob);
            }
            BytecodeInst::LoadGlobalPtr => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_load_global_ptr(dest, glob);
            }

            BytecodeInst::StoreGlobalBool => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_store_global_bool(dest, glob);
            }
            BytecodeInst::StoreGlobalByte => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_store_global_byte(dest, glob);
            }
            BytecodeInst::StoreGlobalChar => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_store_global_char(dest, glob);
            }
            BytecodeInst::StoreGlobalInt => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_store_global_int(dest, glob);
            }
            BytecodeInst::StoreGlobalLong => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_store_global_long(dest, glob);
            }
            BytecodeInst::StoreGlobalFloat => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_store_global_float(dest, glob);
            }
            BytecodeInst::StoreGlobalDouble => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_store_global_double(dest, glob);
            }
            BytecodeInst::StoreGlobalPtr => {
                let dest = self.read_register(wide);
                let glob = self.read_global(wide);
                self.visitor.visit_store_global_ptr(dest, glob);
            }

            BytecodeInst::ConstNil => {
                let dest = self.read_register(wide);
                self.visitor.visit_const_nil(dest);
            }
            BytecodeInst::ConstTrue => {
                let dest = self.read_register(wide);
                self.visitor.visit_const_true(dest);
            }
            BytecodeInst::ConstFalse => {
                let dest = self.read_register(wide);
                self.visitor.visit_const_false(dest);
            }
            BytecodeInst::ConstZeroByte => {
                let dest = self.read_register(wide);
                self.visitor.visit_const_zero_byte(dest);
            }
            BytecodeInst::ConstZeroChar => {
                let dest = self.read_register(wide);
                self.visitor.visit_const_zero_char(dest);
            }
            BytecodeInst::ConstZeroInt => {
                let dest = self.read_register(wide);
                self.visitor.visit_const_zero_int(dest);
            }
            BytecodeInst::ConstZeroLong => {
                let dest = self.read_register(wide);
                self.visitor.visit_const_zero_long(dest);
            }
            BytecodeInst::ConstZeroFloat => {
                let dest = self.read_register(wide);
                self.visitor.visit_const_zero_float(dest);
            }
            BytecodeInst::ConstZeroDouble => {
                let dest = self.read_register(wide);
                self.visitor.visit_const_zero_double(dest);
            }
            BytecodeInst::ConstChar => {
                let dest = self.read_register(wide);
                let idx = self.read_const_pool_idx(wide);
                self.visitor.visit_const_char(dest, idx);
            }
            BytecodeInst::ConstByte => {
                let dest = self.read_register(wide);
                let value = self.read_byte();
                self.visitor.visit_const_byte(dest, value as u8);
            }
            BytecodeInst::ConstInt => {
                let dest = self.read_register(wide);
                let idx = self.read_const_pool_idx(wide);
                self.visitor.visit_const_int(dest, idx);
            }
            BytecodeInst::ConstLong => {
                let dest = self.read_register(wide);
                let idx = self.read_const_pool_idx(wide);
                self.visitor.visit_const_long(dest, idx);
            }
            BytecodeInst::ConstFloat => {
                let dest = self.read_register(wide);
                let idx = self.read_const_pool_idx(wide);
                self.visitor.visit_const_float(dest, idx);
            }
            BytecodeInst::ConstDouble => {
                let dest = self.read_register(wide);
                let idx = self.read_const_pool_idx(wide);
                self.visitor.visit_const_double(dest, idx);
            }
            BytecodeInst::ConstString => {
                let dest = self.read_register(wide);
                let idx = self.read_const_pool_idx(wide);
                self.visitor.visit_const_string(dest, idx);
            }

            BytecodeInst::TestEqPtr => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_eq_ptr(dest, lhs, rhs);
            }
            BytecodeInst::TestNePtr => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ne_ptr(dest, lhs, rhs);
            }

            BytecodeInst::TestEqInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_eq_int(dest, lhs, rhs);
            }
            BytecodeInst::TestNeInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ne_int(dest, lhs, rhs);
            }
            BytecodeInst::TestGtInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_gt_int(dest, lhs, rhs);
            }
            BytecodeInst::TestGeInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ge_int(dest, lhs, rhs);
            }
            BytecodeInst::TestLtInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_lt_int(dest, lhs, rhs);
            }
            BytecodeInst::TestLeInt => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_le_int(dest, lhs, rhs);
            }

            BytecodeInst::TestEqLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_eq_long(dest, lhs, rhs);
            }
            BytecodeInst::TestNeLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ne_long(dest, lhs, rhs);
            }
            BytecodeInst::TestGtLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_gt_long(dest, lhs, rhs);
            }
            BytecodeInst::TestGeLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ge_long(dest, lhs, rhs);
            }
            BytecodeInst::TestLtLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_lt_long(dest, lhs, rhs);
            }
            BytecodeInst::TestLeLong => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_le_long(dest, lhs, rhs);
            }

            BytecodeInst::TestEqFloat => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_eq_long(dest, lhs, rhs);
            }
            BytecodeInst::TestNeFloat => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ne_long(dest, lhs, rhs);
            }
            BytecodeInst::TestGtFloat => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_gt_long(dest, lhs, rhs);
            }
            BytecodeInst::TestGeFloat => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ge_long(dest, lhs, rhs);
            }
            BytecodeInst::TestLtFloat => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_lt_long(dest, lhs, rhs);
            }
            BytecodeInst::TestLeFloat => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_le_long(dest, lhs, rhs);
            }

            BytecodeInst::TestEqDouble => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_eq_long(dest, lhs, rhs);
            }
            BytecodeInst::TestNeDouble => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ne_long(dest, lhs, rhs);
            }
            BytecodeInst::TestGtDouble => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_gt_long(dest, lhs, rhs);
            }
            BytecodeInst::TestGeDouble => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_ge_long(dest, lhs, rhs);
            }
            BytecodeInst::TestLtDouble => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_lt_long(dest, lhs, rhs);
            }
            BytecodeInst::TestLeDouble => {
                let dest = self.read_register(wide);
                let lhs = self.read_register(wide);
                let rhs = self.read_register(wide);
                self.visitor.visit_test_le_long(dest, lhs, rhs);
            }

            BytecodeInst::JumpLoop => {
                let offset = self.read_offset(wide);
                self.visitor.visit_jump_loop(offset);
            }
            BytecodeInst::JumpIfFalse => {
                let opnd = self.read_register(wide);
                let offset = self.read_offset(wide);
                self.visitor.visit_jump_if_false(opnd, offset);
            }
            BytecodeInst::JumpIfFalseConst => {
                let opnd = self.read_register(wide);
                let idx = self.read_const_pool_idx(wide);
                self.visitor.visit_jump_if_false_const(opnd, idx);
            }
            BytecodeInst::JumpIfTrue => {
                let opnd = self.read_register(wide);
                let offset = self.read_offset(wide);
                self.visitor.visit_jump_if_true(opnd, offset);
            }
            BytecodeInst::JumpIfTrueConst => {
                let opnd = self.read_register(wide);
                let idx = self.read_const_pool_idx(wide);
                self.visitor.visit_jump_if_true_const(opnd, idx);
            }
            BytecodeInst::Jump => {
                let offset = self.read_offset(wide);
                self.visitor.visit_jump(offset);
            }
            BytecodeInst::JumpConst => {
                let idx = self.read_const_pool_idx(wide);
                self.visitor.visit_jump_const(idx);
            }

            BytecodeInst::InvokeDirectVoid => {
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor.visit_invoke_direct_void(fct, start, count);
            }
            BytecodeInst::InvokeDirectBool => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_direct_bool(dest, fct, start, count);
            }
            BytecodeInst::InvokeDirectByte => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_direct_byte(dest, fct, start, count);
            }
            BytecodeInst::InvokeDirectChar => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_direct_char(dest, fct, start, count);
            }
            BytecodeInst::InvokeDirectInt => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_direct_int(dest, fct, start, count);
            }
            BytecodeInst::InvokeDirectLong => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_direct_long(dest, fct, start, count);
            }
            BytecodeInst::InvokeDirectFloat => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_direct_float(dest, fct, start, count);
            }
            BytecodeInst::InvokeDirectDouble => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_direct_double(dest, fct, start, count);
            }
            BytecodeInst::InvokeDirectPtr => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_direct_ptr(dest, fct, start, count);
            }

            BytecodeInst::InvokeVirtualVoid => {
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor.visit_invoke_virtual_void(fct, start, count);
            }
            BytecodeInst::InvokeVirtualBool => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_virtual_bool(dest, fct, start, count);
            }
            BytecodeInst::InvokeVirtualByte => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_virtual_byte(dest, fct, start, count);
            }
            BytecodeInst::InvokeVirtualChar => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_virtual_char(dest, fct, start, count);
            }
            BytecodeInst::InvokeVirtualInt => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_virtual_int(dest, fct, start, count);
            }
            BytecodeInst::InvokeVirtualLong => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_virtual_long(dest, fct, start, count);
            }
            BytecodeInst::InvokeVirtualFloat => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_virtual_float(dest, fct, start, count);
            }
            BytecodeInst::InvokeVirtualDouble => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_virtual_double(dest, fct, start, count);
            }
            BytecodeInst::InvokeVirtualPtr => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_virtual_ptr(dest, fct, start, count);
            }

            BytecodeInst::InvokeStaticVoid => {
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor.visit_invoke_static_void(fct, start, count);
            }
            BytecodeInst::InvokeStaticBool => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_static_bool(dest, fct, start, count);
            }
            BytecodeInst::InvokeStaticByte => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_static_byte(dest, fct, start, count);
            }
            BytecodeInst::InvokeStaticChar => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_static_char(dest, fct, start, count);
            }
            BytecodeInst::InvokeStaticInt => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_static_int(dest, fct, start, count);
            }
            BytecodeInst::InvokeStaticLong => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_static_long(dest, fct, start, count);
            }
            BytecodeInst::InvokeStaticFloat => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_static_float(dest, fct, start, count);
            }
            BytecodeInst::InvokeStaticDouble => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_static_double(dest, fct, start, count);
            }
            BytecodeInst::InvokeStaticPtr => {
                let dest = self.read_register(wide);
                let fct = self.read_fct(wide);
                let start = self.read_register(wide);
                let count = self.read_index(wide);
                self.visitor
                    .visit_invoke_static_ptr(dest, fct, start, count);
            }

            BytecodeInst::NewObject => {
                let dest = self.read_register(wide);
                let cls = self.read_class(wide);
                self.visitor.visit_new_object(dest, cls);
            }

            BytecodeInst::Throw => {
                let opnd = self.read_register(wide);
                self.visitor.visit_throw(opnd);
            }

            BytecodeInst::RetVoid => {
                self.visitor.visit_ret_void();
            }
            BytecodeInst::RetBool => {
                let opnd = self.read_register(wide);
                self.visitor.visit_ret_bool(opnd);
            }
            BytecodeInst::RetByte => {
                let opnd = self.read_register(wide);
                self.visitor.visit_ret_byte(opnd);
            }
            BytecodeInst::RetChar => {
                let opnd = self.read_register(wide);
                self.visitor.visit_ret_char(opnd);
            }
            BytecodeInst::RetInt => {
                let opnd = self.read_register(wide);
                self.visitor.visit_ret_int(opnd);
            }
            BytecodeInst::RetLong => {
                let opnd = self.read_register(wide);
                self.visitor.visit_ret_long(opnd);
            }
            BytecodeInst::RetFloat => {
                let opnd = self.read_register(wide);
                self.visitor.visit_ret_float(opnd);
            }
            BytecodeInst::RetDouble => {
                let opnd = self.read_register(wide);
                self.visitor.visit_ret_double(opnd);
            }
            BytecodeInst::RetPtr => {
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
        if self.data[self.pos] as u32 == BytecodeInst::Wide as u32 {
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
