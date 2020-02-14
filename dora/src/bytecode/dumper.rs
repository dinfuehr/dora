use std::io;

use crate::bytecode::{
    read, BytecodeFunction, BytecodeOffset, BytecodeVisitor, ConstPoolIdx, Register,
};
use crate::vm::{ClassDefId, FctId, FieldId, GlobalId};

pub fn dump(bc: &BytecodeFunction) {
    let mut stdout = io::stdout();
    let mut visitor = BytecodeDumper {
        bc,
        pos: BytecodeOffset(0),
        w: &mut stdout,
    };
    read(bc.code(), &mut visitor);
}

struct BytecodeDumper<'a> {
    bc: &'a BytecodeFunction,
    pos: BytecodeOffset,
    w: &'a mut dyn io::Write,
}

impl<'a> BytecodeDumper<'a> {
    fn emit_inst(&mut self, name: &str) {
        self.emit_start(name);
        writeln!(self.w, "").expect("write! failed");
    }

    fn emit_reg3(&mut self, name: &str, r1: Register, r2: Register, r3: Register) {
        self.emit_start(name);
        writeln!(self.w, " {}, {}, {}", r1, r2, r3).expect("write! failed");
    }

    fn emit_reg2(&mut self, name: &str, r1: Register, r2: Register) {
        self.emit_start(name);
        writeln!(self.w, " {}, {}", r1, r2).expect("write! failed");
    }

    fn emit_reg1(&mut self, name: &str, r1: Register) {
        self.emit_start(name);
        writeln!(self.w, " {}", r1).expect("write! failed");
    }

    fn emit_reg1_idx(&mut self, name: &str, r1: Register, idx: ConstPoolIdx) {
        self.emit_start(name);
        writeln!(self.w, " {}, @{}", r1, idx.to_usize()).expect("write! failed");
    }

    fn emit_idx(&mut self, name: &str, idx: ConstPoolIdx) {
        self.emit_start(name);
        writeln!(self.w, " @{}", idx.to_usize()).expect("write! failed");
    }

    fn emit_reg1_u32(&mut self, name: &str, r1: Register, value: u32) {
        self.emit_start(name);
        writeln!(self.w, " {}, 0x{:x}/{}", r1, value, value).expect("write! failed");
    }

    fn emit_u32(&mut self, name: &str, value: u32) {
        self.emit_start(name);
        writeln!(self.w, " 0x{:x}/{}", value, value).expect("write! failed");
    }

    fn emit_field(
        &mut self,
        name: &str,
        r1: Register,
        r2: Register,
        cid: ClassDefId,
        fid: FieldId,
    ) {
        self.emit_start(name);
        writeln!(
            self.w,
            " {}, {}, {}:{}",
            r1,
            r2,
            cid.to_usize(),
            fid.to_usize()
        )
        .expect("write! failed");
    }

    fn emit_global(&mut self, name: &str, r1: Register, gid: GlobalId) {
        self.emit_start(name);
        writeln!(self.w, " {}, {}", r1, gid.to_usize()).expect("write! failed");
    }

    fn emit_fct_void(&mut self, name: &str, fid: FctId, r1: Register, cnt: u32) {
        self.emit_start(name);
        writeln!(self.w, " {}, {}, {}", fid.to_usize(), r1, cnt).expect("write! failed");
    }

    fn emit_fct(&mut self, name: &str, r1: Register, fid: FctId, r2: Register, cnt: u32) {
        self.emit_start(name);
        writeln!(self.w, " {}, {}, {}, {}", r1, fid.to_usize(), r2, cnt).expect("write! failed");
    }

    fn emit_new(&mut self, name: &str, r1: Register, cls: ClassDefId) {
        self.emit_start(name);
        writeln!(self.w, " {}, {}", r1, cls.to_usize()).expect("write! failed");
    }

    fn emit_start(&mut self, name: &str) {
        write!(self.w, "{}: {}", self.pos.to_usize(), name).expect("write! failed");
    }
}

impl<'a> BytecodeVisitor for BytecodeDumper<'a> {
    fn visit_instruction(&mut self, offset: BytecodeOffset) {
        self.pos = offset;
    }

    fn visit_add_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("AddInt", dest, lhs, rhs);
    }
    fn visit_add_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("AddLong", dest, lhs, rhs);
    }
    fn visit_add_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("AddFloat", dest, lhs, rhs);
    }
    fn visit_add_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("AddDouble", dest, lhs, rhs);
    }

    fn visit_sub_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("SubInt", dest, lhs, rhs);
    }
    fn visit_sub_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("SubLong", dest, lhs, rhs);
    }
    fn visit_sub_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("SubFloat", dest, lhs, rhs);
    }
    fn visit_sub_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("SubDouble", dest, lhs, rhs);
    }

    fn visit_neg_int(&mut self, dest: Register, src: Register) {
        self.emit_reg2("NegInt", dest, src);
    }
    fn visit_neg_long(&mut self, dest: Register, src: Register) {
        self.emit_reg2("NegLong", dest, src);
    }
    fn visit_neg_float(&mut self, dest: Register, src: Register) {
        self.emit_reg2("NegFloat", dest, src);
    }
    fn visit_neg_double(&mut self, dest: Register, src: Register) {
        self.emit_reg2("NegDouble", dest, src);
    }

    fn visit_mul_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("MulInt", dest, lhs, rhs);
    }
    fn visit_mul_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("MulLong", dest, lhs, rhs);
    }
    fn visit_mul_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("MulFloat", dest, lhs, rhs);
    }
    fn visit_mul_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("MulDouble", dest, lhs, rhs);
    }

    fn visit_div_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("DivInt", dest, lhs, rhs);
    }
    fn visit_div_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("DivLong", dest, lhs, rhs);
    }
    fn visit_div_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("DivFloat", dest, lhs, rhs);
    }
    fn visit_div_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("DivDouble", dest, lhs, rhs);
    }

    fn visit_mod_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("ModInt", dest, lhs, rhs);
    }
    fn visit_mod_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("ModLong", dest, lhs, rhs);
    }

    fn visit_and_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("AndInt", dest, lhs, rhs);
    }
    fn visit_and_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("AndLong", dest, lhs, rhs);
    }

    fn visit_or_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("OrInt", dest, lhs, rhs);
    }
    fn visit_or_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("OrLong", dest, lhs, rhs);
    }

    fn visit_xor_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("XorInt", dest, lhs, rhs);
    }
    fn visit_xor_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("XorLong", dest, lhs, rhs);
    }

    fn visit_not_bool(&mut self, dest: Register, src: Register) {
        self.emit_reg2("NotBool", dest, src);
    }
    fn visit_not_int(&mut self, dest: Register, src: Register) {
        self.emit_reg2("NotInt", dest, src);
    }
    fn visit_not_long(&mut self, dest: Register, src: Register) {
        self.emit_reg2("NotLong", dest, src);
    }

    fn visit_shl_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("ShlInt", dest, lhs, rhs);
    }
    fn visit_shr_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("ShrInt", dest, lhs, rhs);
    }
    fn visit_sar_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("SarInt", dest, lhs, rhs);
    }

    fn visit_shl_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("ShlLong", dest, lhs, rhs);
    }
    fn visit_shr_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("ShrLong", dest, lhs, rhs);
    }
    fn visit_sar_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("SarLong", dest, lhs, rhs);
    }

    fn visit_rotate_left_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("RotateLeftInt", dest, lhs, rhs);
    }
    fn visit_rotate_right_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("RotateRightInt", dest, lhs, rhs);
    }

    fn visit_mov_bool(&mut self, dest: Register, src: Register) {
        self.emit_reg2("MovBool", dest, src);
    }
    fn visit_mov_byte(&mut self, dest: Register, src: Register) {
        self.emit_reg2("MovByte", dest, src);
    }
    fn visit_mov_char(&mut self, dest: Register, src: Register) {
        self.emit_reg2("MovChar", dest, src);
    }
    fn visit_mov_int(&mut self, dest: Register, src: Register) {
        self.emit_reg2("MovInt", dest, src);
    }
    fn visit_mov_long(&mut self, dest: Register, src: Register) {
        self.emit_reg2("MovLong", dest, src);
    }
    fn visit_mov_float(&mut self, dest: Register, src: Register) {
        self.emit_reg2("MovFloat", dest, src);
    }
    fn visit_mov_double(&mut self, dest: Register, src: Register) {
        self.emit_reg2("MovDouble", dest, src);
    }
    fn visit_mov_ptr(&mut self, dest: Register, src: Register) {
        self.emit_reg2("MovPtr", dest, src);
    }

    fn visit_load_field_bool(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_field("LoadFieldBool", dest, obj, cls, field);
    }
    fn visit_load_field_byte(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_field("LoadFieldByte", dest, obj, cls, field);
    }
    fn visit_load_field_char(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_field("LoadFieldChar", dest, obj, cls, field);
    }
    fn visit_load_field_int(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_field("LoadFieldInt", dest, obj, cls, field);
    }
    fn visit_load_field_long(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_field("LoadFieldLong", dest, obj, cls, field);
    }
    fn visit_load_field_float(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_field("LoadFieldFloat", dest, obj, cls, field);
    }
    fn visit_load_field_double(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_field("LoadFieldDouble", dest, obj, cls, field);
    }
    fn visit_load_field_ptr(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_field("LoadFieldPtr", dest, obj, cls, field);
    }

    fn visit_store_field_bool(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_field("StoreFieldBool", src, obj, cls, field);
    }
    fn visit_store_field_byte(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_field("StoreFieldByte", src, obj, cls, field);
    }
    fn visit_store_field_char(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_field("StoreFieldChar", src, obj, cls, field);
    }
    fn visit_store_field_int(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_field("StoreFieldInt", src, obj, cls, field);
    }
    fn visit_store_field_long(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_field("StoreFieldLong", src, obj, cls, field);
    }
    fn visit_store_field_float(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_field("StoreFieldFloat", src, obj, cls, field);
    }
    fn visit_store_field_double(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_field("StoreFieldDouble", src, obj, cls, field);
    }
    fn visit_store_field_ptr(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_field("StoreFieldPtr", src, obj, cls, field);
    }

    fn visit_load_global_bool(&mut self, dest: Register, glob: GlobalId) {
        self.emit_global("LoadGlobalBool", dest, glob);
    }
    fn visit_load_global_byte(&mut self, dest: Register, glob: GlobalId) {
        self.emit_global("LoadGlobalByte", dest, glob);
    }
    fn visit_load_global_char(&mut self, dest: Register, glob: GlobalId) {
        self.emit_global("LoadGlobalChar", dest, glob);
    }
    fn visit_load_global_int(&mut self, dest: Register, glob: GlobalId) {
        self.emit_global("LoadGlobalInt", dest, glob);
    }
    fn visit_load_global_long(&mut self, dest: Register, glob: GlobalId) {
        self.emit_global("LoadGlobalLong", dest, glob);
    }
    fn visit_load_global_float(&mut self, dest: Register, glob: GlobalId) {
        self.emit_global("LoadGlobalFloat", dest, glob);
    }
    fn visit_load_global_double(&mut self, dest: Register, glob: GlobalId) {
        self.emit_global("LoadGlobalDouble", dest, glob);
    }
    fn visit_load_global_ptr(&mut self, dest: Register, glob: GlobalId) {
        self.emit_global("LoadGlobalPtr", dest, glob);
    }

    fn visit_store_global_bool(&mut self, src: Register, glob: GlobalId) {
        self.emit_global("StoreGlobalBool", src, glob);
    }
    fn visit_store_global_byte(&mut self, src: Register, glob: GlobalId) {
        self.emit_global("StoreGlobalByte", src, glob);
    }
    fn visit_store_global_char(&mut self, src: Register, glob: GlobalId) {
        self.emit_global("StoreGlobalChar", src, glob);
    }
    fn visit_store_global_int(&mut self, src: Register, glob: GlobalId) {
        self.emit_global("StoreGlobalInt", src, glob);
    }
    fn visit_store_global_long(&mut self, src: Register, glob: GlobalId) {
        self.emit_global("StoreGlobalLong", src, glob);
    }
    fn visit_store_global_float(&mut self, src: Register, glob: GlobalId) {
        self.emit_global("StoreGlobalFloat", src, glob);
    }
    fn visit_store_global_double(&mut self, src: Register, glob: GlobalId) {
        self.emit_global("StoreGlobalDouble", src, glob);
    }
    fn visit_store_global_ptr(&mut self, src: Register, glob: GlobalId) {
        self.emit_global("StoreGlobalPtr", src, glob);
    }

    fn visit_const_nil(&mut self, dest: Register) {
        self.emit_reg1("ConstNil", dest);
    }
    fn visit_const_true(&mut self, dest: Register) {
        self.emit_reg1("ConstTrue", dest);
    }
    fn visit_const_false(&mut self, dest: Register) {
        self.emit_reg1("ConstFalse", dest);
    }
    fn visit_const_zero_byte(&mut self, dest: Register) {
        self.emit_reg1("ConstZeroByte", dest);
    }
    fn visit_const_zero_char(&mut self, dest: Register) {
        self.emit_reg1("ConstZeroChar", dest);
    }
    fn visit_const_zero_int(&mut self, dest: Register) {
        self.emit_reg1("ConstZeroInt", dest);
    }
    fn visit_const_zero_long(&mut self, dest: Register) {
        self.emit_reg1("ConstZeroLong", dest);
    }
    fn visit_const_zero_float(&mut self, dest: Register) {
        self.emit_reg1("ConstZeroFloat", dest);
    }
    fn visit_const_zero_double(&mut self, dest: Register) {
        self.emit_reg1("ConstZeroDouble", dest);
    }
    fn visit_const_char(&mut self, dest: Register, value: ConstPoolIdx) {
        self.emit_reg1_idx("ConstChar", dest, value);
    }
    fn visit_const_byte(&mut self, dest: Register, value: u8) {
        self.emit_reg1_u32("ConstByte", dest, value as u32);
    }
    fn visit_const_int(&mut self, dest: Register, value: ConstPoolIdx) {
        self.emit_reg1_idx("ConstInt", dest, value);
    }
    fn visit_const_long(&mut self, dest: Register, value: ConstPoolIdx) {
        self.emit_reg1_idx("ConstLong", dest, value);
    }
    fn visit_const_float(&mut self, dest: Register, value: ConstPoolIdx) {
        self.emit_reg1_idx("ConstFloat", dest, value);
    }
    fn visit_const_double(&mut self, dest: Register, value: ConstPoolIdx) {
        self.emit_reg1_idx("ConstDouble", dest, value);
    }
    fn visit_const_string(&mut self, dest: Register, value: ConstPoolIdx) {
        self.emit_reg1_idx("ConstString", dest, value);
    }

    fn visit_test_eq_ptr(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestEqPtr", dest, lhs, rhs);
    }
    fn visit_test_ne_ptr(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestNePtr", dest, lhs, rhs);
    }
    fn visit_test_eq_bool(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestEqBool", dest, lhs, rhs);
    }
    fn visit_test_ne_bool(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestNeBool", dest, lhs, rhs);
    }

    fn visit_test_eq_byte(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestEqByte", dest, lhs, rhs);
    }
    fn visit_test_ne_byte(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestNeByte", dest, lhs, rhs);
    }
    fn visit_test_gt_byte(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestGtByte", dest, lhs, rhs);
    }
    fn visit_test_ge_byte(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestGeByte", dest, lhs, rhs);
    }
    fn visit_test_lt_byte(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestLtByte", dest, lhs, rhs);
    }
    fn visit_test_le_byte(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestLeByte", dest, lhs, rhs);
    }

    fn visit_test_eq_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestEqChar", dest, lhs, rhs);
    }
    fn visit_test_ne_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestNeChar", dest, lhs, rhs);
    }
    fn visit_test_gt_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestGtChar", dest, lhs, rhs);
    }
    fn visit_test_ge_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestGeChar", dest, lhs, rhs);
    }
    fn visit_test_lt_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestLtChar", dest, lhs, rhs);
    }
    fn visit_test_le_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestLeChar", dest, lhs, rhs);
    }

    fn visit_test_eq_enum(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestEqEnum", dest, lhs, rhs);
    }
    fn visit_test_ne_enum(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestNeEnum", dest, lhs, rhs);
    }

    fn visit_test_eq_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestEqInt", dest, lhs, rhs);
    }
    fn visit_test_ne_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestNeInt", dest, lhs, rhs);
    }
    fn visit_test_gt_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestGtInt", dest, lhs, rhs);
    }
    fn visit_test_ge_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestGeInt", dest, lhs, rhs);
    }
    fn visit_test_lt_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestLtInt", dest, lhs, rhs);
    }
    fn visit_test_le_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestLeInt", dest, lhs, rhs);
    }

    fn visit_test_eq_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestEqLong", dest, lhs, rhs);
    }
    fn visit_test_ne_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestNeLong", dest, lhs, rhs);
    }
    fn visit_test_gt_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestGtLong", dest, lhs, rhs);
    }
    fn visit_test_ge_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestGeLong", dest, lhs, rhs);
    }
    fn visit_test_lt_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestLtLong", dest, lhs, rhs);
    }
    fn visit_test_le_long(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestLeLong", dest, lhs, rhs);
    }

    fn visit_test_eq_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestEqFloat", dest, lhs, rhs);
    }
    fn visit_test_ne_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestNeFloat", dest, lhs, rhs);
    }
    fn visit_test_gt_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestGtFloat", dest, lhs, rhs);
    }
    fn visit_test_ge_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestGeFloat", dest, lhs, rhs);
    }
    fn visit_test_lt_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestLtFloat", dest, lhs, rhs);
    }
    fn visit_test_le_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestLeFloat", dest, lhs, rhs);
    }

    fn visit_test_eq_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestEqDouble", dest, lhs, rhs);
    }
    fn visit_test_ne_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestNeDouble", dest, lhs, rhs);
    }
    fn visit_test_gt_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestGtDouble", dest, lhs, rhs);
    }
    fn visit_test_ge_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestGeDouble", dest, lhs, rhs);
    }
    fn visit_test_lt_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestLtDouble", dest, lhs, rhs);
    }
    fn visit_test_le_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3("TestLeDouble", dest, lhs, rhs);
    }

    fn visit_jump_if_false(&mut self, opnd: Register, offset: u32) {
        self.emit_reg1_u32("JumpIfFalse", opnd, offset);
    }
    fn visit_jump_if_false_const(&mut self, opnd: Register, idx: ConstPoolIdx) {
        self.emit_reg1_idx("JumpIfFalseConst", opnd, idx);
    }
    fn visit_jump_if_true(&mut self, opnd: Register, offset: u32) {
        self.emit_reg1_u32("JumpIfTrue", opnd, offset);
    }
    fn visit_jump_if_true_const(&mut self, opnd: Register, idx: ConstPoolIdx) {
        self.emit_reg1_idx("JumpIfTrueConst", opnd, idx);
    }
    fn visit_jump_loop(&mut self, offset: u32) {
        self.emit_u32("JumpLoop", offset);
    }
    fn visit_jump(&mut self, offset: u32) {
        self.emit_u32("Jump", offset);
    }
    fn visit_jump_const(&mut self, idx: ConstPoolIdx) {
        self.emit_idx("JumpConst", idx);
    }

    fn visit_invoke_direct_void(&mut self, fct: FctId, start: Register, count: u32) {
        self.emit_fct_void("InvokeDirectVoid", fct, start, count);
    }
    fn visit_invoke_direct_bool(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit_fct("InvokeDirectBool", dest, fct, start, count);
    }
    fn visit_invoke_direct_byte(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit_fct("InvokeDirectByte", dest, fct, start, count);
    }
    fn visit_invoke_direct_char(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit_fct("InvokeDirectChar", dest, fct, start, count);
    }
    fn visit_invoke_direct_int(&mut self, dest: Register, fct: FctId, start: Register, count: u32) {
        self.emit_fct("InvokeDirectInt", dest, fct, start, count);
    }
    fn visit_invoke_direct_long(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit_fct("InvokeDirectLong", dest, fct, start, count);
    }
    fn visit_invoke_direct_float(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit_fct("InvokeDirectFloat", dest, fct, start, count);
    }
    fn visit_invoke_direct_double(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit_fct("InvokeDirectDouble", dest, fct, start, count);
    }
    fn visit_invoke_direct_ptr(&mut self, dest: Register, fct: FctId, start: Register, count: u32) {
        self.emit_fct("InvokeDirectPtr", dest, fct, start, count);
    }

    fn visit_invoke_virtual_void(&mut self, fct: FctId, start: Register, count: u32) {
        self.emit_fct_void("InvokeVirtualVoid", fct, start, count);
    }
    fn visit_invoke_virtual_bool(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit_fct("InvokeVirtualBool", dest, fct, start, count);
    }
    fn visit_invoke_virtual_byte(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit_fct("InvokeVirtualByte", dest, fct, start, count);
    }
    fn visit_invoke_virtual_char(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit_fct("InvokeVirtualChar", dest, fct, start, count);
    }
    fn visit_invoke_virtual_int(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit_fct("InvokeVirtualInt", dest, fct, start, count);
    }
    fn visit_invoke_virtual_long(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit_fct("InvokeVirtualLong", dest, fct, start, count);
    }
    fn visit_invoke_virtual_float(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit_fct("InvokeVirtualFloat", dest, fct, start, count);
    }
    fn visit_invoke_virtual_double(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit_fct("InvokeVirtualDouble", dest, fct, start, count);
    }
    fn visit_invoke_virtual_ptr(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit_fct("InvokeVirtualPtr", dest, fct, start, count);
    }

    fn visit_invoke_static_void(&mut self, fct: FctId, start: Register, count: u32) {
        self.emit_fct_void("InvokeStaticVoid", fct, start, count);
    }
    fn visit_invoke_static_bool(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit_fct("InvokeStaticBool", dest, fct, start, count);
    }
    fn visit_invoke_static_byte(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit_fct("InvokeStaticByte", dest, fct, start, count);
    }
    fn visit_invoke_static_char(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit_fct("InvokeStaticChar", dest, fct, start, count);
    }
    fn visit_invoke_static_int(&mut self, dest: Register, fct: FctId, start: Register, count: u32) {
        self.emit_fct("InvokeStaticInt", dest, fct, start, count);
    }
    fn visit_invoke_static_long(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit_fct("InvokeStaticLong", dest, fct, start, count);
    }
    fn visit_invoke_static_float(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit_fct("InvokeStaticFloat", dest, fct, start, count);
    }
    fn visit_invoke_static_double(
        &mut self,
        dest: Register,
        fct: FctId,
        start: Register,
        count: u32,
    ) {
        self.emit_fct("InvokeStaticDouble", dest, fct, start, count);
    }
    fn visit_invoke_static_ptr(&mut self, dest: Register, fct: FctId, start: Register, count: u32) {
        self.emit_fct("InvokeStaticPtr", dest, fct, start, count);
    }

    fn visit_new_object(&mut self, dest: Register, cls: ClassDefId) {
        self.emit_new("NewObject", dest, cls);
    }
    fn visit_throw(&mut self, opnd: Register) {
        self.emit_reg1("Throw", opnd);
    }

    fn visit_ret_void(&mut self) {
        self.emit_inst("RetVoid");
    }
    fn visit_ret_bool(&mut self, opnd: Register) {
        self.emit_reg1("RetBool", opnd);
    }
    fn visit_ret_byte(&mut self, opnd: Register) {
        self.emit_reg1("RetByte", opnd);
    }
    fn visit_ret_char(&mut self, opnd: Register) {
        self.emit_reg1("RetChar", opnd);
    }
    fn visit_ret_int(&mut self, opnd: Register) {
        self.emit_reg1("RetInt", opnd);
    }
    fn visit_ret_long(&mut self, opnd: Register) {
        self.emit_reg1("RetLong", opnd);
    }
    fn visit_ret_float(&mut self, opnd: Register) {
        self.emit_reg1("RetFloat", opnd);
    }
    fn visit_ret_double(&mut self, opnd: Register) {
        self.emit_reg1("RetDouble", opnd);
    }
    fn visit_ret_ptr(&mut self, opnd: Register) {
        self.emit_reg1("RetPtr", opnd);
    }
}
