use std::mem;

use crate::bytecode::{
    BytecodeFunction, BytecodeOffset, BytecodeOpcode, BytecodeType, ConstPoolEntry, ConstPoolIdx,
    Register,
};
use crate::language::sem_analysis::GlobalDefinitionId;

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

    params: Vec<BytecodeType>,
    return_type: Option<BytecodeType>,
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

            params: Vec::new(),
            return_type: None,
        }
    }

    pub fn add_register(&mut self, ty: BytecodeType) -> Register {
        self.registers.push(ty);
        Register(self.registers.len() - 1)
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

    pub fn set_params(&mut self, params: Vec<BytecodeType>) {
        self.params = params;
    }

    pub fn set_return_type(&mut self, return_type: Option<BytecodeType>) {
        self.return_type = return_type;
    }

    pub fn emit_add(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::Add, dest, lhs, rhs);
    }

    pub fn emit_and(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::And, dest, lhs, rhs);
    }

    pub fn emit_or(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::Or, dest, lhs, rhs);
    }

    pub fn emit_xor(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::Xor, dest, lhs, rhs);
    }

    pub fn emit_div(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::Div, dest, lhs, rhs);
    }

    pub fn emit_load_struct_field(
        &mut self,
        dest: Register,
        obj: Register,
        field_idx: ConstPoolIdx,
    ) {
        self.emit_access_field(BytecodeOpcode::LoadStructField, dest, obj, field_idx);
    }

    pub fn emit_load_field(&mut self, dest: Register, obj: Register, field_idx: ConstPoolIdx) {
        self.emit_access_field(BytecodeOpcode::LoadField, dest, obj, field_idx);
    }

    pub fn emit_store_field(&mut self, src: Register, obj: Register, field_idx: ConstPoolIdx) {
        self.emit_access_field(BytecodeOpcode::StoreField, src, obj, field_idx);
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

    pub fn emit_const_true(&mut self, dest: Register) {
        self.emit_reg1(BytecodeOpcode::ConstTrue, dest);
    }

    pub fn emit_const_false(&mut self, dest: Register) {
        self.emit_reg1(BytecodeOpcode::ConstFalse, dest);
    }

    pub fn emit_not(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::Not, dest, src);
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

    pub fn emit_loop_start(&mut self) {
        self.emit_op(BytecodeOpcode::LoopStart);
    }

    pub fn emit_jump(&mut self, lbl: Label) {
        assert!(self.lookup_label(lbl).is_none());
        self.emit_jmp_forward(BytecodeOpcode::Jump, BytecodeOpcode::JumpConst, None, lbl);
    }

    pub fn emit_mod(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::Mod, dest, lhs, rhs);
    }

    pub fn emit_mul(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::Mul, dest, lhs, rhs);
    }

    pub fn emit_neg(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::Neg, dest, src);
    }

    pub fn emit_shl(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::Shl, dest, lhs, rhs);
    }

    pub fn emit_shr(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::Shr, dest, lhs, rhs);
    }

    pub fn emit_sar(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::Sar, dest, lhs, rhs);
    }

    pub fn emit_sub(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::Sub, dest, lhs, rhs);
    }

    pub fn emit_mov(&mut self, dest: Register, src: Register) {
        self.emit_reg2(BytecodeOpcode::Mov, dest, src);
    }

    pub fn emit_load_tuple_element(&mut self, dest: Register, src: Register, idx: ConstPoolIdx) {
        self.emit_access_tuple(BytecodeOpcode::LoadTupleElement, dest, src, idx);
    }

    pub fn emit_load_enum_element(&mut self, dest: Register, src: Register, idx: ConstPoolIdx) {
        self.emit_access_enum(BytecodeOpcode::LoadEnumElement, dest, src, idx);
    }

    pub fn emit_load_enum_variant(&mut self, dest: Register, src: Register, idx: ConstPoolIdx) {
        self.emit_reg2_idx(BytecodeOpcode::LoadEnumVariant, dest, src, idx);
    }

    pub fn emit_load_trait_object_value(&mut self, dest: Register, object: Register) {
        self.emit_reg2(BytecodeOpcode::LoadTraitObjectValue, dest, object);
    }

    pub fn emit_ret(&mut self, src: Register) {
        self.emit_reg1(BytecodeOpcode::Ret, src);
    }

    pub fn emit_test_identity(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestIdentity, dest, lhs, rhs);
    }

    pub fn emit_test_eq(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestEq, dest, lhs, rhs);
    }

    pub fn emit_test_ne(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestNe, dest, lhs, rhs);
    }

    pub fn emit_test_gt(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestGt, dest, lhs, rhs);
    }

    pub fn emit_test_ge(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestGe, dest, lhs, rhs);
    }

    pub fn emit_test_lt(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestLt, dest, lhs, rhs);
    }

    pub fn emit_test_le(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_reg3(BytecodeOpcode::TestLe, dest, lhs, rhs);
    }

    pub fn emit_load_global(&mut self, dest: Register, gid: GlobalDefinitionId) {
        self.emit_load_global_inst(BytecodeOpcode::LoadGlobal, dest, gid);
    }

    pub fn emit_store_global(&mut self, src: Register, gid: GlobalDefinitionId) {
        self.emit_store_global_inst(BytecodeOpcode::StoreGlobal, src, gid);
    }

    pub fn emit_push_register(&mut self, src: Register) {
        self.emit_reg1(BytecodeOpcode::PushRegister, src);
    }

    pub fn emit_invoke_direct(&mut self, dest: Register, fid: ConstPoolIdx) {
        self.emit_fct(BytecodeOpcode::InvokeDirect, dest, fid);
    }

    pub fn emit_invoke_virtual(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit_fct(BytecodeOpcode::InvokeVirtual, dest, idx);
    }

    pub fn emit_invoke_lambda(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit_reg1_idx(BytecodeOpcode::InvokeLambda, dest, idx);
    }

    pub fn emit_invoke_static(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit_fct(BytecodeOpcode::InvokeStatic, dest, idx);
    }

    pub fn emit_invoke_generic_static(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit_fct(BytecodeOpcode::InvokeGenericStatic, dest, idx);
    }

    pub fn emit_invoke_generic_direct(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit_fct(BytecodeOpcode::InvokeGenericDirect, dest, idx);
    }

    pub fn emit_new_object(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit_new(BytecodeOpcode::NewObject, dest, idx);
    }
    pub fn emit_new_object_initialized(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit_new(BytecodeOpcode::NewObjectInitialized, dest, idx);
    }
    pub fn emit_new_array(&mut self, dest: Register, cls_id: ConstPoolIdx, length: Register) {
        self.emit_new_arr(BytecodeOpcode::NewArray, dest, cls_id, length);
    }
    pub fn emit_new_tuple(&mut self, dest: Register, idx: ConstPoolIdx) {
        let values = [dest.to_usize() as u32, idx.to_usize() as u32];
        self.emit_values(BytecodeOpcode::NewTuple, &values);
    }
    pub fn emit_new_enum(&mut self, dest: Register, idx: ConstPoolIdx) {
        let values = [dest.to_usize() as u32, idx.to_usize() as u32];
        self.emit_values(BytecodeOpcode::NewEnum, &values);
    }
    pub fn emit_new_struct(&mut self, dest: Register, idx: ConstPoolIdx) {
        let values = [dest.to_usize() as u32, idx.to_usize() as u32];
        self.emit_values(BytecodeOpcode::NewStruct, &values);
    }
    pub fn emit_new_trait_object(&mut self, dest: Register, idx: ConstPoolIdx, src: Register) {
        let values = [
            dest.to_usize() as u32,
            idx.to_usize() as u32,
            src.to_usize() as u32,
        ];
        self.emit_values(BytecodeOpcode::NewTraitObject, &values);
    }
    pub fn emit_new_lambda(&mut self, dest: Register, idx: ConstPoolIdx) {
        let values = [dest.to_usize() as u32, idx.to_usize() as u32];
        self.emit_values(BytecodeOpcode::NewLambda, &values);
    }

    pub fn emit_array_length(&mut self, dest: Register, array: Register) {
        self.emit_reg2(BytecodeOpcode::ArrayLength, dest, array);
    }

    pub fn emit_store_array(&mut self, src: Register, array: Register, idx: Register) {
        self.emit_reg3(BytecodeOpcode::StoreArray, src, array, idx);
    }

    pub fn emit_load_array(&mut self, dest: Register, array: Register, idx: Register) {
        self.emit_reg3(BytecodeOpcode::LoadArray, dest, array, idx);
    }

    pub fn generate(mut self) -> BytecodeFunction {
        self.resolve_forward_jumps();

        BytecodeFunction::new(
            self.code,
            self.const_pool,
            self.registers,
            self.arguments,
            self.positions,
            Vec::new(),
            None,
        )
    }

    pub fn generate_with_registers(mut self, registers: Vec<BytecodeType>) -> BytecodeFunction {
        self.resolve_forward_jumps();

        assert!(self.registers.is_empty());

        BytecodeFunction::new(
            self.code,
            self.const_pool,
            registers,
            self.arguments,
            self.positions,
            Vec::new(),
            None,
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

                self.code[start.to_usize()] = inst_imm.to_u8();
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

    fn emit_reg2_idx(
        &mut self,
        inst: BytecodeOpcode,
        r1: Register,
        r2: Register,
        idx: ConstPoolIdx,
    ) {
        let values = [
            r1.to_usize() as u32,
            r2.to_usize() as u32,
            idx.to_usize() as u32,
        ];
        self.emit_values(inst, &values);
    }

    fn emit_reg1(&mut self, inst: BytecodeOpcode, r1: Register) {
        let values = [r1.to_usize() as u32];
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

    pub fn add_const(&mut self, value: ConstPoolEntry) -> ConstPoolIdx {
        let idx = self.const_pool.len();
        self.const_pool.push(value);
        idx.into()
    }

    fn emit_new(&mut self, inst: BytecodeOpcode, r1: Register, idx: ConstPoolIdx) {
        let values = [r1.to_usize() as u32, idx.to_usize() as u32];
        self.emit_values(inst, &values);
    }

    fn emit_new_arr(
        &mut self,
        inst: BytecodeOpcode,
        r1: Register,
        idx: ConstPoolIdx,
        lth: Register,
    ) {
        let values = [
            r1.to_usize() as u32,
            idx.to_usize() as u32,
            lth.to_usize() as u32,
        ];
        self.emit_values(inst, &values);
    }

    fn emit_fct(&mut self, inst: BytecodeOpcode, r1: Register, idx: ConstPoolIdx) {
        let values = [r1.to_usize() as u32, idx.to_usize() as u32];
        self.emit_values(inst, &values);
    }

    fn emit_access_field(
        &mut self,
        inst: BytecodeOpcode,
        r1: Register,
        r2: Register,
        field_idx: ConstPoolIdx,
    ) {
        let values = [
            r1.to_usize() as u32,
            r2.to_usize() as u32,
            field_idx.to_usize() as u32,
        ];
        self.emit_values(inst, &values);
    }

    fn emit_access_tuple(
        &mut self,
        inst: BytecodeOpcode,
        r1: Register,
        r2: Register,
        idx: ConstPoolIdx,
    ) {
        let values = [
            r1.to_usize() as u32,
            r2.to_usize() as u32,
            idx.to_usize() as u32,
        ];
        self.emit_values(inst, &values);
    }

    fn emit_access_enum(
        &mut self,
        inst: BytecodeOpcode,
        r1: Register,
        r2: Register,
        idx: ConstPoolIdx,
    ) {
        let values = [
            r1.to_usize() as u32,
            r2.to_usize() as u32,
            idx.to_usize() as u32,
        ];
        self.emit_values(inst, &values);
    }

    fn emit_load_global_inst(
        &mut self,
        inst: BytecodeOpcode,
        r1: Register,
        gid: GlobalDefinitionId,
    ) {
        let values = [r1.to_usize() as u32, gid.to_usize() as u32];
        self.emit_values(inst, &values);
    }

    fn emit_store_global_inst(
        &mut self,
        inst: BytecodeOpcode,
        r1: Register,
        gid: GlobalDefinitionId,
    ) {
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
        assert!(self.position.is_some());

        let position = self.position.unwrap();
        let last_position = self.positions.last().map(|(_, p)| p);

        if let Some(last_position) = last_position {
            if *last_position == position {
                self.position = None;
                return;
            }
        }

        self.positions.push((offset, position));
        self.position = None;
    }

    fn emit_values(&mut self, op: BytecodeOpcode, values: &[u32]) {
        if op.needs_position() {
            self.emit_position();
        } else {
            assert!(self.position.is_none());
        }

        let is_wide = values.iter().any(|&val| val > u8::max_value() as u32);

        if is_wide {
            self.emit_wide();
            self.emit_opcode(op.to_u8() as u32);
            for &value in values {
                self.emit_u32(value);
            }
        } else {
            self.emit_opcode(op.to_u8() as u32);
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
        self.code.push(BytecodeOpcode::Wide.to_u8());
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
        debug_assert!(fits_u8(inst.to_u8() as u32));
        debug_assert!(fits_u8(inst_const.to_u8() as u32));
        let start = self.offset();

        if (cond.is_some() && !fits_u8(cond.unwrap().to_usize() as u32))
            || !fits_u8(self.const_pool.len() as u32)
        {
            self.emit_wide();
            self.emit_opcode(inst.to_u8() as u32);
            if let Some(cond) = cond {
                self.emit_u32(cond.to_usize() as u32);
            }
            let address = self.offset();
            self.emit_u32(0);
            self.unresolved_jump_offsets.push((start, address, lbl));
        } else {
            self.emit_opcode(inst_const.to_u8() as u32);
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
        BytecodeOpcode::from_u8(self.code[offset.to_usize()]).expect("unknown bytecode")
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
