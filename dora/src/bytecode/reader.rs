use std::iter::Iterator;

use crate::bytecode::{
    BytecodeInstruction, BytecodeOffset, BytecodeOpcode, ConstPoolIdx, OperandWidth, Register,
};
use crate::language::sem_analysis::{GlobalDefinition, GlobalDefinitionId};
use crate::utils::Id;

pub fn read<T: BytecodeVisitor>(data: &[u8], visitor: &mut T) {
    BytecodeFullIteration::new(data, visitor).read();
}

pub struct BytecodeReader<'a> {
    code: &'a [u8],
    offset: usize,
    width: OperandWidth,
}

impl<'a> BytecodeReader<'a> {
    pub fn new(code: &'a [u8]) -> BytecodeReader<'a> {
        BytecodeReader {
            code,
            offset: 0,
            width: OperandWidth::Normal,
        }
    }

    pub fn read_opcode_at(code: &'a [u8], offset: usize) -> BytecodeOpcode {
        let mut reader = BytecodeReader {
            code,
            offset,
            width: OperandWidth::Normal,
        };

        reader.read_opcode()
    }

    fn offset(&self) -> usize {
        self.offset
    }

    fn read_instruction(&mut self) -> (usize, OperandWidth, BytecodeOpcode, BytecodeInstruction) {
        let start = self.offset();
        let opcode = self.read_opcode();

        let inst = match opcode {
            BytecodeOpcode::Wide => unreachable!(),

            BytecodeOpcode::Add => {
                let dest = self.read_register();
                let lhs = self.read_register();
                let rhs = self.read_register();
                BytecodeInstruction::Add { dest, lhs, rhs }
            }
            BytecodeOpcode::Sub => {
                let dest = self.read_register();
                let lhs = self.read_register();
                let rhs = self.read_register();
                BytecodeInstruction::Sub { dest, lhs, rhs }
            }
            BytecodeOpcode::Neg => {
                let dest = self.read_register();
                let src = self.read_register();
                BytecodeInstruction::Neg { dest, src }
            }
            BytecodeOpcode::Mul => {
                let dest = self.read_register();
                let lhs = self.read_register();
                let rhs = self.read_register();
                BytecodeInstruction::Mul { dest, lhs, rhs }
            }
            BytecodeOpcode::Div => {
                let dest = self.read_register();
                let lhs = self.read_register();
                let rhs = self.read_register();
                BytecodeInstruction::Div { dest, lhs, rhs }
            }

            BytecodeOpcode::Rem => {
                let dest = self.read_register();
                let lhs = self.read_register();
                let rhs = self.read_register();
                BytecodeInstruction::Rem { dest, lhs, rhs }
            }

            BytecodeOpcode::And => {
                let dest = self.read_register();
                let lhs = self.read_register();
                let rhs = self.read_register();
                BytecodeInstruction::And { dest, lhs, rhs }
            }

            BytecodeOpcode::Or => {
                let dest = self.read_register();
                let lhs = self.read_register();
                let rhs = self.read_register();
                BytecodeInstruction::Or { dest, lhs, rhs }
            }

            BytecodeOpcode::Xor => {
                let dest = self.read_register();
                let lhs = self.read_register();
                let rhs = self.read_register();
                BytecodeInstruction::Xor { dest, lhs, rhs }
            }

            BytecodeOpcode::Not => {
                let dest = self.read_register();
                let src = self.read_register();
                BytecodeInstruction::Not { dest, src }
            }

            BytecodeOpcode::Shl => {
                let dest = self.read_register();
                let lhs = self.read_register();
                let rhs = self.read_register();
                BytecodeInstruction::Shl { dest, lhs, rhs }
            }
            BytecodeOpcode::Shr => {
                let dest = self.read_register();
                let lhs = self.read_register();
                let rhs = self.read_register();
                BytecodeInstruction::Shr { dest, lhs, rhs }
            }
            BytecodeOpcode::Sar => {
                let dest = self.read_register();
                let lhs = self.read_register();
                let rhs = self.read_register();
                BytecodeInstruction::Sar { dest, lhs, rhs }
            }

            BytecodeOpcode::Mov => {
                let dest = self.read_register();
                let src = self.read_register();
                BytecodeInstruction::Mov { dest, src }
            }

            BytecodeOpcode::LoadTupleElement => {
                let dest = self.read_register();
                let src = self.read_register();
                let idx = self.read_const_pool_idx();
                BytecodeInstruction::LoadTupleElement { dest, src, idx }
            }

            BytecodeOpcode::LoadEnumElement => {
                let dest = self.read_register();
                let src = self.read_register();
                let idx = self.read_const_pool_idx();
                BytecodeInstruction::LoadEnumElement { dest, src, idx }
            }

            BytecodeOpcode::LoadEnumVariant => {
                let dest = self.read_register();
                let src = self.read_register();
                let idx = self.read_const_pool_idx();
                BytecodeInstruction::LoadEnumVariant { dest, src, idx }
            }

            BytecodeOpcode::LoadStructField => {
                let dest = self.read_register();
                let obj = self.read_register();
                let field = self.read_const_pool_idx();
                BytecodeInstruction::LoadStructField { dest, obj, field }
            }

            BytecodeOpcode::LoadField => {
                let dest = self.read_register();
                let obj = self.read_register();
                let field = self.read_const_pool_idx();
                BytecodeInstruction::LoadField { dest, obj, field }
            }

            BytecodeOpcode::StoreField => {
                let src = self.read_register();
                let obj = self.read_register();
                let field = self.read_const_pool_idx();
                BytecodeInstruction::StoreField { src, obj, field }
            }

            BytecodeOpcode::LoadGlobal => {
                let dest = self.read_register();
                let global_id = self.read_global();
                BytecodeInstruction::LoadGlobal { dest, global_id }
            }

            BytecodeOpcode::StoreGlobal => {
                let src = self.read_register();
                let global_id = self.read_global();
                BytecodeInstruction::StoreGlobal { src, global_id }
            }

            BytecodeOpcode::PushRegister => {
                let src = self.read_register();
                BytecodeInstruction::PushRegister { src }
            }

            BytecodeOpcode::ConstTrue => {
                let dest = self.read_register();
                BytecodeInstruction::ConstTrue { dest }
            }
            BytecodeOpcode::ConstFalse => {
                let dest = self.read_register();
                BytecodeInstruction::ConstFalse { dest }
            }
            BytecodeOpcode::ConstChar => {
                let dest = self.read_register();
                let idx = self.read_const_pool_idx();
                BytecodeInstruction::ConstChar { dest, idx }
            }
            BytecodeOpcode::ConstUInt8 => {
                let dest = self.read_register();
                let value = self.read_byte() as u8;
                BytecodeInstruction::ConstUInt8 { dest, value }
            }
            BytecodeOpcode::ConstInt32 => {
                let dest = self.read_register();
                let idx = self.read_const_pool_idx();
                BytecodeInstruction::ConstInt32 { dest, idx }
            }
            BytecodeOpcode::ConstInt64 => {
                let dest = self.read_register();
                let idx = self.read_const_pool_idx();
                BytecodeInstruction::ConstInt64 { dest, idx }
            }
            BytecodeOpcode::ConstFloat32 => {
                let dest = self.read_register();
                let idx = self.read_const_pool_idx();
                BytecodeInstruction::ConstFloat32 { dest, idx }
            }
            BytecodeOpcode::ConstFloat64 => {
                let dest = self.read_register();
                let idx = self.read_const_pool_idx();
                BytecodeInstruction::ConstFloat64 { dest, idx }
            }
            BytecodeOpcode::ConstString => {
                let dest = self.read_register();
                let idx = self.read_const_pool_idx();
                BytecodeInstruction::ConstString { dest, idx }
            }

            BytecodeOpcode::TestIdentity => {
                let dest = self.read_register();
                let lhs = self.read_register();
                let rhs = self.read_register();
                BytecodeInstruction::TestIdentity { dest, lhs, rhs }
            }
            BytecodeOpcode::TestEq => {
                let dest = self.read_register();
                let lhs = self.read_register();
                let rhs = self.read_register();
                BytecodeInstruction::TestEq { dest, lhs, rhs }
            }
            BytecodeOpcode::TestNe => {
                let dest = self.read_register();
                let lhs = self.read_register();
                let rhs = self.read_register();
                BytecodeInstruction::TestNe { dest, lhs, rhs }
            }
            BytecodeOpcode::TestGt => {
                let dest = self.read_register();
                let lhs = self.read_register();
                let rhs = self.read_register();
                BytecodeInstruction::TestGt { dest, lhs, rhs }
            }
            BytecodeOpcode::TestGe => {
                let dest = self.read_register();
                let lhs = self.read_register();
                let rhs = self.read_register();
                BytecodeInstruction::TestGe { dest, lhs, rhs }
            }
            BytecodeOpcode::TestLt => {
                let dest = self.read_register();
                let lhs = self.read_register();
                let rhs = self.read_register();
                BytecodeInstruction::TestLt { dest, lhs, rhs }
            }
            BytecodeOpcode::TestLe => {
                let dest = self.read_register();
                let lhs = self.read_register();
                let rhs = self.read_register();
                BytecodeInstruction::TestLe { dest, lhs, rhs }
            }

            BytecodeOpcode::JumpLoop => {
                let offset = self.read_offset();
                BytecodeInstruction::JumpLoop { offset }
            }
            BytecodeOpcode::LoopStart => BytecodeInstruction::LoopStart,
            BytecodeOpcode::JumpIfFalse => {
                let opnd = self.read_register();
                let offset = self.read_offset();
                BytecodeInstruction::JumpIfFalse { opnd, offset }
            }
            BytecodeOpcode::JumpIfFalseConst => {
                let opnd = self.read_register();
                let idx = self.read_const_pool_idx();
                BytecodeInstruction::JumpIfFalseConst { opnd, idx }
            }
            BytecodeOpcode::JumpIfTrue => {
                let opnd = self.read_register();
                let offset = self.read_offset();
                BytecodeInstruction::JumpIfTrue { opnd, offset }
            }
            BytecodeOpcode::JumpIfTrueConst => {
                let opnd = self.read_register();
                let idx = self.read_const_pool_idx();
                BytecodeInstruction::JumpIfTrueConst { opnd, idx }
            }
            BytecodeOpcode::Jump => {
                let offset = self.read_offset();
                BytecodeInstruction::Jump { offset }
            }
            BytecodeOpcode::JumpConst => {
                let idx = self.read_const_pool_idx();
                BytecodeInstruction::JumpConst { idx }
            }

            BytecodeOpcode::InvokeDirect => {
                let dest = self.read_register();
                let fct = self.read_const_pool_idx();
                BytecodeInstruction::InvokeDirect { dest, fct }
            }

            BytecodeOpcode::InvokeVirtual => {
                let dest = self.read_register();
                let fct = self.read_const_pool_idx();
                BytecodeInstruction::InvokeVirtual { dest, fct }
            }

            BytecodeOpcode::InvokeStatic => {
                let dest = self.read_register();
                let fct = self.read_const_pool_idx();
                BytecodeInstruction::InvokeStatic { dest, fct }
            }

            BytecodeOpcode::InvokeLambda => {
                let dest = self.read_register();
                let idx = self.read_const_pool_idx();
                BytecodeInstruction::InvokeLambda { dest, idx }
            }

            BytecodeOpcode::InvokeGenericStatic => {
                let dest = self.read_register();
                let fct = self.read_const_pool_idx();
                BytecodeInstruction::InvokeGenericStatic { dest, fct }
            }

            BytecodeOpcode::InvokeGenericDirect => {
                let dest = self.read_register();
                let fct = self.read_const_pool_idx();
                BytecodeInstruction::InvokeGenericDirect { dest, fct }
            }

            BytecodeOpcode::NewObject => {
                let dest = self.read_register();
                let cls = self.read_const_pool_idx();
                BytecodeInstruction::NewObject { dest, cls }
            }
            BytecodeOpcode::NewObjectInitialized => {
                let dest = self.read_register();
                let cls = self.read_const_pool_idx();
                BytecodeInstruction::NewObjectInitialized { dest, cls }
            }
            BytecodeOpcode::NewArray => {
                let dest = self.read_register();
                let cls = self.read_const_pool_idx();
                let length = self.read_register();
                BytecodeInstruction::NewArray { dest, cls, length }
            }
            BytecodeOpcode::NewTuple => {
                let dest = self.read_register();
                let idx = self.read_const_pool_idx();
                BytecodeInstruction::NewTuple { dest, idx }
            }
            BytecodeOpcode::NewEnum => {
                let dest = self.read_register();
                let idx = self.read_const_pool_idx();
                BytecodeInstruction::NewEnum { dest, idx }
            }
            BytecodeOpcode::NewStruct => {
                let dest = self.read_register();
                let idx = self.read_const_pool_idx();
                BytecodeInstruction::NewStruct { dest, idx }
            }
            BytecodeOpcode::NewTraitObject => {
                let dest = self.read_register();
                let idx = self.read_const_pool_idx();
                let src = self.read_register();
                BytecodeInstruction::NewTraitObject { dest, idx, src }
            }
            BytecodeOpcode::NewLambda => {
                let dest = self.read_register();
                let idx = self.read_const_pool_idx();
                BytecodeInstruction::NewLambda { dest, idx }
            }

            BytecodeOpcode::ArrayLength => {
                let dest = self.read_register();
                let arr = self.read_register();
                BytecodeInstruction::ArrayLength { dest, arr }
            }

            BytecodeOpcode::LoadArray => {
                let dest = self.read_register();
                let arr = self.read_register();
                let idx = self.read_register();
                BytecodeInstruction::LoadArray { dest, arr, idx }
            }

            BytecodeOpcode::StoreArray => {
                let src = self.read_register();
                let arr = self.read_register();
                let idx = self.read_register();
                BytecodeInstruction::StoreArray { src, arr, idx }
            }

            BytecodeOpcode::StringLength => {
                let dest = self.read_register();
                let str = self.read_register();
                BytecodeInstruction::StringLength { dest, str }
            }

            BytecodeOpcode::LoadStringUInt8 => {
                let dest = self.read_register();
                let str = self.read_register();
                let idx = self.read_register();
                BytecodeInstruction::LoadStringUInt8 { dest, str, idx }
            }

            BytecodeOpcode::Ret => {
                let opnd = self.read_register();
                BytecodeInstruction::Ret { opnd }
            }

            _ => unreachable!(),
        };

        (start, self.width, opcode, inst)
    }

    fn read_register(&mut self) -> Register {
        Register(self.read_index() as usize)
    }

    fn read_global(&mut self) -> GlobalDefinitionId {
        GlobalDefinition::usize_to_id(self.read_index() as usize)
    }

    fn read_const_pool_idx(&mut self) -> ConstPoolIdx {
        (self.read_index() as usize).into()
    }

    fn read_offset(&mut self) -> u32 {
        self.read_index()
    }

    fn read_index(&mut self) -> u32 {
        match self.width {
            OperandWidth::Normal => self.read_byte() as u32,
            OperandWidth::Wide => self.read_wide(),
        }
    }

    fn read_opcode(&mut self) -> BytecodeOpcode {
        let mut opcode = self.read_byte();

        if opcode == BytecodeOpcode::Wide.to_u8() {
            self.width = OperandWidth::Wide;
            opcode = self.read_byte();
        } else {
            self.width = OperandWidth::Normal;
        }

        BytecodeOpcode::from_u8(opcode).expect("illegal opcode")
    }

    fn read_wide(&mut self) -> u32 {
        let v1 = self.read_byte() as u32;
        let v2 = self.read_byte() as u32;
        let v3 = self.read_byte() as u32;
        let v4 = self.read_byte() as u32;

        (v4 << 24) | (v3 << 16) | (v2 << 8) | v1
    }

    fn read_byte(&mut self) -> u8 {
        let value = self.code[self.offset];
        self.offset += 1;
        value
    }
}

impl<'a> Iterator for BytecodeReader<'a> {
    type Item = BytecodeInstruction;

    fn next(&mut self) -> Option<BytecodeInstruction> {
        if self.offset < self.code.len() {
            let (_start, _width, _opcode, inst) = self.read_instruction();
            Some(inst)
        } else {
            assert_eq!(self.offset, self.code.len());
            None
        }
    }
}

struct BytecodeFullIteration<'a, T: BytecodeVisitor> {
    iterator: BytecodeReader<'a>,
    visitor: &'a mut T,
}

impl<'a, T> BytecodeFullIteration<'a, T>
where
    T: BytecodeVisitor,
{
    fn new(code: &'a [u8], visitor: &'a mut T) -> BytecodeFullIteration<'a, T> {
        BytecodeFullIteration {
            iterator: BytecodeReader::new(code),
            visitor,
        }
    }

    fn read(&mut self) {
        let mut last = self.iterator.offset();

        while let Some(inst) = self.iterator.next() {
            self.visitor.visit_instruction(BytecodeOffset(last as u32));
            self.dispatch_instruction(inst);
            last = self.iterator.offset();
        }
    }

    fn dispatch_instruction(&mut self, inst: BytecodeInstruction) {
        match inst {
            BytecodeInstruction::Add { dest, lhs, rhs } => {
                self.visitor.visit_add(dest, lhs, rhs);
            }
            BytecodeInstruction::Sub { dest, lhs, rhs } => {
                self.visitor.visit_sub(dest, lhs, rhs);
            }
            BytecodeInstruction::Neg { dest, src } => {
                self.visitor.visit_neg(dest, src);
            }
            BytecodeInstruction::Mul { dest, lhs, rhs } => {
                self.visitor.visit_mul(dest, lhs, rhs);
            }
            BytecodeInstruction::Div { dest, lhs, rhs } => {
                self.visitor.visit_div(dest, lhs, rhs);
            }

            BytecodeInstruction::Rem { dest, lhs, rhs } => {
                self.visitor.visit_rem(dest, lhs, rhs);
            }

            BytecodeInstruction::And { dest, lhs, rhs } => {
                self.visitor.visit_and(dest, lhs, rhs);
            }

            BytecodeInstruction::Or { dest, lhs, rhs } => {
                self.visitor.visit_or(dest, lhs, rhs);
            }

            BytecodeInstruction::Xor { dest, lhs, rhs } => {
                self.visitor.visit_xor(dest, lhs, rhs);
            }

            BytecodeInstruction::Not { dest, src } => {
                self.visitor.visit_not(dest, src);
            }

            BytecodeInstruction::Shl { dest, lhs, rhs } => {
                self.visitor.visit_shl(dest, lhs, rhs);
            }
            BytecodeInstruction::Shr { dest, lhs, rhs } => {
                self.visitor.visit_shr(dest, lhs, rhs);
            }
            BytecodeInstruction::Sar { dest, lhs, rhs } => {
                self.visitor.visit_sar(dest, lhs, rhs);
            }

            BytecodeInstruction::Mov { dest, src } => {
                self.visitor.visit_mov(dest, src);
            }

            BytecodeInstruction::LoadTupleElement { dest, src, idx } => {
                self.visitor.visit_load_tuple_element(dest, src, idx);
            }

            BytecodeInstruction::LoadEnumElement { dest, src, idx } => {
                self.visitor.visit_load_enum_element(dest, src, idx);
            }

            BytecodeInstruction::LoadEnumVariant { dest, src, idx } => {
                self.visitor.visit_load_enum_variant(dest, src, idx);
            }

            BytecodeInstruction::LoadStructField { dest, obj, field } => {
                self.visitor.visit_load_struct_field(dest, obj, field);
            }

            BytecodeInstruction::LoadField { dest, obj, field } => {
                self.visitor.visit_load_field(dest, obj, field);
            }

            BytecodeInstruction::StoreField { src, obj, field } => {
                self.visitor.visit_store_field(src, obj, field);
            }

            BytecodeInstruction::LoadGlobal { dest, global_id } => {
                self.visitor.visit_load_global(dest, global_id);
            }

            BytecodeInstruction::StoreGlobal { src, global_id } => {
                self.visitor.visit_store_global(src, global_id);
            }

            BytecodeInstruction::PushRegister { src } => {
                self.visitor.visit_push_register(src);
            }

            BytecodeInstruction::ConstTrue { dest } => {
                self.visitor.visit_const_true(dest);
            }
            BytecodeInstruction::ConstFalse { dest } => {
                self.visitor.visit_const_false(dest);
            }
            BytecodeInstruction::ConstChar { dest, idx } => {
                self.visitor.visit_const_char(dest, idx);
            }
            BytecodeInstruction::ConstUInt8 { dest, value } => {
                self.visitor.visit_const_uint8(dest, value as u8);
            }
            BytecodeInstruction::ConstInt32 { dest, idx } => {
                self.visitor.visit_const_int32(dest, idx);
            }
            BytecodeInstruction::ConstInt64 { dest, idx } => {
                self.visitor.visit_const_int64(dest, idx);
            }
            BytecodeInstruction::ConstFloat32 { dest, idx } => {
                self.visitor.visit_const_float32(dest, idx);
            }
            BytecodeInstruction::ConstFloat64 { dest, idx } => {
                self.visitor.visit_const_float64(dest, idx);
            }
            BytecodeInstruction::ConstString { dest, idx } => {
                self.visitor.visit_const_string(dest, idx);
            }

            BytecodeInstruction::TestIdentity { dest, lhs, rhs } => {
                self.visitor.visit_test_identity(dest, lhs, rhs);
            }
            BytecodeInstruction::TestEq { dest, lhs, rhs } => {
                self.visitor.visit_test_eq(dest, lhs, rhs);
            }
            BytecodeInstruction::TestNe { dest, lhs, rhs } => {
                self.visitor.visit_test_ne(dest, lhs, rhs);
            }
            BytecodeInstruction::TestGt { dest, lhs, rhs } => {
                self.visitor.visit_test_gt(dest, lhs, rhs);
            }
            BytecodeInstruction::TestGe { dest, lhs, rhs } => {
                self.visitor.visit_test_ge(dest, lhs, rhs);
            }
            BytecodeInstruction::TestLt { dest, lhs, rhs } => {
                self.visitor.visit_test_lt(dest, lhs, rhs);
            }
            BytecodeInstruction::TestLe { dest, lhs, rhs } => {
                self.visitor.visit_test_le(dest, lhs, rhs);
            }

            BytecodeInstruction::JumpLoop { offset } => {
                self.visitor.visit_jump_loop(offset);
            }
            BytecodeInstruction::LoopStart => {
                self.visitor.visit_loop_start();
            }
            BytecodeInstruction::JumpIfFalse { opnd, offset } => {
                self.visitor.visit_jump_if_false(opnd, offset);
            }
            BytecodeInstruction::JumpIfFalseConst { opnd, idx } => {
                self.visitor.visit_jump_if_false_const(opnd, idx);
            }
            BytecodeInstruction::JumpIfTrue { opnd, offset } => {
                self.visitor.visit_jump_if_true(opnd, offset);
            }
            BytecodeInstruction::JumpIfTrueConst { opnd, idx } => {
                self.visitor.visit_jump_if_true_const(opnd, idx);
            }
            BytecodeInstruction::Jump { offset } => {
                self.visitor.visit_jump(offset);
            }
            BytecodeInstruction::JumpConst { idx } => {
                self.visitor.visit_jump_const(idx);
            }

            BytecodeInstruction::InvokeDirect { dest, fct } => {
                self.visitor.visit_invoke_direct(dest, fct);
            }

            BytecodeInstruction::InvokeVirtual { dest, fct } => {
                self.visitor.visit_invoke_virtual(dest, fct);
            }

            BytecodeInstruction::InvokeStatic { dest, fct } => {
                self.visitor.visit_invoke_static(dest, fct);
            }

            BytecodeInstruction::InvokeLambda { dest, idx } => {
                self.visitor.visit_invoke_lambda(dest, idx);
            }

            BytecodeInstruction::InvokeGenericStatic { dest, fct } => {
                self.visitor.visit_invoke_generic_static(dest, fct);
            }

            BytecodeInstruction::InvokeGenericDirect { dest, fct } => {
                self.visitor.visit_invoke_generic_direct(dest, fct);
            }

            BytecodeInstruction::NewObject { dest, cls } => {
                self.visitor.visit_new_object(dest, cls);
            }
            BytecodeInstruction::NewObjectInitialized { dest, cls } => {
                self.visitor.visit_new_object_initialized(dest, cls);
            }
            BytecodeInstruction::NewArray { dest, cls, length } => {
                self.visitor.visit_new_array(dest, cls, length);
            }
            BytecodeInstruction::NewTuple { dest, idx } => {
                self.visitor.visit_new_tuple(dest, idx);
            }
            BytecodeInstruction::NewEnum { dest, idx } => {
                self.visitor.visit_new_enum(dest, idx);
            }
            BytecodeInstruction::NewStruct { dest, idx } => {
                self.visitor.visit_new_struct(dest, idx);
            }
            BytecodeInstruction::NewTraitObject { dest, idx, src } => {
                self.visitor.visit_new_trait_object(dest, idx, src);
            }
            BytecodeInstruction::NewLambda { dest, idx } => {
                self.visitor.visit_new_lambda(dest, idx);
            }

            BytecodeInstruction::ArrayLength { dest, arr } => {
                self.visitor.visit_array_length(dest, arr);
            }

            BytecodeInstruction::LoadArray { dest, arr, idx } => {
                self.visitor.visit_load_array(dest, arr, idx);
            }

            BytecodeInstruction::StoreArray { src, arr, idx } => {
                self.visitor.visit_store_array(src, arr, idx);
            }

            BytecodeInstruction::StringLength { dest, str } => {
                self.visitor.visit_string_length(dest, str);
            }

            BytecodeInstruction::LoadStringUInt8 { dest, str, idx } => {
                self.visitor.visit_load_string_uint8(dest, str, idx);
            }

            BytecodeInstruction::Ret { opnd } => {
                self.visitor.visit_ret(opnd);
            }
        }
    }
}

pub trait BytecodeVisitor {
    fn visit_instruction(&mut self, _offset: BytecodeOffset) {}

    fn visit_add(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_sub(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_neg(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }

    fn visit_mul(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_div(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_rem(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_and(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_or(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_xor(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_not(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }

    fn visit_shl(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_shr(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_sar(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }

    fn visit_mov(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }

    fn visit_load_tuple_element(&mut self, _dest: Register, _src: Register, _idx: ConstPoolIdx) {
        unimplemented!();
    }

    fn visit_load_enum_element(&mut self, _dest: Register, _src: Register, _idx: ConstPoolIdx) {
        unimplemented!();
    }

    fn visit_load_enum_variant(&mut self, _dest: Register, _src: Register, _idx: ConstPoolIdx) {
        unimplemented!();
    }

    fn visit_load_struct_field(&mut self, _dest: Register, _obj: Register, _field: ConstPoolIdx) {
        unimplemented!();
    }

    fn visit_load_field(&mut self, _dest: Register, _obj: Register, _field: ConstPoolIdx) {
        unimplemented!();
    }

    fn visit_store_field(&mut self, _src: Register, _obj: Register, _field: ConstPoolIdx) {
        unimplemented!();
    }

    fn visit_load_global(&mut self, _dest: Register, _global_id: GlobalDefinitionId) {
        unimplemented!();
    }

    fn visit_store_global(&mut self, _src: Register, _global_id: GlobalDefinitionId) {
        unimplemented!();
    }

    fn visit_push_register(&mut self, _src: Register) {
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

    fn visit_test_identity(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_eq(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_ne(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_gt(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_ge(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_lt(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
        unimplemented!();
    }
    fn visit_test_le(&mut self, _dest: Register, _lhs: Register, _rhs: Register) {
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

    fn visit_invoke_direct(&mut self, _dest: Register, _fct: ConstPoolIdx) {
        unimplemented!();
    }

    fn visit_invoke_virtual(&mut self, _dest: Register, _fct: ConstPoolIdx) {
        unimplemented!();
    }

    fn visit_invoke_static(&mut self, _dest: Register, _fct: ConstPoolIdx) {
        unimplemented!();
    }

    fn visit_invoke_lambda(&mut self, _dest: Register, _idx: ConstPoolIdx) {
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
    fn visit_new_object_initialized(&mut self, _dest: Register, _cls: ConstPoolIdx) {
        unimplemented!();
    }
    fn visit_new_array(&mut self, _dest: Register, _cls: ConstPoolIdx, _length: Register) {
        unimplemented!();
    }
    fn visit_new_tuple(&mut self, _dest: Register, _idx: ConstPoolIdx) {
        unimplemented!();
    }
    fn visit_new_enum(&mut self, _dest: Register, _idx: ConstPoolIdx) {
        unimplemented!();
    }
    fn visit_new_struct(&mut self, _dest: Register, _idx: ConstPoolIdx) {
        unimplemented!();
    }
    fn visit_new_trait_object(&mut self, _dest: Register, _idx: ConstPoolIdx, _src: Register) {
        unimplemented!();
    }
    fn visit_new_lambda(&mut self, _dest: Register, _idx: ConstPoolIdx) {
        unimplemented!();
    }

    fn visit_array_length(&mut self, _dest: Register, _arr: Register) {
        unimplemented!();
    }

    fn visit_load_array(&mut self, _dest: Register, _arr: Register, _idx: Register) {
        unimplemented!();
    }

    fn visit_store_array(&mut self, _src: Register, _arr: Register, _idx: Register) {
        unimplemented!();
    }

    fn visit_string_length(&mut self, _dest: Register, _arr: Register) {
        unimplemented!();
    }

    fn visit_load_string_uint8(&mut self, _dest: Register, _arr: Register, _idx: Register) {
        unimplemented!();
    }

    fn visit_ret(&mut self, _opnd: Register) {
        unimplemented!();
    }
}
