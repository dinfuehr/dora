use std::iter::Iterator;

use crate::bytecode::{
    BytecodeFunction, BytecodeInstruction, BytecodeOffset, BytecodeOpcode, ConstPoolIdx,
    OperandWidth, Register,
};
use crate::language::sem_analysis::{GlobalDefinitionId, TupleId};

pub fn read<T: BytecodeVisitor>(data: &[u8], visitor: &mut T) {
    BytecodeReader::new(data, visitor).read();
}

struct BytecodeIterator<'a> {
    code: &'a [u8],
    current: usize,
}

impl<'a> BytecodeIterator<'a> {
    fn new(code: &'a [u8]) -> BytecodeIterator<'a> {
        BytecodeIterator { code, current: 0 }
    }

    fn offset(&self) -> usize {
        self.current
    }

    fn read_instruction(
        &mut self,
        width: OperandWidth,
        opcode: BytecodeOpcode,
    ) -> BytecodeInstruction {
        match opcode {
            BytecodeOpcode::Wide => unreachable!(),

            BytecodeOpcode::AddInt32 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::AddInt32 { dest, lhs, rhs }
            }
            BytecodeOpcode::AddInt64 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::AddInt64 { dest, lhs, rhs }
            }
            BytecodeOpcode::AddFloat32 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::AddFloat32 { dest, lhs, rhs }
            }
            BytecodeOpcode::AddFloat64 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::AddFloat64 { dest, lhs, rhs }
            }
            BytecodeOpcode::SubInt32 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::SubInt32 { dest, lhs, rhs }
            }
            BytecodeOpcode::SubInt64 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::SubInt64 { dest, lhs, rhs }
            }
            BytecodeOpcode::SubFloat32 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::SubFloat32 { dest, lhs, rhs }
            }
            BytecodeOpcode::SubFloat64 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::SubFloat64 { dest, lhs, rhs }
            }
            BytecodeOpcode::NegInt32 => {
                let dest = self.read_register(0, width);
                let src = self.read_register(1, width);
                BytecodeInstruction::NegInt32 { dest, src }
            }
            BytecodeOpcode::NegInt64 => {
                let dest = self.read_register(0, width);
                let src = self.read_register(1, width);
                BytecodeInstruction::NegInt64 { dest, src }
            }
            BytecodeOpcode::NegFloat32 => {
                let dest = self.read_register(0, width);
                let src = self.read_register(1, width);
                BytecodeInstruction::NegFloat32 { dest, src }
            }
            BytecodeOpcode::NegFloat64 => {
                let dest = self.read_register(0, width);
                let src = self.read_register(1, width);
                BytecodeInstruction::NegFloat64 { dest, src }
            }
            BytecodeOpcode::MulInt32 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::MulInt32 { dest, lhs, rhs }
            }
            BytecodeOpcode::MulInt64 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::MulInt64 { dest, lhs, rhs }
            }
            BytecodeOpcode::MulFloat32 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::MulFloat32 { dest, lhs, rhs }
            }
            BytecodeOpcode::MulFloat64 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::MulFloat64 { dest, lhs, rhs }
            }
            BytecodeOpcode::DivInt32 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::DivInt32 { dest, lhs, rhs }
            }
            BytecodeOpcode::DivInt64 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::DivInt64 { dest, lhs, rhs }
            }
            BytecodeOpcode::DivFloat32 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::DivFloat32 { dest, lhs, rhs }
            }
            BytecodeOpcode::DivFloat64 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::DivFloat64 { dest, lhs, rhs }
            }

            BytecodeOpcode::ModInt32 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::ModInt32 { dest, lhs, rhs }
            }
            BytecodeOpcode::ModInt64 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::ModInt64 { dest, lhs, rhs }
            }

            BytecodeOpcode::AndInt32 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::AndInt32 { dest, lhs, rhs }
            }
            BytecodeOpcode::AndInt64 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::AndInt64 { dest, lhs, rhs }
            }

            BytecodeOpcode::OrInt32 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::OrInt32 { dest, lhs, rhs }
            }
            BytecodeOpcode::OrInt64 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::OrInt64 { dest, lhs, rhs }
            }

            BytecodeOpcode::XorInt32 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::XorInt32 { dest, lhs, rhs }
            }
            BytecodeOpcode::XorInt64 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::XorInt64 { dest, lhs, rhs }
            }

            BytecodeOpcode::NotBool => {
                let dest = self.read_register(0, width);
                let src = self.read_register(1, width);
                BytecodeInstruction::NotBool { dest, src }
            }
            BytecodeOpcode::NotInt32 => {
                let dest = self.read_register(0, width);
                let src = self.read_register(1, width);
                BytecodeInstruction::NotInt32 { dest, src }
            }
            BytecodeOpcode::NotInt64 => {
                let dest = self.read_register(0, width);
                let src = self.read_register(1, width);
                BytecodeInstruction::NotInt64 { dest, src }
            }

            BytecodeOpcode::ShlInt32 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::ShlInt32 { dest, lhs, rhs }
            }
            BytecodeOpcode::ShrInt32 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::ShrInt32 { dest, lhs, rhs }
            }
            BytecodeOpcode::SarInt32 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::SarInt32 { dest, lhs, rhs }
            }

            BytecodeOpcode::ShlInt64 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::ShlInt64 { dest, lhs, rhs }
            }
            BytecodeOpcode::ShrInt64 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::ShrInt64 { dest, lhs, rhs }
            }
            BytecodeOpcode::SarInt64 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::SarInt64 { dest, lhs, rhs }
            }

            BytecodeOpcode::RolInt32 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::RolInt32 { dest, lhs, rhs }
            }
            BytecodeOpcode::RorInt32 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::RorInt32 { dest, lhs, rhs }
            }

            BytecodeOpcode::RolInt64 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::RolInt64 { dest, lhs, rhs }
            }
            BytecodeOpcode::RorInt64 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::RorInt64 { dest, lhs, rhs }
            }

            BytecodeOpcode::ExtendUInt8ToChar => {
                let dest = self.read_register(0, width);
                let src = self.read_register(1, width);
                BytecodeInstruction::ExtendUInt8ToChar { dest, src }
            }
            BytecodeOpcode::ExtendUInt8ToInt32 => {
                let dest = self.read_register(0, width);
                let src = self.read_register(1, width);
                BytecodeInstruction::ExtendUInt8ToInt32 { dest, src }
            }
            BytecodeOpcode::ExtendUInt8ToInt64 => {
                let dest = self.read_register(0, width);
                let src = self.read_register(1, width);
                BytecodeInstruction::ExtendUInt8ToInt64 { dest, src }
            }
            BytecodeOpcode::ExtendInt32ToInt64 => {
                let dest = self.read_register(0, width);
                let src = self.read_register(1, width);
                BytecodeInstruction::ExtendInt32ToInt64 { dest, src }
            }
            BytecodeOpcode::ExtendCharToInt64 => {
                let dest = self.read_register(0, width);
                let src = self.read_register(1, width);
                BytecodeInstruction::ExtendCharToInt64 { dest, src }
            }
            BytecodeOpcode::CastCharToInt32 => {
                let dest = self.read_register(0, width);
                let src = self.read_register(1, width);
                BytecodeInstruction::CastCharToInt32 { dest, src }
            }
            BytecodeOpcode::CastInt32ToUInt8 => {
                let dest = self.read_register(0, width);
                let src = self.read_register(1, width);
                BytecodeInstruction::CastInt32ToUInt8 { dest, src }
            }
            BytecodeOpcode::CastInt32ToChar => {
                let dest = self.read_register(0, width);
                let src = self.read_register(1, width);
                BytecodeInstruction::CastInt32ToChar { dest, src }
            }
            BytecodeOpcode::CastInt64ToUInt8 => {
                let dest = self.read_register(0, width);
                let src = self.read_register(1, width);
                BytecodeInstruction::CastInt64ToUInt8 { dest, src }
            }
            BytecodeOpcode::CastInt64ToChar => {
                let dest = self.read_register(0, width);
                let src = self.read_register(1, width);
                BytecodeInstruction::CastInt64ToChar { dest, src }
            }
            BytecodeOpcode::CastInt64ToInt32 => {
                let dest = self.read_register(0, width);
                let src = self.read_register(1, width);
                BytecodeInstruction::CastInt64ToInt32 { dest, src }
            }

            BytecodeOpcode::InstanceOf => {
                let dest = self.read_register(0, width);
                let src = self.read_register(1, width);
                let cls_id = self.read_const_pool_idx(2, width);
                BytecodeInstruction::InstanceOf { dest, src, cls_id }
            }
            BytecodeOpcode::CheckedCast => {
                let src = self.read_register(0, width);
                let cls_id = self.read_const_pool_idx(1, width);
                BytecodeInstruction::CheckedCast { src, cls_id }
            }

            BytecodeOpcode::Mov => {
                let dest = self.read_register(0, width);
                let src = self.read_register(1, width);
                BytecodeInstruction::Mov { dest, src }
            }

            BytecodeOpcode::LoadTupleElement => {
                let dest = self.read_register(0, width);
                let src = self.read_register(1, width);
                let tuple_id = self.read_tuple(2, width);
                let element = self.read_index(3, width);
                BytecodeInstruction::LoadTupleElement {
                    dest,
                    src,
                    tuple_id,
                    element,
                }
            }

            BytecodeOpcode::LoadEnumElement => {
                let dest = self.read_register(0, width);
                let src = self.read_register(1, width);
                let idx = self.read_const_pool_idx(2, width);
                let element = self.read_index(3, width);
                BytecodeInstruction::LoadEnumElement {
                    dest,
                    src,
                    idx,
                    element,
                }
            }

            BytecodeOpcode::LoadEnumVariant => {
                let dest = self.read_register(0, width);
                let src = self.read_register(1, width);
                let idx = self.read_const_pool_idx(2, width);
                BytecodeInstruction::LoadEnumVariant { dest, src, idx }
            }

            BytecodeOpcode::LoadStructField => {
                let dest = self.read_register(0, width);
                let obj = self.read_register(1, width);
                let field = self.read_const_pool_idx(2, width);
                BytecodeInstruction::LoadStructField { dest, obj, field }
            }

            BytecodeOpcode::LoadField => {
                let dest = self.read_register(0, width);
                let obj = self.read_register(1, width);
                let field = self.read_const_pool_idx(2, width);
                BytecodeInstruction::LoadField { dest, obj, field }
            }

            BytecodeOpcode::StoreField => {
                let src = self.read_register(0, width);
                let obj = self.read_register(1, width);
                let field = self.read_const_pool_idx(2, width);
                BytecodeInstruction::StoreField { src, obj, field }
            }

            BytecodeOpcode::LoadGlobal => {
                let dest = self.read_register(0, width);
                let glob = self.read_global(1, width);
                BytecodeInstruction::LoadGlobal { dest, glob }
            }

            BytecodeOpcode::StoreGlobal => {
                let src = self.read_register(0, width);
                let glob = self.read_global(1, width);
                BytecodeInstruction::StoreGlobal { src, glob }
            }

            BytecodeOpcode::PushRegister => {
                let src = self.read_register(0, width);
                BytecodeInstruction::PushRegister { src }
            }

            BytecodeOpcode::ConstTrue => {
                let dest = self.read_register(0, width);
                BytecodeInstruction::ConstTrue { dest }
            }
            BytecodeOpcode::ConstFalse => {
                let dest = self.read_register(0, width);
                BytecodeInstruction::ConstFalse { dest }
            }
            BytecodeOpcode::ConstZeroUInt8 => {
                let dest = self.read_register(0, width);
                BytecodeInstruction::ConstZeroUInt8 { dest }
            }
            BytecodeOpcode::ConstZeroChar => {
                let dest = self.read_register(0, width);
                BytecodeInstruction::ConstZeroChar { dest }
            }
            BytecodeOpcode::ConstZeroInt32 => {
                let dest = self.read_register(0, width);
                BytecodeInstruction::ConstZeroInt32 { dest }
            }
            BytecodeOpcode::ConstZeroInt64 => {
                let dest = self.read_register(0, width);
                BytecodeInstruction::ConstZeroInt64 { dest }
            }
            BytecodeOpcode::ConstZeroFloat32 => {
                let dest = self.read_register(0, width);
                BytecodeInstruction::ConstZeroFloat32 { dest }
            }
            BytecodeOpcode::ConstZeroFloat64 => {
                let dest = self.read_register(0, width);
                BytecodeInstruction::ConstZeroFloat64 { dest }
            }
            BytecodeOpcode::ConstChar => {
                let dest = self.read_register(0, width);
                let idx = self.read_const_pool_idx(1, width);
                BytecodeInstruction::ConstChar { dest, idx }
            }
            BytecodeOpcode::ConstUInt8 => {
                let dest = self.read_register(0, width);
                let value = self.read_byte_argument(1, width) as u8;
                BytecodeInstruction::ConstUInt8 { dest, value }
            }
            BytecodeOpcode::ConstInt32 => {
                let dest = self.read_register(0, width);
                let idx = self.read_const_pool_idx(1, width);
                BytecodeInstruction::ConstInt32 { dest, idx }
            }
            BytecodeOpcode::ConstInt64 => {
                let dest = self.read_register(0, width);
                let idx = self.read_const_pool_idx(1, width);
                BytecodeInstruction::ConstInt64 { dest, idx }
            }
            BytecodeOpcode::ConstFloat32 => {
                let dest = self.read_register(0, width);
                let idx = self.read_const_pool_idx(1, width);
                BytecodeInstruction::ConstFloat32 { dest, idx }
            }
            BytecodeOpcode::ConstFloat64 => {
                let dest = self.read_register(0, width);
                let idx = self.read_const_pool_idx(1, width);
                BytecodeInstruction::ConstFloat64 { dest, idx }
            }
            BytecodeOpcode::ConstString => {
                let dest = self.read_register(0, width);
                let idx = self.read_const_pool_idx(1, width);
                BytecodeInstruction::ConstString { dest, idx }
            }

            BytecodeOpcode::TestIdentity => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestIdentity { dest, lhs, rhs }
            }

            BytecodeOpcode::TestEqBool => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestEqBool { dest, lhs, rhs }
            }
            BytecodeOpcode::TestNeBool => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestNeBool { dest, lhs, rhs }
            }
            BytecodeOpcode::TestEqUInt8 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestEqUInt8 { dest, lhs, rhs }
            }
            BytecodeOpcode::TestNeUInt8 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestNeUInt8 { dest, lhs, rhs }
            }
            BytecodeOpcode::TestGtUInt8 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestGtUInt8 { dest, lhs, rhs }
            }
            BytecodeOpcode::TestGeUInt8 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestGeUInt8 { dest, lhs, rhs }
            }
            BytecodeOpcode::TestLtUInt8 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestLtUInt8 { dest, lhs, rhs }
            }
            BytecodeOpcode::TestLeUInt8 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestLeUInt8 { dest, lhs, rhs }
            }
            BytecodeOpcode::TestEqChar => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestEqChar { dest, lhs, rhs }
            }
            BytecodeOpcode::TestNeChar => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestNeChar { dest, lhs, rhs }
            }
            BytecodeOpcode::TestGtChar => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestGtChar { dest, lhs, rhs }
            }
            BytecodeOpcode::TestGeChar => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestGeChar { dest, lhs, rhs }
            }
            BytecodeOpcode::TestLtChar => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestLtChar { dest, lhs, rhs }
            }
            BytecodeOpcode::TestLeChar => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestLeChar { dest, lhs, rhs }
            }
            BytecodeOpcode::TestEqEnum => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestEqEnum { dest, lhs, rhs }
            }
            BytecodeOpcode::TestNeEnum => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestNeEnum { dest, lhs, rhs }
            }
            BytecodeOpcode::TestEqInt64 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestEqInt64 { dest, lhs, rhs }
            }
            BytecodeOpcode::TestNeInt64 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestNeInt64 { dest, lhs, rhs }
            }
            BytecodeOpcode::TestGtInt64 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestGtInt64 { dest, lhs, rhs }
            }
            BytecodeOpcode::TestGeInt64 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestGeInt64 { dest, lhs, rhs }
            }
            BytecodeOpcode::TestLtInt64 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestLtInt64 { dest, lhs, rhs }
            }
            BytecodeOpcode::TestLeInt64 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestLeInt64 { dest, lhs, rhs }
            }
            BytecodeOpcode::TestEqInt32 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestEqInt32 { dest, lhs, rhs }
            }
            BytecodeOpcode::TestNeInt32 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestNeInt32 { dest, lhs, rhs }
            }
            BytecodeOpcode::TestGtInt32 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestGtInt32 { dest, lhs, rhs }
            }
            BytecodeOpcode::TestGeInt32 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestGeInt32 { dest, lhs, rhs }
            }
            BytecodeOpcode::TestLtInt32 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestLtInt32 { dest, lhs, rhs }
            }
            BytecodeOpcode::TestLeInt32 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestLeInt32 { dest, lhs, rhs }
            }

            BytecodeOpcode::TestEqFloat32 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestEqFloat32 { dest, lhs, rhs }
            }
            BytecodeOpcode::TestNeFloat32 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestNeFloat32 { dest, lhs, rhs }
            }
            BytecodeOpcode::TestGtFloat32 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestGtFloat32 { dest, lhs, rhs }
            }
            BytecodeOpcode::TestGeFloat32 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestGeFloat32 { dest, lhs, rhs }
            }
            BytecodeOpcode::TestLtFloat32 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestLtFloat32 { dest, lhs, rhs }
            }
            BytecodeOpcode::TestLeFloat32 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestLeFloat32 { dest, lhs, rhs }
            }

            BytecodeOpcode::TestEqFloat64 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestEqFloat64 { dest, lhs, rhs }
            }
            BytecodeOpcode::TestNeFloat64 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestNeFloat64 { dest, lhs, rhs }
            }
            BytecodeOpcode::TestGtFloat64 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestGtFloat64 { dest, lhs, rhs }
            }
            BytecodeOpcode::TestGeFloat64 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestGeFloat64 { dest, lhs, rhs }
            }
            BytecodeOpcode::TestLtFloat64 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestLtFloat64 { dest, lhs, rhs }
            }
            BytecodeOpcode::TestLeFloat64 => {
                let dest = self.read_register(0, width);
                let lhs = self.read_register(1, width);
                let rhs = self.read_register(2, width);
                BytecodeInstruction::TestLeFloat64 { dest, lhs, rhs }
            }
            BytecodeOpcode::Assert => {
                let value = self.read_register(0, width);
                BytecodeInstruction::Assert { value }
            }

            BytecodeOpcode::JumpLoop => {
                let offset = self.read_offset(0, width);
                BytecodeInstruction::JumpLoop { offset }
            }
            BytecodeOpcode::LoopStart => BytecodeInstruction::LoopStart,
            BytecodeOpcode::JumpIfFalse => {
                let opnd = self.read_register(0, width);
                let offset = self.read_offset(1, width);
                BytecodeInstruction::JumpIfFalse { opnd, offset }
            }
            BytecodeOpcode::JumpIfFalseConst => {
                let opnd = self.read_register(0, width);
                let idx = self.read_const_pool_idx(1, width);
                BytecodeInstruction::JumpIfFalseConst { opnd, idx }
            }
            BytecodeOpcode::JumpIfTrue => {
                let opnd = self.read_register(0, width);
                let offset = self.read_offset(1, width);
                BytecodeInstruction::JumpIfTrue { opnd, offset }
            }
            BytecodeOpcode::JumpIfTrueConst => {
                let opnd = self.read_register(0, width);
                let idx = self.read_const_pool_idx(1, width);
                BytecodeInstruction::JumpIfTrueConst { opnd, idx }
            }
            BytecodeOpcode::Jump => {
                let offset = self.read_offset(0, width);
                BytecodeInstruction::Jump { offset }
            }
            BytecodeOpcode::JumpConst => {
                let idx = self.read_const_pool_idx(0, width);
                BytecodeInstruction::JumpConst { idx }
            }

            BytecodeOpcode::InvokeDirectVoid => {
                let fct = self.read_const_pool_idx(0, width);
                BytecodeInstruction::InvokeDirectVoid { fct }
            }
            BytecodeOpcode::InvokeDirect => {
                let dest = self.read_register(0, width);
                let fct = self.read_const_pool_idx(1, width);
                BytecodeInstruction::InvokeDirect { dest, fct }
            }

            BytecodeOpcode::InvokeVirtualVoid => {
                let fct = self.read_const_pool_idx(0, width);
                BytecodeInstruction::InvokeVirtualVoid { fct }
            }
            BytecodeOpcode::InvokeVirtual => {
                let dest = self.read_register(0, width);
                let fct = self.read_const_pool_idx(1, width);
                BytecodeInstruction::InvokeVirtual { dest, fct }
            }

            BytecodeOpcode::InvokeStaticVoid => {
                let fct = self.read_const_pool_idx(0, width);
                BytecodeInstruction::InvokeStaticVoid { fct }
            }
            BytecodeOpcode::InvokeStatic => {
                let dest = self.read_register(0, width);
                let fct = self.read_const_pool_idx(1, width);
                BytecodeInstruction::InvokeStatic { dest, fct }
            }

            BytecodeOpcode::InvokeGenericStaticVoid => {
                let fct = self.read_const_pool_idx(0, width);
                BytecodeInstruction::InvokeGenericStaticVoid { fct }
            }
            BytecodeOpcode::InvokeGenericStatic => {
                let dest = self.read_register(0, width);
                let fct = self.read_const_pool_idx(1, width);
                BytecodeInstruction::InvokeGenericStatic { dest, fct }
            }

            BytecodeOpcode::InvokeGenericDirectVoid => {
                let fct = self.read_const_pool_idx(0, width);
                BytecodeInstruction::InvokeGenericDirectVoid { fct }
            }
            BytecodeOpcode::InvokeGenericDirect => {
                let dest = self.read_register(0, width);
                let fct = self.read_const_pool_idx(1, width);
                BytecodeInstruction::InvokeGenericDirect { dest, fct }
            }

            BytecodeOpcode::NewObject => {
                let dest = self.read_register(0, width);
                let cls = self.read_const_pool_idx(1, width);
                BytecodeInstruction::NewObject { dest, cls }
            }
            BytecodeOpcode::NewArray => {
                let dest = self.read_register(0, width);
                let cls = self.read_const_pool_idx(1, width);
                let length = self.read_register(2, width);
                BytecodeInstruction::NewArray { dest, cls, length }
            }
            BytecodeOpcode::NewTuple => {
                let dest = self.read_register(0, width);
                let tuple = self.read_tuple(1, width);
                BytecodeInstruction::NewTuple { dest, tuple }
            }
            BytecodeOpcode::NewEnum => {
                let dest = self.read_register(0, width);
                let idx = self.read_const_pool_idx(1, width);
                BytecodeInstruction::NewEnum { dest, idx }
            }
            BytecodeOpcode::NewStruct => {
                let dest = self.read_register(0, width);
                let idx = self.read_const_pool_idx(1, width);
                BytecodeInstruction::NewStruct { dest, idx }
            }
            BytecodeOpcode::NewTraitObject => {
                let dest = self.read_register(0, width);
                let idx = self.read_const_pool_idx(1, width);
                let src = self.read_register(2, width);
                BytecodeInstruction::NewTraitObject { dest, idx, src }
            }

            BytecodeOpcode::NilCheck => {
                let obj = self.read_register(0, width);
                BytecodeInstruction::NilCheck { obj }
            }

            BytecodeOpcode::ArrayLength => {
                let dest = self.read_register(0, width);
                let arr = self.read_register(1, width);
                BytecodeInstruction::ArrayLength { dest, arr }
            }
            BytecodeOpcode::ArrayBoundCheck => {
                let arr = self.read_register(0, width);
                let idx = self.read_register(1, width);
                BytecodeInstruction::ArrayBoundCheck { arr, idx }
            }

            BytecodeOpcode::LoadArray => {
                let dest = self.read_register(0, width);
                let arr = self.read_register(1, width);
                let idx = self.read_register(2, width);
                BytecodeInstruction::LoadArray { dest, arr, idx }
            }

            BytecodeOpcode::StoreArray => {
                let src = self.read_register(0, width);
                let arr = self.read_register(1, width);
                let idx = self.read_register(2, width);
                BytecodeInstruction::StoreArray { src, arr, idx }
            }

            BytecodeOpcode::RetVoid => BytecodeInstruction::RetVoid,
            BytecodeOpcode::Ret => {
                let opnd = self.read_register(0, width);
                BytecodeInstruction::Ret { opnd }
            }

            _ => unreachable!(),
        }
    }

    fn read_register(&mut self, index: usize, width: OperandWidth) -> Register {
        Register(self.read_index(index, width) as usize)
    }

    fn read_tuple(&mut self, index: usize, width: OperandWidth) -> TupleId {
        self.read_index(index, width).into()
    }

    fn read_global(&mut self, index: usize, width: OperandWidth) -> GlobalDefinitionId {
        self.read_index(index, width).into()
    }

    fn read_const_pool_idx(&mut self, index: usize, width: OperandWidth) -> ConstPoolIdx {
        (self.read_index(index, width) as usize).into()
    }

    fn read_offset(&mut self, index: usize, wide: OperandWidth) -> u32 {
        self.read_index(index, wide)
    }

    fn read_index(&mut self, index: usize, width: OperandWidth) -> u32 {
        let pos = self.current + offset_argument(index, width);
        match width {
            OperandWidth::Normal => self.read_byte_at(pos),
            OperandWidth::Wide => self.read_wide_at(pos),
        }
    }

    fn read_byte_argument(&mut self, index: usize, width: OperandWidth) -> u32 {
        let pos = self.current + offset_argument(index, width);
        self.read_byte_at(pos)
    }

    fn read_byte_at(&mut self, pos: usize) -> u32 {
        self.code[pos] as u32
    }

    fn read_wide_at(&mut self, pos: usize) -> u32 {
        let v1 = self.read_byte_at(pos);
        let v2 = self.read_byte_at(pos + 1);
        let v3 = self.read_byte_at(pos + 2);
        let v4 = self.read_byte_at(pos + 3);

        (v4 << 24) | (v3 << 16) | (v2 << 8) | v1
    }
}

impl<'a> Iterator for BytecodeIterator<'a> {
    type Item = BytecodeInstruction;

    fn next(&mut self) -> Option<BytecodeInstruction> {
        if self.current < self.code.len() {
            let (opcode, width) = read_opcode_and_width(self.code, self.current);
            let inst = self.read_instruction(width, opcode);
            self.current += opcode.size(width) as usize;
            Some(inst)
        } else {
            assert_eq!(self.current, self.code.len());
            None
        }
    }
}

struct BytecodeReader<'a, T: BytecodeVisitor> {
    iterator: BytecodeIterator<'a>,
    current: usize,
    visitor: &'a mut T,
}

impl<'a, T> BytecodeReader<'a, T>
where
    T: BytecodeVisitor,
{
    fn new(code: &'a [u8], visitor: &'a mut T) -> BytecodeReader<'a, T> {
        BytecodeReader {
            iterator: BytecodeIterator::new(code),
            current: 0,
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
            BytecodeInstruction::AddInt32 { dest, lhs, rhs } => {
                self.visitor.visit_add_int32(dest, lhs, rhs);
            }
            BytecodeInstruction::AddInt64 { dest, lhs, rhs } => {
                self.visitor.visit_add_int64(dest, lhs, rhs);
            }
            BytecodeInstruction::AddFloat32 { dest, lhs, rhs } => {
                self.visitor.visit_add_float32(dest, lhs, rhs);
            }
            BytecodeInstruction::AddFloat64 { dest, lhs, rhs } => {
                self.visitor.visit_add_float64(dest, lhs, rhs);
            }
            BytecodeInstruction::SubInt32 { dest, lhs, rhs } => {
                self.visitor.visit_sub_int32(dest, lhs, rhs);
            }
            BytecodeInstruction::SubInt64 { dest, lhs, rhs } => {
                self.visitor.visit_sub_int64(dest, lhs, rhs);
            }
            BytecodeInstruction::SubFloat32 { dest, lhs, rhs } => {
                self.visitor.visit_sub_float32(dest, lhs, rhs);
            }
            BytecodeInstruction::SubFloat64 { dest, lhs, rhs } => {
                self.visitor.visit_sub_float64(dest, lhs, rhs);
            }
            BytecodeInstruction::NegInt32 { dest, src } => {
                self.visitor.visit_neg_int32(dest, src);
            }
            BytecodeInstruction::NegInt64 { dest, src } => {
                self.visitor.visit_neg_int64(dest, src);
            }
            BytecodeInstruction::NegFloat32 { dest, src } => {
                self.visitor.visit_neg_float32(dest, src);
            }
            BytecodeInstruction::NegFloat64 { dest, src } => {
                self.visitor.visit_neg_float64(dest, src);
            }
            BytecodeInstruction::MulInt32 { dest, lhs, rhs } => {
                self.visitor.visit_mul_int32(dest, lhs, rhs);
            }
            BytecodeInstruction::MulInt64 { dest, lhs, rhs } => {
                self.visitor.visit_mul_int64(dest, lhs, rhs);
            }
            BytecodeInstruction::MulFloat32 { dest, lhs, rhs } => {
                self.visitor.visit_mul_float32(dest, lhs, rhs);
            }
            BytecodeInstruction::MulFloat64 { dest, lhs, rhs } => {
                self.visitor.visit_mul_float64(dest, lhs, rhs);
            }
            BytecodeInstruction::DivInt32 { dest, lhs, rhs } => {
                self.visitor.visit_div_int32(dest, lhs, rhs);
            }
            BytecodeInstruction::DivInt64 { dest, lhs, rhs } => {
                self.visitor.visit_div_int64(dest, lhs, rhs);
            }
            BytecodeInstruction::DivFloat32 { dest, lhs, rhs } => {
                self.visitor.visit_div_float32(dest, lhs, rhs);
            }
            BytecodeInstruction::DivFloat64 { dest, lhs, rhs } => {
                self.visitor.visit_div_float64(dest, lhs, rhs);
            }

            BytecodeInstruction::ModInt32 { dest, lhs, rhs } => {
                self.visitor.visit_mod_int32(dest, lhs, rhs);
            }
            BytecodeInstruction::ModInt64 { dest, lhs, rhs } => {
                self.visitor.visit_mod_int64(dest, lhs, rhs);
            }

            BytecodeInstruction::AndInt32 { dest, lhs, rhs } => {
                self.visitor.visit_and_int32(dest, lhs, rhs);
            }
            BytecodeInstruction::AndInt64 { dest, lhs, rhs } => {
                self.visitor.visit_and_int64(dest, lhs, rhs);
            }

            BytecodeInstruction::OrInt32 { dest, lhs, rhs } => {
                self.visitor.visit_or_int32(dest, lhs, rhs);
            }
            BytecodeInstruction::OrInt64 { dest, lhs, rhs } => {
                self.visitor.visit_or_int64(dest, lhs, rhs);
            }

            BytecodeInstruction::XorInt32 { dest, lhs, rhs } => {
                self.visitor.visit_xor_int32(dest, lhs, rhs);
            }
            BytecodeInstruction::XorInt64 { dest, lhs, rhs } => {
                self.visitor.visit_xor_int64(dest, lhs, rhs);
            }

            BytecodeInstruction::NotBool { dest, src } => {
                self.visitor.visit_not_bool(dest, src);
            }
            BytecodeInstruction::NotInt32 { dest, src } => {
                self.visitor.visit_not_int32(dest, src);
            }
            BytecodeInstruction::NotInt64 { dest, src } => {
                self.visitor.visit_not_int64(dest, src);
            }

            BytecodeInstruction::ShlInt32 { dest, lhs, rhs } => {
                self.visitor.visit_shl_int32(dest, lhs, rhs);
            }
            BytecodeInstruction::ShrInt32 { dest, lhs, rhs } => {
                self.visitor.visit_shr_int32(dest, lhs, rhs);
            }
            BytecodeInstruction::SarInt32 { dest, lhs, rhs } => {
                self.visitor.visit_sar_int32(dest, lhs, rhs);
            }

            BytecodeInstruction::ShlInt64 { dest, lhs, rhs } => {
                self.visitor.visit_shl_int64(dest, lhs, rhs);
            }
            BytecodeInstruction::ShrInt64 { dest, lhs, rhs } => {
                self.visitor.visit_shr_int64(dest, lhs, rhs);
            }
            BytecodeInstruction::SarInt64 { dest, lhs, rhs } => {
                self.visitor.visit_sar_int64(dest, lhs, rhs);
            }

            BytecodeInstruction::RorInt64 { dest, lhs, rhs } => {
                self.visitor.visit_ror_int64(dest, lhs, rhs);
            }
            BytecodeInstruction::RorInt32 { dest, lhs, rhs } => {
                self.visitor.visit_ror_int32(dest, lhs, rhs);
            }

            BytecodeInstruction::RolInt64 { dest, lhs, rhs } => {
                self.visitor.visit_rol_int64(dest, lhs, rhs);
            }
            BytecodeInstruction::RolInt32 { dest, lhs, rhs } => {
                self.visitor.visit_rol_int32(dest, lhs, rhs);
            }

            BytecodeInstruction::ExtendUInt8ToChar { dest, src } => {
                self.visitor.visit_extend_uint8_to_char(dest, src);
            }
            BytecodeInstruction::ExtendUInt8ToInt32 { dest, src } => {
                self.visitor.visit_extend_uint8_to_int32(dest, src);
            }
            BytecodeInstruction::ExtendUInt8ToInt64 { dest, src } => {
                self.visitor.visit_extend_uint8_to_int64(dest, src);
            }
            BytecodeInstruction::ExtendInt32ToInt64 { dest, src } => {
                self.visitor.visit_extend_int32_to_int64(dest, src);
            }
            BytecodeInstruction::ExtendCharToInt64 { dest, src } => {
                self.visitor.visit_extend_char_to_int64(dest, src);
            }
            BytecodeInstruction::CastCharToInt32 { dest, src } => {
                self.visitor.visit_cast_char_to_int32(dest, src);
            }
            BytecodeInstruction::CastInt32ToUInt8 { dest, src } => {
                self.visitor.visit_cast_int32_to_uint8(dest, src);
            }
            BytecodeInstruction::CastInt32ToChar { dest, src } => {
                self.visitor.visit_cast_int32_to_char(dest, src);
            }
            BytecodeInstruction::CastInt64ToUInt8 { dest, src } => {
                self.visitor.visit_cast_int64_to_uint8(dest, src);
            }
            BytecodeInstruction::CastInt64ToChar { dest, src } => {
                self.visitor.visit_cast_int64_to_char(dest, src);
            }
            BytecodeInstruction::CastInt64ToInt32 { dest, src } => {
                self.visitor.visit_cast_int64_to_int32(dest, src);
            }

            BytecodeInstruction::InstanceOf { dest, src, cls_id } => {
                self.visitor.visit_instance_of(dest, src, cls_id);
            }
            BytecodeInstruction::CheckedCast { src, cls_id } => {
                self.visitor.visit_checked_cast(src, cls_id);
            }

            BytecodeInstruction::Mov { dest, src } => {
                self.visitor.visit_mov(dest, src);
            }

            BytecodeInstruction::LoadTupleElement {
                dest,
                src,
                tuple_id,
                element,
            } => {
                self.visitor
                    .visit_load_tuple_element(dest, src, tuple_id, element);
            }

            BytecodeInstruction::LoadEnumElement {
                dest,
                src,
                idx,
                element,
            } => {
                self.visitor
                    .visit_load_enum_element(dest, src, idx, element);
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

            BytecodeInstruction::LoadGlobal { dest, glob } => {
                self.visitor.visit_load_global(dest, glob);
            }

            BytecodeInstruction::StoreGlobal { src, glob } => {
                self.visitor.visit_store_global(src, glob);
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
            BytecodeInstruction::ConstZeroUInt8 { dest } => {
                self.visitor.visit_const_zero_uint8(dest);
            }
            BytecodeInstruction::ConstZeroChar { dest } => {
                self.visitor.visit_const_zero_char(dest);
            }
            BytecodeInstruction::ConstZeroInt32 { dest } => {
                self.visitor.visit_const_zero_int32(dest);
            }
            BytecodeInstruction::ConstZeroInt64 { dest } => {
                self.visitor.visit_const_zero_int64(dest);
            }
            BytecodeInstruction::ConstZeroFloat32 { dest } => {
                self.visitor.visit_const_zero_float32(dest);
            }
            BytecodeInstruction::ConstZeroFloat64 { dest } => {
                self.visitor.visit_const_zero_float64(dest);
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

            BytecodeInstruction::TestEqBool { dest, lhs, rhs } => {
                self.visitor.visit_test_eq_bool(dest, lhs, rhs);
            }
            BytecodeInstruction::TestNeBool { dest, lhs, rhs } => {
                self.visitor.visit_test_ne_bool(dest, lhs, rhs);
            }
            BytecodeInstruction::TestEqUInt8 { dest, lhs, rhs } => {
                self.visitor.visit_test_eq_uint8(dest, lhs, rhs);
            }
            BytecodeInstruction::TestNeUInt8 { dest, lhs, rhs } => {
                self.visitor.visit_test_ne_uint8(dest, lhs, rhs);
            }
            BytecodeInstruction::TestGtUInt8 { dest, lhs, rhs } => {
                self.visitor.visit_test_gt_uint8(dest, lhs, rhs);
            }
            BytecodeInstruction::TestGeUInt8 { dest, lhs, rhs } => {
                self.visitor.visit_test_ge_uint8(dest, lhs, rhs);
            }
            BytecodeInstruction::TestLtUInt8 { dest, lhs, rhs } => {
                self.visitor.visit_test_lt_uint8(dest, lhs, rhs);
            }
            BytecodeInstruction::TestLeUInt8 { dest, lhs, rhs } => {
                self.visitor.visit_test_le_uint8(dest, lhs, rhs);
            }
            BytecodeInstruction::TestEqChar { dest, lhs, rhs } => {
                self.visitor.visit_test_eq_char(dest, lhs, rhs);
            }
            BytecodeInstruction::TestNeChar { dest, lhs, rhs } => {
                self.visitor.visit_test_ne_char(dest, lhs, rhs);
            }
            BytecodeInstruction::TestGtChar { dest, lhs, rhs } => {
                self.visitor.visit_test_gt_char(dest, lhs, rhs);
            }
            BytecodeInstruction::TestGeChar { dest, lhs, rhs } => {
                self.visitor.visit_test_ge_char(dest, lhs, rhs);
            }
            BytecodeInstruction::TestLtChar { dest, lhs, rhs } => {
                self.visitor.visit_test_lt_char(dest, lhs, rhs);
            }
            BytecodeInstruction::TestLeChar { dest, lhs, rhs } => {
                self.visitor.visit_test_le_char(dest, lhs, rhs);
            }
            BytecodeInstruction::TestEqEnum { dest, lhs, rhs } => {
                self.visitor.visit_test_eq_enum(dest, lhs, rhs);
            }
            BytecodeInstruction::TestNeEnum { dest, lhs, rhs } => {
                self.visitor.visit_test_ne_enum(dest, lhs, rhs);
            }
            BytecodeInstruction::TestEqInt64 { dest, lhs, rhs } => {
                self.visitor.visit_test_eq_int64(dest, lhs, rhs);
            }
            BytecodeInstruction::TestNeInt64 { dest, lhs, rhs } => {
                self.visitor.visit_test_ne_int64(dest, lhs, rhs);
            }
            BytecodeInstruction::TestGtInt64 { dest, lhs, rhs } => {
                self.visitor.visit_test_gt_int64(dest, lhs, rhs);
            }
            BytecodeInstruction::TestGeInt64 { dest, lhs, rhs } => {
                self.visitor.visit_test_ge_int64(dest, lhs, rhs);
            }
            BytecodeInstruction::TestLtInt64 { dest, lhs, rhs } => {
                self.visitor.visit_test_lt_int64(dest, lhs, rhs);
            }
            BytecodeInstruction::TestLeInt64 { dest, lhs, rhs } => {
                self.visitor.visit_test_le_int64(dest, lhs, rhs);
            }
            BytecodeInstruction::TestEqInt32 { dest, lhs, rhs } => {
                self.visitor.visit_test_eq_int32(dest, lhs, rhs);
            }
            BytecodeInstruction::TestNeInt32 { dest, lhs, rhs } => {
                self.visitor.visit_test_ne_int32(dest, lhs, rhs);
            }
            BytecodeInstruction::TestGtInt32 { dest, lhs, rhs } => {
                self.visitor.visit_test_gt_int32(dest, lhs, rhs);
            }
            BytecodeInstruction::TestGeInt32 { dest, lhs, rhs } => {
                self.visitor.visit_test_ge_int32(dest, lhs, rhs);
            }
            BytecodeInstruction::TestLtInt32 { dest, lhs, rhs } => {
                self.visitor.visit_test_lt_int32(dest, lhs, rhs);
            }
            BytecodeInstruction::TestLeInt32 { dest, lhs, rhs } => {
                self.visitor.visit_test_le_int32(dest, lhs, rhs);
            }

            BytecodeInstruction::TestEqFloat32 { dest, lhs, rhs } => {
                self.visitor.visit_test_eq_float32(dest, lhs, rhs);
            }
            BytecodeInstruction::TestNeFloat32 { dest, lhs, rhs } => {
                self.visitor.visit_test_ne_float32(dest, lhs, rhs);
            }
            BytecodeInstruction::TestGtFloat32 { dest, lhs, rhs } => {
                self.visitor.visit_test_gt_float32(dest, lhs, rhs);
            }
            BytecodeInstruction::TestGeFloat32 { dest, lhs, rhs } => {
                self.visitor.visit_test_ge_float32(dest, lhs, rhs);
            }
            BytecodeInstruction::TestLtFloat32 { dest, lhs, rhs } => {
                self.visitor.visit_test_lt_float32(dest, lhs, rhs);
            }
            BytecodeInstruction::TestLeFloat32 { dest, lhs, rhs } => {
                self.visitor.visit_test_le_float32(dest, lhs, rhs);
            }

            BytecodeInstruction::TestEqFloat64 { dest, lhs, rhs } => {
                self.visitor.visit_test_eq_float64(dest, lhs, rhs);
            }
            BytecodeInstruction::TestNeFloat64 { dest, lhs, rhs } => {
                self.visitor.visit_test_ne_float64(dest, lhs, rhs);
            }
            BytecodeInstruction::TestGtFloat64 { dest, lhs, rhs } => {
                self.visitor.visit_test_gt_float64(dest, lhs, rhs);
            }
            BytecodeInstruction::TestGeFloat64 { dest, lhs, rhs } => {
                self.visitor.visit_test_ge_float64(dest, lhs, rhs);
            }
            BytecodeInstruction::TestLtFloat64 { dest, lhs, rhs } => {
                self.visitor.visit_test_lt_float64(dest, lhs, rhs);
            }
            BytecodeInstruction::TestLeFloat64 { dest, lhs, rhs } => {
                self.visitor.visit_test_le_float64(dest, lhs, rhs);
            }
            BytecodeInstruction::Assert { value } => {
                self.visitor.visit_assert(value);
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

            BytecodeInstruction::InvokeDirectVoid { fct } => {
                self.visitor.visit_invoke_direct_void(fct);
            }
            BytecodeInstruction::InvokeDirect { dest, fct } => {
                self.visitor.visit_invoke_direct(dest, fct);
            }

            BytecodeInstruction::InvokeVirtualVoid { fct } => {
                self.visitor.visit_invoke_virtual_void(fct);
            }
            BytecodeInstruction::InvokeVirtual { dest, fct } => {
                self.visitor.visit_invoke_virtual(dest, fct);
            }

            BytecodeInstruction::InvokeStaticVoid { fct } => {
                self.visitor.visit_invoke_static_void(fct);
            }
            BytecodeInstruction::InvokeStatic { dest, fct } => {
                self.visitor.visit_invoke_static(dest, fct);
            }

            BytecodeInstruction::InvokeGenericStaticVoid { fct } => {
                self.visitor.visit_invoke_generic_static_void(fct);
            }
            BytecodeInstruction::InvokeGenericStatic { dest, fct } => {
                self.visitor.visit_invoke_generic_static(dest, fct);
            }

            BytecodeInstruction::InvokeGenericDirectVoid { fct } => {
                self.visitor.visit_invoke_generic_direct_void(fct);
            }
            BytecodeInstruction::InvokeGenericDirect { dest, fct } => {
                self.visitor.visit_invoke_generic_direct(dest, fct);
            }

            BytecodeInstruction::NewObject { dest, cls } => {
                self.visitor.visit_new_object(dest, cls);
            }
            BytecodeInstruction::NewArray { dest, cls, length } => {
                self.visitor.visit_new_array(dest, cls, length);
            }
            BytecodeInstruction::NewTuple { dest, tuple } => {
                self.visitor.visit_new_tuple(dest, tuple);
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

            BytecodeInstruction::NilCheck { obj } => {
                self.visitor.visit_nil_check(obj);
            }

            BytecodeInstruction::ArrayLength { dest, arr } => {
                self.visitor.visit_array_length(dest, arr);
            }
            BytecodeInstruction::ArrayBoundCheck { arr, idx } => {
                self.visitor.visit_array_bound_check(arr, idx);
            }

            BytecodeInstruction::LoadArray { dest, arr, idx } => {
                self.visitor.visit_load_array(dest, arr, idx);
            }

            BytecodeInstruction::StoreArray { src, arr, idx } => {
                self.visitor.visit_store_array(src, arr, idx);
            }

            BytecodeInstruction::RetVoid => {
                self.visitor.visit_ret_void();
            }
            BytecodeInstruction::Ret { opnd } => {
                self.visitor.visit_ret(opnd);
            }
        }
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

    fn visit_extend_uint8_to_char(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_extend_uint8_to_int32(&mut self, _dest: Register, _src: Register) {
        unimplemented!();
    }
    fn visit_extend_uint8_to_int64(&mut self, _dest: Register, _src: Register) {
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

    fn visit_mov(&mut self, _dest: Register, _src: Register) {
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

    fn visit_load_enum_element(
        &mut self,
        _dest: Register,
        _src: Register,
        _idx: ConstPoolIdx,
        _element: u32,
    ) {
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

    fn visit_load_global(&mut self, _dest: Register, _glob: GlobalDefinitionId) {
        unimplemented!();
    }

    fn visit_store_global(&mut self, _src: Register, _glob: GlobalDefinitionId) {
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
    fn visit_new_enum(&mut self, _dest: Register, _idx: ConstPoolIdx) {
        unimplemented!();
    }
    fn visit_new_struct(&mut self, _dest: Register, _idx: ConstPoolIdx) {
        unimplemented!();
    }
    fn visit_new_trait_object(&mut self, _dest: Register, _idx: ConstPoolIdx, _src: Register) {
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

    fn visit_load_array(&mut self, _dest: Register, _arr: Register, _idx: Register) {
        unimplemented!();
    }

    fn visit_store_array(&mut self, _src: Register, _arr: Register, _idx: Register) {
        unimplemented!();
    }

    fn visit_ret_void(&mut self) {
        unimplemented!();
    }
    fn visit_ret(&mut self, _opnd: Register) {
        unimplemented!();
    }
}

struct BytecodeRandomAccess<'a> {
    fct: &'a BytecodeFunction,
    instructions: Vec<usize>,
}

impl<'a> BytecodeRandomAccess<'a> {
    pub fn new(_fct: &'a BytecodeFunction) -> BytecodeRandomAccess<'a> {
        unimplemented!()
    }
}

fn find_instruction_starts(fct: &BytecodeFunction) -> Vec<usize> {
    let code = fct.code();
    let mut pos = 0;
    let mut intruction_start_offsets = Vec::new();

    while pos < fct.code().len() {
        intruction_start_offsets.push(pos);
        let (opcode, width) = read_opcode_and_width(code, pos);
        pos += opcode.size(width) as usize;
    }

    intruction_start_offsets
}

pub(super) fn read_opcode_and_width(data: &[u8], pos: usize) -> (BytecodeOpcode, OperandWidth) {
    if data[pos] == BytecodeOpcode::Wide.to_int() {
        (read_opcode(data[pos + 1]), OperandWidth::Wide)
    } else {
        (read_opcode(data[pos]), OperandWidth::Normal)
    }
}

fn read_opcode(opcode: u8) -> BytecodeOpcode {
    BytecodeOpcode::from_u8(opcode).expect("illegal opcode")
}

fn offset_argument(index: usize, width: OperandWidth) -> usize {
    offset_arguments(width) + width.size() * index
}

fn offset_arguments(width: OperandWidth) -> usize {
    if width.needs_bytecode() {
        2
    } else {
        1
    }
}
