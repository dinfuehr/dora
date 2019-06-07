use crate::boots::function::{Block, Type, Value};
use crate::boots::utils::VecKey;

#[derive(PartialEq, Eq, Hash)]
pub struct Inst(u32);

impl VecKey for Inst {
    fn new(value: usize) -> Inst {
        Inst(value as u32)
    }

    fn index(&self) -> usize {
        self.0 as usize
    }
}

pub enum InstData {
    Add {
        ty: Type,
        lhs: Value,
        rhs: Value,
    },

    Sub {
        ty: Type,
        lhs: Value,
        rhs: Value,
    },

    Goto {
        target: Block,
    },

    If {
        opnd: Value,
        then_block: Block,
        else_block: Block,
    },

    Cmp {
        ty: Type,
        op: CmpOp,
        lhs: Value,
        rhs: Value,
    },

    Ret {
        opnd: Option<Value>,
    },

    TrueConst,
    FalseConst,
    NilConst,
    Int8Const(u8),
    Int32Const(i32),
    Int64Const(i64),
    Float32Const(f32),
    Float64Const(f64),

    Param {
        ty: Type,
        idx: u32,
    },

    Deleted,
}

pub enum CmpOp {
    Lt,
    Le,
    Eq,
    Ne,
    Gt,
    Ge,
}
