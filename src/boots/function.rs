use std::marker::PhantomData;
use std::ops::{Index, IndexMut};

use boots::cfg::ControlFlowGraph;
use boots::dfg::DataFlowGraph;

struct Function {
    next_block: usize,
    dfg: DataFlowGraph,
    cfg: ControlFlowGraph,

}

impl Function {
    fn new() -> Function {
        Function {
            next_block: 0,
            dfg: DataFlowGraph::new(),
            cfg: ControlFlowGraph::new(),
        }
    }

    fn make_block(&mut self) -> Block {
        let block = self.next_block;
        self.next_block += 1;
        Block::new(block)
    }
}

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

#[derive(PartialEq, Eq, Hash)]
pub struct Block(u32);

impl VecKey for Block {
    fn new(value: usize) -> Block {
        Block(value as u32)
    }

    fn index(&self) -> usize {
        self.0 as usize
    }
}

pub struct UserList(Vec<Inst>);

impl UserList {
    pub fn new() -> UserList {
        UserList(Vec::new())
    }
}

enum CmpOp {
    Lt,
    Le,
    Eq,
    Ne,
    Gt,
    Ge,
}

pub enum InstData {
    Add {
        ty: Type,
        lhs: Inst,
        rhs: Inst,
    },

    Sub {
        ty: Type,
        lhs: Inst,
        rhs: Inst,
    },

    Goto {
        target: Block,
    },

    If {
        opnd: Inst,
        then_block: Block,
        else_block: Block,
    },

    Cmp {
        ty: Type,
        op: CmpOp,
        lhs: Inst,
        rhs: Inst,
    },

    Ret {
        opnd: Option<Inst>,
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

enum Type {
    Bool,
    Int8,
    Int32,
    Int64,
    Float32,
    Float64,
    Reference,
}

pub struct VecMap<K, V>
where
    K: VecKey,
{
    data: Vec<V>,
    unused: PhantomData<K>,
}

impl<K, V> VecMap<K, V>
where
    K: VecKey,
{
    pub fn new() -> VecMap<K, V> {
        VecMap {
            data: Vec::new(),
            unused: PhantomData,
        }
    }

    pub fn push(&mut self, value: V) -> K {
        let idx = self.data.len();
        self.data.push(value);
        K::new(idx)
    }
}

impl<K, V> Index<K> for VecMap<K, V>
where
    K: VecKey,
{
    type Output = V;

    fn index(&self, k: K) -> &V {
        &self.data[k.index()]
    }
}

impl<K, V> IndexMut<K> for VecMap<K, V>
where
    K: VecKey,
{
    fn index_mut(&mut self, k: K) -> &mut V {
        &mut self.data[k.index()]
    }
}

pub trait VecKey {
    fn new(value: usize) -> Self;
    fn index(&self) -> usize;
}

#[cfg(test)]
mod tests {
    use super::Function;

    #[test]
    fn simple_fn() {
        let mut fct = Function::new();
        let _blk = fct.make_block();
    }
}
