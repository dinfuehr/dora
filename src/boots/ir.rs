use std::collections::hash_map::HashMap;
use std::marker::PhantomData;
use std::ops::{Index, IndexMut};

struct Function {
    blocks: VecMap<Block, BlockData>,
    instructions: VecMap<Inst, InstData>,
    uses: HashMap<Inst, UseList>,
    users: HashMap<Inst, UserList>,
    entry_block: Option<Block>,
    exit_block: Option<Block>,
}

impl Function {
    fn new() -> Function {
        Function {
            blocks: VecMap::new(),
            instructions: VecMap::new(),
            uses: HashMap::new(),
            users: HashMap::new(),
            entry_block: None,
            exit_block: None,
        }
    }

    fn make_block(&mut self) -> Block {
        self.blocks.push(BlockData::new())
    }
}

#[derive(PartialEq, Eq, Hash)]
struct Inst(u32);

impl VecKey for Inst {
    fn new(value: usize) -> Inst {
        Inst(value as u32)
    }

    fn index(&self) -> usize {
        self.0 as usize
    }
}

#[derive(PartialEq, Eq, Hash)]
struct Block(u32);

impl VecKey for Block {
    fn new(value: usize) -> Block {
        Block(value as u32)
    }

    fn index(&self) -> usize {
        self.0 as usize
    }
}

struct UseList(Vec<Inst>);
struct UserList(Vec<Inst>);

struct BlockData {
    predecessors: Vec<Block>,
    successors: Vec<Block>,
    first_inst: Option<Inst>,
    last_inst: Option<Inst>,
}

impl BlockData {
    fn new() -> BlockData {
        BlockData {
            predecessors: Vec::new(),
            successors: Vec::new(),
            first_inst: None,
            last_inst: None,
        }
    }
}

enum CmpOp {
    Lt, Le, Eq, Ne, Gt, Ge,
}

enum InstData {
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

struct InstLoc {
    block: Option<Block>,
    prev: Option<Inst>,
    next: Option<Inst>,
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

struct VecMap<K, V> where K: VecKey {
    data: Vec<V>,
    unused: PhantomData<K>,
}

impl<K, V> VecMap<K, V> where K: VecKey {
    fn new() -> VecMap<K, V> {
        VecMap {
            data: Vec::new(),
            unused: PhantomData,
        }
    }

    fn push(&mut self, value: V) -> K {
        let idx = self.data.len();
        self.data.push(value);
        K::new(idx)
    }
}

impl<K, V> Index<K> for VecMap<K, V> where K: VecKey
{
    type Output = V;

    fn index(&self, k: K) -> &V {
        &self.data[k.index()]
    }
}

impl<K, V> IndexMut<K> for VecMap<K, V> where K: VecKey
{
    fn index_mut(&mut self, k: K) -> &mut V {
        &mut self.data[k.index()]
    }
}

trait VecKey {
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