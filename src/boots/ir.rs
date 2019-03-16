use std::collections::hash_map::HashMap;

struct Function {
    blocks: HashMap<Block, BlockData>,
    instructions: HashMap<Inst, InstData>,
    uses: HashMap<Inst, UseList>,
    entry_block: Option<Block>,
    exit_block: Option<Block>,
}

impl Function {
    fn new() -> Function {
        Function {
            blocks: HashMap::new(),
            instructions: HashMap::new(),
            uses: HashMap::new(),
            entry_block: None,
            exit_block: None,
        }
    }
}

#[derive(PartialEq, Eq, Hash)]
struct Inst(u32);

#[derive(PartialEq, Eq, Hash)]
struct Block(u32);

struct UseList(Vec<Inst>);

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

#[cfg(test)]
mod tests {
    use super::Function;

    #[test]
    fn simple_fn() {
        let _fct = Function::new();
    }
}