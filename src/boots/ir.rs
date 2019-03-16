use std::collections::hash_map::HashMap;

struct Function {
    blocks: HashMap<Block, BlockData>,
    instructions: HashMap<Inst, InstData>,
    uses: HashMap<Inst, UseList>,
    params: Vec<Type>,
    entry_block: Option<Block>,
    exit_block: Option<Block>,
}

struct Inst(u32);
struct Block(u32);
struct Param(u32);

struct UseList(Vec<Inst>);

struct BlockData {
    predecessors: Vec<Block>,
    successors: Vec<Block>,
    first_inst: Option<Inst>,
    last_inst: Option<Inst>,
}

enum CmpOp {
    Lt, Le, Eq, Ne, Gt, Ge,
}

enum InstData {
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

    Deleted,
}

enum Value {
    Inst(Inst),
    Param(Param),
}

struct InstLoc {
    block: Option<Block>,
    prev: Option<Inst>,
    next: Option<Inst>,
}

enum Type {
    Int8,
    Int32,
    Int64,
    Float32,
    Float64,
    Reference,
}