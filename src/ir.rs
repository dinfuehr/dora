struct Fct {
    blocks: Vec<Block>,
    start_block: BlockId,
    end_blocks: Vec<BlockId>,
    vars: Vec<Var>,
    params: Vec<Var>,
}

struct VarId(usize);

struct Var {
    name: String,
}

struct BlockId(usize);

struct Block {
    id: BlockId,
    instructions: Vec<Instr>,
    successors: Vec<BlockId>,
    predecessors: Vec<BlockId>,
}

enum Instr {
    InstrRet(Option<Opnd>),
    InstrTest(Opnd),
    InstrBin(Opnd, Opnd, Opnd),
    InstrUn(Opnd, Opnd),
    InstrAssign(Opnd, Opnd),
    InstrPhi(VarId, u32, Vec<u32>),
    InstrCall(String, Opnd, Vec<Opnd>),
}

enum Opnd {
    OpndReg(u32),
    OpndVar(VarId, u32),
}
