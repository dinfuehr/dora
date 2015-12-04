pub mod builder;

pub struct Fct {
    blocks: Vec<Block>,
    start_block: BlockId,
    end_blocks: Vec<BlockId>,
    vars: Vec<Var>,
    params: Vec<Var>,
}

pub struct VarId(usize);

pub struct Var {
    id: VarId,
    name: String,
}

pub struct BlockId(usize);

pub struct Block {
    id: BlockId,
    instructions: Vec<Instr>,
    successors: Vec<BlockId>,
    predecessors: Vec<BlockId>,
}

pub enum Instr {
    InstrRet(Option<Opnd>),
    InstrTest(Opnd),
    InstrBin(Opnd, Opnd, Opnd),
    InstrUn(Opnd, Opnd),
    InstrAssign(Opnd, Opnd),
    InstrPhi(VarId, u32, Vec<u32>),
    InstrCall(String, Opnd, Vec<Opnd>),
}

pub enum Opnd {
    OpndReg(u32),
    OpndVar(VarId, u32),
}
