pub struct Function {
    block_entry: Option<BlockId>,
    block_exit: Option<BlockId>,
    blocks: Vec<Block>,
    instructions: Vec<Inst>,

    next_block_id: usize,
    next_inst_id: usize,
}

pub struct BlockId(usize);

pub struct Block {
    id: BlockId,
    predecessors: Vec<BlockId>,
    instructions_head: Option<InstId>,
    instructions_tail: Option<InstId>,
}

pub struct InstId(usize);

pub struct Inst {
    id: InstId,
    previous_inst: InstId,
    next_inst: InstId,
    inputs: Vec<InstId>,
}
