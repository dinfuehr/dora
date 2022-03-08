pub struct Graph {
    block_entry: Option<BlockId>,
    block_exit: Option<BlockId>,
    blocks: Vec<Block>,
    instructions: Vec<Inst>,

    next_block_id: usize,
    next_inst_id: usize,
}

impl Graph {
    fn new() -> Graph {
        Graph {
            block_entry: None,
            block_exit: None,
            blocks: Vec::new(),
            instructions: Vec::new(),
            next_block_id: 0,
            next_inst_id: 0,
        }
    }

    fn block(&self, id: BlockId) -> &Block {
        &self.blocks[id.0]
    }

    fn block_mut(&mut self, id: BlockId) -> &mut Block {
        &mut self.blocks[id.0]
    }

    fn create_block(&mut self) -> BlockId {
        let block_id = BlockId(self.blocks.len());
        self.blocks.push(Block {
            id: block_id,
            predecessors: Vec::new(),
            instructions_head: None,
            instructions_tail: None,
        });
        block_id
    }

    fn create_inst(&mut self) -> InstId {
        let inst_id = InstId(self.instructions.len());
        self.instructions.push(Inst {
            id: inst_id,
            previous_inst: None,
            next_inst: None,
            inputs: Vec::new(),
        });
        inst_id
    }
}

#[derive(Copy, Clone)]
pub struct BlockId(usize);

pub struct Block {
    id: BlockId,
    predecessors: Vec<BlockId>,
    instructions_head: Option<InstId>,
    instructions_tail: Option<InstId>,
}

#[derive(Copy, Clone)]
pub struct InstId(usize);

pub struct Inst {
    id: InstId,
    previous_inst: Option<InstId>,
    next_inst: Option<InstId>,
    inputs: Vec<InstId>,
}
