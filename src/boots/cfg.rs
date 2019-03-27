use std::collections::hash_map::HashMap;

use boots::function::{Block, Inst};

pub struct ControlFlowGraph {
    entry_block: Option<Block>,
    exit_block: Option<Block>,
    blocks: HashMap<Block, BlockNode>,
    instructions: HashMap<Inst, InstNode>,
}

impl ControlFlowGraph {
    pub fn new() -> ControlFlowGraph {
        ControlFlowGraph {
            entry_block: None,
            exit_block: None,
            blocks: HashMap::new(),
            instructions: HashMap::new(),
        }
    }
}

struct BlockNode {
    predecessors: Vec<Block>,
    successors: Vec<Block>,
    first_inst: Option<Inst>,
    last_inst: Option<Inst>,
}

impl BlockNode {
    fn new() -> BlockNode {
        BlockNode {
            predecessors: Vec::new(),
            successors: Vec::new(),
            first_inst: None,
            last_inst: None,
        }
    }
}

struct InstNode {
    prev_inst: Option<Inst>,
    next_inst: Option<Inst>,
    block: Option<Block>,
}

impl InstNode {
    fn new() -> InstNode {
        InstNode {
            prev_inst: None,
            next_inst: None,
            block: None,
        }
    }
}
