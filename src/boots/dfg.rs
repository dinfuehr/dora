use boots::function::{Block, BlockData, Inst, InstData, UserList, VecMap};

pub struct DataFlowGraph {
    instructions: VecMap<Inst, InstData>,
    blocks: VecMap<Block, BlockData>,
    users: VecMap<Inst, UserList>,
}

impl DataFlowGraph {
    pub fn new() -> DataFlowGraph {
        DataFlowGraph {
            instructions: VecMap::new(),
            blocks: VecMap::new(),
            users: VecMap::new(),
        }
    }
}
