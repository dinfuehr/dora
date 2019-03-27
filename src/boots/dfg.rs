use boots::function::{Inst, InstData, UserList, VecMap};

pub struct DataFlowGraph {
    instructions: VecMap<Inst, InstData>,
    users: VecMap<Inst, UserList>,
}

impl DataFlowGraph {
    pub fn new() -> DataFlowGraph {
        DataFlowGraph {
            instructions: VecMap::new(),
            users: VecMap::new(),
        }
    }
}