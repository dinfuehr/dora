use crate::boots::function::{Block, BlockData, Value, ValueData};
use crate::boots::inst::{Inst, InstData};
use crate::boots::utils::VecMap;

pub struct DataFlowGraph {
    insts: VecMap<Inst, InstData>,
    blocks: VecMap<Block, BlockData>,
    values: VecMap<Value, ValueData>,
}

impl DataFlowGraph {
    pub fn new() -> DataFlowGraph {
        DataFlowGraph {
            insts: VecMap::new(),
            blocks: VecMap::new(),
            values: VecMap::new(),
        }
    }

    pub fn make_block(&mut self) -> Block {
        self.blocks.push(BlockData::new())
    }

    pub fn make_inst(&mut self, inst_data: InstData) -> Inst {
        self.insts.push(inst_data)
    }

    pub fn make_value(&mut self, value_data: ValueData) -> Value {
        self.values.push(value_data)
    }
}
