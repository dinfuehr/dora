use boots::cfg::ControlFlowGraph;
use boots::dfg::DataFlowGraph;
use boots::inst::Inst;
use boots::utils::VecKey;

struct Function {
    dfg: DataFlowGraph,
    cfg: ControlFlowGraph,
}

impl Function {
    fn new() -> Function {
        Function {
            dfg: DataFlowGraph::new(),
            cfg: ControlFlowGraph::new(),
        }
    }

    fn make_block(&mut self) -> Block {
        self.dfg.make_block()
    }
}

#[derive(PartialEq, Eq, Hash)]
pub struct Block(u32);

impl VecKey for Block {
    fn new(value: usize) -> Block {
        Block(value as u32)
    }

    fn index(&self) -> usize {
        self.0 as usize
    }
}

pub struct BlockData {
    params: Vec<Type>,
}

impl BlockData {
    pub fn new() -> BlockData {
        BlockData { params: Vec::new() }
    }
}

#[derive(PartialEq, Eq, Hash)]
pub struct Value(u32);

impl VecKey for Value {
    fn new(value: usize) -> Value {
        Value(value as u32)
    }

    fn index(&self) -> usize {
        self.0 as usize
    }
}

pub enum ValueData {
    Inst { ty: Type, inst: Inst },
    Arg { ty: Type, block: Block, arg: u32 },
}

pub enum Type {
    Bool,
    Byte,
    Char,
    Int,
    Long,
    Float,
    Double,
    Ptr,
}

#[cfg(test)]
mod tests {
    use super::Function;

    #[test]
    fn simple_fn() {
        let mut fct = Function::new();
        let _b = fct.make_block();
    }
}
