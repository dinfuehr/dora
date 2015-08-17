use std::fmt;

use mir::instruction::*;
use mir::function::*;

#[derive(Copy, Clone)]
pub struct BasicBlockId(pub usize);

impl fmt::Display for BasicBlockId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#{}", self.0)
    }
}

impl fmt::Debug for BasicBlockId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#{}", self.0)
    }
}

pub struct BasicBlock {
    id: BasicBlockId,
    predecessors: Vec<BasicBlockId>,
    successors: Vec<BasicBlockId>,
    instructions: Vec<Instr>
}

impl BasicBlock {
    pub fn new(id: BasicBlockId) -> BasicBlock {
        BasicBlock {
            id: id,
            predecessors: Vec::new(),
            successors: Vec::new(),
            instructions: Vec::new()
        }
    }

    pub fn dump(&self, fct: &Function) {
        println!("basic block #{}", self.id);
        println!("- predecessors: {:?}", self.predecessors);
        println!("- successors: {:?}", self.successors);
        println!("- {} instructions", self.instructions.len());
        println!("");

        let mut ind : usize = 0;

        for instr in &self.instructions {
            print!("{}: ", ind+1);
            instr.dump(fct);

            ind += 1;
        }
    }
}
