use ctxt::FctId;
use dora_parser::interner::Name;

pub struct Mir {
    pub name: Name,
    pub blocks: Vec<BasicBlock>,
}

impl Mir {
    pub fn new(name: Name) -> Mir {
        let entry = BasicBlock::new(BasicBlockId(0));
        let exit = BasicBlock::new(BasicBlockId(1));

        Mir {
            name: name,
            blocks: vec![entry, exit],
        }
    }

    pub fn block_mut(&mut self, id: BasicBlockId) -> &mut BasicBlock {
        &mut self.blocks[id.0 as usize]
    }

    pub fn block(&self, id: BasicBlockId) -> &BasicBlock {
        &self.blocks[id.0 as usize]
    }

    pub fn add_block(&mut self) -> BasicBlockId {
        let id = BasicBlockId(self.blocks.len() as u32);
        self.blocks.push(BasicBlock::new(id));

        id
    }
}

#[derive(Copy, Clone)]
pub struct BasicBlockId(pub u32);

pub struct BasicBlock {
    pub id: BasicBlockId,
    pub stmts: Vec<Stmt>,
}

impl BasicBlock {
    pub fn new(id: BasicBlockId) -> BasicBlock {
        BasicBlock {
            id: id,
            stmts: Vec::new(),
        }
    }

    pub fn add_ret(&mut self) {
        self.stmts.push(Stmt::Ret(None));
    }
}

pub enum Stmt {
    Un(Opnd, UnaryOp, Opnd),
    Bin(Opnd, Opnd, BinaryOp, Opnd),
    Fct(Opnd, FctId, Vec<Opnd>),
    If(Opnd, BasicBlockId, BasicBlockId),
    Goto(BasicBlockId),
    Ret(Option<Opnd>),
}

pub enum Opnd {
    Reg(u32),
    Var(Name),
}

pub enum UnaryOp {
    Neg,
}

pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}
