pub mod gen;
pub mod dump;

use std::cell::RefCell;
use std::rc::Rc;
use std::slice::{Iter, IterMut};
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use ast::{BinOp, UnOp};
use interner::Name;
use sym::BuiltinType;

pub struct Mir {
    blocks: Vec<Rc<RefCell<Block>>>,
    vars: Vec<VarDecl>,
    temps: Vec<TempDecl>,
}

impl Debug for Mir {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "mir.fct")
    }
}

impl Mir {
    pub fn new() -> Mir {
        Mir {
            blocks: Vec::new(),
            vars: Vec::new(),
            temps: Vec::new()
        }
    }

    pub fn block(&self, id: BlockId) -> Rc<RefCell<Block>> {
        self.blocks[id.0].clone()
    }

    pub fn var_mut(&mut self, id: VarId) -> &mut VarDecl {
        &mut self.vars[id.0]
    }

    pub fn var(&self, id: VarId) -> &VarDecl {
        &self.vars[id.0]
    }

    pub fn add_block(&mut self) -> BlockId {
        let id = BlockId(self.blocks.len());
        self.blocks.push(Rc::new(RefCell::new(Block::new(id))));

        id
    }

    pub fn add_var(&mut self, name: Name, ty: BuiltinType) -> VarId {
        let id = VarId(self.vars.len());
        self.vars.push(VarDecl {
            name: name,
            ty: ty
        });

        id
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct VarId(usize);

impl ToString for VarId {
    fn to_string(&self) -> String {
        format!("{}", self.0)
    }
}

pub struct VarDecl {
    name: Name,
    ty: BuiltinType,
}

pub struct TempDecl {
    ty: BuiltinType
}

pub enum Lvalue {
    Var(u32),
    Temp(u32)
}

pub enum Rvalue {
    Use(Operand),
    Un(UnOp, Operand),
    Bin(BinOp, Operand, Operand),
}

pub enum Operand {
    Consume(Lvalue),
    Const(Const)
}

pub enum Const {
    Str(String),
    Bool(bool),
    Int(u32)
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct BlockId(usize);

impl ToString for BlockId {
    fn to_string(&self) -> String {
        format!("{}", self.0)
    }
}

pub struct Block {
    id: BlockId,
    instructions: Vec<Instr>,
    predecessors: Vec<BlockId>,
}

impl Block {
    fn new(id: BlockId) -> Block {
        Block {
            id: id,
            instructions: Vec::new(),
            predecessors: Vec::new()
        }
    }

    fn add_instr(&mut self, instr: Instr) {
        self.instructions.push(instr);
    }

    fn last_instr(&self) -> Option<&Instr> {
        self.instructions.last()
    }

    fn add_predecessor(&mut self, id: BlockId) {
        self.predecessors.push(id);
    }
}

pub enum Terminator {
    Ret,

    If {
        cond: Operand,
        targets: (BlockId, BlockId)
    },

    Goto {
        target: BlockId
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum Instr {
    InstrRet(Option<Opnd>),
    InstrTest(InstrTestType),
    InstrBin(InstrBinType),
    InstrUn(InstrUnType),
    InstrAssign(InstrAssignType),
    InstrCall(InstrCallType),
    InstrStr(InstrStrType),
    InstrGoto(BlockId),
}

impl Instr {
    fn ret() -> Instr {
        Instr::InstrRet(None)
    }

    fn ret_value(opnd: Opnd) -> Instr {
        Instr::InstrRet(Some(opnd))
    }

    fn test(opnd: Opnd, true_block: BlockId, false_block: BlockId) -> Instr {
        Instr::InstrTest(InstrTestType {
            opnd: opnd,
            true_block: true_block,
            false_block: false_block,
        })
    }

    fn bin(dest: Opnd, lhs: Opnd, op: BinOp, rhs: Opnd) -> Instr {
        Instr::InstrBin(InstrBinType {
            dest: dest,
            lhs: lhs,
            op: op,
            rhs: rhs,
        })
    }

    fn un(dest: Opnd, op: UnOp, src: Opnd) -> Instr {
        Instr::InstrUn(InstrUnType {
            dest: dest,
            op: op,
            src: src,
        })
    }

    fn assign(dest: Opnd, src: Opnd) -> Instr {
        Instr::InstrAssign(InstrAssignType {
            dest: dest,
            src: src,
        })
    }

    fn call(dest: Opnd, name: String, args: Vec<Opnd>) -> Instr {
        Instr::InstrCall(InstrCallType {
            dest: dest,
            name: name,
            args: args,
        })
    }

    fn str(dest: Opnd, value: String) -> Instr {
        Instr::InstrStr(InstrStrType {
            dest: dest,
            value: value,
        })
    }

    fn goto(target: BlockId) -> Instr {
        Instr::InstrGoto(target)
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct InstrTestType {
    pub opnd: Opnd,
    pub true_block: BlockId,
    pub false_block: BlockId,
}

#[derive(PartialEq, Eq, Debug)]
pub struct InstrBinType {
    pub dest: Opnd,
    pub lhs: Opnd,
    pub op: BinOp,
    pub rhs: Opnd,
}

#[derive(PartialEq, Eq, Debug)]
pub struct InstrUnType {
    pub dest: Opnd,
    pub op: UnOp,
    pub src: Opnd,
}

#[derive(PartialEq, Eq, Debug)]
pub struct InstrAssignType {
    pub dest: Opnd,
    pub src: Opnd,
}

#[derive(PartialEq, Eq, Debug)]
pub struct InstrCallType {
    pub dest: Opnd,
    pub name: String,
    pub args: Vec<Opnd>,
}

#[derive(PartialEq, Eq, Debug)]
pub struct InstrStrType {
    pub dest: Opnd,
    pub value: String,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Opnd {
    OpndReg(u32),
    OpndVar(VarId),
    OpndInt(i32),
    OpndBool(bool),
}
