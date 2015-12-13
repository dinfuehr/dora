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

pub struct Fct {
    blocks: Vec<Rc<RefCell<Block>>>,
    start_id: BlockId,
    end_ids: Vec<BlockId>,
    vars: Vec<Var>,
}


impl Debug for Fct {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "fct")
    }
}

impl Fct {
    pub fn new() -> Fct {
        Fct {
            blocks: Vec::new(),
            start_id: BlockId(0),
            end_ids: Vec::new(),
            vars: Vec::new(),
        }
    }

    pub fn block(&self, id: BlockId) -> Rc<RefCell<Block>> {
        self.blocks[id.0].clone()
    }

    pub fn increase_var(&mut self, id: VarId) -> u32 {
        let var = self.var_mut(id);
        var.next_ssa += 1;

        var.next_ssa
    }

    pub fn var_mut(&mut self, id: VarId) -> &mut Var {
        &mut self.vars[id.0]
    }

    pub fn var(&self, id: VarId) -> &Var {
        &self.vars[id.0]
    }

    pub fn add_block(&mut self) -> BlockId {
        let id = BlockId(self.blocks.len());
        self.blocks.push(Rc::new(RefCell::new(Block::new(id))));

        id
    }

    pub fn add_var(&mut self, name: Name, data_type: BuiltinType) -> VarId {
        let id = VarId(self.vars.len());
        self.vars.push(Var {
            id: id,
            name: name,
            data_type: data_type,
            cur_ssa: 0,
            next_ssa: 0
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

pub struct Var {
    id: VarId,
    name: Name,
    data_type: BuiltinType,
    cur_ssa: u32,
    next_ssa: u32,
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

    fn find_phi_mut(&mut self, id: VarId) -> Option<&mut InstrPhiType> {
        for instr in &mut self.instructions {
            if let Instr::InstrPhi(ref mut phi) = *instr {
                if phi.var_id == id {
                    return Some(phi);
                }

            } else {
                return None;
            }
        }

        None
    }

    fn add_phi(&mut self, phi: InstrPhiType) {
        let mut ind : usize = 0;

        for instr in &self.instructions {
            if let Instr::InstrPhi(ref instr_phi) = *instr {
                if instr_phi.var_id == phi.var_id {
                    panic!("phi-instruction for var {} already exists.",
                        phi.var_id.to_string());
                }

                ind += 1;
            } else {
                break;
            }
        }

        self.instructions.insert(ind, Instr::InstrPhi(phi));
    }

    fn phi_iter(&self) -> PhiIter {
        PhiIter {
            iter: self.instructions.iter()
        }
    }

    fn phi_iter_mut(&mut self) -> PhiIterMut {
        PhiIterMut {
            iter: self.instructions.iter_mut()
        }
    }
}

struct PhiIter<'a> {
    iter: Iter<'a, Instr>
}

impl<'a> Iterator for PhiIter<'a> {
    type Item = &'a InstrPhiType;

    fn next(&mut self) -> Option<&'a InstrPhiType> {
        if let Some(instr) = self.iter.next() {
            if let Instr::InstrPhi(ref phi) = *instr {
                Some(phi)
            } else {
                None
            }
        } else {
            None
        }
    }
}

struct PhiIterMut<'a> {
    iter: IterMut<'a, Instr>
}

impl<'a> Iterator for PhiIterMut<'a> {
    type Item = &'a mut InstrPhiType;

    fn next(&mut self) -> Option<&'a mut InstrPhiType> {
        if let Some(instr) = self.iter.next() {
            if let Instr::InstrPhi(ref mut phi) = *instr {
                Some(phi)
            } else {
                None
            }
        } else {
            None
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum Instr {
    InstrRet(Option<Opnd>),
    InstrTest(InstrTestType),
    InstrBin(InstrBinType),
    InstrUn(InstrUnType),
    InstrAssign(InstrAssignType),
    InstrPhi(InstrPhiType),
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

    fn phi(var_id: VarId, dest: u32, opnds: Vec<u32>, backup: u32) -> Instr {
        Instr::InstrPhi(InstrPhiType {
            var_id: var_id,
            dest: dest,
            opnds: opnds,
            backup: backup,
        })
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
pub struct InstrPhiType {
    pub var_id: VarId,
    pub dest: u32,
    pub opnds: Vec<u32>,
    pub backup: u32,
}

#[derive(PartialEq, Eq, Debug)]
pub struct InstrStrType {
    pub dest: Opnd,
    pub value: String,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Opnd {
    OpndReg(u32),
    OpndVar(VarId, u32),
    OpndInt(i32),
    OpndBool(bool),
}

#[cfg(test)]
mod tests {
    use interner::Name;
    use mir::*;
    use mir::Instr::*;
    use sym::BuiltinType;

    #[test]
    fn find_phi_fails() {
        let mut block = Block::new(BlockId(0));
        assert_eq!(None, block.find_phi_mut(VarId(0)));
    }

    #[test]
    fn find_phi_succeeds() {
        let mut block = Block::new(BlockId(0));
        let phi = Instr::phi(VarId(0), 0, Vec::new(), 0);
        block.add_instr(phi);

        assert!(block.find_phi_mut(VarId(0)).is_some());
        assert_eq!(None, block.find_phi_mut(VarId(1)));
    }

    #[test]
    fn increase_var() {
        let mut fct = Fct::new();
        let var_id = fct.add_var(Name(0), BuiltinType::Int);

        assert_eq!(1, fct.increase_var(var_id));
    }

    #[test]
    #[should_panic]
    fn add_phi_panics() {
        let mut block = Block::new(BlockId(0));
        block.add_phi(phi(0));
        block.add_phi(phi(0));
    }

    #[test]
    fn add_phi() {
        let mut block = Block::new(BlockId(0));

        block.add_instr(Instr::ret());
        block.add_phi(phi(0));
        block.add_phi(phi(1));
        block.add_phi(phi(2));

        let instrs = &block.instructions;

        assert_eq!(phi_instr(0), instrs[0]);
        assert_eq!(phi_instr(1), instrs[1]);
        assert_eq!(phi_instr(2), instrs[2]);
        assert_eq!(Instr::ret(), instrs[3]);
    }

    #[test]
    fn phi_iterator_empty() {
        let mut block = Block::new(BlockId(0));
        assert_eq!(None, block.phi_iter().next());
    }

    #[test]
    fn phi_iterator_single() {
        let mut block = Block::new(BlockId(0));
        block.add_phi(phi(3));
        block.add_instr(Instr::ret());

        let mut iter = block.phi_iter();
        assert_eq!(Some(&phi(3)), iter.next());
        assert_eq!(None, iter.next());
    }

    fn phi_instr(var: usize) -> Instr {
        InstrPhi(phi(var))
    }

    fn phi(var: usize) -> InstrPhiType {
        InstrPhiType {
            var_id: VarId(var),
            dest: 0,
            opnds: Vec::new(),
            backup: 0
        }
    }
}
