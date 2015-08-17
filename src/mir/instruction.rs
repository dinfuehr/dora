use self::Operand::*;
use self::Instr::*;

use super::function::*;
use super::basic_block::*;

pub enum Instr {
    InstrAssign(InstrAssignType),
    InstrUn(InstrUnType),
    InstrBin(InstrBinType),
    InstrJmp(InstrJmpType),
    InstrJmpIf(InstrJmpIfType),
    InstrPhi(InstrPhiType),
    InstrCall(InstrCallType),
    InstrReturn(Option<Operand>),
}

impl Instr {
    pub fn dump(&self, fct: &Function) {
        match *self {
            InstrAssign(ref val) => val.dump(fct),
            _ => unreachable!()
        }
    }
}

struct InstrCallType {
    dest: Option<Var>,
    fct: String,
    opnds: Vec<Operand>
}

impl InstrCallType {
    pub fn dump(&self, fct: &Function) {
        if let Some(ref var) = self.dest {
            print!("{} = ", var.to_string(fct));
        }

        print!("call(");
        let mut first = true;

        for opnd in &self.opnds {
            if !first { print!(", "); }

            print!("{}", opnd.to_string(fct));
            first = false;
        }

        println!(")");
    }
}

struct InstrPhiType {
    dest: Var,
    ssa_indices: Vec<usize>
}

impl InstrPhiType {
    pub fn dump(&self, fct: &Function) {
        print!("{} = phi(", self.dest.to_string(fct));
        let mut first = true;

        for ssa in &self.ssa_indices {
            if !first { print!(", "); }

            print!("{}", Var::new(self.dest.id, *ssa).to_string(fct));
        }

        println!(")");
    }
}

struct InstrAssignType {
    dest: Var,
    opnd: Operand
}

impl InstrAssignType {
    pub fn dump(&self, fct: &Function) {
        println!("{} = {}", self.dest.to_string(fct),
            self.opnd.to_string(fct));
    }
}

struct InstrUnType {
    dest: Var,
    op: UnOp,
    opnd: Operand,
}

impl InstrUnType {
    pub fn dump(&self, fct: &Function) {
        println!("{} = {} {}", self.dest.to_string(fct),
            self.op.to_string(), self.opnd.to_string(fct));
    }
}

enum UnOp {
    Neg,
    Not

}

impl UnOp {
    pub fn to_string(&self) -> &'static str {
        match *self {
            UnOp::Neg => "-",
            UnOp::Not => "!",
        }
    }
}

struct InstrBinType {
    dest: Var,
    lhs: Operand,
    op: UnOp,
    rhs: Operand,
}

impl InstrBinType {
    pub fn dump(&self, fct: &Function) {
        println!("{} = {} {} {}", self.dest.to_string(fct),
            self.lhs.to_string(fct), self.op.to_string(),
            self.rhs.to_string(fct));
    }
}

enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    And,
    Or,

    Eq,
    Ne,
    Gt,
    Ge,
    Lt,
    Le
}

impl BinOp {
    pub fn to_string(&self) -> &'static str {
        match *self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Mod => "%",

            BinOp::And => "&",
            BinOp::Or => "|",

            BinOp::Eq => "==",
            BinOp::Ne => "!=",
            BinOp::Gt => ">",
            BinOp::Ge => ">=",
            BinOp::Lt => "<",
            BinOp::Le => "<="
        }
    }
}

enum Operand {
    OpndVar(Var),
    OpndBool(bool),
    OpndInt32(i32),
}

impl Operand {
    pub fn to_string(&self, fct: &Function) -> String {
        match *self {
            OpndVar(ref var) => var.to_string(fct),
            OpndBool(val) => format!("{}:bool", val),
            OpndInt32(val) => format!("{}:i32", val)
        }
    }
}

pub struct Var {
    id: LocalVarId,
    ssa: usize
}

impl Var {
    pub fn new(id: LocalVarId, ssa: usize) -> Var {
        Var {
            id: id,
            ssa: ssa
        }
    }

    pub fn to_string(&self, fct: &Function) -> String {
        let var = fct.local_var(self.id);

        format!("{}@{}:{}", var.name(), self.ssa, var.ty().to_string())
    }
}

struct InstrJmpIfType {
    opnd: Operand,
    then_block: BasicBlockId,
    else_block: BasicBlockId,
}

impl InstrJmpIfType {
    pub fn dump(&self, fct: &Function) {
        println!("jmp if {} then #{} else #{}", self.opnd.to_string(fct),
            self.then_block, self.else_block);
    }
}

struct InstrJmpType {
    block: BasicBlockId
}

impl InstrJmpType {
    pub fn dump(&self) {
        println!("jmp #{}", self.block);
    }
}
