use self::Operand::*;
use self::Instr::*;

use hir::hir::*;

pub enum Instr {
    InstrNop,
    InstrAssign(InstrAssignType),
    InstrUn(InstrUnType),
    InstrBin(InstrBinType),
    InstrIf(InstrIfType),
    InstrPhi(InstrPhiType),
    InstrCall(InstrCallType),
    InstrReturn(InstrReturnType),
}

struct InstrCallType {
    dest: Option<Var>,
    fct: String,
    opnds: Vec<Operand>
}

struct InstrPhiType {
    dest: Var,
    ssa_indices: Vec<usize>
}

struct InstrAssignType {
    dest: Var,
    opnd: Operand
}

struct InstrUnType {
    dest: Var,
    op: UnOp,
    opnd: Operand,
}

enum UnOp {
    Neg,
    Not

}

struct InstrBinType {
    dest: Var,
    lhs: Operand,
    op: UnOp,
    rhs: Operand,
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

enum Operand {
    OpndVar(Var),
    OpndBool(bool),
    OpndInt32(i32),
}

pub enum Var {
    VarLocal(VarId, u32),
    VarTemp(TempId),
}

impl Var {
    pub fn local(id: VarId, ssa: u32) -> Var {
        Var::VarLocal(id, ssa)
    }

    pub fn temp(id: TempId) -> Var {
        Var::VarTemp(id)
    }
}

struct InstrIfType {
    opnd: Operand,
}

struct InstrReturnType {
    opnd: Option<Operand>
}
