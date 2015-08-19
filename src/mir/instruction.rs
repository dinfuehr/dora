use self::Operand::*;
use self::Instr::*;

pub struct LocalVarId(pub usize);

pub enum Instr {
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
}

struct InstrIfType {
    opnd: Operand,
}

struct InstrReturnType {
    opnd: Option<Operand>
}
