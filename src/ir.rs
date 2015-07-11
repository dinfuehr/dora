pub enum IR {
    IRBin(IRBinType),
    IRUn(IRUnType),
    IRPhi(IRPhiType),
    IRCall(IRCallType),
}

pub struct Var(usize);
pub struct VarIdx(usize);

pub struct IRPhiType {
    pub var: Var,
    pub dest: VarIndex,
    pub opnds: Vec<VarIndex>,
}

pub struct IRBinType {
    pub dest: IROperand,
    pub s1: IROperand,
    pub op: IRBinOperator,
    pub s2: IROperand,
}

pub enum IRBinOperator {
    Add, Sub, Mul, Div, Mod
}

pub enum IROpnd {
    IROpndVReg(usize),
    IROpndInt(i64),
    IROpndVar(usize, usize)
}
