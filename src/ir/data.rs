struct BasicBlockId(usize);

struct BasicBlock {
  id: BasicBlockId,
  instructions: Vec<Instruction>,

  predecessors: Vec<BasicBlockId>,
  successors: Vec<BasicBlockId>,
}

enum Instruction {
  InstParam(InstParamType),
  InstGoto(BasicBlockId),
  InstTest(Operand),
  InstReturn(Operand),
  InstPhi(InstPhiType),
  InstBin(InstBinType),
  InstUn(InstUnType),
  InstAssign(InstAssignType),
}

struct InstParamType {
  dest: Operand,
  idx: u32
}

struct InstAssignType {
  dest: Operand,
  src: Operand,
}

struct InstBinType {
  dest: Operand,
  left: Operand,
  right: Operand,
  op: BinOperator,
}

enum BinOperator {
  Add, Sub, Mul, Div, Mod,
  Eq, Ne, Lt, Le, Gt, Ge
}

struct InstUnType {
  dest: Operand,
  src: Operand,
  op: UnOperator,
}

enum UnOperator {
  Neg,
}

struct InstPhiType {
  var: VarId,
  dest_idx: u32,
  src_indices: Vec<u32>,
}

struct VarId(usize);
struct SSAIdx(u32);

enum Operand {
  ConstStr(String),
  ConstI32(i32),
  Var(VarId, SSAIdx)
}
