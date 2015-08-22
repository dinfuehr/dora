use hir::graph::Graph;
use hir::instruction::Instr;
use hir::ty::Type;

pub struct VarId(pub usize);
pub struct TempId(pub usize);

pub struct HIR {
    graph: Graph<(), Instr>,

    // arguments and local variables
    vars: Vec<VarDecl>,

    // temporary variables, only assigned once
    temps: Vec<TempDecl>,
}

pub struct VarDecl {
    ty: Type
}

pub struct TempDecl {
    ty: Type
}
