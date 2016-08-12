use ctxt::{Fct, FctId};
use interner::Name;

pub struct Mir {
    name: Name,
    blocks: Vec<BasicBlock>,
}

pub struct BasicBlockId(u32);

pub struct BasicBlock {
    id: BasicBlockId,
    stmts: Vec<Stmt>,
}

pub enum Stmt {
    Un(Opnd, UnaryOp, Opnd),
    Bin(Opnd, Opnd, BinaryOp, Opnd),
    Fct(Opnd, FctId, Vec<Opnd>),
    If(Opnd, BasicBlockId, BasicBlockId),
    Ret(Opnd),
}

pub enum Opnd {
    Reg(u32),
    Var(Name),
}

pub enum UnaryOp {
    Neg
}

pub enum BinaryOp {
    Add, Sub, Mul, Div, Mod,
}

pub fn generate(fct: &Fct) -> Mir {
    Mir {
        name: fct.name,
        blocks: Vec::new()
    }
}

#[cfg(test)]
mod tests {
    use super::generate;
    use test::parse;

    #[test]
    fn generate_empty() {
        parse("fun f() {}", |ctxt| {
            let mir = generate(ctxt.fcts.last().unwrap());
            assert_eq!("f", *ctxt.interner.str(mir.name));
        });
    }
}

