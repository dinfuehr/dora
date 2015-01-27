#[derive(PartialEq,Debug)]
pub enum UnOp {
    Neg, Not
}

#[derive(PartialEq,Debug)]
pub enum BinOp {
    Add, Sub, Mul, Div, Mod,
    Eq, NEq, LThan, LEq, GThan, GEq
}

#[derive(PartialEq,Debug)]
pub enum Expr {
    ExprUn(UnOp,Box<Expr>),
    ExprBin(BinOp,Box<Expr>,Box<Expr>),
    ExprLitInt(i64),
    ExprLitStr(String),
    ExprIdent(String),
    ExprAssign(Box<Expr>,Box<Expr>),
    ExprCall(String,Vec<Box<Expr>>)
}
