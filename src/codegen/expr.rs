use codegen::x64::reg::Reg;

pub enum ExprLoc {
    LocConst(i32),
    LocReg(Reg),
    LocStack(i32),
}
