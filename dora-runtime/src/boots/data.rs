use num_enum::{IntoPrimitive, TryFromPrimitive};

#[derive(IntoPrimitive)]
#[repr(u8)]
pub enum InstructionSet {
    X64,
    Arm64,
}

#[derive(TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub enum LazyCompilationSiteKind {
    Direct,
    Virtual,
    Lambda,
}
