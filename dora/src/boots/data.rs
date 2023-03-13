use num_enum::IntoPrimitive;

#[derive(IntoPrimitive)]
#[repr(u8)]
pub enum InstructionSet {
    X64,
    Arm64,
}
