use ty::BuiltinType;

// first param offset to rbp is +16,
// rbp+0 -> saved rbp
// rbp+8 -> return address
pub static PARAM_OFFSET: i32 = 16;

// on x64 each parameter needs exactly 8 bytes
pub fn next_param_offset(param_offset: i32, _: BuiltinType) -> i32 {
    param_offset + 8
}
