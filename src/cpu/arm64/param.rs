use ty::BuiltinType;

pub static PARAM_OFFSET: i32 = 16;

pub fn next_param_offset(param_offset: i32, _: BuiltinType) -> i32 {
    param_offset + 8
}
