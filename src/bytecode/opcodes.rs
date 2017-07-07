macro_rules! opcodes {
    ( $($v: ident),* ) => {
        opcode!((0) $($v)*);
    };
}

macro_rules! opcode {
    ( ($e: expr) $i: ident $($v: ident)* ) => {
        pub const $i: u8 = $e;
        opcode!(($e + 1) $($v)*);
    };

    ( ($e: expr) ) => { };
}

opcodes!(BC_WIDE,
         BC_XWIDE,

         BC_ADD_INT32,
         BC_ADD_INT64,
         BC_ADD_FLT32,
         BC_ADD_FLT64,

         BC_SUB_INT32,
         BC_SUB_INT64,
         BC_SUB_FLT32,
         BC_SUB_FLT64,

         BC_MUL_INT32,
         BC_MUL_INT64,
         BC_MUL_FLT32,
         BC_MUL_FLT64,

         BC_DIV_INT32,
         BC_DIV_INT64,
         BC_DIV_FLT32,
         BC_DIV_FLT64,

         BC_MOD_INT32,
         BC_MOD_INT64,
         BC_MOD_FLT32,
         BC_MOD_FLT64,

         BC_NEG_INT32,
         BC_NEG_INT64,
         BC_NEG_FLT32,
         BC_NEG_FLT64,

         BC_GET_LOCAL_INT32,
         BC_SET_LOCAL_INT32);
