use crate::bytecode::BytecodeFunction;

pub(super) struct BytecodeLiveness;

impl BytecodeLiveness {
    pub(super) fn analyze(_fct: &BytecodeFunction) -> BytecodeLiveness {
        BytecodeLiveness
    }
}
