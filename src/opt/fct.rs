use crate::gc::Address;
use crate::vm::FctId;

pub struct JitOptFct {
    pub fct_id: FctId,
    pub fct_start: Address,
}

impl JitOptFct {
    pub fn fct_id(&self) -> FctId {
        self.fct_id
    }

    pub fn fct_ptr(&self) -> Address {
        self.fct_start
    }
}
