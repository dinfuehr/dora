use ctxt::FctId;

pub struct JitOptFct {
    pub fct_id: FctId,
    pub fct_start: *const u8,
}

impl JitOptFct {
    pub fn fct_id(&self) -> FctId {
        self.fct_id
    }

    pub fn fct_ptr(&self) -> *const u8 {
        self.fct_start
    }
}
