use masm::MacroAssembler;

pub struct BaselineAssembler {
    pub masm: MacroAssembler,
}

impl BaselineAssembler {
    pub fn new() -> BaselineAssembler {
        BaselineAssembler {
            masm: MacroAssembler::new(),
        }
    }
}