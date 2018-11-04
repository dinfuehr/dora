use baseline::codegen::CondCode;
use baseline::fct::{Comment, GcPoint};
use cpu::Reg;
use masm::{MacroAssembler, Label};
use ty::MachineMode;

pub struct BaselineAssembler {
    pub masm: MacroAssembler,
}

impl BaselineAssembler {
    pub fn new() -> BaselineAssembler {
        BaselineAssembler {
            masm: MacroAssembler::new(),
        }
    }

    pub fn debug(&mut self) {
        self.masm.debug();
    }

    pub fn emit_comment(&mut self, comment: Comment) {
        self.masm.emit_comment(comment);
    }

    pub fn copy_reg(&mut self, mode: MachineMode, dest: Reg, src: Reg) {
        self.masm.copy_reg(mode, dest, src);
    }

    pub fn check_polling_page(&mut self, page: *const u8) {
        self.masm.check_polling_page(page);
    }

    pub fn emit_gcpoint(&mut self, gcpoint: GcPoint) {
        self.masm.emit_gcpoint(gcpoint);
    }

    pub fn bind_label(&mut self, label: Label) {
        self.masm.bind_label(label);
    }

    pub fn create_label(&mut self) -> Label {
        self.masm.create_label()
    }

    pub fn jump(&mut self, label: Label) {
        self.masm.jump(label);
    }

    pub fn jump_if(&mut self, cond: CondCode, label: Label) {
        self.masm.jump_if(cond, label);
    }
}