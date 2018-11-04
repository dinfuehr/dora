use dora_parser::lexer::position::Position;

use baseline::codegen::CondCode;
use baseline::expr::ExprStore;
use baseline::fct::{CatchType, Comment, GcPoint};
use cpu::{Mem, Reg};
use masm::{MacroAssembler, Label};
use ty::MachineMode;
use os::signal::Trap;

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

    pub fn prolog(&mut self, stacksize: i32) {
        self.masm.prolog(stacksize);
    }

    pub fn epilog_with_polling(&mut self, stacksize: i32, polling_page: *const u8) {
        self.masm.epilog_with_polling(stacksize, polling_page);
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

    pub fn pos(&self) -> usize {
        self.masm.pos()
    }

    pub fn throw(&mut self, receiver: Reg, pos: Position) {
        self.masm.throw(receiver, pos);
    }

    pub fn store_mem(&mut self, mode: MachineMode, mem: Mem, src: ExprStore) {
        self.masm.store_mem(mode, mem, src);
    }

    pub fn load_mem(&mut self, mode: MachineMode, dest: ExprStore, mem: Mem) {
        self.masm.load_mem(mode, dest, mem);
    }

    pub fn test_and_jump_if(&mut self, cond: CondCode, reg: Reg, lbl: Label) {
        self.masm.test_and_jump_if(cond, reg, lbl);
    }

    pub fn test_if_nil_bailout(&mut self, pos: Position, reg: Reg, trap: Trap) {
        self.masm.test_if_nil_bailout(pos, reg, trap);
    }

    pub fn load_nil(&mut self, dest: Reg) {
        self.masm.load_nil(dest);
    }

    pub fn emit_exception_handler(
        &mut self,
        span: (usize, usize),
        catch: usize,
        offset: Option<i32>,
        catch_type: CatchType,
    ) {
        self.masm.emit_exception_handler(span, catch, offset, catch_type);
    }
}