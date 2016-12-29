use std::cell::Cell;
use std::ops::Deref;
use std::rc::Rc;

use baseline::fct::{Bailouts, BailoutInfo, CatchType, Comments, Comment, ExHandler, JitFct,
                    LineNumberTable, GcPoints, GcPoint};
use baseline::codegen::CondCode;
use byteorder::{LittleEndian, WriteBytesExt};
use cpu::{Reg, SCRATCH};
use ctxt::FctId;
use dseg::DSeg;
use lexer::position::Position;
use os::signal::Trap;
use ty::MachineMode;

#[cfg(target_arch = "x86_64")]
pub use self::x64::*;

#[cfg(target_arch = "x86_64")]
pub mod x64;

#[cfg(target_arch = "aarch64")]
pub use self::arm64::*;

#[cfg(target_arch = "aarch64")]
pub mod arm64;

pub struct MacroAssembler {
    data: Vec<u8>,
    labels: Vec<Option<usize>>,
    jumps: Vec<ForwardJump>,
    bailouts: Vec<(Label, Trap, Position)>,
    bailout_infos: Bailouts,
    dseg: DSeg,
    gcpoints: GcPoints,
    comments: Comments,
    linenos: LineNumberTable,
    exception_handlers: Vec<ExHandler>,
    scratch_registers: ScratchRegisters,
}

impl MacroAssembler {
    pub fn new() -> MacroAssembler {
        MacroAssembler {
            data: Vec::new(),
            labels: Vec::new(),
            jumps: Vec::new(),
            bailouts: Vec::new(),
            bailout_infos: Bailouts::new(),
            dseg: DSeg::new(),
            gcpoints: GcPoints::new(),
            comments: Comments::new(),
            linenos: LineNumberTable::new(),
            exception_handlers: Vec::new(),
            scratch_registers: ScratchRegisters::new(),
        }
    }

    pub fn jit(mut self, id: FctId, stacksize: i32) -> JitFct {
        self.finish();

        JitFct::from_buffer(id,
                            &self.dseg,
                            &self.data,
                            self.bailout_infos,
                            self.gcpoints,
                            stacksize,
                            self.comments,
                            self.linenos,
                            self.exception_handlers)
    }

    pub fn data(mut self) -> Vec<u8> {
        self.finish();

        self.data
    }

    fn finish(&mut self) {
        let bailouts = self.bailouts.drain(0..).collect::<Vec<_>>();

        for bailout in &bailouts {
            let (lbl, trap, pos) = *bailout;

            self.bind_label(lbl);
            self.emit_lineno(pos.line as i32);
            self.trap(trap);
        }

        self.fix_forward_jumps();
    }

    pub fn add_addr(&mut self, ptr: *const u8) -> i32 {
        self.dseg.add_addr(ptr)
    }

    pub fn pos(&self) -> usize {
        self.data.len()
    }

    pub fn test_if_nil_bailout(&mut self, pos: Position, reg: Reg, trap: Trap) {
        let lbl = self.test_if_nil(reg);
        self.emit_bailout(lbl, trap, pos);
    }

    pub fn test_if_nil(&mut self, reg: Reg) -> Label {
        self.cmp_zero(MachineMode::Ptr, reg);

        let lbl = self.create_label();
        self.jump_if(CondCode::Equal, lbl);

        lbl
    }

    pub fn emit_lineno(&mut self, lineno: i32) {
        let pos = self.pos() as i32;
        self.linenos.insert(pos, lineno);
    }

    pub fn emit_gcpoint(&mut self, gcpoint: GcPoint) {
        let pos = self.pos() as i32;
        self.gcpoints.insert(pos, gcpoint);
    }

    pub fn emit_bailout_info(&mut self, info: BailoutInfo) {
        let pos = self.pos() as i32;
        self.bailout_infos.insert(pos, info);
    }

    pub fn create_label(&mut self) -> Label {
        let idx = self.labels.len();
        self.labels.push(None);

        Label(idx)
    }

    pub fn emit_comment(&mut self, comment: Comment) {
        let pos = self.pos() as i32;
        self.comments.insert(pos, comment);
    }

    pub fn bind_label(&mut self, lbl: Label) {
        let lbl_idx = lbl.index();

        assert!(self.labels[lbl_idx].is_none());
        self.labels[lbl_idx] = Some(self.pos());
    }

    pub fn emit_bailout(&mut self, lbl: Label, trap: Trap, pos: Position) {
        self.bailouts.push((lbl, trap, pos));
    }

    pub fn emit_bailout_inplace(&mut self, trap: Trap, pos: Position) {
        self.emit_lineno(pos.line as i32);
        self.trap(trap);
    }

    pub fn emit_exception_handler(&mut self,
                                  span: (usize, usize),
                                  catch: usize,
                                  offset: Option<i32>,
                                  catch_type: CatchType) {
        self.exception_handlers.push(ExHandler {
            try_start: span.0,
            try_end: span.1,
            catch: catch,
            offset: offset,
            catch_type: catch_type,
        });
    }

    pub fn get_scratch(&self) -> ScratchReg {
        self.scratch_registers.get()
    }

    pub fn emit_u8(&mut self, value: u8) {
        self.data.write_u8(value).unwrap();
    }

    pub fn emit_u32(&mut self, value: u32) {
        self.data.write_u32::<LittleEndian>(value).unwrap();
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Label(usize);

impl Label {
    pub fn index(&self) -> usize {
        self.0
    }
}

#[derive(Clone, Debug)]
pub struct ScratchRegisters {
    regs: &'static [Reg],
    value: Rc<Cell<u32>>,
}

impl ScratchRegisters {
    pub fn new() -> ScratchRegisters {
        ScratchRegisters {
            regs: &SCRATCH,
            value: Rc::new(Cell::new(0))
        }
    }

    #[cfg(test)]
    pub fn with_regs(regs: &'static [Reg]) -> ScratchRegisters {
        ScratchRegisters {
            regs: regs,
            value: Rc::new(Cell::new(0))
        }
    }

    pub fn get(&self) -> ScratchReg {
        let value = self.value.get();

        for (ind, &reg) in self.regs.iter().enumerate() {
            if (value >> ind) & 1 == 0 {
                let bitmask = 1 << ind;
                self.value.set(value | bitmask);

                return ScratchReg {
                    ind: ind as u32,
                    reg: reg,
                    scratch: self.clone(),
                };
            }
        }

        panic!("all scratch registers used");
    }

    fn free(&self, reg: &ScratchReg) {
        let value = self.value.get();
        let bitmask = !(1 << reg.ind);

        self.value.set(value & bitmask);
    }
}

#[derive(Debug)]
pub struct ScratchReg {
    ind: u32,
    reg: Reg,
    scratch: ScratchRegisters,
}

impl ScratchReg {
    pub fn reg(&self) -> Reg {
        self.reg
    }
}

impl Drop for ScratchReg {
    fn drop(&mut self) {
        self.scratch.free(self);
    }
}

impl Deref for ScratchReg {
    type Target = Reg;

    fn deref(&self) -> &Reg {
        &self.reg
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_label() {
        let mut masm = MacroAssembler::new();

        assert_eq!(Label(0), masm.create_label());
        assert_eq!(Label(1), masm.create_label());
    }

    #[test]
    #[should_panic]
    fn test_bind_label_twice() {
        let mut masm = MacroAssembler::new();
        let lbl = masm.create_label();

        masm.bind_label(lbl);
        masm.bind_label(lbl);
    }

    static SCRATCH_REGS1: [Reg; 1] = [Reg(0)];
    static SCRATCH_REGS2: [Reg; 1] = [Reg(1)];

    #[test]
    #[should_panic]
    #[allow(unused_variables)]
    fn test_scratch_fail() {
        let masm = ScratchRegisters::with_regs(&SCRATCH_REGS1);

        let scratch1 = masm.get();
        let scratch2 = masm.get();
    }

    #[test]
    fn test_scratch_drop() {
        let masm = ScratchRegisters::with_regs(&SCRATCH_REGS2);

        {
            let scratch1 = masm.get();
            assert_eq!(*scratch1, Reg(1));
        }


        {
            let scratch1 = masm.get();
            assert_eq!(*scratch1, Reg(1));
        }
    }
}
