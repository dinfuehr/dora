use std::cell::Cell;
use std::ops::Deref;
use std::rc::Rc;

use crate::asm::Assembler;
use crate::compiler::codegen::ExprStore;
use crate::compiler::fct::{
    CatchType, Code, Comments, GcPoint, GcPoints, Handler, JitDescriptor, LazyCompilationData,
    LazyCompilationSite, PositionTable,
};
use crate::cpu::{Mem, Reg, SCRATCH};
use crate::dseg::DSeg;
use crate::gc::Address;
use crate::mem;
use crate::object::Header;
use crate::ty::MachineMode;
use crate::vm::{Trap, VM};
use byteorder::{ByteOrder, LittleEndian, WriteBytesExt};
use dora_parser::lexer::position::Position;

#[cfg(target_arch = "x86_64")]
pub use self::x64::*;

#[cfg(target_arch = "x86_64")]
pub mod x64;

#[cfg(target_arch = "aarch64")]
pub use self::arm64::*;

#[cfg(target_arch = "aarch64")]
pub mod arm64;

pub struct MacroAssembler {
    asm: Assembler,
    labels: Vec<Option<usize>>,
    jumps: Vec<ForwardJump>,
    bailouts: Vec<(Label, Trap, Position)>,
    lazy_compilation: LazyCompilationData,
    dseg: DSeg,
    gcpoints: GcPoints,
    comments: Comments,
    positions: PositionTable,
    exception_handlers: Vec<Handler>,
    scratch_registers: ScratchRegisters,
}

impl MacroAssembler {
    pub fn new() -> MacroAssembler {
        MacroAssembler {
            asm: Assembler::new(),
            labels: Vec::new(),
            jumps: Vec::new(),
            bailouts: Vec::new(),
            lazy_compilation: LazyCompilationData::new(),
            dseg: DSeg::new(),
            gcpoints: GcPoints::new(),
            comments: Comments::new(),
            positions: PositionTable::new(),
            exception_handlers: Vec::new(),
            scratch_registers: ScratchRegisters::new(),
        }
    }

    pub fn jit(mut self, vm: &VM, stacksize: i32, desc: JitDescriptor, throws: bool) -> Code {
        self.finish();

        // align data such that code starts at address that is
        // aligned to 16
        self.dseg.align(16);

        Code::from_buffer(
            vm,
            &self.dseg,
            self.asm.code_mut(),
            self.lazy_compilation,
            self.gcpoints,
            stacksize,
            self.comments,
            self.positions,
            desc,
            throws,
            self.exception_handlers,
        )
    }

    #[cfg(test)]
    pub fn buffer(&self) -> &[u8] {
        self.asm.code()
    }

    pub fn data(mut self) -> Vec<u8> {
        self.finish();

        self.asm.finalize()
    }

    fn finish(&mut self) {
        let bailouts = self.bailouts.drain(0..).collect::<Vec<_>>();

        for bailout in &bailouts {
            let (lbl, trap, pos) = *bailout;

            self.bind_label(lbl);
            self.trap(trap, pos);
        }

        // add nop after bailout traps, so that we can't find return address
        // in code map, even though return address is at function end.
        if bailouts.len() > 0 {
            self.nop();
        }

        self.fix_forward_jumps();
    }

    pub fn add_addr(&mut self, ptr: *const u8) -> i32 {
        self.dseg.add_addr(ptr)
    }

    pub fn pos(&self) -> usize {
        self.asm.pc() as usize
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

    pub fn test_if_not_nil(&mut self, reg: Reg) -> Label {
        self.cmp_zero(MachineMode::Ptr, reg);

        let lbl = self.create_label();
        self.jump_if(CondCode::NotEqual, lbl);

        lbl
    }

    pub fn emit_position(&mut self, position: Position) {
        let offset = self.pos() as u32;
        self.positions.insert(offset, position);
    }

    pub fn emit_gcpoint(&mut self, gcpoint: GcPoint) {
        let pos = self.pos() as u32;
        self.gcpoints.insert(pos, gcpoint);
    }

    pub fn emit_only_gcpoint(&mut self, gcpoint: GcPoint) {
        self.gcpoints.insert(0, gcpoint);
    }

    pub fn emit_lazy_compilation_site(&mut self, info: LazyCompilationSite) {
        let pos = self.pos() as u32;
        self.lazy_compilation.insert(pos, info);
    }

    pub fn create_label(&mut self) -> Label {
        let idx = self.labels.len();
        self.labels.push(None);

        Label(idx)
    }

    pub fn emit_comment(&mut self, comment: String) {
        let offset = self.pos() as u32;
        self.comments.insert(offset, comment);
    }

    pub fn bind_label(&mut self, lbl: Label) {
        self.bind_label_to(lbl, self.pos());
    }

    pub fn bind_label_to(&mut self, lbl: Label, pos: usize) {
        let lbl_idx = lbl.index();

        assert!(self.labels[lbl_idx].is_none());
        assert!(pos <= self.pos());
        self.labels[lbl_idx] = Some(pos);
    }

    pub fn emit_bailout(&mut self, lbl: Label, trap: Trap, pos: Position) {
        self.bailouts.push((lbl, trap, pos));
    }

    pub fn emit_bailout_inplace(&mut self, trap: Trap, pos: Position) {
        self.trap(trap, pos);
    }

    pub fn emit_exception_handler(
        &mut self,
        span: (usize, usize),
        catch: usize,
        offset: Option<i32>,
        catch_type: CatchType,
    ) {
        self.exception_handlers.push(Handler {
            try_start: span.0,
            try_end: span.1,
            catch,
            offset,
            catch_type,
        });
    }

    pub fn get_scratch(&self) -> ScratchReg {
        self.scratch_registers.get()
    }

    pub fn emit_u8(&mut self, value: u8) {
        self.asm.code_mut().write_u8(value).unwrap();
    }

    pub fn emit_u8_at(&mut self, pos: i32, value: u8) {
        self.asm.code_mut()[pos as usize] = value;
    }

    pub fn emit_u32(&mut self, value: u32) {
        self.asm
            .code_mut()
            .write_u32::<LittleEndian>(value)
            .unwrap();
    }

    pub fn emit_u32_at(&mut self, pos: i32, value: u32) {
        let buf = &mut self.asm.code_mut()[pos as usize..];
        LittleEndian::write_u32(buf, value);
    }

    pub fn emit_u64(&mut self, value: u64) {
        self.asm
            .code_mut()
            .write_u64::<LittleEndian>(value)
            .unwrap();
    }

    pub fn copy(&mut self, mode: MachineMode, dest: ExprStore, src: ExprStore) {
        assert!(dest.is_reg() == src.is_reg());

        if dest.is_reg() {
            self.copy_reg(mode, dest.reg(), src.reg());
        } else {
            self.copy_freg(mode, dest.freg(), src.freg());
        }
    }

    pub fn fill_zero(&mut self, obj: Reg, size: usize) {
        debug_assert!(size >= (Header::size() as usize));
        debug_assert!(size % mem::ptr_width_usize() == 0);
        let size = size - (Header::size() as usize);
        let size_words = size / mem::ptr_width_usize();

        if size_words == 0 {
            // nothing to fill zero
        } else if size_words <= 8 {
            let zero = self.get_scratch();
            self.load_int_const(MachineMode::Int32, *zero, 0);

            for offset in 0..size_words {
                let offset = Header::size() + offset as i32 * mem::ptr_width();
                self.store_mem(MachineMode::Ptr, Mem::Base(obj, offset), (*zero).into());
            }
        } else {
            let obj_end = self.get_scratch();
            self.copy_reg(MachineMode::Ptr, *obj_end, obj);
            let offset = Header::size() + (size_words as i32) * mem::ptr_width();
            self.int_add_imm(MachineMode::Ptr, *obj_end, *obj_end, offset as i64);
            self.int_add_imm(MachineMode::Ptr, obj, obj, Header::size() as i64);
            self.fill_zero_dynamic(obj, *obj_end);
        }
    }

    pub fn fill_zero_dynamic(&mut self, obj: Reg, obj_end: Reg) {
        let done = self.create_label();
        let start = self.create_label();

        let zero = self.get_scratch();
        self.load_int_const(MachineMode::Ptr, *zero, 0);

        let curr = self.get_scratch();
        self.copy_reg(MachineMode::Ptr, *curr, obj);

        self.bind_label(start);
        // loop until end of object reached
        self.cmp_reg(MachineMode::Ptr, *curr, obj_end);
        self.jump_if(CondCode::Equal, done);
        self.store_mem(MachineMode::Ptr, Mem::Base(*curr, 0), (*zero).into());
        self.int_add_imm(MachineMode::Ptr, *curr, *curr, mem::ptr_width() as i64);
        // jump to begin of loop
        self.jump(start);
        self.bind_label(done);
    }

    pub fn safepoint(&mut self, polling_page: Address) {
        self.check_polling_page(polling_page);
        let gcpoint = GcPoint::new();
        self.emit_gcpoint(gcpoint);
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
            value: Rc::new(Cell::new(0)),
        }
    }

    #[cfg(test)]
    pub fn with_regs(regs: &'static [Reg]) -> ScratchRegisters {
        ScratchRegisters {
            regs,
            value: Rc::new(Cell::new(0)),
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
                    reg,
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

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum CondCode {
    Zero,
    NonZero,
    Equal,
    NotEqual,
    Greater,
    GreaterEq,
    Less,
    LessEq,
    UnsignedGreater,
    UnsignedGreaterEq,
    UnsignedLess,
    UnsignedLessEq,
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
    fn test_emit_u32() {
        let mut masm = MacroAssembler::new();
        masm.emit_u32(0x11223344);
        assert_eq!(&[0x44, 0x33, 0x22, 0x11], masm.buffer());

        masm.emit_u32_at(0, 0x55667788);
        assert_eq!(&[0x88, 0x77, 0x66, 0x55], masm.buffer());
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
    static SCRATCH_REGS3: [Reg; 3] = [Reg(2), Reg(3), Reg(4)];

    #[test]
    #[should_panic]
    #[allow(unused_variables)]
    fn test_scratch_fail() {
        let masm = ScratchRegisters::with_regs(&SCRATCH_REGS1);

        let scratch1 = masm.get();
        let scratch2 = masm.get();
    }

    #[test]
    fn tets_scratch_multiple() {
        let masm = ScratchRegisters::with_regs(&SCRATCH_REGS3);

        let scratch1 = masm.get();
        let scratch2 = masm.get();

        assert_eq!(*scratch1, Reg(2));
        assert_eq!(*scratch2, Reg(3));

        {
            let scratch3 = masm.get();
            assert_eq!(*scratch3, Reg(4));
        }

        let scratch3 = masm.get();
        assert_eq!(*scratch3, Reg(4));
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
