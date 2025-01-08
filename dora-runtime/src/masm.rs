use std::cell::Cell;
use std::ops::Deref;
use std::rc::Rc;

use crate::compiler::codegen::AnyReg;
use crate::cpu::{Reg, SCRATCH};
use crate::gc::Address;
use crate::mem;
use crate::mirror::Header;
use crate::mode::MachineMode;
use crate::vm::{
    CodeDescriptor, CommentTable, GcPoint, GcPointTable, InlinedLocation, LazyCompilationData,
    LazyCompilationSite, LocationTable, RelocationKind, RelocationTable, Trap, CODE_ALIGNMENT,
};
pub use dora_asm::Label;
use dora_bytecode::Location;

#[cfg(target_arch = "x86_64")]
pub use self::x64::*;

#[cfg(target_arch = "x86_64")]
pub mod x64;

#[cfg(target_arch = "aarch64")]
pub use self::arm64::*;

#[cfg(target_arch = "aarch64")]
pub mod arm64;

pub enum Mem {
    // rbp + val1
    Local(i32),

    // reg1 + val1
    Base(Reg, i32),

    // reg1 + reg2 * val1 + val2
    Index(Reg, Reg, i32, i32),

    // reg1 * val1 + val2
    Offset(Reg, i32, i32),
}

pub enum EmbeddedConstant {
    Float32(f32),
    Float64(f64),
    Int128(u128),
    Address(Address),
    JumpTable(Vec<Label>),
}

pub struct MacroAssembler {
    asm: Assembler,
    bailouts: Vec<(Label, Trap, Location)>,
    lazy_compilation: LazyCompilationData,
    direct_call_sites: Vec<(u32, Label)>,
    embedded_constants: Vec<(Label, EmbeddedConstant)>,
    gcpoints: GcPointTable,
    comments: CommentTable,
    positions: LocationTable,
    relocations: Vec<(u32, Label)>,
    scratch_registers: ScratchRegisters,
}

impl MacroAssembler {
    pub fn new() -> MacroAssembler {
        MacroAssembler {
            asm: MacroAssembler::create_assembler(),
            bailouts: Vec::new(),
            lazy_compilation: LazyCompilationData::new(),
            direct_call_sites: Vec::new(),
            embedded_constants: Vec::new(),
            gcpoints: GcPointTable::new(),
            comments: CommentTable::new(),
            positions: LocationTable::new(),
            relocations: Vec::new(),
            scratch_registers: ScratchRegisters::new(),
        }
    }

    pub fn data(mut self) -> Vec<u8> {
        self.emit_bailouts();
        self.emit_embedded_constants();
        self.asm.finalize(1).code()
    }

    pub fn code(mut self) -> CodeDescriptor {
        self.emit_bailouts();
        self.emit_embedded_constants();

        for (pos, label) in self.direct_call_sites {
            let entry = self
                .lazy_compilation
                .get_mut(pos)
                .expect("missing call site");
            match entry.clone() {
                LazyCompilationSite::Direct {
                    fct_id,
                    type_params,
                    const_pool_offset_from_ra,
                } => {
                    if const_pool_offset_from_ra == 0 {
                        let label_pos = self.asm.offset(label).expect("missing label");
                        let const_pool_offset_from_ra = label_pos as i32 - pos as i32;
                        *entry = LazyCompilationSite::Direct {
                            fct_id,
                            type_params,
                            const_pool_offset_from_ra,
                        };
                    }
                }
                _ => unreachable!(),
            }
        }

        let asm = self.asm.finalize(CODE_ALIGNMENT);

        let relocations = self
            .relocations
            .into_iter()
            .map(|(pos, label)| {
                let offset = asm.offset(label).expect("unresolved label");
                (pos, RelocationKind::JumpTableEntry(offset))
            })
            .collect::<Vec<_>>();

        CodeDescriptor {
            code: asm.code(),
            lazy_compilation: self.lazy_compilation,
            gcpoints: self.gcpoints,
            comments: self.comments,
            positions: self.positions,
            relocations: RelocationTable::from(relocations),
            inlined_functions: Vec::new(),
        }
    }

    fn emit_bailouts(&mut self) {
        let bailouts = self.bailouts.drain(0..).collect::<Vec<_>>();

        for bailout in &bailouts {
            let (lbl, trap, location) = *bailout;

            self.bind_label(lbl);
            self.trap(trap, location);
        }

        // add nop after bailout traps, so that we can't find return address
        // in code map, even though return address is at function end.
        if bailouts.len() > 0 {
            self.nop();
        }
    }

    fn emit_embedded_constants(&mut self) {
        for (label, value) in &self.embedded_constants {
            let align = match value {
                EmbeddedConstant::Float32(..) => std::mem::size_of::<u32>(),
                EmbeddedConstant::Address(..)
                | EmbeddedConstant::Float64(..)
                | EmbeddedConstant::Int128(..)
                | EmbeddedConstant::JumpTable(..) => std::mem::size_of::<u64>(),
            };

            self.asm.align_to(align);
            self.asm.bind_label(*label);

            match value {
                EmbeddedConstant::Address(value) => {
                    self.asm.emit_u64(value.to_usize() as u64);
                }

                EmbeddedConstant::Float32(value) => {
                    self.asm.emit_u32(unsafe { std::mem::transmute(*value) });
                }

                EmbeddedConstant::Float64(value) => {
                    self.asm.emit_u64(unsafe { std::mem::transmute(*value) });
                }

                EmbeddedConstant::Int128(value) => {
                    self.asm.emit_u128(*value);
                }

                EmbeddedConstant::JumpTable(targets) => {
                    for target in targets {
                        let offset = self.asm.position();
                        self.asm.emit_u64(0);
                        self.relocations
                            .push((offset.try_into().expect("overflow"), *target));
                    }
                }
            }
        }
    }

    pub fn emit_const(&mut self, value: EmbeddedConstant) -> Label {
        let label = self.create_label();
        self.embedded_constants.push((label, value));
        label
    }

    pub fn pos(&self) -> usize {
        self.asm.position()
    }

    pub fn test_if_nil_bailout(&mut self, location: Location, reg: Reg, trap: Trap) {
        let lbl = self.test_if_nil(reg);
        self.emit_bailout(lbl, trap, location);
    }

    pub fn test_if_nil(&mut self, reg: Reg) -> Label {
        self.cmp_zero(MachineMode::Ptr, reg);

        let lbl = self.asm.create_label();
        self.jump_if(CondCode::Equal, lbl);

        lbl
    }

    pub fn test_if_not_nil(&mut self, reg: Reg) -> Label {
        self.cmp_zero(MachineMode::Ptr, reg);

        let lbl = self.asm.create_label();
        self.jump_if(CondCode::NotEqual, lbl);

        lbl
    }

    pub fn emit_position(&mut self, location: Location) {
        let offset = self.pos() as u32;
        self.positions.insert(
            offset,
            InlinedLocation {
                inlined_function_id: None,
                location,
            },
        );
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
        self.asm.create_label()
    }

    pub fn create_and_bind_label(&mut self) -> Label {
        self.asm.create_and_bind_label()
    }

    pub fn emit_comment(&mut self, comment: String) {
        let offset = self.pos() as u32;
        self.comments.insert(offset, comment);
    }

    pub fn bind_label(&mut self, lbl: Label) {
        self.asm.bind_label(lbl);
    }

    pub fn emit_bailout(&mut self, lbl: Label, trap: Trap, location: Location) {
        self.bailouts.push((lbl, trap, location));
    }

    pub fn bailout_if(&mut self, cond: CondCode, trap: Trap, location: Location) {
        let lbl = self.create_label();
        self.jump_if(cond, lbl);
        self.emit_bailout(lbl, trap, location);
    }

    pub fn get_scratch(&self) -> ScratchReg {
        self.scratch_registers.get()
    }

    pub fn emit_u8(&mut self, value: u8) {
        self.asm.emit_u8(value);
    }

    pub fn emit_u32(&mut self, value: u32) {
        self.asm.emit_u32(value);
    }

    pub fn emit_u64(&mut self, value: u64) {
        self.asm.emit_u64(value);
    }

    pub fn copy(&mut self, mode: MachineMode, dest: AnyReg, src: AnyReg) {
        assert!(dest.is_reg() == src.is_reg());

        if dest.is_reg() {
            self.copy_reg(mode, dest.reg(), src.reg());
        } else {
            self.copy_freg(mode, dest.freg(), src.freg());
        }
    }

    pub fn fill_zero(&mut self, obj: Reg, array: bool, size: usize) {
        let header_size =
            (Header::size() as usize) + if array { mem::ptr_width_usize() } else { 0 };

        debug_assert!(size >= header_size);
        debug_assert!(size % mem::ptr_width_usize() == 0);
        let size = size - header_size;
        let size_words = size / mem::ptr_width_usize();

        if size_words == 0 {
            // nothing to fill zero
        } else if size_words <= 8 {
            let zero = self.get_scratch();
            self.load_int_const(MachineMode::Int32, *zero, 0);

            for offset in 0..size_words {
                let offset = header_size as i32 + (offset as i32) * mem::ptr_width();
                self.store_mem(MachineMode::Ptr, Mem::Base(obj, offset), (*zero).into());
            }
        } else {
            let obj_end = self.get_scratch();
            self.copy_reg(MachineMode::Ptr, *obj_end, obj);
            let offset = header_size as i32 + (size_words as i32) * mem::ptr_width();
            self.int_add_imm(MachineMode::Ptr, *obj_end, *obj_end, offset as i64);
            self.int_add_imm(MachineMode::Ptr, obj, obj, header_size as i64);
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

    pub fn emit_jump_table(&mut self, targets: Vec<Label>) -> Label {
        assert!(!targets.is_empty());
        self.emit_const(EmbeddedConstant::JumpTable(targets))
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

        masm.create_label();
        masm.create_label();
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
