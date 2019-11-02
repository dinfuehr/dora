use libc;

use std::collections::HashMap;
use std::collections::HashSet;
use std::fs::OpenOptions;
use std::io::{self, BufWriter, Write};
use std::slice;
use std::sync::Arc;

use capstone::prelude::*;

use crate::baseline::asm::BaselineAssembler;
use crate::baseline::ast::{generate_info, AstCodeGen, JitInfo};
use crate::baseline::cannon::CannonCodeGen;
use crate::baseline::dora_native::{self, InternalFct};
use crate::baseline::fct::{CommentFormat, GcPoint, JitBaselineFct, JitFct};
use crate::baseline::map::CodeDescriptor;
use crate::cpu::x64::reg::{FREG_RESULT, REG_RESULT};
use crate::cpu::{FReg, Reg};
use crate::driver::cmd::{AsmSyntax, BaselineName};
use crate::gc::Address;
use crate::masm::*;
use crate::mem;
use crate::os;
use crate::ty::{BuiltinType, MachineMode, TypeList};
use crate::vm::VM;
use crate::vm::{Fct, FctId, FctSrc};

pub fn generate<'ast>(
    vm: &VM<'ast>,
    id: FctId,
    cls_type_params: &TypeList,
    fct_type_params: &TypeList,
) -> Address {
    let fct = vm.fcts.idx(id);
    let fct = fct.read();
    let src = fct.src();
    let mut src = src.write();

    generate_fct(vm, &fct, &mut src, cls_type_params, fct_type_params)
}

pub fn generate_fct<'ast>(
    vm: &VM<'ast>,
    fct: &Fct<'ast>,
    src: &mut FctSrc,
    cls_type_params: &TypeList,
    fct_type_params: &TypeList,
) -> Address {
    debug_assert!(cls_type_params
        .iter()
        .all(|ty| !ty.contains_type_param(vm),));
    debug_assert!(fct_type_params
        .iter()
        .all(|ty| !ty.contains_type_param(vm),));

    {
        let specials = src.specializations.read();
        let key = (cls_type_params.clone(), fct_type_params.clone());

        if let Some(&jit_fct_id) = specials.get(&key) {
            let jit_fct = vm.jit_fcts.idx(jit_fct_id);
            return jit_fct.fct_ptr();
        }
    }

    let ast = fct.ast;

    let jit_fct = match vm.args.bc() {
        BaselineName::Cannon => CannonCodeGen {
            vm,
            fct: &fct,
            ast,
            asm: BaselineAssembler::new(vm),
            src,

            lbl_break: None,
            lbl_continue: None,

            active_finallys: Vec::new(),
            active_upper: None,
            active_loop: None,
            lbl_return: None,

            cls_type_params,
            fct_type_params,
        }
        .generate(),
        BaselineName::AstCompiler => {
            let mut jit_info = JitInfo::new();
            generate_info(
                vm,
                fct,
                src,
                &mut jit_info,
                cls_type_params,
                fct_type_params,
            );
            AstCodeGen {
                vm,
                fct: &fct,
                ast,
                asm: BaselineAssembler::new(vm),
                src,
                jit_info: &jit_info,

                lbl_break: None,
                lbl_continue: None,
                lbl_return: None,

                active_finallys: Vec::new(),
                active_upper: None,
                active_loop: None,
                stack: StackFrame::new(),
                stacksize_offset: 0,
                managed_stack: ManagedStackFrame::new(),
                var_to_slot: HashMap::new(),

                cls_type_params,
                fct_type_params,
            }
            .generate()
        }
    };

    if vm.args.flag_enable_perf {
        os::perf::register_with_perf(&jit_fct, vm, ast.name);
    }

    if should_emit_asm(vm, &*fct) {
        dump_asm(
            vm,
            &*fct,
            &jit_fct,
            Some(&src),
            vm.args.flag_asm_syntax.unwrap_or(AsmSyntax::Att),
        );
    }

    let fct_ptr = jit_fct.fct_ptr();
    let ptr_start = jit_fct.ptr_start();
    let ptr_end = jit_fct.ptr_end();

    debug_assert!(mem::is_aligned(ptr_start.to_usize(), 16));
    debug_assert!(mem::is_aligned(fct_ptr.to_usize(), 16));

    let jit_fct_id = {
        let mut jit_fcts = vm.jit_fcts.lock();
        let jit_fct_id = jit_fcts.len().into();
        jit_fcts.push(Arc::new(JitFct::Base(jit_fct)));

        jit_fct_id
    };

    {
        let mut specials = src.specializations.write();
        let key = (cls_type_params.clone(), fct_type_params.clone());
        specials.insert(key, jit_fct_id);
    }

    {
        let mut code_map = vm.code_map.lock();
        let cdata = CodeDescriptor::DoraFct(jit_fct_id);
        code_map.insert(ptr_start, ptr_end, cdata);
    }

    fct_ptr
}

#[cfg(target_arch = "x86_64")]
fn get_engine(asm_syntax: AsmSyntax) -> CsResult<Capstone> {
    let arch_syntax = match asm_syntax {
        AsmSyntax::Intel => arch::x86::ArchSyntax::Intel,
        AsmSyntax::Att => arch::x86::ArchSyntax::Att,
    };

    Capstone::new()
        .x86()
        .mode(arch::x86::ArchMode::Mode64)
        .syntax(arch_syntax)
        .build()
}

#[cfg(target_arch = "aarch64")]
fn get_engine(asm_syntax: AsmSyntax) -> CsResult {
    unimplemented!()
}

pub fn dump_asm<'ast>(
    vm: &VM<'ast>,
    fct: &Fct<'ast>,
    jit_fct: &JitBaselineFct,
    fct_src: Option<&FctSrc>,
    asm_syntax: AsmSyntax,
) {
    let buf: &[u8] =
        unsafe { slice::from_raw_parts(jit_fct.fct_ptr().to_ptr(), jit_fct.fct_len()) };

    let engine = get_engine(asm_syntax).expect("cannot create capstone engine");

    let mut w: Box<dyn Write> = if vm.args.flag_emit_asm_file {
        let pid = unsafe { libc::getpid() };
        let name = format!("code-{}.asm", pid);
        let file = OpenOptions::new()
            .create(true)
            .append(true)
            .open(&name)
            .expect("couldn't append to asm file");

        Box::new(BufWriter::new(file))
    } else {
        Box::new(io::stdout())
    };

    let start_addr = jit_fct.fct_ptr().to_usize() as u64;
    let end_addr = jit_fct.fct_end().to_usize() as u64;

    let instrs = engine
        .disasm_all(buf, start_addr)
        .expect("could not disassemble code");

    let name = fct.full_name(vm);

    writeln!(&mut w, "fun {} {:#x} {:#x}", &name, start_addr, end_addr).unwrap();

    if let Some(fct_src) = fct_src {
        for var in &fct_src.vars {
            let name = vm.interner.str(var.name);
            writeln!(&mut w, "  var `{}`: type {}", name, var.ty.name(vm)).unwrap();
        }

        if fct_src.vars.len() > 0 {
            writeln!(&mut w).unwrap();
        }
    }

    for instr in instrs.iter() {
        let addr = (instr.address() - start_addr) as i32;

        if let Some(gc_point) = jit_fct.gcpoint_for_offset(addr) {
            write!(&mut w, "\t\t  ; gc point = (").unwrap();
            let mut first = true;

            for &offset in &gc_point.offsets {
                if !first {
                    write!(&mut w, ", ").unwrap();
                }

                if offset < 0 {
                    write!(&mut w, "-").unwrap();
                }

                write!(&mut w, "0x{:x}", offset.abs()).unwrap();
                first = false;
            }

            writeln!(&mut w, ")").unwrap();
        }

        if let Some(comments) = jit_fct.get_comment(addr) {
            for comment in comments {
                if comment.is_newline() {
                    writeln!(&mut w).unwrap();
                    continue;
                }

                let cfmt = CommentFormat {
                    comment,
                    vm,
                    fct_src,
                };

                writeln!(&mut w, "\t\t  ; {}", cfmt).unwrap();
            }
        }

        writeln!(
            &mut w,
            "  {:#06x}: {}\t\t{}",
            instr.address(),
            instr.mnemonic().expect("no mnmemonic found"),
            instr.op_str().expect("no op_str found"),
        )
        .unwrap();
    }

    writeln!(&mut w).unwrap();
}

pub fn register_for_mode(mode: MachineMode) -> ExprStore {
    if mode.is_float() {
        FREG_RESULT.into()
    } else {
        REG_RESULT.into()
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

pub struct StackFrame {
    all: HashSet<i32>,
    references: HashSet<i32>,
    scopes: Vec<StackScope>,
}

impl StackFrame {
    pub fn new() -> StackFrame {
        StackFrame {
            all: HashSet::new(),
            references: HashSet::new(),
            scopes: Vec::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.scopes.is_empty() && self.all.is_empty() && self.references.is_empty()
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(StackScope::new());
    }

    pub fn add_var(&mut self, ty: BuiltinType, offset: i32) {
        if ty.reference_type() {
            assert!(self.references.insert(offset));
        }

        assert!(self.all.insert(offset));

        let scope = self.scopes.last_mut().expect("no active scope");
        scope.add_var(ty, offset);
    }

    pub fn pop_scope(&mut self) {
        let scope = self.scopes.pop().expect("no active scope");

        for (offset, ty) in scope.vars.into_iter() {
            if ty.reference_type() {
                assert!(self.references.remove(&offset));
            }

            assert!(self.all.remove(&offset));
        }
    }

    pub fn add_temp(&mut self, ty: BuiltinType, offset: i32) {
        if ty.reference_type() {
            assert!(self.references.insert(offset));
        }

        assert!(self.all.insert(offset));
    }

    pub fn free_temp(&mut self, ty: BuiltinType, offset: i32) {
        if ty.reference_type() {
            assert!(self.references.remove(&offset));
        }

        assert!(self.all.remove(&offset));
    }

    pub fn gcpoint(&self) -> GcPoint {
        let mut offsets = Vec::new();

        for &offset in &self.references {
            offsets.push(offset);
        }

        GcPoint::from_offsets(offsets)
    }
}

pub struct StackScope {
    vars: HashMap<i32, BuiltinType>,
}

impl StackScope {
    fn new() -> StackScope {
        StackScope {
            vars: HashMap::new(),
        }
    }

    fn add_var(&mut self, ty: BuiltinType, offset: i32) {
        assert!(self.vars.insert(offset, ty).is_none());
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
struct ManagedVar(usize);

#[derive(Copy, Clone)]
pub struct ManagedStackSlot {
    var: ManagedVar,
    offset: i32,
}

impl ManagedStackSlot {
    fn offset(&self) -> i32 {
        self.offset
    }
}

pub struct ManagedStackFrame {
    vars: HashMap<ManagedVar, (BuiltinType, i32)>,
    scopes: Vec<ManagedStackScope>,
    next_var: ManagedVar,

    free_slots: FreeSlots,
    stacksize: i32,
}

impl ManagedStackFrame {
    pub fn new() -> ManagedStackFrame {
        ManagedStackFrame {
            vars: HashMap::new(),
            scopes: Vec::new(),
            next_var: ManagedVar(0),

            free_slots: FreeSlots::new(),
            stacksize: 0,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.scopes.is_empty() && self.vars.is_empty()
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(ManagedStackScope::new());
    }

    pub fn pop_scope(&mut self, vm: &VM) {
        let scope = self.scopes.pop().expect("no active scope");

        for var in scope.vars.into_iter() {
            self.free(var, vm);
        }
    }

    pub fn add_scope(&mut self, ty: BuiltinType, vm: &VM) -> ManagedStackSlot {
        let var_and_offset = self.alloc(ty, vm);
        let scope = self.scopes.last_mut().expect("no active scope");
        scope.add_var(var_and_offset.var);

        var_and_offset
    }

    pub fn add_temp(&mut self, ty: BuiltinType, vm: &VM) -> ManagedStackSlot {
        self.alloc(ty, vm)
    }

    pub fn free_temp(&mut self, temp: ManagedStackSlot, vm: &VM) {
        self.free(temp.var, vm)
    }

    fn alloc(&mut self, ty: BuiltinType, vm: &VM) -> ManagedStackSlot {
        let var = self.next_var;
        self.next_var = ManagedVar(var.0 + 1);

        let (size, alignment) = if ty.is_nil() {
            (mem::ptr_width(), mem::ptr_width())
        } else {
            (ty.size(vm), ty.align(vm))
        };

        let alloc = self.free_slots.alloc(size as u32, alignment as u32);

        let offset = if let Some(free_start) = alloc {
            -(free_start as i32 + size)
        } else {
            self.extend_stack(size, alignment)
        };

        self.vars.insert(var, (ty, offset));
        ManagedStackSlot { var, offset }
    }

    fn extend_stack(&mut self, size: i32, alignment: i32) -> i32 {
        self.stacksize = mem::align_i32(self.stacksize as i32, alignment) + size;
        -self.stacksize
    }

    fn free(&mut self, var: ManagedVar, vm: &VM) {
        if let Some((ty, offset)) = self.vars.remove(&var) {
            let size = if ty.is_nil() {
                mem::ptr_width()
            } else {
                ty.size(vm)
            };
            let start = -(offset + size);
            self.free_slots
                .free(FreeSlot::new(start as u32, size as u32));
        } else {
            panic!("var not found");
        }
    }

    pub fn gcpoint(&self) -> GcPoint {
        let mut offsets: Vec<i32> = Vec::new();

        for (_, (ty, offset)) in &self.vars {
            if ty.reference_type() {
                offsets.push(*offset);
            }
        }

        GcPoint::from_offsets(offsets)
    }
}

pub struct ManagedStackScope {
    vars: Vec<ManagedVar>,
}

impl ManagedStackScope {
    fn new() -> ManagedStackScope {
        ManagedStackScope { vars: Vec::new() }
    }

    fn add_var(&mut self, var: ManagedVar) {
        self.vars.push(var);
    }
}

struct FreeSlots {
    slots: Vec<FreeSlot>,
}

impl FreeSlots {
    fn new() -> FreeSlots {
        FreeSlots { slots: Vec::new() }
    }

    fn free(&mut self, new: FreeSlot) {
        let slots = self.slots.len();

        for idx in 0..slots {
            let slot = self.slots[idx];

            if idx > 0 {
                debug_assert!(self.slots[idx - 1].end() < slot.start());
            }

            if new.end() < slot.start() {
                // insert before
                self.slots.insert(idx, new);
            } else if new.end() == slot.start() {
                // extend current slot from left
                self.slots[idx] = FreeSlot::new(new.start(), new.size() + slot.size());
            } else if slot.end() == new.start() {
                if idx + 1 < slots && self.slots[idx + 1].start() == new.end() {
                    // merge two slots
                    let left = slot;
                    let right = self.slots[idx + 1];

                    self.slots.remove(idx);

                    let size = right.end() - left.start();
                    self.slots[idx] = FreeSlot::new(left.start(), size);
                } else {
                    // extend current slot from right
                    self.slots[idx] = FreeSlot::new(slot.start(), slot.size() + new.size());

                    if idx + 1 < slots {
                        debug_assert!(self.slots[idx].end() < self.slots[idx + 1].start());
                    }
                }
            } else {
                // continue to next slot
                continue;
            }

            return;
        }

        self.slots.push(new);
    }

    fn alloc(&mut self, size: u32, alignment: u32) -> Option<u32> {
        let mut result = None;
        let mut best = u32::max_value();
        let slots = self.slots.len();

        for idx in 0..slots {
            let slot = self.slots[idx];

            if idx > 0 {
                debug_assert!(self.slots[idx - 1].end() < slot.start());
            }

            if slot.size() < size {
                continue;
            } else if slot.size() == size {
                if is_aligned(slot.start(), alignment) {
                    self.slots.remove(idx);
                    return Some(slot.start());
                }
            } else {
                let start = align(slot.start(), alignment);

                if start + size < slot.end() {
                    let gap_left = start - slot.start();
                    let gap_right = slot.end() - (start + size);
                    let gap = gap_left + gap_right;

                    if gap < best {
                        best = gap;
                        result = Some(idx);
                    }
                }
            }
        }

        if let Some(mut idx) = result {
            let slot = self.slots[idx];
            self.slots.remove(idx);
            let start = align(slot.start(), alignment);
            let gap_left = start - slot.start();
            let gap_right = slot.end() - (start + size);

            if gap_left > 0 {
                self.slots
                    .insert(idx, FreeSlot::new(slot.start(), gap_left));
                idx += 1;
            }

            if gap_right > 0 {
                self.slots
                    .insert(idx, FreeSlot::new(slot.end() - gap_right, gap_right));
            }

            Some(start)
        } else {
            None
        }
    }
}

fn is_aligned(value: u32, size: u32) -> bool {
    value % size == 0
}

fn align(value: u32, alignment: u32) -> u32 {
    (value * alignment + alignment - 1) / alignment
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
struct FreeSlot {
    start: u32,
    size: u32,
}

impl FreeSlot {
    fn new(start: u32, size: u32) -> FreeSlot {
        FreeSlot { start, size }
    }

    fn start(self) -> u32 {
        self.start
    }

    fn end(self) -> u32 {
        self.start + self.size
    }

    fn size(self) -> u32 {
        self.size
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Next {
    Flow(Label),
    Return,
}

pub fn should_emit_debug(vm: &VM, fct: &Fct) -> bool {
    if let Some(ref dbg_names) = vm.args.flag_emit_debug {
        fct_pattern_match(vm, fct, dbg_names)
    } else {
        false
    }
}

pub fn should_emit_asm(vm: &VM, fct: &Fct) -> bool {
    if let Some(ref dbg_names) = vm.args.flag_emit_asm {
        fct_pattern_match(vm, fct, dbg_names)
    } else {
        false
    }
}

pub fn fct_pattern_match(vm: &VM, fct: &Fct, pattern: &str) -> bool {
    if pattern == "all" {
        return true;
    }

    let name = vm.interner.str(fct.name);

    for part in pattern.split(',') {
        if *name == part {
            return true;
        }
    }

    false
}

pub trait CodeGen<'v> {
    fn generate(self) -> JitBaselineFct;
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ExprStore {
    Reg(Reg),
    FReg(FReg),
}

impl ExprStore {
    pub fn is_reg(&self) -> bool {
        match self {
            &ExprStore::Reg(_) => true,
            _ => false,
        }
    }

    pub fn is_freg(&self) -> bool {
        match self {
            &ExprStore::FReg(_) => true,
            _ => false,
        }
    }

    pub fn reg(&self) -> Reg {
        match self {
            &ExprStore::Reg(reg) => reg,
            _ => panic!("fp-register accessed as gp-register."),
        }
    }

    pub fn freg(&self) -> FReg {
        match self {
            &ExprStore::FReg(reg) => reg,
            _ => panic!("gp-register accessed as fp-register."),
        }
    }
}

impl From<Reg> for ExprStore {
    fn from(reg: Reg) -> ExprStore {
        ExprStore::Reg(reg)
    }
}

impl From<FReg> for ExprStore {
    fn from(reg: FReg) -> ExprStore {
        ExprStore::FReg(reg)
    }
}

pub enum AllocationSize {
    Fixed(usize),
    Dynamic(Reg),
}

pub fn ensure_native_stub(vm: &VM, fct_id: FctId, internal_fct: InternalFct) -> Address {
    let mut native_thunks = vm.native_thunks.lock();
    let ptr = internal_fct.ptr;

    if let Some(jit_fct_id) = native_thunks.find_fct(ptr) {
        let jit_fct = vm.jit_fcts.idx(jit_fct_id);
        jit_fct.fct_ptr()
    } else {
        let fct = vm.fcts.idx(fct_id);
        let fct = fct.read();
        let dbg = should_emit_debug(vm, &*fct);

        let jit_fct_id = dora_native::generate(vm, internal_fct, dbg);
        let jit_fct = vm.jit_fcts.idx(jit_fct_id);
        let jit_fct = jit_fct.to_base().expect("baseline expected");

        let fct_start = jit_fct.fct_start;

        if should_emit_asm(vm, &*fct) {
            dump_asm(
                vm,
                &*fct,
                &jit_fct,
                None,
                vm.args.flag_asm_syntax.unwrap_or(AsmSyntax::Att),
            );
        }

        native_thunks.insert_fct(ptr, jit_fct_id);
        fct_start
    }
}

#[cfg(test)]
mod tests {
    use super::{FreeSlot, FreeSlots};

    #[test]
    fn merge_free_slots() {
        let mut free_slots = FreeSlots::new();
        free_slots.free(FreeSlot::new(0, 2));
        free_slots.free(FreeSlot::new(8, 8));
        free_slots.free(FreeSlot::new(2, 2));
        free_slots.free(FreeSlot::new(4, 4));

        assert_eq!(free_slots.slots, vec![FreeSlot::new(0, 16)]);

        let mut free_slots = FreeSlots::new();
        free_slots.free(FreeSlot::new(4, 8));
        free_slots.free(FreeSlot::new(0, 2));
        free_slots.free(FreeSlot::new(2, 2));

        assert_eq!(free_slots.slots, vec![FreeSlot::new(0, 12)]);
    }

    #[test]
    fn alloc_free_slot() {
        let mut free_slots = FreeSlots::new();

        assert_eq!(free_slots.alloc(2, 2), None);
        free_slots.free(FreeSlot::new(0, 2));

        assert_eq!(free_slots.alloc(2, 2), Some(0));
        assert_eq!(free_slots.slots, Vec::new());

        free_slots.free(FreeSlot::new(0, 8));
        free_slots.free(FreeSlot::new(12, 4));
        assert_eq!(free_slots.alloc(4, 4), Some(12));
        assert_eq!(free_slots.slots, vec![FreeSlot::new(0, 8)]);
    }
}
