use libc;

use std::fs::OpenOptions;
use std::io::{self, BufWriter, Write};
use std::slice;
use std::sync::Arc;

use capstone::prelude::*;

use crate::baseline;
use crate::boots;
use crate::cannon;
use crate::compiler::fct::JitFct;
use crate::compiler::map::CodeDescriptor;
use crate::compiler::native_stub::{self, NativeFct};
use crate::cpu::{FReg, Reg, FREG_RESULT, REG_RESULT};
use crate::driver::cmd::{AsmSyntax, CompilerName};
use crate::gc::Address;
use crate::masm::*;
use crate::mem;
use crate::os;
use crate::ty::{MachineMode, TypeList};
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
    let src = src.write();

    generate_fct(vm, &fct, &src, cls_type_params, fct_type_params)
}

pub fn generate_fct<'ast>(
    vm: &VM<'ast>,
    fct: &Fct<'ast>,
    src: &FctSrc,
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

    let bc = if fct.use_cannon {
        CompilerName::Cannon
    } else if fct.has_optimize_immediately {
        CompilerName::Boots
    } else {
        vm.args.compiler()
    };

    let jit_fct = match bc {
        CompilerName::Cannon => cannon::compile(vm, &fct, src, cls_type_params, fct_type_params),
        CompilerName::Baseline => {
            baseline::compile(vm, &fct, src, cls_type_params, fct_type_params)
        }
        CompilerName::Boots => boots::compile(vm, &fct, src, cls_type_params, fct_type_params),
    };

    if vm.args.flag_enable_perf {
        os::perf::register_with_perf(&jit_fct, vm, fct.ast.name);
    }

    if should_emit_asm(vm, &*fct) {
        dump_asm(
            vm,
            &*fct,
            cls_type_params,
            fct_type_params,
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
        jit_fcts.push(Arc::new(jit_fct));

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
fn get_engine(_asm_syntax: AsmSyntax) -> CsResult<Capstone> {
    unimplemented!()
}

pub fn dump_asm<'ast>(
    vm: &VM<'ast>,
    fct: &Fct<'ast>,
    cls_type_params: &TypeList,
    fct_type_params: &TypeList,
    jit_fct: &JitFct,
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

    let cls_type_params: String = if cls_type_params.len() > 0 {
        let mut ty_names = Vec::new();

        for ty in cls_type_params.iter() {
            ty_names.push(ty.name(vm));
        }

        format!(" CLS[{}]", ty_names.join(", "))
    } else {
        "".into()
    };

    let fct_type_params: String = if fct_type_params.len() > 0 {
        let mut ty_names = Vec::new();

        for ty in fct_type_params.iter() {
            ty_names.push(ty.name(vm));
        }

        format!(" FCT[{}]", ty_names.join(", "))
    } else {
        "".into()
    };

    writeln!(
        &mut w,
        "fun {}{}{} {:#x} {:#x}",
        &name, cls_type_params, fct_type_params, start_addr, end_addr
    )
    .unwrap();

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

        if let Some(comments) = jit_fct.get_comment(addr as u32) {
            for comment in comments {
                writeln!(&mut w, "\t\t  // {}", comment).unwrap();
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

pub fn should_emit_bytecode(vm: &VM, fct: &Fct) -> bool {
    if let Some(ref dbg_names) = vm.args.flag_emit_bytecode {
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

pub fn ensure_native_stub(vm: &VM, fct_id: Option<FctId>, internal_fct: NativeFct) -> Address {
    let mut native_stubs = vm.native_stubs.lock();
    let ptr = internal_fct.ptr;

    if let Some(jit_fct_id) = native_stubs.find_fct(ptr) {
        let jit_fct = vm.jit_fcts.idx(jit_fct_id);
        jit_fct.fct_ptr()
    } else {
        let dbg = if let Some(fct_id) = fct_id {
            let fct = vm.fcts.idx(fct_id);
            let fct = fct.read();
            should_emit_debug(vm, &*fct)
        } else {
            false
        };

        let jit_fct_id = native_stub::generate(vm, internal_fct, dbg);
        let jit_fct = vm.jit_fcts.idx(jit_fct_id);

        let fct_ptr = jit_fct.fct_ptr();

        if let Some(fct_id) = fct_id {
            let fct = vm.fcts.idx(fct_id);
            let fct = fct.read();
            if should_emit_asm(vm, &*fct) {
                dump_asm(
                    vm,
                    &*fct,
                    &TypeList::empty(),
                    &TypeList::empty(),
                    &jit_fct,
                    None,
                    vm.args.flag_asm_syntax.unwrap_or(AsmSyntax::Att),
                );
            }
        }

        native_stubs.insert_fct(ptr, jit_fct_id);
        fct_ptr
    }
}
