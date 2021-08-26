use std::sync::Arc;

use crate::boots;
use crate::cannon;
use crate::compiler::JitFct;
use crate::compiler::{native_stub, CodeDescriptor, NativeFct};
use crate::cpu::{FReg, Reg, FREG_RESULT, REG_RESULT};
use crate::disassembler;
use crate::driver::cmd::{AsmSyntax, CompilerName};
use crate::gc::Address;
use crate::masm::Label;
use crate::mem;
use crate::os;
use crate::ty::{MachineMode, SourceTypeArray};
use crate::vm::VM;
use crate::vm::{Fct, FctId};

pub fn generate(vm: &VM, id: FctId, type_params: &SourceTypeArray) -> Address {
    let fct = vm.fcts.idx(id);
    let fct = fct.read();
    generate_fct(vm, &fct, type_params)
}

pub fn generate_fct(vm: &VM, fct: &Fct, type_params: &SourceTypeArray) -> Address {
    debug_assert!(type_params.iter().all(|ty| !ty.contains_type_param(vm)));

    {
        let specials = fct.specializations.read();

        if let Some(&jit_fct_id) = specials.get(&type_params) {
            let jit_fct = vm.jit_fcts.idx(jit_fct_id);
            return jit_fct.instruction_start();
        }
    }

    let bc = if fct.has_optimize_immediately {
        CompilerName::Boots
    } else {
        CompilerName::Cannon
    };

    let code = match bc {
        CompilerName::Cannon => cannon::compile(vm, &fct, &type_params),
        CompilerName::Boots => boots::compile(vm, &fct, type_params),
    };

    if vm.args.flag_enable_perf {
        os::perf::register_with_perf(&code, vm, fct.ast.name);
    }

    if should_emit_asm(vm, &*fct) {
        disassembler::disassemble(
            vm,
            &*fct,
            &type_params,
            &code,
            vm.args.flag_asm_syntax.unwrap_or(AsmSyntax::Att),
        );
    }

    let fct_ptr = code.instruction_start();
    let ptr_start = code.ptr_start();
    let ptr_end = code.ptr_end();

    debug_assert!(mem::is_aligned(ptr_start.to_usize(), 16));
    debug_assert!(mem::is_aligned(fct_ptr.to_usize(), 16));

    {
        let mut specials = fct.specializations.write();

        // check whether function was compiled in-between from another thread.
        if let Some(&jit_fct_id) = specials.get(type_params) {
            let jit_fct = vm.jit_fcts.idx(jit_fct_id);
            return jit_fct.instruction_start();
        }

        // insert the returned Code into the JitFct table to get the JitFctId.
        let jit_fct_id = {
            let mut jit_fcts = vm.jit_fcts.lock();
            let jit_fct_id = jit_fcts.len().into();
            jit_fcts.push(Arc::new(JitFct::Compiled(code)));
            jit_fct_id
        };

        // We need to insert into CodeMap before releasing the specializations-lock. Otherwise
        // another thread could run that function while the function can't be found in the
        // CodeMap yet. This would lead to crash e.g. for lazy compilation.
        {
            let mut code_map = vm.code_map.lock();
            let cdata = CodeDescriptor::DoraFct(jit_fct_id);
            code_map.insert(ptr_start, ptr_end, cdata);
        }

        specials.insert(type_params.clone(), jit_fct_id);
    }

    fct_ptr
}

pub fn register_for_mode(mode: MachineMode) -> AnyReg {
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
    if !disassembler::supported() {
        return false;
    }

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

    let fct_name = fct.name_with_params(vm);

    for part in pattern.split(',') {
        if fct_name.contains(part) {
            return true;
        }
    }

    false
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum AnyReg {
    Reg(Reg),
    FReg(FReg),
}

impl AnyReg {
    pub fn is_reg(&self) -> bool {
        match self {
            &AnyReg::Reg(_) => true,
            _ => false,
        }
    }

    pub fn is_freg(&self) -> bool {
        match self {
            &AnyReg::FReg(_) => true,
            _ => false,
        }
    }

    pub fn reg(&self) -> Reg {
        match self {
            &AnyReg::Reg(reg) => reg,
            _ => panic!("fp-register accessed as gp-register."),
        }
    }

    pub fn freg(&self) -> FReg {
        match self {
            &AnyReg::FReg(reg) => reg,
            _ => panic!("gp-register accessed as fp-register."),
        }
    }
}

impl From<Reg> for AnyReg {
    fn from(reg: Reg) -> AnyReg {
        AnyReg::Reg(reg)
    }
}

impl From<FReg> for AnyReg {
    fn from(reg: FReg) -> AnyReg {
        AnyReg::FReg(reg)
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
        jit_fct.instruction_start()
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

        let fct_ptr = jit_fct.instruction_start();

        if let Some(fct_id) = fct_id {
            let fct = vm.fcts.idx(fct_id);
            let fct = fct.read();
            if should_emit_asm(vm, &*fct) {
                disassembler::disassemble(
                    vm,
                    &*fct,
                    &SourceTypeArray::empty(),
                    jit_fct.to_code().expect("still uncompiled"),
                    vm.args.flag_asm_syntax.unwrap_or(AsmSyntax::Att),
                );
            }
        }

        native_stubs.insert_fct(ptr, jit_fct_id);
        fct_ptr
    }
}
