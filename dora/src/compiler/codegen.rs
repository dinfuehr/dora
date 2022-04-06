use crate::boots;
use crate::cannon;
use crate::compiler::{native_stub, NativeFct};
use crate::cpu::{FReg, Reg, FREG_RESULT, REG_RESULT};
use crate::disassembler;
use crate::driver::cmd::{AsmSyntax, CompilerName};
use crate::gc::Address;
use crate::language::sem_analysis::{FctDefinition, FctDefinitionId};
use crate::language::ty::SourceTypeArray;
use crate::mode::MachineMode;
use crate::os;
use crate::vm::{install_code, CodeKind, VM};

pub fn generate(vm: &VM, id: FctDefinitionId, type_params: &SourceTypeArray) -> Address {
    let fct = vm.fcts.idx(id);
    let fct = fct.read();
    generate_fct(vm, &fct, type_params)
}

pub fn generate_fct(vm: &VM, fct: &FctDefinition, type_params: &SourceTypeArray) -> Address {
    debug_assert!(type_params.iter().all(|ty| !ty.contains_type_param(vm)));

    {
        let specials = vm.compiled_fcts.read();

        if let Some(&code_id) = specials.get(&(fct.id, type_params.clone())) {
            let code = vm.code.idx(code_id);
            return code.instruction_start();
        }
    }

    let bc = if fct.is_optimize_immediately {
        CompilerName::Boots
    } else {
        CompilerName::Cannon
    };

    let code_descriptor = match bc {
        CompilerName::Cannon => cannon::compile(vm, &fct, &type_params),
        CompilerName::Boots => boots::compile(vm, &fct, &type_params),
    };

    let code;

    {
        let mut specials = vm.compiled_fcts.write();

        // check whether function was compiled in-between from another thread.
        if let Some(&code_id) = specials.get(&(fct.id, type_params.clone())) {
            let code = vm.code.idx(code_id);
            return code.instruction_start();
        }

        code = install_code(vm, code_descriptor, CodeKind::DoraFct(fct.id));

        // insert the returned Code into the code table to get a CodeId.
        let code_id = {
            let mut code_vec = vm.code.lock();
            let code_id = code_vec.len().into();
            code_vec.push(code.clone());
            code_id
        };

        // We need to insert into CodeMap before releasing the specializations-lock. Otherwise
        // another thread could run that function while the function can't be found in the
        // CodeMap yet. This would lead to a crash e.g. for lazy compilation.
        {
            let mut code_map = vm.code_map.lock();
            code_map.insert(code.object_start(), code.object_end(), code_id);
        }

        specials.insert((fct.id, type_params.clone()), code_id);
    }

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

    code.instruction_start()
}

pub fn register_for_mode(mode: MachineMode) -> AnyReg {
    if mode.is_float() {
        FREG_RESULT.into()
    } else {
        REG_RESULT.into()
    }
}

pub fn should_emit_debug(vm: &VM, fct: &FctDefinition) -> bool {
    if let Some(ref dbg_names) = vm.args.flag_emit_debug {
        fct_pattern_match(vm, fct, dbg_names)
    } else {
        false
    }
}

pub fn should_emit_asm(vm: &VM, fct: &FctDefinition) -> bool {
    if !disassembler::supported() {
        return false;
    }

    if let Some(ref dbg_names) = vm.args.flag_emit_asm {
        fct_pattern_match(vm, fct, dbg_names)
    } else {
        false
    }
}

pub fn should_emit_bytecode(vm: &VM, fct: &FctDefinition) -> bool {
    if let Some(ref dbg_names) = vm.args.flag_emit_bytecode {
        fct_pattern_match(vm, fct, dbg_names)
    } else {
        false
    }
}

pub fn fct_pattern_match(vm: &VM, fct: &FctDefinition, pattern: &str) -> bool {
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

pub fn ensure_native_stub(
    vm: &VM,
    fct_id: Option<FctDefinitionId>,
    native_fct: NativeFct,
) -> Address {
    let mut native_stubs = vm.native_stubs.lock();
    let ptr = native_fct.fctptr;

    if let Some(instruction_start) = native_stubs.find_fct(ptr) {
        instruction_start
    } else {
        let dbg = if let Some(fct_id) = fct_id {
            let fct = vm.fcts.idx(fct_id);
            let fct = fct.read();
            should_emit_debug(vm, &*fct)
        } else {
            false
        };

        let code = native_stub::generate(vm, native_fct, dbg);

        if let Some(fct_id) = fct_id {
            let fct = vm.fcts.idx(fct_id);
            let fct = fct.read();
            if should_emit_asm(vm, &*fct) {
                disassembler::disassemble(
                    vm,
                    &*fct,
                    &SourceTypeArray::empty(),
                    &code,
                    vm.args.flag_asm_syntax.unwrap_or(AsmSyntax::Att),
                );
            }
        }

        native_stubs.insert_fct(ptr, code.instruction_start());
        code.instruction_start()
    }
}
