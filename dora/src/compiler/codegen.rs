use dora_parser::Position;

use crate::bytecode::BytecodeFunction;
use crate::cannon::{self, CompilationFlags};
use crate::compiler::{dora_exit_stubs, NativeFct};
use crate::cpu::{FReg, Reg};
use crate::disassembler;
use crate::driver::cmd::AsmSyntax;
use crate::gc::Address;
use crate::language::sem_analysis::{FctDefinition, FctDefinitionId};
use crate::language::ty::{SourceType, SourceTypeArray};
use crate::os;
use crate::vm::{install_code, CodeKind, VM};

pub fn generate(vm: &VM, id: FctDefinitionId, type_params: &SourceTypeArray) -> Address {
    let fct = vm.fcts.idx(id);
    let fct = fct.read();
    generate_fct(vm, &fct, type_params)
}

pub fn generate_fct(vm: &VM, fct: &FctDefinition, type_params: &SourceTypeArray) -> Address {
    debug_assert!(type_params.iter().all(|ty| ty.is_concrete_type(vm)));

    // Block here if compilation is already in progress.
    if let Some(instruction_start) =
        vm.compilation_database
            .compilation_request(vm, fct.id(), type_params.clone())
    {
        return instruction_start;
    }

    let bytecode_fct = fct.bytecode.as_ref().expect("bytecode missing");

    let emit_debug = should_emit_debug(vm, fct);
    let emit_asm = should_emit_asm(vm, fct);

    let code_descriptor = {
        let pos = fct.pos;
        let params = fct.params_with_self();
        let params = SourceTypeArray::with(params.to_vec());
        let return_type = fct.return_type.clone();
        let has_variadic_parameter = fct.is_variadic;

        let compilation_data = CompilationData {
            bytecode_fct,
            params,
            has_variadic_parameter,
            return_type,
            type_params,
            pos,

            emit_debug,
            emit_code_comments: emit_asm,
        };

        cannon::compile(vm, compilation_data, CompilationFlags::jit())
    };

    let code = install_code(vm, code_descriptor, CodeKind::DoraFct(fct.id()));

    // We need to insert into CodeMap before releasing the compilation-lock. Otherwise
    // another thread could run that function while the function can't be found in the
    // CodeMap yet. This would lead to a crash e.g. for lazy compilation.
    let code_id = vm.add_code(code.clone());

    // Mark compilation as finished and resume threads waiting for compilation.
    vm.compilation_database
        .finish_compilation(fct.id(), type_params.clone(), code_id);

    if vm.args.flag_enable_perf {
        os::perf::register_with_perf(&code, vm, fct.ast.name);
    }

    if emit_asm {
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

pub fn fct_pattern_match(vm: &VM, fct: &FctDefinition, pattern: &str) -> bool {
    if pattern == "all" {
        return true;
    }

    let fct_name = fct.display_name(vm);

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

    #[allow(dead_code)]
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

        let code = dora_exit_stubs::generate(vm, native_fct, dbg);

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

pub struct CompilationData<'a> {
    pub bytecode_fct: &'a BytecodeFunction,
    pub params: SourceTypeArray,
    pub has_variadic_parameter: bool,
    pub return_type: SourceType,
    pub type_params: &'a SourceTypeArray,
    pub pos: Position,

    pub emit_debug: bool,
    pub emit_code_comments: bool,
}
