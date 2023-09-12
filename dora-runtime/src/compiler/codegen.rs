use std::time::Instant;

use crate::boots;
use crate::cannon::{self, CompilationFlags};
use crate::compiler::{runtime_entry_trampoline, NativeFct};
use crate::cpu::{FReg, Reg};
use crate::disassembler;
use crate::gc::Address;
use crate::os;
use crate::vm::CompilerName;
use crate::vm::{display_fct, install_code, CodeDescriptor, CodeKind, VM};
use dora_bytecode::{BytecodeFunction, BytecodeType, BytecodeTypeArray, FunctionId, Location};

pub fn generate_fct(vm: &VM, fct_id: FunctionId, type_params: &BytecodeTypeArray) -> Address {
    debug_assert!(type_params.iter().all(|ty| ty.is_concrete_type()));
    let program_fct = &vm.program.functions[fct_id.0 as usize];

    // Block here if compilation is already in progress.
    if let Some(instruction_start) =
        vm.compilation_database
            .compilation_request(vm, fct_id, type_params.clone())
    {
        return instruction_start;
    }

    let compiler = if program_fct.is_optimize_immediately {
        CompilerName::Boots
    } else {
        CompilerName::Cannon
    };

    let bytecode_fct = program_fct.bytecode.as_ref().expect("bytecode missing");

    let emit_debug = should_emit_debug(vm, fct_id);
    let emit_asm = should_emit_asm(vm, fct_id, compiler);
    let emit_graph = should_emit_graph(vm, fct_id);
    let mut start = None;

    if vm.flags.emit_compiler {
        start = Some(Instant::now());
    }

    let compilation_data = CompilationData {
        bytecode_fct,
        params: BytecodeTypeArray::new(program_fct.params.clone()),
        has_variadic_parameter: program_fct.is_variadic,
        return_type: program_fct.return_type.clone(),
        type_params: type_params.clone(),
        loc: program_fct.loc,

        emit_debug,
        emit_code_comments: emit_asm,
        emit_graph,
    };

    let compilation_flags = CompilationFlags::jit();

    let (code_descriptor, code_kind) = match compiler {
        CompilerName::Cannon => (
            cannon::compile(vm, compilation_data, compilation_flags),
            CodeKind::BaselineFct(fct_id),
        ),
        CompilerName::Boots => (
            boots::compile(vm, compilation_data, compilation_flags),
            CodeKind::OptimizedFct(fct_id),
        ),
    };

    let code = install_code(vm, code_descriptor, code_kind);

    // We need to insert into CodeMap before releasing the compilation-lock. Otherwise
    // another thread could run that function while the function can't be found in the
    // CodeMap yet. This would lead to a crash e.g. for lazy compilation.
    let code_id = vm.add_code(code.clone());

    // Mark compilation as finished and resume threads waiting for compilation.
    vm.compilation_database
        .finish_compilation(fct_id, type_params.clone(), code_id);

    if vm.flags.emit_compiler {
        let duration = start.expect("missing start time").elapsed();
        println!(
            "compile {} using {} in {}ms.",
            display_fct(vm, fct_id),
            compiler,
            (duration.as_micros() as f64) / 1000.0
        );
    }

    if vm.flags.enable_perf {
        let name = display_fct(vm, fct_id);
        os::perf::register_with_perf(&code, &name);
    }

    if emit_asm {
        disassembler::disassemble(vm, fct_id, &type_params, &code);
    }

    code.instruction_start()
}

pub fn generate_thunk(
    vm: &VM,
    trait_fct_id: FunctionId,
    trait_object_ty: BytecodeType,
    type_params: &BytecodeTypeArray,
    bytecode_fct: BytecodeFunction,
) -> Address {
    debug_assert!(type_params.iter().all(|ty| ty.is_concrete_type()));
    let trait_fct = &vm.program.functions[trait_fct_id.0 as usize];

    // Block here if compilation is already in progress.
    if let Some(instruction_start) =
        vm.compilation_database
            .compilation_request(vm, trait_fct_id, type_params.clone())
    {
        return instruction_start;
    }

    let compiler = if trait_fct.is_optimize_immediately {
        CompilerName::Boots
    } else {
        CompilerName::Cannon
    };

    let emit_debug = should_emit_debug(vm, trait_fct_id);
    let emit_asm = should_emit_asm(vm, trait_fct_id, compiler);
    let mut start = None;

    if vm.flags.emit_compiler {
        start = Some(Instant::now());
    }

    let mut params = trait_fct.params.clone();
    assert_eq!(params[0], BytecodeType::This);
    params[0] = trait_object_ty;

    let params = BytecodeTypeArray::new(params);
    let return_type = trait_fct.return_type.clone();
    let has_variadic_parameter = trait_fct.is_variadic;

    let compilation_data = CompilationData {
        bytecode_fct: &bytecode_fct,
        params,
        has_variadic_parameter,
        return_type,
        type_params: type_params.clone(),
        loc: trait_fct.loc,

        emit_debug,
        emit_code_comments: emit_asm,
        emit_graph: false,
    };

    let code_descriptor = match compiler {
        CompilerName::Cannon => cannon::compile(vm, compilation_data, CompilationFlags::jit()),
        CompilerName::Boots => unimplemented!(),
    };

    let code = install_code(vm, code_descriptor, CodeKind::BaselineFct(trait_fct_id));

    // We need to insert into CodeMap before releasing the compilation-lock. Otherwise
    // another thread could run that function while the function can't be found in the
    // CodeMap yet. This would lead to a crash e.g. for lazy compilation.
    let code_id = vm.add_code(code.clone());

    // Mark compilation as finished and resume threads waiting for compilation.
    vm.compilation_database
        .finish_compilation(trait_fct_id, type_params.clone(), code_id);

    if vm.flags.emit_compiler {
        let duration = start.expect("missing start time").elapsed();
        println!(
            "compile {} using {} in {}ms.",
            display_fct(vm, trait_fct_id),
            compiler,
            (duration.as_micros() as f64) / 1000.0
        );
    }

    if vm.flags.enable_perf {
        let name = display_fct(vm, trait_fct_id);
        os::perf::register_with_perf(&code, &name);
    }

    if emit_asm {
        disassembler::disassemble(vm, trait_fct_id, &type_params, &code);
    }

    code.instruction_start()
}

pub fn generate_bytecode(vm: &VM, compilation_data: CompilationData) -> CodeDescriptor {
    cannon::compile(vm, compilation_data, CompilationFlags::jit())
}

pub fn should_emit_debug(vm: &VM, fct_id: FunctionId) -> bool {
    if let Some(ref dbg_names) = vm.flags.emit_debug {
        fct_pattern_match(vm, fct_id, dbg_names)
    } else {
        false
    }
}

pub fn should_emit_asm(vm: &VM, fct_id: FunctionId, compiler: CompilerName) -> bool {
    if !disassembler::supported() {
        return false;
    }

    if compiler == CompilerName::Boots && vm.flags.emit_asm_boots {
        return true;
    }

    if let Some(ref dbg_names) = vm.flags.emit_asm {
        fct_pattern_match(vm, fct_id, dbg_names)
    } else {
        false
    }
}

pub fn should_emit_graph(vm: &VM, fct_id: FunctionId) -> bool {
    if let Some(ref names) = vm.flags.emit_graph {
        fct_pattern_match(vm, fct_id, names)
    } else {
        false
    }
}

pub fn fct_pattern_match(vm: &VM, fct_id: FunctionId, pattern: &str) -> bool {
    if pattern == "all" {
        return true;
    }

    let fct_name = display_fct(vm, fct_id);

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

pub fn ensure_runtime_entry_trampoline(
    vm: &VM,
    fct_id: Option<FunctionId>,
    native_fct: NativeFct,
) -> Address {
    vm.native_methods.lock_trampolines(|native_stubs| {
        let ptr = native_fct.fctptr;

        if let Some(instruction_start) = native_stubs.find_fct(ptr) {
            instruction_start
        } else {
            let dbg = if let Some(fct_id) = fct_id {
                should_emit_debug(vm, fct_id)
            } else {
                false
            };

            let code = runtime_entry_trampoline::generate(vm, native_fct, dbg);

            if let Some(fct_id) = fct_id {
                if should_emit_asm(vm, fct_id, CompilerName::Cannon) {
                    disassembler::disassemble(vm, fct_id, &BytecodeTypeArray::empty(), &code);
                }
            }

            native_stubs.insert_fct(ptr, code.instruction_start());
            code.instruction_start()
        }
    })
}

pub struct CompilationData<'a> {
    pub bytecode_fct: &'a BytecodeFunction,
    pub params: BytecodeTypeArray,
    pub has_variadic_parameter: bool,
    pub return_type: BytecodeType,
    pub type_params: BytecodeTypeArray,
    pub loc: Location,

    pub emit_debug: bool,
    pub emit_code_comments: bool,
    pub emit_graph: bool,
}
