use std::sync::Arc;
use std::time::Instant;

use crate::boots;
use crate::cannon::{self, CompilationFlags};
use crate::compiler::{runtime_entry_trampoline, NativeFct};
use crate::cpu::{FReg, Reg};
use crate::disassembler;
use crate::gc::Address;
use crate::os;
use crate::vm::{
    display_fct, display_ty_without_type_params, install_code, Code, CodeDescriptor, CodeId,
    CodeKind, CompilerName, VM,
};
use dora_bytecode::{
    dump_stdout, BytecodeFunction, BytecodeType, BytecodeTypeArray, FunctionData, FunctionId,
    Location,
};

pub fn compile_fct_jit(vm: &VM, fct_id: FunctionId, type_params: &BytecodeTypeArray) -> Address {
    // Block here if compilation is already in progress.
    if let Some(instruction_start) =
        vm.compilation_database
            .compilation_request(vm, fct_id, type_params.clone())
    {
        return instruction_start;
    }

    let program_fct = &vm.program.functions[fct_id.0 as usize];
    let params = BytecodeTypeArray::new(program_fct.params.clone());
    let bytecode_fct = program_fct.bytecode.as_ref().expect("missing bytecode");

    assert_ne!(Some(program_fct.package_id), vm.program.boots_package_id);
    let compiler = select_compiler(vm, fct_id, program_fct);

    let (code_id, code) = compile_fct_to_code(
        vm,
        fct_id,
        program_fct,
        params,
        bytecode_fct,
        type_params,
        compiler,
    );

    // Mark compilation as finished and resume threads waiting for compilation.
    vm.compilation_database
        .finish_compilation(fct_id, type_params.clone(), code_id);

    code.instruction_start()
}

pub fn compile_fct_aot(vm: &VM, fct_id: FunctionId, type_params: &BytecodeTypeArray) -> Address {
    let program_fct = &vm.program.functions[fct_id.0 as usize];
    let params = BytecodeTypeArray::new(program_fct.params.clone());
    let bytecode_fct = program_fct.bytecode.as_ref().expect("missing bytecode");
    let compiler = CompilerName::Cannon;

    let (code_id, code) = compile_fct_to_code(
        vm,
        fct_id,
        program_fct,
        params,
        bytecode_fct,
        type_params,
        compiler,
    );
    vm.compilation_database
        .compile_aot(fct_id, type_params.clone(), code_id);
    code.instruction_start()
}

pub(super) fn compile_fct_to_code(
    vm: &VM,
    fct_id: FunctionId,
    program_fct: &FunctionData,
    params: BytecodeTypeArray,
    bytecode_fct: &BytecodeFunction,
    type_params: &BytecodeTypeArray,
    compiler: CompilerName,
) -> (CodeId, Arc<Code>) {
    let (code_descriptor, compiler, code_kind) = compile_fct_to_descriptor(
        vm,
        fct_id,
        program_fct,
        params,
        bytecode_fct,
        type_params,
        compiler,
    );
    let code = install_code(vm, code_descriptor, code_kind);

    // We need to insert into CodeMap before releasing the compilation-lock. Otherwise
    // another thread could run that function while the function can't be found in the
    // CodeMap yet. This would lead to a crash e.g. for lazy compilation.
    let code_id = vm.add_code(code.clone());

    if should_emit_asm(vm, fct_id, compiler) {
        disassembler::disassemble(vm, fct_id, &type_params, &code);
    }

    if vm.flags.enable_perf {
        let name = display_fct(vm, fct_id);
        os::perf::register_with_perf(&code, &name);
    }

    (code_id, code)
}

fn compile_fct_to_descriptor(
    vm: &VM,
    fct_id: FunctionId,
    program_fct: &FunctionData,
    params: BytecodeTypeArray,
    bytecode_fct: &BytecodeFunction,
    type_params: &BytecodeTypeArray,
    compiler: CompilerName,
) -> (CodeDescriptor, CompilerName, CodeKind) {
    debug_assert!(type_params.iter().all(|ty| ty.is_concrete_type()));

    let emit_bytecode = should_emit_bytecode(vm, fct_id, compiler);

    if emit_bytecode {
        dump_stdout(&vm.program, program_fct, &bytecode_fct);
    }

    let emit_debug = should_emit_debug(vm, fct_id, compiler);
    let emit_asm = should_emit_asm(vm, fct_id, compiler);
    let emit_graph = should_emit_graph(vm, fct_id);
    let mut start = None;

    if vm.flags.emit_compiler {
        start = Some(Instant::now());
    }

    let compilation_data = CompilationData {
        bytecode_fct,
        params,
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

    if vm.flags.emit_compiler {
        let duration = start.expect("missing start time").elapsed();
        let mut name = display_fct(vm, fct_id);
        if type_params.len() > 0 {
            name.push_str(" with [");
            let mut first = true;

            for ty in type_params.iter() {
                if !first {
                    name.push_str(", ");
                }

                name.push_str(&display_ty_without_type_params(vm, &ty));
                first = false;
            }

            name.push_str("]");
        }
        println!(
            "compile {} using {} in {:.3}ms.",
            name,
            compiler,
            duration.as_secs_f32() * 1000.0
        );
    }

    (code_descriptor, compiler, code_kind)
}

pub(super) fn select_compiler(vm: &VM, fct_id: FunctionId, fct: &FunctionData) -> CompilerName {
    if vm.flags.always_boots && fct.package_id == vm.program.program_package_id {
        return CompilerName::Boots;
    }

    if let Some(ref use_boots) = vm.flags.use_boots {
        if fct_pattern_match(vm, fct_id, use_boots) {
            return CompilerName::Boots;
        }
    }

    if fct.is_optimize_immediately {
        CompilerName::Boots
    } else {
        CompilerName::Cannon
    }
}

fn should_emit_debug(vm: &VM, fct_id: FunctionId, compiler: CompilerName) -> bool {
    if compiler == CompilerName::Boots && vm.flags.emit_debug_boots {
        return true;
    }

    if let Some(ref dbg_names) = vm.flags.emit_debug {
        fct_pattern_match(vm, fct_id, dbg_names)
    } else {
        false
    }
}

fn should_emit_bytecode(vm: &VM, fct_id: FunctionId, compiler: CompilerName) -> bool {
    if !disassembler::supported() {
        return false;
    }

    if compiler == CompilerName::Boots && vm.flags.emit_bytecode_boots {
        return true;
    }

    if let Some(ref dbg_names) = vm.flags.emit_bytecode_compiler {
        fct_pattern_match(vm, fct_id, dbg_names)
    } else {
        false
    }
}

fn should_emit_asm(vm: &VM, fct_id: FunctionId, compiler: CompilerName) -> bool {
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

fn should_emit_graph(vm: &VM, fct_id: FunctionId) -> bool {
    if let Some(ref names) = vm.flags.emit_graph {
        fct_pattern_match(vm, fct_id, names)
    } else {
        false
    }
}

fn fct_pattern_match(vm: &VM, fct_id: FunctionId, pattern: &str) -> bool {
    if pattern == "all" {
        return true;
    }

    let fct_name = display_fct(vm, fct_id);

    for part in pattern.split(';') {
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
                should_emit_debug(vm, fct_id, CompilerName::Cannon)
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
