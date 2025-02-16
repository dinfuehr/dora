use std::sync::Arc;
use std::time::Instant;

use crate::boots;
use crate::cannon;
use crate::compiler::{runtime_entry_trampoline, CompilationMode, NativeFct};
use crate::cpu::{FReg, Reg};
use crate::disassembler;
use crate::gc::Address;
use crate::os;
use crate::vm::{install_code, Code, CodeDescriptor, CodeId, CodeKind, Compiler, VM};
use dora_bytecode::{
    display_fct, display_ty_array, display_ty_without_type_params, dump_stdout, BytecodeFunction,
    BytecodeTraitType, BytecodeType, BytecodeTypeArray, FunctionData, FunctionId, FunctionKind,
    ImplId, Location, TypeParamMode,
};

#[derive(Clone, Copy)]
pub enum CompilerInvocation {
    Cannon,
    Boots(Address),
}

impl CompilerInvocation {
    fn to_compiler(&self) -> Compiler {
        match self {
            CompilerInvocation::Cannon => Compiler::Cannon,
            CompilerInvocation::Boots(..) => Compiler::Boots,
        }
    }
}

pub fn compile_fct_jit(vm: &VM, fct_id: FunctionId, type_params: &BytecodeTypeArray) -> Address {
    // Block here if compilation is already in progress.
    if let Some(instruction_start) =
        vm.compilation_database
            .compilation_request(vm, fct_id, type_params.clone())
    {
        return instruction_start;
    }

    let program_fct = vm.fct(fct_id);
    let params = BytecodeTypeArray::new(program_fct.params.clone());
    let (bytecode_fct, specialize_self) = get_bytecode(vm, program_fct).expect("missing bytecode");

    assert_ne!(Some(program_fct.package_id), vm.program.boots_package_id);
    let compiler = select_compiler(vm, fct_id, program_fct);
    let compiler = match compiler {
        Compiler::Cannon => CompilerInvocation::Cannon,
        Compiler::Boots => {
            let compile_fctptr = vm.known.boots_compile_fct_address();
            CompilerInvocation::Boots(compile_fctptr)
        }
    };

    let (code_id, code) = compile_fct_to_code(
        vm,
        fct_id,
        program_fct,
        params,
        program_fct.return_type.clone(),
        bytecode_fct,
        type_params,
        specialize_self,
        compiler,
        vm.flags.emit_compiler,
        CompilationMode::Jit,
    );

    // Mark compilation as finished and resume threads waiting for compilation.
    vm.compilation_database
        .finish_compilation(fct_id, type_params.clone(), code_id);

    code.instruction_start()
}

pub fn compile_fct_aot(
    vm: &VM,
    fct_id: FunctionId,
    type_params: &BytecodeTypeArray,
    compiler: CompilerInvocation,
    mode: CompilationMode,
) -> (CodeId, Arc<Code>) {
    let program_fct = vm.fct(fct_id);
    let params = BytecodeTypeArray::new(program_fct.params.clone());
    let (bytecode_fct, specialize_self) = get_bytecode(vm, program_fct).expect("missing bytecode");

    let (code_id, code) = compile_fct_to_code(
        vm,
        fct_id,
        program_fct,
        params,
        program_fct.return_type.clone(),
        bytecode_fct,
        type_params,
        specialize_self,
        compiler,
        false,
        mode,
    );
    (code_id, code)
}

pub struct SpecializeSelf {
    pub impl_id: ImplId,
    pub container_type_params: usize,
    pub trait_ty: BytecodeTraitType,
    pub extended_ty: BytecodeType,
}

pub fn get_bytecode<'a>(
    vm: &'a VM,
    program_fct: &'a FunctionData,
) -> Option<(&'a BytecodeFunction, Option<SpecializeSelf>)> {
    match program_fct.bytecode.as_ref() {
        Some(bytecode_fct) => Some((bytecode_fct, None)),
        None => {
            let trait_method_id = program_fct.trait_method_impl?;
            let trait_method = vm.fct(trait_method_id);

            let program_fct_impl_id = match program_fct.kind {
                FunctionKind::Impl(impl_id) => impl_id,
                _ => unreachable!(),
            };

            let bytecode_fct = trait_method.bytecode.as_ref()?;

            let program_fct_impl = vm.impl_(program_fct_impl_id);

            let specialize_self = SpecializeSelf {
                impl_id: program_fct_impl_id,
                container_type_params: program_fct_impl.type_params.type_param_count(),
                trait_ty: program_fct_impl.trait_ty.clone(),
                extended_ty: program_fct_impl.extended_ty.clone(),
            };

            Some((bytecode_fct, Some(specialize_self)))
        }
    }
}

pub(super) fn compile_fct_to_code(
    vm: &VM,
    fct_id: FunctionId,
    program_fct: &FunctionData,
    params: BytecodeTypeArray,
    return_type: BytecodeType,
    bytecode_fct: &BytecodeFunction,
    type_params: &BytecodeTypeArray,
    specialize_self: Option<SpecializeSelf>,
    compiler: CompilerInvocation,
    emit_compiler: bool,
    mode: CompilationMode,
) -> (CodeId, Arc<Code>) {
    let (code_descriptor, compiler, code_kind) = compile_fct_to_descriptor(
        vm,
        fct_id,
        program_fct,
        params,
        return_type,
        bytecode_fct,
        type_params,
        specialize_self,
        compiler,
        emit_compiler,
        mode,
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
        let name = display_fct(&vm.program, fct_id);
        os::perf::register_with_perf(&code, &name);
    }

    (code_id, code)
}

fn compile_fct_to_descriptor(
    vm: &VM,
    fct_id: FunctionId,
    program_fct: &FunctionData,
    params: BytecodeTypeArray,
    return_type: BytecodeType,
    bytecode_fct: &BytecodeFunction,
    type_params: &BytecodeTypeArray,
    specialize_self: Option<SpecializeSelf>,
    compiler: CompilerInvocation,
    emit_compiler: bool,
    mode: CompilationMode,
) -> (CodeDescriptor, Compiler, CodeKind) {
    debug_assert!(type_params.iter().all(|ty| ty.is_concrete_type()));

    // let fct = vm.fct(fct_id);
    // if type_params.len() != fct.type_params.type_param_count() {
    //     println!("wrong type params for {}", display_fct(&vm.program, fct_id));
    //     println!(
    //         " type params are {}",
    //         display_ty_array(&vm.program, &params)
    //     );
    //     println!(" expected {},", fct.type_params.type_param_count());
    //     println!(" but got {} .", type_params.len());
    // }
    // assert_eq!(type_params.len(), fct.type_params.type_param_count());

    let emit_bytecode = should_emit_bytecode(vm, fct_id, compiler.to_compiler());

    if emit_bytecode {
        println!(
            "Compile bytecode for {} with {} as type params:",
            display_fct(&vm.program, fct_id),
            display_ty_array(&vm.program, type_params)
        );
        dump_stdout(
            &vm.program,
            &bytecode_fct,
            TypeParamMode::TypeParams(&program_fct.type_params),
        );
    }

    let emit_debug = should_emit_debug(vm, fct_id, compiler.to_compiler());
    let emit_asm = should_emit_asm(vm, fct_id, compiler.to_compiler());
    let (emit_graph, emit_html) = should_emit_graph(vm, fct_id);
    let mut start = None;

    if emit_compiler {
        start = Some(Instant::now());
    }

    let compilation_data = CompilationData {
        bytecode_fct,
        params,
        has_variadic_parameter: program_fct.is_variadic,
        return_type,
        fct_id,
        type_params: type_params.clone(),
        specialize_self,
        loc: program_fct.loc,

        emit_debug,
        emit_code_comments: emit_asm,
        emit_graph,
        emit_html,
    };

    let (code_descriptor, code_kind) = match compiler {
        CompilerInvocation::Cannon => (
            cannon::compile(vm, compilation_data, mode),
            CodeKind::BaselineFct(fct_id),
        ),
        CompilerInvocation::Boots(compile_address) => (
            boots::compile(vm, compile_address, compilation_data, mode),
            CodeKind::OptimizedFct(fct_id),
        ),
    };

    if emit_compiler {
        let duration = start.expect("missing start time").elapsed();
        let mut name = display_fct(&vm.program, fct_id);
        if type_params.len() > 0 {
            name.push_str(" with [");
            let mut first = true;

            for ty in type_params.iter() {
                if !first {
                    name.push_str(", ");
                }

                name.push_str(&display_ty_without_type_params(&vm.program, &ty));
                first = false;
            }

            name.push_str("]");
        }
        println!(
            "compile {} using {} in {:.3}ms.",
            name,
            compiler.to_compiler(),
            duration.as_secs_f32() * 1000.0
        );
    }

    (code_descriptor, compiler.to_compiler(), code_kind)
}

pub(super) fn select_compiler(vm: &VM, fct_id: FunctionId, fct: &FunctionData) -> Compiler {
    if vm.flags.always_boots {
        return Compiler::Boots;
    }

    if let Some(ref use_boots) = vm.flags.use_boots {
        if fct_pattern_match(vm, fct_id, use_boots).0 {
            return Compiler::Boots;
        }
    }

    if fct.is_optimize_immediately {
        Compiler::Boots
    } else {
        Compiler::Cannon
    }
}

fn should_emit_debug(vm: &VM, fct_id: FunctionId, compiler: Compiler) -> bool {
    if compiler == Compiler::Boots && vm.flags.emit_debug_boots {
        return true;
    }

    if let Some(ref dbg_names) = vm.flags.emit_debug {
        fct_pattern_match(vm, fct_id, dbg_names).0
    } else {
        false
    }
}

fn should_emit_bytecode(vm: &VM, fct_id: FunctionId, compiler: Compiler) -> bool {
    if !disassembler::supported() {
        return false;
    }

    if compiler == Compiler::Boots && vm.flags.emit_bytecode_boots {
        return true;
    }

    if let Some(ref dbg_names) = vm.flags.emit_bytecode_compiler {
        fct_pattern_match(vm, fct_id, dbg_names).0
    } else {
        false
    }
}

fn should_emit_asm(vm: &VM, fct_id: FunctionId, compiler: Compiler) -> bool {
    if !disassembler::supported() {
        return false;
    }

    if compiler == Compiler::Boots && vm.flags.emit_asm_boots {
        return true;
    }

    if let Some(ref dbg_names) = vm.flags.emit_asm {
        fct_pattern_match(vm, fct_id, dbg_names).0
    } else {
        false
    }
}

fn should_emit_graph(vm: &VM, fct_id: FunctionId) -> (bool, bool) {
    if let Some(ref names) = vm.flags.emit_graph {
        let (matches, has_plus) = fct_pattern_match(vm, fct_id, names);
        (matches && !has_plus, matches && has_plus)
    } else {
        (false, false)
    }
}

fn fct_pattern_match(vm: &VM, fct_id: FunctionId, pattern: &str) -> (bool, bool) {
    if pattern == "all" || pattern == "*" {
        return (true, false);
    } else if pattern == "all+" || pattern == "*+" {
        return (true, true);
    }

    let fct_name = display_fct(&vm.program, fct_id);

    for part in pattern.split(';') {
        let (part, plus) = if part.ends_with('+') {
            (&part[0..part.len() - 1], true)
        } else {
            (part, false)
        };

        if fct_name.ends_with(part) {
            return (true, plus);
        }
    }

    (false, false)
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
            let code = compile_runtime_entry_trampoline(vm, fct_id, native_fct);
            native_stubs.insert_fct(ptr, code.instruction_start());
            code.instruction_start()
        }
    })
}

pub fn compile_runtime_entry_trampoline(
    vm: &VM,
    fct_id: Option<FunctionId>,
    native_fct: NativeFct,
) -> Arc<Code> {
    let dbg = if let Some(fct_id) = fct_id {
        should_emit_debug(vm, fct_id, Compiler::Cannon)
    } else {
        false
    };

    let code = runtime_entry_trampoline::generate(vm, native_fct, dbg);

    if let Some(fct_id) = fct_id {
        if should_emit_asm(vm, fct_id, Compiler::Cannon) {
            disassembler::disassemble(vm, fct_id, &BytecodeTypeArray::empty(), &code);
        }
    }

    code
}

pub struct CompilationData<'a> {
    pub bytecode_fct: &'a BytecodeFunction,
    pub params: BytecodeTypeArray,
    pub has_variadic_parameter: bool,
    pub return_type: BytecodeType,
    pub fct_id: FunctionId,
    pub type_params: BytecodeTypeArray,
    pub specialize_self: Option<SpecializeSelf>,
    pub loc: Location,

    pub emit_debug: bool,
    pub emit_code_comments: bool,
    pub emit_graph: bool,
    pub emit_html: bool,
}
