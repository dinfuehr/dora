use std::collections::HashMap;
use std::sync::Arc;
use std::time::Instant;

use dora_bytecode::{
    BytecodeFunction, BytecodeType, BytecodeTypeArray, FunctionData, FunctionId, PackageId,
    Program, display_fct_specialized,
};

use crate::boots;
use crate::cannon;
use crate::compiler::aot::{
    AotNativeLookup, CompiledFunction, CompiledFunctionTarget, CompiledTransitiveClosure,
    aot_compiled_function_name, should_emit_graph,
};
use crate::compiler::closure::{TraitObjectThunk, TransitiveClosure, compute_transitive_closure};
use crate::compiler::runtime_entry_trampoline;
use crate::compiler::{
    CompilationData, CompilationMode, CompilerInvocation, NativeFct, NativeFctKind, SpecializeSelf,
    get_bytecode, trait_object_thunk,
};
use crate::gc::{Address, formatted_size};
use crate::os;
use crate::vm::{
    AotShapeKey, BytecodeTypeExt, Code, CodeDescriptor, CodeKind, LazyCompilationSite, VM,
    execute_on_main, install_code_stub,
};

pub fn compile_boots_compiler_jit(vm: &VM) {
    if vm.has_boots() {
        let package_id = vm
            .program
            .boots_package_id
            .expect("boots package is missing");
        let entry_id = vm.known.boots_compile_fct_id();
        let tests = compute_tests(&vm.program, package_id);
        let tc = compute_transitive_closure(&vm.program, entry_id, &tests, vm.flags.emit_compiler);
        let (stage1_compiler_address, stage1_itc) = stage1_compiler(vm, &tc, entry_id);

        let (boots_compiler_address, itc) = if vm.flags.bootstrap_compiler {
            execute_on_main(|| {
                let (stage2_compiler_address, stage2_itc) =
                    stage2_compiler(vm, &tc, entry_id, stage1_compiler_address);
                let (stage3_compiler_address, stage3_itc) =
                    stage3_compiler(vm, &tc, entry_id, stage2_compiler_address);
                assert_builds_identical(vm, &stage2_itc, &stage3_itc);

                (stage3_compiler_address, stage3_itc)
            })
        } else {
            (stage1_compiler_address, stage1_itc)
        };

        assert!(
            vm.known
                .boots_compile_fct_address
                .set(boots_compiler_address)
                .is_ok()
        );

        let tests = compute_test_addresses(&itc, tests);
        assert!(vm.known.boots_test_addresses.set(tests).is_ok());
    }
}

fn stage1_compiler(
    vm: &VM,
    tc: &TransitiveClosure,
    entry_id: FunctionId,
) -> (Address, InstalledTransitiveClosure) {
    let (compile_address, itc) = compiler_stage_n(
        vm,
        tc,
        entry_id,
        "stage1",
        CompilerInvocation::Cannon,
        CompilationMode::Stage1,
    );
    (compile_address, itc)
}

fn stage2_compiler(
    vm: &VM,
    tc: &TransitiveClosure,
    entry_id: FunctionId,
    stage1_compiler_address: Address,
) -> (Address, InstalledTransitiveClosure) {
    compiler_stage_n(
        vm,
        tc,
        entry_id,
        "stage2",
        CompilerInvocation::Boots(stage1_compiler_address),
        CompilationMode::Stage2,
    )
}

fn stage3_compiler(
    vm: &VM,
    tc: &TransitiveClosure,
    entry_id: FunctionId,
    stage2_compiler_address: Address,
) -> (Address, InstalledTransitiveClosure) {
    compiler_stage_n(
        vm,
        tc,
        entry_id,
        "stage3",
        CompilerInvocation::Boots(stage2_compiler_address),
        CompilationMode::Stage3,
    )
}

fn compiler_stage_n(
    vm: &VM,
    tc: &TransitiveClosure,
    entry_id: FunctionId,
    name: &str,
    compiler: CompilerInvocation,
    mode: CompilationMode,
) -> (Address, InstalledTransitiveClosure) {
    let start = Instant::now();
    let start_code_size = vm.gc.current_code_size();
    let native_lookup = AotNativeLookup::from_program(&vm.program, tc, mode);
    let ctx = BootstrapCodegenContext {
        vm,
        program: &vm.program,
        native_lookup: &native_lookup,
        compiler,
        mode,
        dora_entry_trampoline_address: vm.native_methods.dora_entry_trampoline(),
        emit_graph: vm.flags.emit_graph.as_deref(),
        emit_graph_after_each_pass: vm.flags.emit_graph_after_each_pass,
    };
    assert!(matches!(
        ctx.mode,
        CompilationMode::Stage1 | CompilationMode::Stage2 | CompilationMode::Stage3
    ));
    let ctc = compile_transitive_closure(&ctx, tc);
    let mut itc = install_compiled_transitive_closure(&ctx, ctc);
    prepare_lazy_call_sites(&ctx, &itc);
    // Lazy call preparation patches installed code in place; keep descriptors
    // in sync so stage2/stage3 comparison sees the final executable bytes.
    sync_installed_code(&mut itc);
    prepare_virtual_method_tables(ctx.vm, tc, &itc);
    let compile_address = itc.get_address(entry_id).expect("missing entry point");
    let duration = start.elapsed();
    let code_size = vm.gc.current_code_size() - start_code_size;

    if vm.flags.emit_compiler {
        println!(
            "compiled all of boots ({}) in {:.2}ms ({} bytes)",
            name,
            duration.as_secs_f32() * 1000.0f32,
            formatted_size(code_size),
        );
    }

    (compile_address, itc)
}

struct BootstrapCodegenContext<'a> {
    vm: &'a VM,
    program: &'a Program,
    native_lookup: &'a AotNativeLookup,
    compiler: CompilerInvocation,
    mode: CompilationMode,
    dora_entry_trampoline_address: Address,
    emit_graph: Option<&'a str>,
    emit_graph_after_each_pass: bool,
}

fn assert_builds_identical(
    vm: &VM,
    stage2: &InstalledTransitiveClosure,
    stage3: &InstalledTransitiveClosure,
) {
    assert_eq!(
        stage2.compiled.functions.len(),
        stage3.compiled.functions.len()
    );

    for (stage2_entry, stage3_entry) in stage2
        .compiled
        .functions
        .iter()
        .zip(&stage3.compiled.functions)
    {
        assert_eq!(
            &stage2_entry.code.code,
            &stage3_entry.code.code,
            "stage2 and stage3 differ in function {}",
            aot_compiled_function_name(&vm.program, stage2_entry)
        );
    }
}

struct InstalledTransitiveClosure {
    compiled: CompiledTransitiveClosure,
    installed_codes: Vec<Arc<Code>>,
    function_addresses: HashMap<(FunctionId, BytecodeTypeArray), Address>,
    trait_object_thunk_addresses: HashMap<TraitObjectThunk, Address>,
}

impl InstalledTransitiveClosure {
    fn new(compiled: CompiledTransitiveClosure) -> InstalledTransitiveClosure {
        let len = compiled.functions.len();
        InstalledTransitiveClosure {
            compiled,
            installed_codes: Vec::with_capacity(len),
            function_addresses: HashMap::new(),
            trait_object_thunk_addresses: HashMap::new(),
        }
    }

    fn get_address(&self, id: FunctionId) -> Option<Address> {
        self.function_addresses
            .get(&(id, BytecodeTypeArray::empty()))
            .cloned()
    }

    fn set_address(&mut self, target: CompiledFunctionTarget, address: Address) {
        let existing = match target {
            CompiledFunctionTarget::Function {
                fct_id,
                type_params,
            } => self
                .function_addresses
                .insert((fct_id, type_params), address),

            CompiledFunctionTarget::TraitObjectThunk(thunk) => {
                self.trait_object_thunk_addresses.insert(thunk, address)
            }
        };
        assert!(existing.is_none());
    }

    fn get_function_address(
        &self,
        fct_id: FunctionId,
        type_params: &BytecodeTypeArray,
    ) -> Option<Address> {
        self.function_addresses
            .get(&(fct_id, type_params.clone()))
            .cloned()
    }

    fn get_trait_object_thunk_address(&self, thunk: &TraitObjectThunk) -> Option<Address> {
        self.trait_object_thunk_addresses.get(thunk).cloned()
    }

    fn function_addresses_len(&self) -> usize {
        self.function_addresses.len()
    }
}

fn compile_transitive_closure(
    ctx: &BootstrapCodegenContext<'_>,
    tc: &TransitiveClosure,
) -> CompiledTransitiveClosure {
    let mut ctc = CompiledTransitiveClosure::new();
    compile_functions(ctx, tc, &mut ctc);
    compile_thunks(ctx, tc, &mut ctc);
    ctc
}

fn compile_functions(
    ctx: &BootstrapCodegenContext<'_>,
    tc: &TransitiveClosure,
    ctc: &mut CompiledTransitiveClosure,
) {
    for (fct_id, type_params) in &tc.functions {
        compile_function(ctx, *fct_id, type_params.clone(), ctc);
    }
}

fn compile_function(
    ctx: &BootstrapCodegenContext<'_>,
    fct_id: FunctionId,
    type_params: BytecodeTypeArray,
    ctc: &mut CompiledTransitiveClosure,
) {
    let fct = ctx.program.fct(fct_id);

    if let Some(target) = ctx.native_lookup.get_target(fct_id) {
        let internal_fct = NativeFct {
            target,
            args: BytecodeTypeArray::new(fct.params.clone()),
            return_type: fct.return_type.clone(),
            desc: NativeFctKind::RuntimeEntryTrampoline(fct_id),
        };

        let code_kind = runtime_entry_trampoline::code_kind(&internal_fct.desc);
        let code = runtime_entry_trampoline::generate(internal_fct, false);
        ctc.functions.push(CompiledFunction {
            target: CompiledFunctionTarget::Function {
                fct_id,
                type_params,
            },
            code,
            code_kind,
        });
    } else if let Some(_) = get_bytecode(ctx.program, fct) {
        let program_fct = ctx.program.fct(fct_id);
        let params = BytecodeTypeArray::new(program_fct.params.clone());
        let (bytecode_fct, specialize_self) =
            get_bytecode(ctx.program, program_fct).expect("missing bytecode");

        let (code, code_kind) = compile_fct_to_descriptor(
            ctx,
            fct_id,
            program_fct,
            params,
            program_fct.return_type.clone(),
            bytecode_fct,
            &type_params,
            specialize_self,
        );

        ctc.functions.push(CompiledFunction {
            target: CompiledFunctionTarget::Function {
                fct_id,
                type_params,
            },
            code,
            code_kind,
        });
    }
}

fn compile_fct_to_descriptor(
    ctx: &BootstrapCodegenContext<'_>,
    fct_id: FunctionId,
    program_fct: &FunctionData,
    params: BytecodeTypeArray,
    return_type: BytecodeType,
    bytecode_fct: &BytecodeFunction,
    type_params: &BytecodeTypeArray,
    specialize_self: Option<SpecializeSelf>,
) -> (CodeDescriptor, CodeKind) {
    debug_assert!(type_params.iter().all(|ty| ty.is_concrete_type()));

    let (emit_graph, emit_html) = should_emit_graph(ctx.program, fct_id, ctx.emit_graph);
    let emit_final_graph = emit_graph;
    let emit_graph_after_each_pass = emit_graph && ctx.emit_graph_after_each_pass;

    let compilation_data = CompilationData {
        bytecode_fct,
        params,
        has_variadic_parameter: program_fct.is_variadic,
        return_type,
        fct_id,
        type_params: type_params.clone(),
        specialize_self,
        loc: program_fct.loc,

        emit_debug: false,
        emit_code_comments: false,
        emit_final_graph,
        emit_graph_after_each_pass,
        emit_html,
    };

    match ctx.compiler {
        CompilerInvocation::Cannon => (
            cannon::compile(ctx.vm, compilation_data, ctx.mode),
            CodeKind::BaselineFct(fct_id),
        ),
        CompilerInvocation::Boots(compile_address) => (
            boots::compile(
                compile_address,
                ctx.dora_entry_trampoline_address,
                compilation_data,
                ctx.mode,
            ),
            CodeKind::OptimizedFct(fct_id),
        ),
    }
}

fn compile_thunks(
    ctx: &BootstrapCodegenContext<'_>,
    tc: &TransitiveClosure,
    ctc: &mut CompiledTransitiveClosure,
) {
    for thunk in &tc.thunks {
        let (code, code_kind) = compile_trait_object_thunk(ctx, thunk);

        ctc.functions.push(CompiledFunction {
            target: CompiledFunctionTarget::TraitObjectThunk(thunk.clone()),
            code,
            code_kind,
        });
    }
}

fn compile_trait_object_thunk(
    ctx: &BootstrapCodegenContext<'_>,
    thunk: &TraitObjectThunk,
) -> (CodeDescriptor, CodeKind) {
    let trait_fct_id = thunk.trait_fct_id;
    let trait_type_params = thunk.trait_object_ty.type_params();
    let all_type_params = trait_type_params.append(thunk.actual_object_ty.clone());

    assert!(all_type_params.iter().all(|ty| ty.is_concrete_type()));

    let trait_object_type_param_id = all_type_params.len() - 1;
    assert_eq!(
        &all_type_params[trait_object_type_param_id],
        &thunk.actual_object_ty
    );

    let bytecode_fct = trait_object_thunk::generate_bytecode_for_thunk(
        ctx.program,
        trait_fct_id,
        thunk.trait_object_ty.clone(),
        trait_object_type_param_id,
        thunk.actual_object_ty.clone(),
    );

    let trait_fct = ctx.program.fct(trait_fct_id);
    let params = {
        let mut params = trait_fct.params.clone();
        assert_eq!(params[0], BytecodeType::This);
        params[0] = thunk.trait_object_ty.clone();
        BytecodeTypeArray::new(params)
    };

    compile_fct_to_descriptor(
        ctx,
        trait_fct_id,
        trait_fct,
        params,
        bytecode_fct.return_type().clone(),
        &bytecode_fct,
        &all_type_params,
        None,
    )
}

fn install_compiled_transitive_closure(
    ctx: &BootstrapCodegenContext<'_>,
    ctc: CompiledTransitiveClosure,
) -> InstalledTransitiveClosure {
    let mut itc = InstalledTransitiveClosure::new(ctc);

    for idx in 0..itc.compiled.functions.len() {
        let (code_descriptor, code_kind, target) = {
            let entry = &itc.compiled.functions[idx];
            (
                entry.code.clone(),
                entry.code_kind.clone(),
                entry.target.clone(),
            )
        };
        let code = install_code_stub(ctx.vm, code_descriptor, code_kind);
        itc.set_address(target, code.instruction_start());
        itc.installed_codes.push(code);
    }

    itc
}

fn sync_installed_code(itc: &mut InstalledTransitiveClosure) {
    assert_eq!(itc.compiled.functions.len(), itc.installed_codes.len());

    for (entry, code) in itc.compiled.functions.iter_mut().zip(&itc.installed_codes) {
        entry.code.code = code.instruction_slice().to_vec();
    }
}

fn prepare_lazy_call_sites(ctx: &BootstrapCodegenContext<'_>, itc: &InstalledTransitiveClosure) {
    assert_eq!(itc.compiled.functions.len(), itc.installed_codes.len());
    os::jit_writable();

    for (entry, code) in itc.compiled.functions.iter().zip(&itc.installed_codes) {
        for (offset, site) in entry.code.lazy_compilation.entries() {
            match site {
                LazyCompilationSite::Direct {
                    fct_id,
                    type_params,
                    const_pool_offset_from_ra,
                } => {
                    let target = itc.get_function_address(*fct_id, type_params);

                    let target = match target {
                        Some(target) => target,
                        None => {
                            eprintln!(
                                "code = {:?} {}",
                                entry.code_kind,
                                aot_compiled_function_name(ctx.program, entry)
                            );
                            eprintln!(
                                " calls {} with {:?}",
                                display_fct_specialized(ctx.program, *fct_id, type_params),
                                type_params
                            );
                            eprintln!("offset = {}", offset);
                            let has_native = ctx.native_lookup.contains(*fct_id);
                            let has_bytecode =
                                get_bytecode(ctx.program, ctx.program.fct(*fct_id)).is_some();
                            eprintln!(
                                "has_native={}, has_bytecode={}, function_addresses.len={}",
                                has_native,
                                has_bytecode,
                                itc.function_addresses_len()
                            );
                            panic!("missing function");
                        }
                    };
                    let ra = code.instruction_start().offset(*offset as usize);

                    if ctx.mode.is_stage2_or_3() {
                        crate::cpu::patch_direct_call_site(ra, target);
                    } else {
                        let const_pool_address = ra.ioffset(*const_pool_offset_from_ra as isize);

                        unsafe {
                            std::ptr::write_unaligned(
                                const_pool_address.to_mut_ptr::<Address>(),
                                target,
                            );
                        }
                    }
                }

                LazyCompilationSite::Lambda { .. } | LazyCompilationSite::Virtual { .. } => {
                    // Nothing to do.
                }
            }
        }
    }

    os::jit_executable();
}

fn prepare_virtual_method_tables(
    vm: &VM,
    tc: &TransitiveClosure,
    itc: &InstalledTransitiveClosure,
) {
    for shape_key in &tc.shape_keys {
        match shape_key {
            AotShapeKey::Lambda(fct_id, type_params) => {
                let shape = vm.shape_for_lambda(*fct_id, type_params.clone());
                let address = itc
                    .get_function_address(*fct_id, type_params)
                    .expect("missing function");
                shape.set_method_table_entry(0, address);
            }

            AotShapeKey::TraitObject {
                trait_ty,
                actual_object_ty,
            } => {
                let shape = vm.shape_for_trait_object(trait_ty.clone(), actual_object_ty.clone());
                let trait_id = trait_ty.trait_id().expect("trait expected");
                let trait_ = vm.trait_(trait_id);
                for (idx, &trait_fct_id) in trait_.virtual_methods.iter().enumerate() {
                    let key = TraitObjectThunk {
                        trait_fct_id,
                        trait_object_ty: trait_ty.clone(),
                        actual_object_ty: actual_object_ty.clone(),
                    };
                    if let Some(address) = itc.get_trait_object_thunk_address(&key) {
                        shape.set_method_table_entry(idx, address);
                    }
                }
            }

            _ => unreachable!(),
        }
    }
}

fn compute_tests(program: &Program, package_id: PackageId) -> Vec<FunctionId> {
    let mut results = Vec::new();

    for (id, function) in program.functions.iter().enumerate() {
        if function.package_id == package_id && function.is_test {
            results.push(id.into());
        }
    }

    results
}

fn compute_test_addresses(
    itc: &InstalledTransitiveClosure,
    tests: Vec<FunctionId>,
) -> HashMap<FunctionId, Address> {
    let mut results = HashMap::new();

    for id in tests {
        let address = itc.get_address(id).expect("missing function");
        results.insert(id, address);
    }

    results
}
