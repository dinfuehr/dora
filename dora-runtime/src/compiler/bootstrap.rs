use std::collections::HashMap;
use std::sync::Arc;
use std::time::Instant;

use dora_bytecode::{FunctionId, PackageId, Program, display_fct_specialized};

use crate::compiler::CompilationMode;
use crate::compiler::aot::{
    AotCodegenContext, AotNativeLookup, CompiledTransitiveClosure, aot_compiled_function_name,
    compile_transitive_closure,
};
use crate::compiler::closure::{TraitObjectThunk, TransitiveClosure, compute_transitive_closure};
use crate::compiler::codegen::CompilerInvocation;
use crate::gc::{Address, formatted_size};
use crate::os;
use crate::vm::{
    AotShapeKey, BytecodeTypeExt, Code, LazyCompilationSite, VM, execute_on_main, install_code_stub,
};

pub fn compile_boots_aot(vm: &VM) {
    if vm.has_boots() {
        let package_id = vm
            .program
            .boots_package_id
            .expect("boots package is missing");
        let entry_id = vm.known.boots_compile_fct_id();
        let tests = compute_tests(&vm.program, package_id);
        let tc = compute_transitive_closure(&vm.program, entry_id, &tests, vm.flags.emit_compiler);
        let (stage1_compiler_address, stage1_ctc) = stage1_compiler(vm, &tc, entry_id);

        let (boots_compiler_address, ctc) = if vm.flags.bootstrap_compiler {
            execute_on_main(|| {
                let (stage2_compiler_address, stage2_ctc) =
                    stage2_compiler(vm, &tc, entry_id, stage1_compiler_address);
                let (stage3_compiler_address, stage3_ctc) =
                    stage3_compiler(vm, &tc, entry_id, stage2_compiler_address);
                assert_builds_identical(vm, &stage2_ctc, &stage3_ctc);

                (stage3_compiler_address, stage3_ctc)
            })
        } else {
            (stage1_compiler_address, stage1_ctc)
        };

        assert!(
            vm.known
                .boots_compile_fct_address
                .set(boots_compiler_address)
                .is_ok()
        );

        let tests = compute_test_addresses(&ctc, tests);
        assert!(vm.known.boots_test_addresses.set(tests).is_ok());
    }
}

fn stage1_compiler(
    vm: &VM,
    tc: &TransitiveClosure,
    entry_id: FunctionId,
) -> (Address, CompiledTransitiveClosure) {
    let (compile_address, ctc) = compiler_stage_n(
        vm,
        tc,
        entry_id,
        "stage1",
        CompilerInvocation::Cannon,
        CompilationMode::Stage1,
    );
    (compile_address, ctc)
}

fn stage2_compiler(
    vm: &VM,
    tc: &TransitiveClosure,
    entry_id: FunctionId,
    stage1_compiler_address: Address,
) -> (Address, CompiledTransitiveClosure) {
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
) -> (Address, CompiledTransitiveClosure) {
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
) -> (Address, CompiledTransitiveClosure) {
    let start = Instant::now();
    let start_code_size = vm.gc.current_code_size();
    let native_lookup = AotNativeLookup::from_vm(vm, tc);
    let ctx = AotCodegenContext {
        vm,
        program: &vm.program,
        native_lookup: &native_lookup,
        compiler,
        mode,
    };
    assert!(matches!(
        ctx.mode,
        CompilationMode::Stage1 | CompilationMode::Stage2 | CompilationMode::Stage3
    ));
    let mut ctc = compile_transitive_closure(&ctx, &tc);
    let installed_codes = install_compiled_transitive_closure(&ctx, &mut ctc);
    prepare_lazy_call_sites(&ctx, &ctc, &installed_codes);
    // Lazy call preparation patches installed code in place; keep descriptors
    // in sync so stage2/stage3 comparison sees the final executable bytes.
    sync_installed_code(&mut ctc, &installed_codes);
    prepare_virtual_method_tables(ctx.vm, tc, &ctc);
    let compile_address = ctc.get_address(entry_id).expect("missing entry point");
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

    (compile_address, ctc)
}

fn assert_builds_identical(
    vm: &VM,
    stage2: &CompiledTransitiveClosure,
    stage3: &CompiledTransitiveClosure,
) {
    assert_eq!(stage2.functions.len(), stage3.functions.len());

    for (stage2_entry, stage3_entry) in stage2.functions.iter().zip(&stage3.functions) {
        assert_eq!(
            &stage2_entry.code.code,
            &stage3_entry.code.code,
            "stage2 and stage3 differ in function {}",
            aot_compiled_function_name(&vm.program, stage2_entry)
        );
    }
}

fn install_compiled_transitive_closure(
    ctx: &AotCodegenContext<'_>,
    ctc: &mut CompiledTransitiveClosure,
) -> Vec<Arc<Code>> {
    let mut installed_codes = Vec::with_capacity(ctc.functions.len());

    for idx in 0..ctc.functions.len() {
        let (code_descriptor, code_kind, address_key) = {
            let entry = &ctc.functions[idx];
            (
                entry.code.clone(),
                entry.code_kind.clone(),
                entry.target.address_key(),
            )
        };
        let code = install_code_stub(ctx.vm, code_descriptor, code_kind);
        ctc.set_address(address_key, code.instruction_start());
        installed_codes.push(code);
    }

    installed_codes
}

fn sync_installed_code(ctc: &mut CompiledTransitiveClosure, installed_codes: &[Arc<Code>]) {
    assert_eq!(ctc.functions.len(), installed_codes.len());

    for (entry, code) in ctc.functions.iter_mut().zip(installed_codes) {
        entry.code.code = code.instruction_slice().to_vec();
    }
}

fn prepare_lazy_call_sites(
    ctx: &AotCodegenContext<'_>,
    ctc: &CompiledTransitiveClosure,
    installed_codes: &[Arc<Code>],
) {
    assert_eq!(ctc.functions.len(), installed_codes.len());
    os::jit_writable();

    for (entry, code) in ctc.functions.iter().zip(installed_codes) {
        for (offset, site) in entry.code.lazy_compilation.entries() {
            match site {
                LazyCompilationSite::Direct {
                    fct_id,
                    type_params,
                    const_pool_offset_from_ra,
                } => {
                    let target = ctc.get_function_address(*fct_id, type_params);

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
                            let has_native = ctx.native_lookup.get_address(*fct_id).is_some();
                            let has_bytecode =
                                crate::get_bytecode(ctx.program, ctx.program.fct(*fct_id))
                                    .is_some();
                            eprintln!(
                                "has_native={}, has_bytecode={}, function_addresses.len={}",
                                has_native,
                                has_bytecode,
                                ctc.function_addresses_len()
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

fn prepare_virtual_method_tables(vm: &VM, tc: &TransitiveClosure, ctc: &CompiledTransitiveClosure) {
    for shape_key in &tc.shape_keys {
        match shape_key {
            AotShapeKey::Lambda(fct_id, type_params) => {
                let shape = vm.shape_for_lambda(*fct_id, type_params.clone());
                let address = ctc
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
                    if let Some(address) = ctc.get_trait_object_thunk_address(&key) {
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
    ctc: &CompiledTransitiveClosure,
    tests: Vec<FunctionId>,
) -> HashMap<FunctionId, Address> {
    let mut results = HashMap::new();

    for id in tests {
        let address = ctc.get_address(id).expect("missing function");
        results.insert(id, address);
    }

    results
}
