use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::time::Instant;

use dora_bytecode::{
    BytecodeFunction, BytecodeInstruction, BytecodeReader, BytecodeType, BytecodeTypeArray,
    ConstPoolEntry, FunctionId, FunctionKind, PackageId,
};

use crate::compiler::codegen::{ensure_runtime_entry_trampoline, CompilerInvocation};
use crate::compiler::{compile_fct_aot, trait_object_thunk, NativeFct, NativeFctKind};
use crate::gc::{formatted_size, Address};
use crate::os;
use crate::vm::{
    ensure_class_instance_for_lambda, ensure_class_instance_for_trait_object, execute_on_main,
    find_trait_impl, specialize_bty, specialize_bty_array, BytecodeTypeExt, ClassInstanceId, Code,
    LazyCompilationSite, ShapeKind, VM,
};

pub fn compile_boots_aot(vm: &VM) {
    if let Some(package_id) = vm.program.boots_package_id {
        let entry_id = vm.known.boots_compile_fct_id();
        let tests = compute_tests(vm, package_id);
        let tc = compute_transitive_closure(vm, package_id, entry_id, &tests);
        let (stage1_compiler_address, stage1_ctc) = stage1_compiler(vm, &tc, entry_id);

        let (boots_compiler_address, ctc) = if vm.flags.bootstrap_compiler {
            execute_on_main(|| {
                let (stage2_compiler_address, stage2_ctc) =
                    stage2_compiler(vm, &tc, entry_id, stage1_compiler_address);
                let (stage3_compiler_address, stage3_ctc) =
                    stage3_compiler(vm, &tc, entry_id, stage2_compiler_address);
                assert_builds_identical(&stage2_ctc, &stage3_ctc);

                (stage3_compiler_address, stage3_ctc)
            })
        } else {
            (stage1_compiler_address, stage1_ctc)
        };

        assert!(vm
            .known
            .boots_compile_fct_address
            .set(boots_compiler_address)
            .is_ok());

        let tests = compute_test_addresses(&ctc, tests);
        assert!(vm.known.boots_test_addresses.set(tests).is_ok());
    }
}

fn stage1_compiler(
    vm: &VM,
    tc: &TransitiveClosure,
    entry_id: FunctionId,
) -> (Address, CompiledTransitiveClosure) {
    let (compile_address, ctc) =
        compiler_stage_n(vm, tc, entry_id, "stage1", CompilerInvocation::Cannon);
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
    )
}

fn compiler_stage_n(
    vm: &VM,
    tc: &TransitiveClosure,
    entry_id: FunctionId,
    name: &str,
    compiler: CompilerInvocation,
) -> (Address, CompiledTransitiveClosure) {
    let start = Instant::now();
    let start_code_size = vm.gc.current_code_size();
    let ctc = compile_transitive_closure(vm, &tc, compiler);
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

fn assert_builds_identical(stage2: &CompiledTransitiveClosure, stage3: &CompiledTransitiveClosure) {
    assert_eq!(stage2.code_objects.len(), stage3.code_objects.len());

    for (stage2_code, stage3_code) in stage2.code_objects.iter().zip(&stage3.code_objects) {
        assert_eq!(
            stage2_code.instruction_slice(),
            stage3_code.instruction_slice()
        );
    }
}

fn compute_transitive_closure(
    vm: &VM,
    _package_id: PackageId,
    entry_id: FunctionId,
    tests: &[FunctionId],
) -> TransitiveClosure {
    let start = Instant::now();

    let mut compile_all = TransitiveClosureComputation::new(vm);
    compile_all.push(entry_id, BytecodeTypeArray::empty());

    for test_fct_id in tests {
        compile_all.push(*test_fct_id, BytecodeTypeArray::empty());
    }

    let tc = compile_all.compute();
    let duration = start.elapsed();

    if vm.flags.emit_compiler {
        println!(
            "computed transitive closure of boots in {:.2}ms ({} functions, {} thunks)",
            duration.as_secs_f32() * 1000.0f32,
            tc.functions.len(),
            tc.thunks.len(),
        );
    }

    tc
}

fn compute_tests(vm: &VM, package_id: PackageId) -> Vec<FunctionId> {
    let mut results = Vec::new();

    for (id, function) in vm.program.functions.iter().enumerate() {
        if function.package_id == package_id && function.is_test {
            results.push(FunctionId(id.try_into().expect("overflow")));
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
struct TransitiveClosure {
    functions: Vec<(FunctionId, BytecodeTypeArray)>,
    thunks: Vec<(FunctionId, BytecodeTypeArray, BytecodeType)>,
    class_instances: Vec<ClassInstanceId>,
}

struct TransitiveClosureComputation<'a> {
    vm: &'a VM,
    worklist: Vec<(FunctionId, BytecodeTypeArray)>,
    worklist_idx: usize,
    visited: HashSet<(FunctionId, BytecodeTypeArray)>,
    counter: usize,
    class_instances: Vec<ClassInstanceId>,
    thunks: Vec<(FunctionId, BytecodeTypeArray, BytecodeType)>,
}

impl<'a> TransitiveClosureComputation<'a> {
    fn new(vm: &VM) -> TransitiveClosureComputation {
        TransitiveClosureComputation {
            vm,
            worklist: Vec::new(),
            worklist_idx: 0,
            visited: HashSet::new(),
            class_instances: Vec::new(),
            counter: 0,
            thunks: Vec::new(),
        }
    }

    fn compute(mut self) -> TransitiveClosure {
        while let Some((fct_id, type_params)) = self.pop() {
            self.trace(fct_id, type_params.clone());
        }

        TransitiveClosure {
            functions: self.worklist,
            thunks: self.thunks,
            class_instances: self.class_instances,
        }
    }

    fn trace(&mut self, fct_id: FunctionId, type_params: BytecodeTypeArray) {
        let fct = &self.vm.fct(fct_id);

        if let Some(ref bytecode_function) = fct.bytecode {
            self.iterate_bytecode(bytecode_function, type_params);
        }
    }

    fn iterate_bytecode(
        &mut self,
        bytecode_function: &BytecodeFunction,
        type_params: BytecodeTypeArray,
    ) {
        let reader = BytecodeReader::new(bytecode_function.code());

        for (_start, _opcode, inst) in reader {
            match inst {
                BytecodeInstruction::InvokeDirect { fct, .. }
                | BytecodeInstruction::InvokeStatic { fct, .. } => {
                    let (callee_fct_id, callee_type_params) = match bytecode_function
                        .const_pool(fct)
                    {
                        ConstPoolEntry::Fct(fct_id, type_params) => (*fct_id, type_params.clone()),
                        _ => unreachable!(),
                    };

                    let callee_type_params =
                        specialize_bty_array(&callee_type_params, &type_params);
                    self.push(callee_fct_id, callee_type_params);
                }

                BytecodeInstruction::InvokeGenericDirect { fct, .. }
                | BytecodeInstruction::InvokeGenericStatic { fct, .. } => {
                    let (id, callee_trait_fct_id, callee_type_params) =
                        match bytecode_function.const_pool(fct) {
                            ConstPoolEntry::Generic(id, fct_id, type_params) => {
                                (*id, *fct_id, type_params.clone())
                            }
                            _ => unreachable!(),
                        };
                    let fct = self.vm.fct(callee_trait_fct_id);

                    let trait_id = match fct.kind {
                        FunctionKind::Trait(trait_id) => trait_id,
                        _ => unreachable!(),
                    };
                    let trait_ty = BytecodeType::Trait(trait_id, callee_type_params.clone());

                    let ty = type_params[id as usize].clone();

                    let callee_id = find_trait_impl(self.vm, callee_trait_fct_id, trait_ty, ty);

                    let callee_type_params =
                        specialize_bty_array(&callee_type_params, &type_params);
                    self.push(callee_id, callee_type_params);
                }

                BytecodeInstruction::NewLambda { idx, .. } => {
                    let (callee_id, callee_type_params) = match bytecode_function.const_pool(idx) {
                        ConstPoolEntry::Fct(fct_id, type_params) => (*fct_id, type_params),
                        _ => unreachable!(),
                    };

                    let callee_type_params =
                        specialize_bty_array(&callee_type_params, &type_params);
                    self.push(callee_id, callee_type_params.clone());

                    let class_instance_id =
                        ensure_class_instance_for_lambda(self.vm, callee_id, callee_type_params);
                    self.class_instances.push(class_instance_id);
                }

                BytecodeInstruction::NewTraitObject { idx, .. } => {
                    let (trait_ty, actual_object_ty) = match bytecode_function.const_pool(idx) {
                        ConstPoolEntry::TraitObject {
                            trait_ty,
                            actual_object_ty,
                        } => (trait_ty.clone(), actual_object_ty.clone()),
                        _ => unreachable!(),
                    };

                    let trait_ty = specialize_bty(trait_ty, &type_params);
                    let actual_object_ty = specialize_bty(actual_object_ty, &type_params);

                    let class_instance_id =
                        ensure_class_instance_for_trait_object(self.vm, trait_ty, actual_object_ty);
                    self.class_instances.push(class_instance_id);
                }

                BytecodeInstruction::LoadGlobal { global_id, .. }
                | BytecodeInstruction::StoreGlobal { global_id, .. } => {
                    let global = self.vm.global(global_id);
                    if let Some(callee_id) = global.initial_value {
                        self.push(callee_id, BytecodeTypeArray::empty());
                    }
                }

                BytecodeInstruction::InvokeVirtual { fct, .. } => {
                    let (trait_object_ty, trait_fct_id) = match bytecode_function.const_pool(fct) {
                        ConstPoolEntry::TraitObjectMethod(trait_object_ty, fct_id) => {
                            (trait_object_ty.clone(), *fct_id)
                        }
                        _ => unreachable!(),
                    };

                    let trait_type_params = trait_object_ty.type_params();

                    for impl_ in self.vm.program.impls.iter() {
                        if impl_.trait_ty == trait_object_ty {
                            for (trait_method_id, impl_method_id) in &impl_.trait_method_map {
                                if *trait_method_id == trait_fct_id {
                                    let actual_ty = impl_.extended_ty.clone();
                                    if self.push_thunk(
                                        trait_fct_id,
                                        trait_type_params.clone(),
                                        actual_ty.clone(),
                                    ) {
                                        self.thunks.push((
                                            trait_fct_id,
                                            trait_type_params.clone(),
                                            actual_ty,
                                        ));
                                    }

                                    self.push(*impl_method_id, type_params.clone());
                                }
                            }
                        }
                    }
                }

                _ => {}
            }
        }
    }

    fn push(&mut self, function_id: FunctionId, type_params: BytecodeTypeArray) -> bool {
        if self.visited.insert((function_id, type_params.clone())) {
            self.worklist.push((function_id, type_params));
            true
        } else {
            false
        }
    }

    fn push_thunk(
        &mut self,
        function_id: FunctionId,
        type_params: BytecodeTypeArray,
        actual_ty: BytecodeType,
    ) -> bool {
        let all_type_params = type_params.append(actual_ty);

        if self.visited.insert((function_id, all_type_params.clone())) {
            self.counter += 1;
            true
        } else {
            false
        }
    }

    fn pop(&mut self) -> Option<(FunctionId, BytecodeTypeArray)> {
        if self.worklist_idx < self.worklist.len() {
            let current = self.worklist[self.worklist_idx].clone();
            self.worklist_idx += 1;
            Some(current)
        } else {
            None
        }
    }
}

struct CompiledTransitiveClosure {
    function_addresses: HashMap<(FunctionId, BytecodeTypeArray), Address>,
    code_objects: Vec<Arc<Code>>,
    counter: usize,
}

impl CompiledTransitiveClosure {
    fn new() -> CompiledTransitiveClosure {
        CompiledTransitiveClosure {
            function_addresses: HashMap::new(),
            code_objects: Vec::new(),
            counter: 0,
        }
    }

    fn get_address(&self, id: FunctionId) -> Option<Address> {
        self.function_addresses
            .get(&(id, BytecodeTypeArray::empty()))
            .cloned()
    }
}

fn compile_transitive_closure(
    vm: &VM,
    tc: &TransitiveClosure,
    compiler: CompilerInvocation,
) -> CompiledTransitiveClosure {
    let mut ctc = CompiledTransitiveClosure::new();
    compile_functions(vm, tc, &mut ctc, compiler);
    compile_thunks(vm, tc, &mut ctc);
    prepare_lazy_call_sites(vm, &ctc);
    prepare_virtual_method_tables(vm, tc, &ctc);
    ctc
}

fn compile_functions(
    vm: &VM,
    tc: &TransitiveClosure,
    ctc: &mut CompiledTransitiveClosure,
    compiler: CompilerInvocation,
) {
    for (fct_id, type_params) in &tc.functions {
        compile_function(vm, *fct_id, type_params.clone(), ctc, compiler);
    }
}

fn compile_function(
    vm: &VM,
    fct_id: FunctionId,
    type_params: BytecodeTypeArray,
    ctc: &mut CompiledTransitiveClosure,
    compiler: CompilerInvocation,
) {
    let fct = vm.fct(fct_id);

    if let Some(native_fctptr) = vm.native_methods.get(fct_id) {
        // Method is implemented in native code. Create trampoline for invoking it.
        let internal_fct = NativeFct {
            fctptr: native_fctptr,
            args: BytecodeTypeArray::new(fct.params.clone()),
            return_type: fct.return_type.clone(),
            desc: NativeFctKind::RuntimeEntryTrampoline(fct_id),
        };

        let fctptr_wrapper = ensure_runtime_entry_trampoline(vm, Some(fct_id), internal_fct);

        let existing = ctc
            .function_addresses
            .insert((fct_id, type_params), fctptr_wrapper);
        assert!(existing.is_none());
    } else if let Some(_) = fct.bytecode {
        let (_code_id, code) = compile_fct_aot(vm, fct_id, &type_params, compiler);
        ctc.counter += 1;
        let existing = ctc
            .function_addresses
            .insert((fct_id, type_params.clone()), code.instruction_start());
        assert!(existing.is_none());
        ctc.code_objects.push(code);
    }
}

fn compile_thunks(vm: &VM, tc: &TransitiveClosure, ctc: &mut CompiledTransitiveClosure) {
    for (trait_fct_id, trait_type_params, actual_ty) in &tc.thunks {
        let (_code_id, code) = trait_object_thunk::ensure_compiled_aot(
            vm,
            *trait_fct_id,
            trait_type_params.clone(),
            actual_ty.clone(),
        );

        let combined_type_params = trait_type_params.append(actual_ty.clone());
        let existing = ctc.function_addresses.insert(
            (*trait_fct_id, combined_type_params),
            code.instruction_start(),
        );
        assert!(existing.is_none());

        ctc.code_objects.push(code);
    }
}

fn prepare_lazy_call_sites(_vm: &VM, ctc: &CompiledTransitiveClosure) {
    os::jit_writable();

    for code in &ctc.code_objects {
        for (offset, site) in code.lazy_compilation().entries() {
            match site {
                LazyCompilationSite::Direct {
                    fct_id,
                    type_params,
                    const_pool_offset_from_ra: const_pool_offset,
                } => {
                    let address = ctc.function_addresses.get(&(*fct_id, type_params.clone()));
                    if let Some(address) = address {
                        let ra = code.instruction_start().offset(*offset as usize);
                        let const_pool_address = ra.sub(*const_pool_offset as usize);

                        unsafe {
                            *const_pool_address.to_mut_ptr::<Address>() = *address;
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
    for class_instance_id in &tc.class_instances {
        let class_instance = vm.class_instances.idx(*class_instance_id);
        match &class_instance.kind {
            ShapeKind::Lambda(fct_id, type_params) => {
                let address = ctc
                    .function_addresses
                    .get(&(*fct_id, type_params.clone()))
                    .cloned()
                    .expect("missing function");

                let mut vtable = class_instance.vtable.write();
                let vtable = vtable.as_mut().expect("missing vtable");
                let methodtable = vtable.table_mut();
                methodtable[0] = address.to_usize();
            }

            ShapeKind::TraitObject {
                object_ty: _object_ty,
                trait_id,
                combined_type_params,
            } => {
                let trait_ = vm.trait_(*trait_id);
                for (idx, &trait_fct_id) in trait_.methods.iter().enumerate() {
                    if let Some(address) = ctc
                        .function_addresses
                        .get(&(trait_fct_id, combined_type_params.clone()))
                        .cloned()
                    {
                        let mut vtable = class_instance.vtable.write();
                        let vtable = vtable.as_mut().expect("missing vtable");
                        let methodtable = vtable.table_mut();
                        methodtable[idx] = address.to_usize();
                    }
                }
            }

            _ => unreachable!(),
        }
    }
}
