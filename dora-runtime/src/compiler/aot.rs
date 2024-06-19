use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::time::Instant;

use dora_bytecode::{
    BytecodeFunction, BytecodeInstruction, BytecodeReader, BytecodeType, BytecodeTypeArray,
    ConstPoolEntry, FunctionId, FunctionKind, PackageId,
};

use crate::compiler::codegen::ensure_runtime_entry_trampoline;
use crate::compiler::{compile_fct_aot, trait_object_thunk, NativeFct, NativeFctKind};
use crate::gc::{formatted_size, Address};
use crate::os;
use crate::vm::{find_trait_impl, specialize_bty_array, Code, LazyCompilationSite, VM};

pub fn compile_boots_aot(vm: &VM, include_tests: bool) {
    if let Some(package_id) = vm.program.boots_package_id {
        let mut compile_all = TransitiveClosure::new(vm);

        let start = Instant::now();
        compile_all.push(vm.known.boots_compile_fct_id(), BytecodeTypeArray::empty());
        if include_tests {
            compile_all.push_tests(package_id);
        }
        compile_all.compute();
        compile_all.prepare_lazy_call_sites();
        let duration = start.elapsed();
        if vm.flags.emit_compiler {
            println!(
                "compiled all of boots in {:.2}ms ({} functions, {} bytes)",
                duration.as_secs_f32() * 1000.0f32,
                compile_all.counter,
                formatted_size(vm.gc.current_code_size()),
            );
        }
    }
}

struct TransitiveClosure<'a> {
    vm: &'a VM,
    worklist: Vec<(FunctionId, BytecodeTypeArray)>,
    visited: HashSet<(FunctionId, BytecodeTypeArray)>,
    function_addresses: HashMap<(FunctionId, BytecodeTypeArray), Address>,
    counter: usize,
    code_objects: Vec<Arc<Code>>,
}

impl<'a> TransitiveClosure<'a> {
    fn new(vm: &VM) -> TransitiveClosure {
        TransitiveClosure {
            vm,
            worklist: Vec::new(),
            visited: HashSet::new(),
            function_addresses: HashMap::new(),
            code_objects: Vec::new(),
            counter: 0,
        }
    }

    fn push_tests(&mut self, package_id: PackageId) {
        for (id, fct) in self.vm.program.functions.iter().enumerate() {
            if fct.package_id == package_id && fct.is_test {
                self.push(FunctionId(id as u32), BytecodeTypeArray::empty());
            }
        }
    }

    fn compute(&mut self) {
        while let Some((fct_id, type_params)) = self.worklist.pop() {
            self.compile(fct_id, type_params.clone());
        }
    }

    fn compile(&mut self, fct_id: FunctionId, type_params: BytecodeTypeArray) {
        let fct = &self.vm.program.functions[fct_id.0 as usize];

        if let Some(native_fctptr) = self.vm.native_methods.get(fct_id) {
            // Method is implemented in native code. Create trampoline for invoking it.
            let internal_fct = NativeFct {
                fctptr: native_fctptr,
                args: BytecodeTypeArray::new(fct.params.clone()),
                return_type: fct.return_type.clone(),
                desc: NativeFctKind::RuntimeEntryTrampoline(fct_id),
            };

            let fctptr_wrapper =
                ensure_runtime_entry_trampoline(self.vm, Some(fct_id), internal_fct);

            let existing = self
                .function_addresses
                .insert((fct_id, type_params), fctptr_wrapper);
            assert!(existing.is_none());
        } else if let Some(ref bytecode_function) = fct.bytecode {
            let (_code_id, code) = compile_fct_aot(self.vm, fct_id, &type_params);
            self.counter += 1;
            let existing = self
                .function_addresses
                .insert((fct_id, type_params.clone()), code.instruction_start());
            assert!(existing.is_none());
            self.code_objects.push(code);

            self.trace_function(bytecode_function, type_params);
        }
    }

    fn trace_function(
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
                    let fct = &self.vm.program.functions[callee_trait_fct_id.0 as usize];

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
                    self.push(callee_id, callee_type_params);
                }

                BytecodeInstruction::LoadGlobal { global_id, .. }
                | BytecodeInstruction::StoreGlobal { global_id, .. } => {
                    let global = &self.vm.program.globals[global_id.0 as usize];
                    if let Some(callee_id) = global.initial_value {
                        self.push(callee_id, BytecodeTypeArray::empty());
                    }
                }

                BytecodeInstruction::InvokeVirtual { fct, .. } => {
                    let (trait_object_ty, trait_fct_id, trait_type_params) = match bytecode_function
                        .const_pool(fct)
                    {
                        ConstPoolEntry::TraitObjectMethod(trait_object_ty, fct_id, type_params) => {
                            (trait_object_ty.clone(), *fct_id, type_params)
                        }
                        _ => unreachable!(),
                    };

                    for impl_ in self.vm.program.impls.iter() {
                        if impl_.trait_ty == trait_object_ty {
                            for (trait_method_id, impl_method_id) in &impl_.trait_method_map {
                                if *trait_method_id == trait_fct_id {
                                    let actual_ty = impl_.extended_ty.clone();
                                    if self.push_thunk(
                                        trait_fct_id,
                                        trait_type_params.clone(),
                                        actual_ty,
                                    ) {
                                        let (_code_id, code) =
                                            trait_object_thunk::ensure_compiled_aot(
                                                self.vm,
                                                trait_fct_id,
                                                trait_type_params.clone(),
                                                impl_.extended_ty.clone(),
                                            );

                                        self.code_objects.push(code);
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

    fn prepare_lazy_call_sites(&self) {
        os::jit_writable();

        for code in &self.code_objects {
            for (offset, site) in code.lazy_compilation().entries() {
                match site {
                    LazyCompilationSite::Direct(fct_id, type_params, const_pool_offset) => {
                        let address = self.function_addresses.get(&(*fct_id, type_params.clone()));
                        if let Some(address) = address {
                            let ra = code.instruction_start().offset(*offset as usize);
                            let const_pool_address = ra.sub(*const_pool_offset as usize);

                            unsafe {
                                *const_pool_address.to_mut_ptr::<Address>() = *address;
                            }
                        }
                    }

                    LazyCompilationSite::Lambda(..) | LazyCompilationSite::Virtual(..) => {
                        // Nothing to do.
                    }
                }
            }
        }

        os::jit_executable();
    }
}
