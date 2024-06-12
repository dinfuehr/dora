use std::collections::HashSet;
use std::time::Instant;

use dora_bytecode::{
    BytecodeInstruction, BytecodeReader, BytecodeType, BytecodeTypeArray, ConstPoolEntry,
    FunctionId, FunctionKind,
};

use crate::compiler::generate_fct;
use crate::vm::{find_trait_impl, specialize_bty_array, VM};

pub fn compile_boots_aot(vm: &VM) {
    if vm.program.boots_package_id.is_some() {
        let mut compile_all = TransitiveClosure::new(vm);
        let compile_fct_id = vm.known.boots_compile_fct_id();

        let start = Instant::now();
        compile_all.compute(compile_fct_id, BytecodeTypeArray::empty());
        let duration = start.elapsed();
        if vm.flags.emit_compiler {
            println!(
                "Compiled boots in {:.1}ms ({} functions)",
                duration.as_secs_f32() * 1000.0f32,
                compile_all.counter
            );
        }
    }
}

struct TransitiveClosure<'a> {
    worklist: Vec<(FunctionId, BytecodeTypeArray)>,
    visited: HashSet<(FunctionId, BytecodeTypeArray)>,
    vm: &'a VM,
    counter: usize,
}

impl<'a> TransitiveClosure<'a> {
    fn new(vm: &VM) -> TransitiveClosure {
        TransitiveClosure {
            worklist: Vec::new(),
            visited: HashSet::new(),
            counter: 0,
            vm,
        }
    }

    fn compute(&mut self, function_id: FunctionId, type_params: BytecodeTypeArray) {
        self.push(function_id, type_params);

        while let Some((function_id, type_params)) = self.worklist.pop() {
            generate_fct(self.vm, function_id, &type_params);
            self.trace_function(function_id, type_params);
        }
    }

    fn trace_function(&mut self, function_id: FunctionId, type_params: BytecodeTypeArray) {
        let function = &self.vm.program.functions[function_id.0 as usize];
        let bytecode_function = function
            .bytecode
            .as_ref()
            .expect("missing bytecode function");
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

                _ => {}
            }
        }
    }

    fn push(&mut self, function_id: FunctionId, type_params: BytecodeTypeArray) {
        let function = &self.vm.program.functions[function_id.0 as usize];
        if function.bytecode.is_some() && self.visited.insert((function_id, type_params.clone())) {
            self.worklist.push((function_id, type_params));
            self.counter += 1;
        }
    }
}
