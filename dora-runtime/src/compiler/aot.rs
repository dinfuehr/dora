use std::collections::HashSet;

use dora_bytecode::{BytecodeTypeArray, FunctionId};

use crate::compiler::generate_fct;
use crate::vm::VM;

pub fn compile_boots_aot(vm: &VM) {
    if vm.program.boots_package_id.is_some() {
        let compile_all = TransitiveClosure::new(vm);
        let compile_fct_id = vm.known.boots_compile_fct_id();
        compile_all.compute(compile_fct_id, BytecodeTypeArray::empty());
    }
}

struct TransitiveClosure<'a> {
    worklist: Vec<(FunctionId, BytecodeTypeArray)>,
    visited: HashSet<(FunctionId, BytecodeTypeArray)>,
    vm: &'a VM,
}

impl<'a> TransitiveClosure<'a> {
    fn new(vm: &VM) -> TransitiveClosure {
        TransitiveClosure {
            worklist: Vec::new(),
            visited: HashSet::new(),
            vm,
        }
    }

    fn compute(mut self, function_id: FunctionId, type_params: BytecodeTypeArray) {
        self.worklist.push((function_id, type_params.clone()));
        self.visited.insert((function_id, type_params));

        while let Some((function_id, type_params)) = self.worklist.pop() {
            generate_fct(self.vm, function_id, &type_params);
        }
    }
}
