use std::collections::HashSet;
use std::time::Instant;

use dora_bytecode::{
    BytecodeFunction, BytecodeInstruction, BytecodeReader, BytecodeTraitType, BytecodeType,
    BytecodeTypeArray, ConstPoolEntry, FunctionId, ImplId, Program,
};

use crate::compiler::{SpecializeSelf, get_bytecode};
use crate::vm::{
    AotShapeKey, find_trait_impl_in_program, find_trait_ty_impl_in_program,
    specialize_trait_ty_in_program, specialize_ty_array_in_program, specialize_ty_in_program,
};

pub(super) fn compute_transitive_closure(
    program: &Program,
    entry_id: FunctionId,
    tests: &[FunctionId],
    emit_compiler: bool,
) -> TransitiveClosure {
    let start = Instant::now();

    let mut compile_all = TransitiveClosureComputation::new(program);
    compile_all.push(entry_id, BytecodeTypeArray::empty());

    for test_fct_id in tests {
        compile_all.push(*test_fct_id, BytecodeTypeArray::empty());
    }

    let tc = compile_all.compute();
    let duration = start.elapsed();

    if emit_compiler {
        println!(
            "computed transitive closure of boots in {:.2}ms ({} functions, {} thunks)",
            duration.as_secs_f32() * 1000.0f32,
            tc.functions.len(),
            tc.thunks.len(),
        );
    }

    tc
}

pub(super) struct TransitiveClosure {
    pub(super) functions: Vec<(FunctionId, BytecodeTypeArray)>,
    pub(super) thunks: Vec<TraitObjectThunk>,
    pub(super) shape_keys: Vec<AotShapeKey>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(super) struct TraitObjectThunk {
    // The trait method exposed through the trait-object vtable.
    pub(super) trait_fct_id: FunctionId,
    // The full trait-object type at the call boundary, including trait params
    // and associated-type bindings.
    pub(super) trait_object_ty: BytecodeType,
    // The concrete type stored inside the trait object.
    pub(super) actual_object_ty: BytecodeType,
}

struct TransitiveClosureComputation<'a> {
    program: &'a Program,
    worklist: Vec<(FunctionId, BytecodeTypeArray)>,
    worklist_idx: usize,
    visited: HashSet<(FunctionId, BytecodeTypeArray)>,
    visited_thunks: HashSet<TraitObjectThunk>,
    shape_keys: Vec<AotShapeKey>,
    thunks: Vec<TraitObjectThunk>,
}

impl<'a> TransitiveClosureComputation<'a> {
    fn new(program: &'a Program) -> TransitiveClosureComputation<'a> {
        TransitiveClosureComputation {
            program,
            worklist: Vec::new(),
            worklist_idx: 0,
            visited: HashSet::new(),
            visited_thunks: HashSet::new(),
            shape_keys: Vec::new(),
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
            shape_keys: self.shape_keys,
        }
    }

    fn trace(&mut self, fct_id: FunctionId, type_params: BytecodeTypeArray) {
        let fct = &self.program.fct(fct_id);

        if let Some((bytecode_function, specialize_self)) = get_bytecode(self.program, fct) {
            self.iterate_bytecode(bytecode_function, type_params, specialize_self);
        }
    }

    fn iterate_bytecode(
        &mut self,
        bytecode_function: &BytecodeFunction,
        type_params: BytecodeTypeArray,
        specialize_self: Option<SpecializeSelf>,
    ) {
        let reader = BytecodeReader::new(bytecode_function.code());
        let specialize_self = specialize_self.as_ref();

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

                    let callee_type_params = specialize_ty_array_in_program(
                        self.program,
                        specialize_self,
                        &callee_type_params,
                        &type_params,
                    );
                    self.push(callee_fct_id, callee_type_params);
                }

                BytecodeInstruction::InvokeGenericDirect { fct, .. }
                | BytecodeInstruction::InvokeGenericStatic { fct, .. } => {
                    let ConstPoolEntry::Generic {
                        object_type,
                        trait_ty,
                        fct_id: callee_trait_fct_id,
                        fct_type_params: callee_fct_type_params,
                    } = bytecode_function.const_pool(fct)
                    else {
                        unreachable!()
                    };

                    let generic_ty = specialize_ty_in_program(
                        self.program,
                        specialize_self,
                        object_type.clone(),
                        &type_params,
                    );
                    let trait_ty = specialize_trait_ty_in_program(
                        self.program,
                        specialize_self,
                        trait_ty,
                        &type_params,
                    );

                    let (callee_id, callee_container_bindings) = find_trait_impl_in_program(
                        self.program,
                        *callee_trait_fct_id,
                        trait_ty,
                        generic_ty,
                    );

                    let callee_fct_type_params = specialize_ty_array_in_program(
                        self.program,
                        specialize_self,
                        callee_fct_type_params,
                        &type_params,
                    );
                    let combined_type_params =
                        callee_container_bindings.connect(&callee_fct_type_params);
                    self.push(callee_id, combined_type_params);
                }

                BytecodeInstruction::NewLambda { idx, .. } => {
                    let (callee_id, callee_type_params) = match bytecode_function.const_pool(idx) {
                        ConstPoolEntry::Fct(fct_id, type_params) => (*fct_id, type_params),
                        _ => unreachable!(),
                    };

                    let callee_type_params = specialize_ty_array_in_program(
                        self.program,
                        specialize_self,
                        &callee_type_params,
                        &type_params,
                    );
                    self.push(callee_id, callee_type_params.clone());
                    self.shape_keys
                        .push(AotShapeKey::Lambda(callee_id, callee_type_params));
                }

                BytecodeInstruction::NewTraitObject { idx, .. } => {
                    let (trait_ty, actual_object_ty) = match bytecode_function.const_pool(idx) {
                        ConstPoolEntry::TraitObject {
                            trait_ty,
                            actual_object_ty,
                        } => (trait_ty.clone(), actual_object_ty.clone()),
                        _ => unreachable!(),
                    };

                    let trait_ty = specialize_ty_in_program(
                        self.program,
                        specialize_self,
                        trait_ty,
                        &type_params,
                    );
                    let actual_object_ty = specialize_ty_in_program(
                        self.program,
                        specialize_self,
                        actual_object_ty,
                        &type_params,
                    );

                    self.push_trait_object_targets(trait_ty.clone(), actual_object_ty.clone());
                    self.shape_keys.push(AotShapeKey::TraitObject {
                        trait_ty,
                        actual_object_ty,
                    });
                }

                BytecodeInstruction::LoadGlobal { global_id, .. }
                | BytecodeInstruction::StoreGlobal { global_id, .. } => {
                    let global = self.program.global(global_id);
                    if let Some(callee_id) = global.initial_value {
                        self.push(callee_id, BytecodeTypeArray::empty());
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

    fn push_thunk(&mut self, thunk: TraitObjectThunk) {
        if self.visited_thunks.insert(thunk.clone()) {
            self.thunks.push(thunk);
        }
    }

    fn push_trait_object_targets(
        &mut self,
        trait_object_ty: BytecodeType,
        actual_object_ty: BytecodeType,
    ) {
        let trait_ty = trait_object_ty_to_trait_ty(self.program, &trait_object_ty);
        let trait_id = trait_ty.trait_id;
        let (impl_id, impl_type_params) =
            find_trait_ty_impl_in_program(self.program, trait_ty, actual_object_ty.clone())
                .expect("no impl found for trait object");
        for &trait_fct_id in &self.program.trait_(trait_id).virtual_methods {
            self.push_trait_object_method_target(
                trait_fct_id,
                trait_object_ty.clone(),
                actual_object_ty.clone(),
                impl_id,
                impl_type_params.clone(),
            );
        }
    }

    fn push_trait_object_method_target(
        &mut self,
        trait_fct_id: FunctionId,
        trait_object_ty: BytecodeType,
        actual_object_ty: BytecodeType,
        impl_id: ImplId,
        impl_type_params: BytecodeTypeArray,
    ) {
        let impl_method_id = {
            let impl_ = self.program.impl_(impl_id);
            impl_
                .trait_method_map
                .iter()
                .find_map(|(trait_method_id, impl_method_id)| {
                    (*trait_method_id == trait_fct_id).then_some(*impl_method_id)
                })
                .expect("trait method id not found")
        };
        let thunk = TraitObjectThunk {
            trait_fct_id,
            trait_object_ty,
            actual_object_ty,
        };
        self.push_thunk(thunk);

        self.push(impl_method_id, impl_type_params);
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

fn trait_object_ty_to_trait_ty(
    program: &Program,
    trait_object_ty: &BytecodeType,
) -> BytecodeTraitType {
    let BytecodeType::TraitObject(trait_id, type_params, assoc_types) = trait_object_ty else {
        unreachable!("trait object expected");
    };
    let trait_ = program.trait_(*trait_id);
    assert_eq!(trait_.aliases.len(), assoc_types.len());
    let bindings = trait_
        .aliases
        .iter()
        .zip(assoc_types.iter())
        .map(|(alias_id, ty)| (*alias_id, ty))
        .collect();

    BytecodeTraitType {
        trait_id: *trait_id,
        type_params: type_params.clone(),
        bindings,
    }
}
