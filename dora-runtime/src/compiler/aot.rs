use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::time::Instant;

use dora_bytecode::{
    BytecodeFunction, BytecodeInstruction, BytecodeReader, BytecodeTraitType, BytecodeType,
    BytecodeTypeArray, ClassId, ConstPoolEntry, ConstPoolIdx, EnumId, FunctionId, ImplId, Location,
    PackageId, Program, display_fct, display_fct_specialized, display_ty, display_ty_array,
};

use crate::aot::layout::AotLayout;
use crate::compiler::codegen::{CompilerInvocation, compile_fct_to_descriptor};
use crate::compiler::runtime_entry_trampoline;
use crate::compiler::{
    CompilationMode, NativeFct, NativeFctKind, NativeTarget, trait_object_thunk,
};
use crate::gc::{Address, formatted_size};
use crate::mem;
use crate::mirror::Header;
use crate::os;
use crate::size::InstanceSize;
use crate::startup::encode_shape_fields;
use crate::vm::CollectorName;
use crate::vm::{
    AotShapeKey, BytecodeTypeExt, Code, CodeDescriptor, CodeKind, FieldInstance,
    LazyCompilationSite, RelocationKind, RuntimeFunction, ShapeKind, VM, execute_on_main,
    find_trait_impl_in_program, find_trait_ty_impl_in_program, install_code_stub, specialize_bty,
    specialize_trait_ty_in_program, specialize_ty_array_in_program, specialize_ty_in_program,
};
use crate::{ShapeVisitor, SpecializeSelf, get_bytecode};

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

fn compute_transitive_closure(
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
struct TransitiveClosure {
    functions: Vec<(FunctionId, BytecodeTypeArray)>,
    thunks: Vec<TraitObjectThunk>,
    shape_keys: Vec<AotShapeKey>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct TraitObjectThunk {
    // The trait method exposed through the trait-object vtable.
    trait_fct_id: FunctionId,
    // The full trait-object type at the call boundary, including trait params
    // and associated-type bindings.
    trait_object_ty: BytecodeType,
    // The concrete type stored inside the trait object.
    actual_object_ty: BytecodeType,
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

enum CompiledFunctionTarget {
    Function {
        fct_id: FunctionId,
        type_params: BytecodeTypeArray,
    },
    TraitObjectThunk(TraitObjectThunk),
}

impl CompiledFunctionTarget {
    fn fct_id(&self) -> FunctionId {
        match self {
            CompiledFunctionTarget::Function { fct_id, .. } => *fct_id,
            CompiledFunctionTarget::TraitObjectThunk(thunk) => thunk.trait_fct_id,
        }
    }
}

struct CompiledFunction {
    target: CompiledFunctionTarget,
    code: CodeDescriptor,
    code_kind: CodeKind,
}

struct CompiledTransitiveClosure {
    function_addresses: HashMap<(FunctionId, BytecodeTypeArray), Address>,
    trait_object_thunk_addresses: HashMap<TraitObjectThunk, Address>,
    functions: Vec<CompiledFunction>,
    counter: usize,
}

impl CompiledTransitiveClosure {
    fn new() -> CompiledTransitiveClosure {
        CompiledTransitiveClosure {
            function_addresses: HashMap::new(),
            trait_object_thunk_addresses: HashMap::new(),
            functions: Vec::new(),
            counter: 0,
        }
    }

    fn get_address(&self, id: FunctionId) -> Option<Address> {
        self.function_addresses
            .get(&(id, BytecodeTypeArray::empty()))
            .cloned()
    }
}

struct AotNativeLookup {
    methods: HashMap<FunctionId, AotNativeMethod>,
}

#[derive(Clone, Copy)]
struct AotNativeMethod {
    address: Address,
    symbol: &'static str,
}

impl AotNativeLookup {
    fn from_vm(vm: &VM, tc: &TransitiveClosure) -> AotNativeLookup {
        let mut methods = HashMap::new();

        for (fct_id, _) in &tc.functions {
            if methods.contains_key(fct_id) {
                continue;
            }

            if let Some(address) = vm.native_methods.get(*fct_id) {
                let symbol = vm
                    .native_methods
                    .get_symbol(*fct_id)
                    .expect("missing native symbol");
                methods.insert(*fct_id, AotNativeMethod { address, symbol });
            }
        }

        AotNativeLookup { methods }
    }

    fn get_address(&self, fct_id: FunctionId) -> Option<Address> {
        self.methods.get(&fct_id).map(|method| method.address)
    }

    fn get_symbol(&self, fct_id: FunctionId) -> Option<&'static str> {
        self.methods.get(&fct_id).map(|method| method.symbol)
    }
}

struct AotCodegenContext<'a> {
    vm: &'a VM,
    program: &'a Program,
    native_lookup: &'a AotNativeLookup,
    compiler: CompilerInvocation,
    mode: CompilationMode,
}

fn compile_transitive_closure(
    ctx: &AotCodegenContext<'_>,
    tc: &TransitiveClosure,
) -> CompiledTransitiveClosure {
    let mut ctc = CompiledTransitiveClosure::new();
    compile_functions(ctx, tc, &mut ctc);
    compile_thunks(ctx, tc, &mut ctc);
    ctc
}

fn install_compiled_transitive_closure(
    ctx: &AotCodegenContext<'_>,
    ctc: &mut CompiledTransitiveClosure,
) -> Vec<Arc<Code>> {
    let mut installed_codes = Vec::with_capacity(ctc.functions.len());

    for entry in &ctc.functions {
        let code = install_code_stub(ctx.vm, entry.code.clone(), entry.code_kind.clone());

        match &entry.target {
            CompiledFunctionTarget::Function {
                fct_id,
                type_params,
            } => {
                let existing = ctc
                    .function_addresses
                    .insert((*fct_id, type_params.clone()), code.instruction_start());
                assert!(existing.is_none());
            }

            CompiledFunctionTarget::TraitObjectThunk(thunk) => {
                let existing = ctc
                    .trait_object_thunk_addresses
                    .insert(thunk.clone(), code.instruction_start());
                assert!(existing.is_none());
            }
        }

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

fn compile_functions(
    ctx: &AotCodegenContext<'_>,
    tc: &TransitiveClosure,
    ctc: &mut CompiledTransitiveClosure,
) {
    for (fct_id, type_params) in &tc.functions {
        compile_function(ctx, *fct_id, type_params.clone(), ctc);
    }
}

fn compile_function(
    ctx: &AotCodegenContext<'_>,
    fct_id: FunctionId,
    type_params: BytecodeTypeArray,
    ctc: &mut CompiledTransitiveClosure,
) {
    let fct = ctx.program.fct(fct_id);

    if let Some(native_fctptr) = ctx.native_lookup.get_address(fct_id) {
        // Method is implemented in native code. Create trampoline for invoking it.
        let target = if matches!(ctx.mode, CompilationMode::Aot) {
            let symbol = ctx
                .native_lookup
                .get_symbol(fct_id)
                .expect("missing native symbol");
            NativeTarget::Symbol(symbol)
        } else {
            NativeTarget::Address(native_fctptr)
        };

        let internal_fct = NativeFct {
            target,
            args: BytecodeTypeArray::new(fct.params.clone()),
            return_type: fct.return_type.clone(),
            desc: NativeFctKind::RuntimeEntryTrampoline(fct_id),
        };

        let code_kind = runtime_entry_trampoline::code_kind(&internal_fct.desc);
        let code = runtime_entry_trampoline::generate(ctx.vm, internal_fct, false);
        ctc.functions.push(CompiledFunction {
            target: CompiledFunctionTarget::Function {
                fct_id,
                type_params,
            },
            code,
            code_kind,
        });
    } else if let Some(_) = get_bytecode(ctx.program, fct) {
        let (code, code_kind) = compile_fct_aot(ctx, fct_id, &type_params);
        ctc.counter += 1;
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

fn compile_fct_aot(
    ctx: &AotCodegenContext<'_>,
    fct_id: FunctionId,
    type_params: &BytecodeTypeArray,
) -> (CodeDescriptor, CodeKind) {
    let program_fct = ctx.program.fct(fct_id);
    let params = BytecodeTypeArray::new(program_fct.params.clone());
    let (bytecode_fct, specialize_self) =
        get_bytecode(ctx.program, program_fct).expect("missing bytecode");

    let (code_descriptor, _, code_kind) = compile_fct_to_descriptor(
        ctx.vm,
        ctx.program,
        fct_id,
        program_fct,
        params,
        program_fct.return_type.clone(),
        bytecode_fct,
        type_params,
        specialize_self,
        ctx.compiler,
        false,
        ctx.mode,
    );
    (code_descriptor, code_kind)
}

fn compile_thunks(
    ctx: &AotCodegenContext<'_>,
    tc: &TransitiveClosure,
    ctc: &mut CompiledTransitiveClosure,
) {
    for thunk in &tc.thunks {
        let (code, code_kind) = trait_object_thunk::ensure_compiled_aot(
            ctx.vm,
            ctx.program,
            thunk.trait_fct_id,
            thunk.trait_object_ty.clone(),
            thunk.actual_object_ty.clone(),
            ctx.compiler,
            ctx.mode,
        );

        ctc.functions.push(CompiledFunction {
            target: CompiledFunctionTarget::TraitObjectThunk(thunk.clone()),
            code,
            code_kind,
        });
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
                    let target = ctc
                        .function_addresses
                        .get(&(*fct_id, type_params.clone()))
                        .cloned();

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
                                get_bytecode(ctx.program, ctx.program.fct(*fct_id)).is_some();
                            eprintln!(
                                "has_native={}, has_bytecode={}, function_addresses.len={}",
                                has_native,
                                has_bytecode,
                                ctc.function_addresses.len()
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

pub struct AotCallRelocation {
    /// Offset of the return address (position after the call instruction).
    pub offset: u32,
    /// Final symbol name of the call target.
    pub target: String,
}

#[derive(Clone, Copy)]
pub enum AotCodeKind {
    Optimized,
    RuntimeEntryTrampoline,
    AllocationFailureTrampoline,
    TrapTrampoline,
    SafepointTrampoline,
    DoraEntryTrampoline,
}

pub struct AotGcPoint {
    pub pc_offset: u32,
    pub offsets: Vec<i32>,
}

#[derive(Clone)]
pub struct AotLocation {
    pub pc_offset: u32,
    pub inlined_function_id: Option<u32>,
    pub line: u32,
    pub column: u32,
}

pub struct AotFunctionInfo {
    pub name: AotStringId,
    pub file: AotStringId,
    pub loc: Location,
}

pub struct AotInlinedFunction {
    pub function: AotFunctionInfo,
    pub inlined_function_id: Option<u32>,
    pub line: u32,
    pub column: u32,
}

pub struct AotStringRelocation {
    /// Offset of the RIP-relative disp32 in the string-load instruction.
    pub offset: u32,
    /// Interned UTF-8 string payload referenced by this relocation.
    pub string_id: AotStringId,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct AotStringId(u32);

impl AotStringId {
    pub fn index(self) -> u32 {
        self.0
    }
}

#[derive(Clone)]
pub struct AotStringTable {
    entries: Vec<String>,
    map: HashMap<String, AotStringId>,
}

impl AotStringTable {
    pub fn new() -> AotStringTable {
        AotStringTable {
            entries: Vec::new(),
            map: HashMap::new(),
        }
    }

    pub fn intern(&mut self, value: &str) -> AotStringId {
        if let Some(&id) = self.map.get(value) {
            return id;
        }

        let id = AotStringId(
            u32::try_from(self.entries.len()).expect("too many strings in AOT string table"),
        );
        let value = value.to_string();
        self.entries.push(value.clone());
        self.map.insert(value, id);
        id
    }

    pub fn entries(&self) -> &[String] {
        &self.entries
    }
}

pub struct AotShapeRelocation {
    pub offset: u32,
    pub shape_id: AotShapeId,
}

pub struct AotGlobalRelocation {
    /// Offset of the RIP-relative disp32 in the lea instruction.
    pub offset: u32,
    /// Byte offset into the global memory block.
    pub global_offset: usize,
}

pub struct AotFunction {
    pub symbol_name: String,
    pub fct_id: u32,
    pub function: AotFunctionInfo,
    pub kind: AotCodeKind,
    pub code: Vec<u8>,
    pub call_relocations: Vec<AotCallRelocation>,
    pub string_relocations: Vec<AotStringRelocation>,
    pub shape_relocations: Vec<AotShapeRelocation>,
    pub global_relocations: Vec<AotGlobalRelocation>,
    pub gcpoints: Vec<AotGcPoint>,
    pub locations: Vec<AotLocation>,
    pub inlined_functions: Vec<AotInlinedFunction>,
}

pub fn mangle_name(name: &str) -> String {
    let mut result = String::with_capacity(name.len() + 6);
    result.push_str("_dora_");
    for ch in name.chars() {
        match ch {
            'a'..='z' | 'A'..='Z' | '0'..='9' => result.push(ch),
            _ => result.push('_'),
        }
    }
    result
}

pub struct AotShape {
    pub id: u32,
    pub name: String,
    pub kind: ShapeKind,
    pub fields: Vec<u8>,
    pub visitor: ShapeVisitor,
    pub refs: Vec<i32>,
    pub instance_size: u64,
    pub element_size: u64,
    pub vtable_entries: Vec<Option<String>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum AotKnownShapeKind {
    ByteArray,
    Int32Array,
    String,
    Thread,
    FillerWord,
    FillerArray,
    FreeSpace,
    Code,
}

pub struct AotKnownShape {
    pub kind: AotKnownShapeKind,
    pub shape_id: AotShapeId,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct AotShapeId(pub(crate) u32);

#[derive(Default)]
struct AotShapeInterner {
    keys: Vec<AotShapeKey>,
    ids: HashMap<AotShapeKey, AotShapeId>,
}

impl AotShapeInterner {
    fn intern(&mut self, key: AotShapeKey) -> AotShapeId {
        if let Some(&id) = self.ids.get(&key) {
            return id;
        }

        let id =
            AotShapeId(u32::try_from(self.keys.len()).expect("too many shapes in AOT shape table"));
        self.keys.push(key.clone());
        self.ids.insert(key, id);
        id
    }

    fn get(&self, key: &AotShapeKey) -> AotShapeId {
        *self.ids.get(key).expect("missing AOT shape key")
    }

    fn keys(&self) -> &[AotShapeKey] {
        &self.keys
    }
}

pub struct AotCompilation {
    pub strings: AotStringTable,
    pub functions: Vec<AotFunction>,
    pub shapes: Vec<AotShape>,
    pub known_shapes: Vec<AotKnownShape>,
    pub global_layout: GlobalLayout,
    pub collector_name: CollectorName,
}

pub struct AotCompileInputs {
    known_elements: AotKnownElements,
    emit_compiler: bool,
}

impl AotCompileInputs {
    pub fn from_vm(vm: &VM) -> AotCompileInputs {
        AotCompileInputs {
            known_elements: AotKnownElements::from_vm(vm),
            emit_compiler: vm.flags.emit_compiler,
        }
    }
}

pub fn compile_program(
    vm: &VM,
    program: &Program,
    boots_compile_fct_address: *const u8,
    inputs: AotCompileInputs,
) -> AotCompilation {
    assert!(
        std::ptr::eq(program, &vm.program),
        "AOT compilation still requires the input Program to be installed in the VM"
    );

    let main_fct_id = program.main_fct_id.expect("no main function");

    let boots_address = Address::from_ptr(boots_compile_fct_address);
    let compiler = CompilerInvocation::Boots(boots_address);

    let tc = compute_transitive_closure(program, main_fct_id, &[], inputs.emit_compiler);
    let native_lookup = AotNativeLookup::from_vm(vm, &tc);
    let ctx = AotCodegenContext {
        vm,
        program,
        native_lookup: &native_lookup,
        compiler,
        mode: CompilationMode::Aot,
    };
    let ctc = compile_transitive_closure(&ctx, &tc);
    let mut strings = AotStringTable::new();
    let runtime_functions =
        compile_aot_runtime_trampolines(&ctx, &mut strings, inputs.known_elements);

    build_aot_compilation(
        program,
        &tc,
        ctc,
        inputs.known_elements,
        strings,
        runtime_functions,
    )
}

pub fn compile_boots_compiler(
    vm: &VM,
    entry_id: FunctionId,
    boots_compile_fct_address: *const u8,
    inputs: AotCompileInputs,
) -> AotCompilation {
    let boots_address = Address::from_ptr(boots_compile_fct_address);
    let compiler = CompilerInvocation::Boots(boots_address);

    let tc = compute_transitive_closure(&vm.program, entry_id, &[], inputs.emit_compiler);
    let native_lookup = AotNativeLookup::from_vm(vm, &tc);
    let ctx = AotCodegenContext {
        vm,
        program: &vm.program,
        native_lookup: &native_lookup,
        compiler,
        mode: CompilationMode::Aot,
    };
    let ctc = compile_transitive_closure(&ctx, &tc);
    let mut strings = AotStringTable::new();
    let runtime_functions =
        compile_aot_runtime_trampolines(&ctx, &mut strings, inputs.known_elements);

    build_aot_compilation(
        &vm.program,
        &tc,
        ctc,
        inputs.known_elements,
        strings,
        runtime_functions,
    )
}

fn build_aot_compilation(
    program: &Program,
    tc: &TransitiveClosure,
    ctc: CompiledTransitiveClosure,
    known_elements: AotKnownElements,
    mut strings: AotStringTable,
    runtime_functions: Vec<AotFunction>,
) -> AotCompilation {
    // Compute global memory layout (same logic as init_global_addresses in globals.rs).
    let layout = AotLayout::new(program);
    let global_layout = compute_global_layout(&layout, program);

    let mut symbols = AotSymbolMaps {
        functions: HashMap::new(),
        trait_object_thunks: HashMap::new(),
    };
    let mut shape_interner = AotShapeInterner::default();
    let mut aot_functions = Vec::new();

    intern_known_shapes(known_elements.classes, &mut shape_interner);

    for entry in &ctc.functions {
        let fct_id = entry.target.fct_id();
        let kind = match &entry.code_kind {
            CodeKind::OptimizedFct(_) => AotCodeKind::Optimized,
            CodeKind::RuntimeEntryTrampoline(_) => AotCodeKind::RuntimeEntryTrampoline,
            _ => unreachable!("unexpected code kind in AOT compilation output"),
        };

        let name = aot_compiled_function_name(program, entry);
        let symbol_name = mangle_name(&name);
        match &entry.target {
            CompiledFunctionTarget::Function {
                fct_id,
                type_params,
            } => {
                symbols
                    .functions
                    .insert((*fct_id, type_params.clone()), symbol_name.clone());
            }

            CompiledFunctionTarget::TraitObjectThunk(thunk) => {
                symbols
                    .trait_object_thunks
                    .insert(thunk.clone(), symbol_name.clone());
            }
        }

        let fct = program.fct(fct_id);
        let file = program.file(fct.file_id).path.clone();
        let function = AotFunctionInfo {
            name: strings.intern(&name),
            file: strings.intern(&file),
            loc: fct.loc,
        };
        let bytes = entry.code.code.clone();
        let mut gcpoints = Vec::new();
        let mut locations = Vec::new();

        for (pc_offset, gcpoint) in entry.code.gcpoints.entries() {
            gcpoints.push(AotGcPoint {
                pc_offset: *pc_offset,
                offsets: gcpoint.offsets.clone(),
            });
        }

        for (pc_offset, location) in entry.code.positions.entries() {
            locations.push(AotLocation {
                pc_offset: *pc_offset,
                inlined_function_id: location.inlined_function_id.map(|id| id.0),
                line: location.location.line(),
                column: location.location.column(),
            });
        }

        let inlined_functions = entry
            .code
            .inlined_functions
            .iter()
            .map(|inlined| {
                let fct = program.fct(inlined.fct_id);

                let name = display_fct_specialized(program, inlined.fct_id, &inlined.type_params);
                let file = program.file(fct.file_id).path.clone();

                AotInlinedFunction {
                    function: AotFunctionInfo {
                        name: strings.intern(&name),
                        file: strings.intern(&file),
                        loc: fct.loc,
                    },
                    inlined_function_id: inlined
                        .inlined_location
                        .inlined_function_id
                        .map(|id| id.0),
                    line: inlined.inlined_location.location.line(),
                    column: inlined.inlined_location.location.column(),
                }
            })
            .collect();

        let mut call_relocations = Vec::new();
        let mut string_relocations = Vec::new();
        let mut shape_relocations = Vec::new();
        let mut global_relocations = Vec::new();
        for (offset, site) in entry.code.lazy_compilation.entries() {
            match site {
                LazyCompilationSite::Direct {
                    fct_id,
                    type_params,
                    ..
                } => {
                    let target_name = display_fct_specialized(program, *fct_id, type_params);
                    call_relocations.push(AotCallRelocation {
                        offset: *offset,
                        target: mangle_name(&target_name),
                    });
                }
                _ => {}
            }
        }

        for (offset, reloc_kind) in &entry.code.relocations.entries {
            match reloc_kind {
                RelocationKind::DirectCall {
                    fct_id,
                    type_params,
                } => {
                    let target_name = display_fct_specialized(program, *fct_id, type_params);
                    call_relocations.push(AotCallRelocation {
                        offset: *offset,
                        target: mangle_name(&target_name),
                    });
                }
                RelocationKind::NativeCall(symbol) => {
                    call_relocations.push(AotCallRelocation {
                        offset: *offset,
                        target: symbol.clone(),
                    });
                }
                RelocationKind::RuntimeFunction(runtime_function) => {
                    call_relocations.push(AotCallRelocation {
                        offset: *offset,
                        target: runtime_function_symbol(*runtime_function).to_string(),
                    });
                }
                RelocationKind::StringConst {
                    owner_fct_id,
                    const_pool_idx,
                } => {
                    let value = resolve_string_relocation(program, *owner_fct_id, *const_pool_idx);
                    string_relocations.push(AotStringRelocation {
                        offset: *offset,
                        string_id: strings.intern(&value),
                    });
                }
                RelocationKind::Shape { key } => {
                    let shape_id = shape_interner.intern(key.clone());
                    shape_relocations.push(AotShapeRelocation {
                        offset: *offset,
                        shape_id,
                    });
                }
                RelocationKind::GlobalValueAddress { global_id } => {
                    let global_offset = global_layout.value_offsets[global_id.index()];
                    global_relocations.push(AotGlobalRelocation {
                        offset: *offset,
                        global_offset,
                    });
                }
                RelocationKind::GlobalStateAddress { global_id } => {
                    let global_offset = global_layout.state_offsets[global_id.index()];
                    global_relocations.push(AotGlobalRelocation {
                        offset: *offset,
                        global_offset,
                    });
                }
                _ => {}
            }
        }

        aot_functions.push(AotFunction {
            symbol_name,
            fct_id: fct_id.index_as_u32(),
            function,
            kind,
            code: bytes,
            call_relocations,
            string_relocations,
            shape_relocations,
            global_relocations,
            gcpoints,
            locations,
            inlined_functions,
        });
    }

    intern_shape_keys(&mut shape_interner, &tc.shape_keys);

    let known_shapes = build_known_shapes(known_elements.classes, &shape_interner);
    let shapes = encode_aot_shapes(
        &layout,
        program,
        known_elements.classes,
        &symbols,
        &shape_interner,
    );

    aot_functions.extend(runtime_functions);

    AotCompilation {
        strings,
        functions: aot_functions,
        shapes,
        known_shapes,
        global_layout,
        collector_name: known_elements.collector_name,
    }
}

fn compile_aot_runtime_trampolines(
    ctx: &AotCodegenContext<'_>,
    strings: &mut AotStringTable,
    known_elements: AotKnownElements,
) -> Vec<AotFunction> {
    let mut runtime_functions = Vec::new();

    let function_info = synthetic_function_info(strings, "dora_aot_trap_trampoline");
    runtime_functions.push(compile_runtime_function_trampoline(
        ctx,
        "dora_aot_trap_trampoline",
        "dora_native_trap",
        function_info,
        BytecodeTypeArray::one(BytecodeType::Int32),
        BytecodeType::Unit,
        NativeFctKind::TrapTrampoline,
        AotCodeKind::TrapTrampoline,
    ));
    let function_info = synthetic_function_info(strings, "dora_aot_safepoint_trampoline");
    runtime_functions.push(compile_runtime_function_trampoline(
        ctx,
        "dora_aot_safepoint_trampoline",
        "dora_native_safepoint_slow",
        function_info,
        BytecodeTypeArray::empty(),
        BytecodeType::Unit,
        NativeFctKind::SafepointTrampoline,
        AotCodeKind::SafepointTrampoline,
    ));
    let function_info = synthetic_function_info(strings, "dora_aot_gc_allocation_trampoline");
    runtime_functions.push(compile_runtime_function_trampoline(
        ctx,
        "dora_aot_gc_allocation_trampoline",
        "dora_native_gc_alloc",
        function_info,
        BytecodeTypeArray::new(vec![BytecodeType::Int64, BytecodeType::Bool]),
        BytecodeType::Ptr,
        NativeFctKind::GcAllocationTrampoline,
        AotCodeKind::AllocationFailureTrampoline,
    ));
    let unreachable_fct_id = known_elements.unreachable_fct_id;
    let function_info = function_info_for_fct(ctx.program, strings, unreachable_fct_id);
    runtime_functions.push(compile_runtime_function_trampoline(
        ctx,
        "dora_aot_unreachable_trampoline",
        "dora_native_unreachable",
        function_info,
        BytecodeTypeArray::empty(),
        BytecodeType::Unit,
        NativeFctKind::RuntimeEntryTrampoline(unreachable_fct_id),
        AotCodeKind::RuntimeEntryTrampoline,
    ));
    let fatal_error_fct_id = known_elements.fatal_error_fct_id;
    let function_info = function_info_for_fct(ctx.program, strings, fatal_error_fct_id);
    runtime_functions.push(compile_runtime_function_trampoline(
        ctx,
        "dora_aot_fatal_error_trampoline",
        "dora_native_fatal_error",
        function_info,
        BytecodeTypeArray::one(BytecodeType::Ptr),
        BytecodeType::Unit,
        NativeFctKind::RuntimeEntryTrampoline(fatal_error_fct_id),
        AotCodeKind::RuntimeEntryTrampoline,
    ));

    runtime_functions
}

fn compile_runtime_function_trampoline(
    ctx: &AotCodegenContext<'_>,
    symbol_name: &'static str,
    target_symbol: &'static str,
    function: AotFunctionInfo,
    args: BytecodeTypeArray,
    return_type: BytecodeType,
    desc: NativeFctKind,
    kind: AotCodeKind,
) -> AotFunction {
    let fct_id = match &desc {
        NativeFctKind::RuntimeEntryTrampoline(fct_id) => fct_id.index_as_u32(),
        _ => 0,
    };
    let native_fct = NativeFct {
        target: NativeTarget::Symbol(target_symbol),
        args,
        return_type,
        desc,
    };
    let code = runtime_entry_trampoline::generate(ctx.vm, native_fct, false);
    let gcpoints = code.gcpoints.entries();
    assert_eq!(gcpoints.len(), 1);
    let (pc_offset, gcpoint) = &gcpoints[0];
    let gcpoints = vec![AotGcPoint {
        pc_offset: *pc_offset,
        offsets: gcpoint.offsets.clone(),
    }];

    let relocations = &code.relocations.entries;
    assert_eq!(relocations.len(), 1);
    let (offset, reloc_kind) = &relocations[0];
    let RelocationKind::NativeCall(symbol) = reloc_kind else {
        unreachable!("unexpected relocation in AOT GC allocation trampoline");
    };
    let call_relocations = vec![AotCallRelocation {
        offset: *offset,
        target: symbol.clone(),
    }];

    AotFunction {
        symbol_name: symbol_name.to_string(),
        fct_id,
        function,
        kind,
        code: code.code,
        call_relocations,
        string_relocations: Vec::new(),
        shape_relocations: Vec::new(),
        global_relocations: Vec::new(),
        gcpoints,
        locations: Vec::new(),
        inlined_functions: Vec::new(),
    }
}

fn synthetic_function_info(strings: &mut AotStringTable, name: &str) -> AotFunctionInfo {
    AotFunctionInfo {
        name: strings.intern(name),
        file: strings.intern(""),
        loc: Location::new(0, 0),
    }
}

fn function_info_for_fct(
    program: &Program,
    strings: &mut AotStringTable,
    fct_id: FunctionId,
) -> AotFunctionInfo {
    let fct = program.fct(fct_id);
    AotFunctionInfo {
        name: strings.intern(&display_fct(program, fct_id)),
        file: strings.intern(&program.file(fct.file_id).path),
        loc: fct.loc,
    }
}

pub struct GlobalLayout {
    pub memory_size: usize,
    pub references: Vec<i32>,
    pub value_offsets: Vec<usize>,
    pub state_offsets: Vec<usize>,
}

fn compute_global_layout(layout: &AotLayout<'_>, program: &Program) -> GlobalLayout {
    let number_globals = program.globals.len();
    let mut memory_size = 0usize;
    let mut references = Vec::new();
    let mut value_offsets = Vec::with_capacity(number_globals);
    let mut state_offsets = Vec::with_capacity(number_globals);

    let initialized_field_size = 1;

    for global_var in &program.globals {
        let state_offset = memory_size;
        memory_size += initialized_field_size;

        let ty = global_var.ty.clone();
        assert!(ty.is_concrete_type());

        let ty_size = layout.size(ty.clone()) as usize;
        let ty_align = layout.align(ty.clone()) as usize;

        let value_offset = mem::align_usize_up(memory_size, ty_align);
        layout.add_ref_fields(&mut references, value_offset as i32, ty);
        state_offsets.push(state_offset);
        value_offsets.push(value_offset);
        memory_size = value_offset + ty_size;
    }

    GlobalLayout {
        memory_size,
        references,
        value_offsets,
        state_offsets,
    }
}

fn resolve_string_relocation(
    program: &Program,
    owner_fct_id: FunctionId,
    const_pool_idx: ConstPoolIdx,
) -> String {
    let owner = program.fct(owner_fct_id);
    let bytecode = if let Some(bytecode) = owner.bytecode.as_ref() {
        bytecode
    } else {
        let trait_method_id = owner
            .trait_method_impl
            .expect("missing trait method for relocation owner");
        program
            .fct(trait_method_id)
            .bytecode
            .as_ref()
            .expect("missing bytecode for relocation owner")
    };

    match bytecode.const_pool(const_pool_idx) {
        ConstPoolEntry::String(value) => value.clone(),
        _ => panic!(
            "expected string constant for relocation in function {} at const-pool index {}",
            owner_fct_id.index_as_u32(),
            const_pool_idx.0
        ),
    }
}

/// Build a unique display name for a compiled function. Trait object thunks
/// also include the trait object type since associated-type bindings are not
/// part of the normal type-parameter list.
fn aot_compiled_function_name(program: &Program, entry: &CompiledFunction) -> String {
    match &entry.target {
        CompiledFunctionTarget::Function {
            fct_id,
            type_params,
        } => aot_display_name(program, *fct_id, type_params),

        CompiledFunctionTarget::TraitObjectThunk(thunk) => {
            format!(
                "{} for {:?} as {:?}",
                display_fct(program, thunk.trait_fct_id),
                thunk.actual_object_ty,
                thunk.trait_object_ty
            )
        }
    }
}

/// Build a unique display name from a function and type params.
fn aot_display_name(
    program: &Program,
    fct_id: FunctionId,
    type_params: &BytecodeTypeArray,
) -> String {
    let mut name = display_fct_specialized(program, fct_id, type_params);
    let declared = program.fct(fct_id).type_params.names.len();
    if type_params.len() > declared {
        let extra = BytecodeTypeArray::new(type_params.iter().skip(declared).collect());
        use dora_bytecode::display::{TypeParamMode, fmt_type_params};
        name.push_str(&format!(
            "{}",
            fmt_type_params(program, &extra, TypeParamMode::Resolved(type_params))
        ));
    }
    name
}

fn runtime_function_symbol(runtime_function: RuntimeFunction) -> &'static str {
    match runtime_function {
        RuntimeFunction::TrapTrampoline => "dora_aot_trap_trampoline",
        RuntimeFunction::SafepointTrampoline => "dora_aot_safepoint_trampoline",
        RuntimeFunction::GcAllocationTrampoline => "dora_aot_gc_allocation_trampoline",
        RuntimeFunction::WriteBarrierSlowPath => "dora_aot_write_barrier_slow_path",
        RuntimeFunction::UnreachableTrampoline => "dora_aot_unreachable_trampoline",
        RuntimeFunction::FatalErrorTrampoline => "dora_aot_fatal_error_trampoline",
    }
}

struct AotSymbolMaps {
    functions: HashMap<(FunctionId, BytecodeTypeArray), String>,
    trait_object_thunks: HashMap<TraitObjectThunk, String>,
}

#[derive(Clone, Copy)]
struct AotKnownElements {
    classes: AotKnownClasses,
    unreachable_fct_id: FunctionId,
    fatal_error_fct_id: FunctionId,
    collector_name: CollectorName,
}

impl AotKnownElements {
    fn from_vm(vm: &VM) -> AotKnownElements {
        AotKnownElements {
            classes: AotKnownClasses::from_vm(vm),
            unreachable_fct_id: vm
                .known
                .unreachable_fct_id
                .expect("unreachable function missing"),
            fatal_error_fct_id: vm
                .known
                .fatal_error_fct_id
                .expect("fatalError function missing"),
            collector_name: vm.flags.gc.unwrap_or(CollectorName::Swiper),
        }
    }
}

#[derive(Clone, Copy)]
struct AotKnownClasses {
    array_class_id: ClassId,
    thread_class_id: ClassId,
}

impl AotKnownClasses {
    fn from_vm(vm: &VM) -> AotKnownClasses {
        AotKnownClasses {
            array_class_id: vm.known.array_class_id(),
            thread_class_id: vm.known.thread_class_id(),
        }
    }
}

fn encode_aot_shapes(
    layout: &AotLayout<'_>,
    program: &Program,
    known_classes: AotKnownClasses,
    symbols: &AotSymbolMaps,
    interner: &AotShapeInterner,
) -> Vec<AotShape> {
    interner
        .keys()
        .iter()
        .enumerate()
        .map(|(idx, key)| {
            let id = u32::try_from(idx).expect("too many shapes in AOT shape table");
            encode_aot_shape_for_key(layout, program, known_classes, id, key, symbols)
        })
        .collect()
}

fn intern_known_shapes(known_classes: AotKnownClasses, interner: &mut AotShapeInterner) {
    for (_known_kind, key) in known_shape_keys(known_classes) {
        interner.intern(key);
    }
}

fn intern_shape_keys(interner: &mut AotShapeInterner, shape_keys: &[AotShapeKey]) {
    for key in shape_keys {
        interner.intern(key.clone());
    }
}

fn build_known_shapes(
    known_classes: AotKnownClasses,
    interner: &AotShapeInterner,
) -> Vec<AotKnownShape> {
    known_shape_keys(known_classes)
        .into_iter()
        .map(|(kind, key)| AotKnownShape {
            kind,
            shape_id: interner.get(&key),
        })
        .collect()
}

fn known_shape_keys(known_classes: AotKnownClasses) -> Vec<(AotKnownShapeKind, AotShapeKey)> {
    let array_class_id = known_classes.array_class_id;
    vec![
        (
            AotKnownShapeKind::ByteArray,
            AotShapeKey::Array(array_class_id, BytecodeTypeArray::one(BytecodeType::UInt8)),
        ),
        (
            AotKnownShapeKind::Int32Array,
            AotShapeKey::Array(array_class_id, BytecodeTypeArray::one(BytecodeType::Int32)),
        ),
        (AotKnownShapeKind::String, AotShapeKey::String),
        (
            AotKnownShapeKind::Thread,
            AotShapeKey::Class(known_classes.thread_class_id, BytecodeTypeArray::empty()),
        ),
        (AotKnownShapeKind::FillerWord, AotShapeKey::FillerWord),
        (AotKnownShapeKind::FillerArray, AotShapeKey::FillerArray),
        (AotKnownShapeKind::FreeSpace, AotShapeKey::FreeSpace),
        (AotKnownShapeKind::Code, AotShapeKey::Code),
    ]
}

fn encode_aot_shape_for_key(
    layout: &AotLayout<'_>,
    program: &Program,
    known_classes: AotKnownClasses,
    id: u32,
    key: &AotShapeKey,
    symbols: &AotSymbolMaps,
) -> AotShape {
    match key {
        AotShapeKey::FillerWord => encode_internal_aot_shape(
            id,
            "FillerWord",
            ShapeKind::FillerWord,
            InstanceSize::FillerWord,
            ShapeVisitor::None,
        ),
        AotShapeKey::FillerArray => encode_internal_aot_shape(
            id,
            "FillerArray",
            ShapeKind::FillerArray,
            InstanceSize::FillerArray,
            ShapeVisitor::None,
        ),
        AotShapeKey::FreeSpace => encode_internal_aot_shape(
            id,
            "FreeSpace",
            ShapeKind::FreeSpace,
            InstanceSize::FreeSpace,
            ShapeVisitor::None,
        ),
        AotShapeKey::Code => encode_internal_aot_shape(
            id,
            "Code",
            ShapeKind::Code,
            InstanceSize::CodeObject,
            ShapeVisitor::Invalid,
        ),
        AotShapeKey::String => encode_string_aot_shape(id),
        AotShapeKey::Class(class_id, type_params) => {
            encode_class_aot_shape(layout, program, id, *class_id, type_params)
        }
        AotShapeKey::Array(class_id, type_params) => {
            encode_array_aot_shape(layout, program, known_classes, id, *class_id, type_params)
        }
        AotShapeKey::EnumVariant {
            enum_id,
            type_params,
            variant_id,
        } => encode_enum_variant_aot_shape(layout, program, id, *enum_id, type_params, *variant_id),
        AotShapeKey::Lambda(fct_id, type_params) => {
            encode_lambda_aot_shape(program, id, *fct_id, type_params, symbols)
        }
        AotShapeKey::TraitObject {
            trait_ty,
            actual_object_ty,
        } => encode_trait_object_aot_shape(
            layout,
            program,
            id,
            trait_ty.clone(),
            actual_object_ty.clone(),
            symbols,
        ),
    }
}

fn encode_internal_aot_shape(
    id: u32,
    name: &'static str,
    kind: ShapeKind,
    size: InstanceSize,
    visitor: ShapeVisitor,
) -> AotShape {
    AotShape {
        id,
        name: name.to_string(),
        kind,
        fields: encode_shape_fields(&[]),
        visitor,
        refs: Vec::new(),
        instance_size: size.instance_size().unwrap_or(0) as u64,
        element_size: size.element_size().unwrap_or(-1) as usize as u64,
        vtable_entries: Vec::new(),
    }
}

fn encode_string_aot_shape(id: u32) -> AotShape {
    let size = InstanceSize::Str;

    AotShape {
        id,
        name: "String".into(),
        kind: ShapeKind::String,
        fields: encode_shape_fields(&[]),
        visitor: aot_shape_visitor(size),
        refs: Vec::new(),
        instance_size: aot_instance_size(size),
        element_size: aot_element_size(size),
        vtable_entries: Vec::new(),
    }
}

fn encode_class_aot_shape(
    layout: &AotLayout<'_>,
    program: &Program,
    id: u32,
    class_id: ClassId,
    type_params: &BytecodeTypeArray,
) -> AotShape {
    let class = program.class(class_id);
    let mut csize = Header::size();
    let mut fields = Vec::new();
    let mut refs = Vec::new();

    debug_assert!(type_params.iter().all(|ty| ty.is_concrete_type()));

    for field in &class.fields {
        let ty = specialize_ty_in_program(program, None, field.ty.clone(), type_params);
        debug_assert!(ty.is_concrete_type());

        let field_size = layout.size(ty.clone());
        let field_align = layout.align(ty.clone());
        let offset = mem::align_i32(csize, field_align);

        fields.push(FieldInstance {
            offset,
            ty: ty.clone(),
        });
        layout.add_ref_fields(&mut refs, offset, ty);

        csize = offset + field_size;
    }

    let size = InstanceSize::Fixed(mem::align_i32(csize, mem::ptr_width()));

    AotShape {
        id,
        name: display_class_shape_name(program, class_id, type_params),
        kind: ShapeKind::Class(class_id, type_params.clone()),
        fields: encode_shape_fields(&fields),
        visitor: aot_shape_visitor(size),
        refs,
        instance_size: aot_instance_size(size),
        element_size: aot_element_size(size),
        vtable_entries: Vec::new(),
    }
}

fn encode_array_aot_shape(
    layout: &AotLayout<'_>,
    program: &Program,
    known_classes: AotKnownClasses,
    id: u32,
    class_id: ClassId,
    type_params: &BytecodeTypeArray,
) -> AotShape {
    let class = program.class(class_id);
    assert!(class.fields.is_empty());
    assert_eq!(known_classes.array_class_id, class_id);
    assert_eq!(type_params.len(), 1);

    let size = layout.array_shape_size(&type_params[0]);
    let refs = array_aot_refs(layout, size, type_params);
    let size = normalize_aot_shape_size(size, &refs);

    AotShape {
        id,
        name: display_class_shape_name(program, class_id, type_params),
        kind: ShapeKind::Array(class_id, type_params.clone()),
        fields: encode_shape_fields(&[]),
        visitor: aot_shape_visitor(size),
        refs,
        instance_size: aot_instance_size(size),
        element_size: aot_element_size(size),
        vtable_entries: Vec::new(),
    }
}

fn array_aot_refs(
    layout: &AotLayout<'_>,
    size: InstanceSize,
    type_params: &BytecodeTypeArray,
) -> Vec<i32> {
    if size == InstanceSize::ObjArray {
        Vec::new()
    } else {
        let mut refs = Vec::new();
        layout.add_ref_fields(&mut refs, 0, type_params[0].clone());
        refs
    }
}

fn normalize_aot_shape_size(size: InstanceSize, refs: &[i32]) -> InstanceSize {
    match size {
        InstanceSize::StructArray(element_size) if refs.is_empty() => {
            InstanceSize::PrimitiveArray(element_size)
        }
        _ => size,
    }
}

fn encode_enum_variant_aot_shape(
    layout: &AotLayout<'_>,
    program: &Program,
    id: u32,
    enum_id: EnumId,
    type_params: &BytecodeTypeArray,
    variant_id: u32,
) -> AotShape {
    let enum_ = program.enum_(enum_id);
    let enum_variant = &enum_.variants[variant_id as usize];
    let mut csize = Header::size() + 4;
    let mut fields = vec![FieldInstance {
        offset: Header::size(),
        ty: BytecodeType::Int32,
    }];
    let mut refs = Vec::new();

    for ty in &enum_variant.arguments {
        let ty = specialize_bty(ty.clone(), type_params);
        assert!(ty.is_concrete_type());

        let field_size = layout.size(ty.clone());
        let field_align = layout.align(ty.clone());
        let offset = mem::align_i32(csize, field_align);
        fields.push(FieldInstance {
            offset,
            ty: ty.clone(),
        });

        csize = offset + field_size;
        layout.add_ref_fields(&mut refs, offset, ty);
    }

    let size = InstanceSize::Fixed(mem::align_i32(csize, mem::ptr_width()));

    AotShape {
        id,
        name: display_enum_variant_shape_name(program, enum_id, type_params, variant_id),
        kind: ShapeKind::EnumVariant(enum_id, type_params.clone(), variant_id),
        fields: encode_shape_fields(&fields),
        visitor: aot_shape_visitor(size),
        refs,
        instance_size: aot_instance_size(size),
        element_size: aot_element_size(size),
        vtable_entries: Vec::new(),
    }
}

fn encode_lambda_aot_shape(
    program: &Program,
    id: u32,
    fct_id: FunctionId,
    type_params: &BytecodeTypeArray,
    symbols: &AotSymbolMaps,
) -> AotShape {
    let size = InstanceSize::Fixed(Header::size() + mem::ptr_width());
    let fields = vec![FieldInstance {
        offset: Header::size(),
        ty: BytecodeType::Ptr,
    }];

    AotShape {
        id,
        name: display_lambda_shape_name(program, fct_id, type_params),
        kind: ShapeKind::Lambda(fct_id, type_params.clone()),
        fields: encode_shape_fields(&fields),
        visitor: ShapeVisitor::Regular,
        refs: vec![Header::size()],
        instance_size: aot_instance_size(size),
        element_size: aot_element_size(size),
        vtable_entries: lambda_vtable_entries(fct_id, type_params, symbols),
    }
}

fn encode_trait_object_aot_shape(
    layout: &AotLayout<'_>,
    program: &Program,
    id: u32,
    trait_ty: BytecodeType,
    actual_object_ty: BytecodeType,
    symbols: &AotSymbolMaps,
) -> AotShape {
    let mut refs = Vec::new();
    let mut csize = Header::size();

    debug_assert!(actual_object_ty.is_concrete_type());

    let field_size = layout.size(actual_object_ty.clone());
    let field_align = layout.align(actual_object_ty.clone());
    let offset = mem::align_i32(csize, field_align);
    let fields = vec![FieldInstance {
        offset,
        ty: actual_object_ty.clone(),
    }];
    layout.add_ref_fields(&mut refs, offset, actual_object_ty.clone());
    csize = mem::align_i32(offset + field_size, mem::ptr_width());
    let size = InstanceSize::Fixed(csize);

    AotShape {
        id,
        name: display_trait_object_shape_name(program, &trait_ty, &actual_object_ty),
        kind: ShapeKind::TraitObject {
            trait_ty: trait_ty.clone(),
            actual_object_ty: actual_object_ty.clone(),
        },
        fields: encode_shape_fields(&fields),
        visitor: ShapeVisitor::Regular,
        refs,
        instance_size: aot_instance_size(size),
        element_size: aot_element_size(size),
        vtable_entries: trait_object_vtable_entries(program, &trait_ty, &actual_object_ty, symbols),
    }
}

fn lambda_vtable_entries(
    fct_id: FunctionId,
    type_params: &BytecodeTypeArray,
    symbols: &AotSymbolMaps,
) -> Vec<Option<String>> {
    vec![
        symbols
            .functions
            .get(&(fct_id, type_params.clone()))
            .cloned(),
    ]
}

fn trait_object_vtable_entries(
    program: &Program,
    trait_ty: &BytecodeType,
    actual_object_ty: &BytecodeType,
    symbols: &AotSymbolMaps,
) -> Vec<Option<String>> {
    let trait_id = trait_ty.trait_id().expect("trait expected");
    let trait_ = program.trait_(trait_id);
    let mut entries = Vec::with_capacity(trait_.virtual_methods.len());
    for &trait_fct_id in trait_.virtual_methods.iter() {
        let key = TraitObjectThunk {
            trait_fct_id,
            trait_object_ty: trait_ty.clone(),
            actual_object_ty: actual_object_ty.clone(),
        };
        entries.push(symbols.trait_object_thunks.get(&key).cloned());
    }
    entries
}

fn display_lambda_shape_name(
    program: &Program,
    fct_id: FunctionId,
    type_params: &BytecodeTypeArray,
) -> String {
    let fct = program.fct(fct_id);
    let params = fct
        .params
        .iter()
        .skip(1)
        .map(|ty| {
            let ty = specialize_ty_in_program(program, None, ty.clone(), type_params);
            display_ty(program, &ty)
        })
        .collect::<Vec<_>>()
        .join(", ");
    let ret_ty = display_ty(program, &fct.return_type);
    format!("({}): {}", params, ret_ty)
}

fn display_class_shape_name(
    program: &Program,
    class_id: ClassId,
    type_params: &BytecodeTypeArray,
) -> String {
    let class = program.class(class_id);
    format!("{}{}", class.name, display_ty_array(program, type_params))
}

fn display_enum_variant_shape_name(
    program: &Program,
    enum_id: EnumId,
    type_params: &BytecodeTypeArray,
    variant_id: u32,
) -> String {
    let enum_ = program.enum_(enum_id);
    format!(
        "{}{}::{}",
        enum_.name,
        display_ty_array(program, type_params),
        enum_.variants[variant_id as usize].name
    )
}

fn display_trait_object_shape_name(
    program: &Program,
    trait_ty: &BytecodeType,
    actual_object_ty: &BytecodeType,
) -> String {
    format!(
        "{} as {}",
        display_ty(program, actual_object_ty),
        display_ty(program, trait_ty)
    )
}

fn aot_instance_size(size: InstanceSize) -> u64 {
    size.instance_size().unwrap_or(0) as u64
}

fn aot_element_size(size: InstanceSize) -> u64 {
    size.element_size().unwrap_or(-1) as usize as u64
}

fn aot_shape_visitor(size: InstanceSize) -> ShapeVisitor {
    match size {
        InstanceSize::PrimitiveArray(_) => ShapeVisitor::None,
        InstanceSize::ObjArray => ShapeVisitor::PointerArray,
        InstanceSize::Str => ShapeVisitor::None,
        InstanceSize::Fixed(..) => ShapeVisitor::Regular,
        InstanceSize::FillerWord | InstanceSize::FillerArray | InstanceSize::FreeSpace => {
            ShapeVisitor::None
        }
        InstanceSize::StructArray(_) => ShapeVisitor::RecordArray,
        InstanceSize::UnitArray => ShapeVisitor::None,
        InstanceSize::CodeObject => ShapeVisitor::Invalid,
    }
}

fn prepare_virtual_method_tables(vm: &VM, tc: &TransitiveClosure, ctc: &CompiledTransitiveClosure) {
    for shape_key in &tc.shape_keys {
        match shape_key {
            AotShapeKey::Lambda(fct_id, type_params) => {
                let shape = vm.shape_for_lambda(*fct_id, type_params.clone());
                let address = ctc
                    .function_addresses
                    .get(&(*fct_id, type_params.clone()))
                    .cloned()
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
                    if let Some(address) = ctc.trait_object_thunk_addresses.get(&key).cloned() {
                        shape.set_method_table_entry(idx, address);
                    }
                }
            }

            _ => unreachable!(),
        }
    }
}
