use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::time::Instant;

use dora_bytecode::{
    BytecodeFunction, BytecodeInstruction, BytecodeReader, BytecodeTraitType, BytecodeType,
    BytecodeTypeArray, ConstPoolEntry, ConstPoolIdx, FunctionId, ImplId, Location, PackageId,
    Program, display_fct, display_fct_specialized,
};

use crate::cannon::codegen::{align, size};
use crate::compiler::codegen::{CompilerInvocation, compile_runtime_entry_trampoline};
use crate::compiler::{
    CompilationMode, NativeFct, NativeFctKind, NativeTarget, compile_fct_aot, trait_object_thunk,
};
use crate::gc::{Address, formatted_size};
use crate::mem;
use crate::os;
use crate::snapshot::display_shape_name;
use crate::startup::{encode_shape_fields, encode_shape_kind};
use crate::vm::CollectorName;
use crate::vm::{
    AotShapeKey, BytecodeTypeExt, Code, CodeKind, LazyCompilationSite, RelocationKind,
    RuntimeFunction, ShapeKind, VM, add_ref_fields, create_enum_instance, ensure_shape_for_lambda,
    ensure_shape_for_trait_object, execute_on_main, find_trait_impl_in_program,
    find_trait_ty_impl_in_program, specialize_trait_ty_in_program, specialize_ty_array_in_program,
    specialize_ty_in_program,
};
use crate::{Shape, ShapeVisitor, SpecializeSelf, get_bytecode};

pub fn compile_boots_aot(vm: &VM) {
    if vm.has_boots() {
        let package_id = vm
            .program
            .boots_package_id
            .expect("boots package is missing");
        let entry_id = vm.known.boots_compile_fct_id();
        let tests = compute_tests(&vm.program, package_id);
        let tc = compute_transitive_closure(vm, &vm.program, package_id, entry_id, &tests);
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
    let ctc = compile_transitive_closure(vm, &tc, compiler, mode);
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
            stage2_entry.code.instruction_slice(),
            stage3_entry.code.instruction_slice(),
            "stage2 and stage3 differ in function {}",
            aot_compiled_function_name(&vm.program, stage2_entry)
        );
    }
}

fn compute_transitive_closure(
    vm: &VM,
    program: &Program,
    _package_id: PackageId,
    entry_id: FunctionId,
    tests: &[FunctionId],
) -> TransitiveClosure {
    let start = Instant::now();

    let mut compile_all = TransitiveClosureComputation::new(vm, program);
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
    shapes: Vec<*const Shape>,
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
    vm: &'a VM,
    program: &'a Program,
    worklist: Vec<(FunctionId, BytecodeTypeArray)>,
    worklist_idx: usize,
    visited: HashSet<(FunctionId, BytecodeTypeArray)>,
    visited_thunks: HashSet<TraitObjectThunk>,
    shapes: Vec<*const Shape>,
    thunks: Vec<TraitObjectThunk>,
}

impl<'a> TransitiveClosureComputation<'a> {
    fn new(vm: &'a VM, program: &'a Program) -> TransitiveClosureComputation<'a> {
        TransitiveClosureComputation {
            vm,
            program,
            worklist: Vec::new(),
            worklist_idx: 0,
            visited: HashSet::new(),
            visited_thunks: HashSet::new(),
            shapes: Vec::new(),
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
            shapes: self.shapes,
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

                    let shape = ensure_shape_for_lambda(self.vm, callee_id, callee_type_params);
                    self.shapes.push(shape);
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

                    let shape = ensure_shape_for_trait_object(self.vm, trait_ty, actual_object_ty);
                    self.shapes.push(shape);
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
    code: Arc<Code>,
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

fn compile_transitive_closure(
    vm: &VM,
    tc: &TransitiveClosure,
    compiler: CompilerInvocation,
    mode: CompilationMode,
) -> CompiledTransitiveClosure {
    let mut ctc = CompiledTransitiveClosure::new();
    compile_functions(vm, tc, &mut ctc, compiler, mode);
    compile_thunks(vm, tc, &mut ctc, compiler, mode);
    if !matches!(mode, CompilationMode::Aot) {
        prepare_lazy_call_sites(vm, &ctc, compiler, mode);
    }
    prepare_virtual_method_tables(vm, tc, &ctc);
    ctc
}

fn compile_functions(
    vm: &VM,
    tc: &TransitiveClosure,
    ctc: &mut CompiledTransitiveClosure,
    compiler: CompilerInvocation,
    mode: CompilationMode,
) {
    for (fct_id, type_params) in &tc.functions {
        compile_function(vm, *fct_id, type_params.clone(), ctc, compiler, mode);
    }
}

fn compile_function(
    vm: &VM,
    fct_id: FunctionId,
    type_params: BytecodeTypeArray,
    ctc: &mut CompiledTransitiveClosure,
    compiler: CompilerInvocation,
    mode: CompilationMode,
) {
    let fct = vm.fct(fct_id);

    if let Some(native_fctptr) = vm.native_methods.get(fct_id) {
        // Method is implemented in native code. Create trampoline for invoking it.
        let target = if matches!(mode, CompilationMode::Aot) {
            let symbol = vm
                .native_methods
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

        let code = compile_runtime_entry_trampoline(vm, Some(fct_id), internal_fct);

        let existing = ctc
            .function_addresses
            .insert((fct_id, type_params.clone()), code.instruction_start());
        assert!(existing.is_none());
        ctc.functions.push(CompiledFunction {
            target: CompiledFunctionTarget::Function {
                fct_id,
                type_params,
            },
            code,
        });
    } else if let Some(_) = get_bytecode(&vm.program, fct) {
        let (_code_id, code) = compile_fct_aot(vm, fct_id, &type_params, compiler, mode);
        ctc.counter += 1;
        let existing = ctc
            .function_addresses
            .insert((fct_id, type_params.clone()), code.instruction_start());
        assert!(existing.is_none());
        ctc.functions.push(CompiledFunction {
            target: CompiledFunctionTarget::Function {
                fct_id,
                type_params,
            },
            code,
        });
    }
}

fn compile_thunks(
    vm: &VM,
    tc: &TransitiveClosure,
    ctc: &mut CompiledTransitiveClosure,
    compiler: CompilerInvocation,
    mode: CompilationMode,
) {
    for thunk in &tc.thunks {
        let (_code_id, code) = trait_object_thunk::ensure_compiled_aot(
            vm,
            thunk.trait_fct_id,
            thunk.trait_object_ty.clone(),
            thunk.actual_object_ty.clone(),
            compiler,
            mode,
        );

        let existing = ctc
            .trait_object_thunk_addresses
            .insert(thunk.clone(), code.instruction_start());
        assert!(existing.is_none());

        ctc.functions.push(CompiledFunction {
            target: CompiledFunctionTarget::TraitObjectThunk(thunk.clone()),
            code,
        });
    }
}

fn prepare_lazy_call_sites(
    _vm: &VM,
    ctc: &CompiledTransitiveClosure,
    _compiler: CompilerInvocation,
    mode: CompilationMode,
) {
    os::jit_writable();

    for entry in &ctc.functions {
        for (offset, site) in entry.code.lazy_compilation().entries() {
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
                                entry.code.descriptor(),
                                aot_compiled_function_name(&_vm.program, entry)
                            );
                            eprintln!(
                                " calls {} with {:?}",
                                display_fct_specialized(&_vm.program, *fct_id, type_params),
                                type_params
                            );
                            eprintln!("offset = {}", offset);
                            let has_native = _vm.native_methods.get(*fct_id).is_some();
                            let has_bytecode =
                                get_bytecode(&_vm.program, _vm.fct(*fct_id)).is_some();
                            eprintln!(
                                "has_native={}, has_bytecode={}, function_addresses.len={}",
                                has_native,
                                has_bytecode,
                                ctc.function_addresses.len()
                            );
                            panic!("missing function");
                        }
                    };
                    let ra = entry.code.instruction_start().offset(*offset as usize);

                    if mode.is_stage2_or_3() {
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
    pub kind: Vec<u8>,
    pub fields: Vec<u8>,
    pub visitor: u8,
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

pub fn compile_program(
    vm: &VM,
    program: &Program,
    boots_compile_fct_address: *const u8,
) -> AotCompilation {
    assert!(
        std::ptr::eq(program, &vm.program),
        "AOT compilation still requires the input Program to be installed in the VM"
    );

    let main_fct_id = program.main_fct_id.expect("no main function");
    let package_id = program.program_package_id;

    let boots_address = Address::from_ptr(boots_compile_fct_address);
    let compiler = CompilerInvocation::Boots(boots_address);

    let tc = compute_transitive_closure(vm, program, package_id, main_fct_id, &[]);
    let ctc = compile_transitive_closure(vm, &tc, compiler, CompilationMode::Aot);

    build_aot_compilation(vm, program, ctc)
}

pub fn compile_boots_compiler(
    vm: &VM,
    entry_id: FunctionId,
    boots_compile_fct_address: *const u8,
) -> AotCompilation {
    let package_id = vm
        .program
        .boots_package_id
        .expect("boots package is missing");
    let boots_address = Address::from_ptr(boots_compile_fct_address);
    let compiler = CompilerInvocation::Boots(boots_address);

    let tc = compute_transitive_closure(vm, &vm.program, package_id, entry_id, &[]);
    let ctc = compile_transitive_closure(vm, &tc, compiler, CompilationMode::Aot);

    build_aot_compilation(vm, &vm.program, ctc)
}

fn build_aot_compilation(
    vm: &VM,
    program: &Program,
    ctc: CompiledTransitiveClosure,
) -> AotCompilation {
    // Compute global memory layout (same logic as init_global_addresses in globals.rs).
    let global_layout = compute_global_layout(vm, program);

    let mut symbols = AotSymbolMaps {
        functions: HashMap::new(),
        trait_object_thunks: HashMap::new(),
    };
    let mut shape_interner = AotShapeInterner::default();
    let mut strings = AotStringTable::new();
    let mut aot_functions = Vec::new();

    intern_known_shapes(vm, &mut shape_interner);

    for entry in &ctc.functions {
        let fct_id = entry.target.fct_id();
        let kind = match entry.code.descriptor() {
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
        let bytes = entry.code.instruction_slice().to_vec();
        let mut gcpoints = Vec::new();
        let mut locations = Vec::new();

        for (pc_offset, gcpoint) in entry.code.gcpoints().entries() {
            gcpoints.push(AotGcPoint {
                pc_offset: *pc_offset,
                offsets: gcpoint.offsets.clone(),
            });
        }

        for (pc_offset, location) in entry.code.locations().entries() {
            locations.push(AotLocation {
                pc_offset: *pc_offset,
                inlined_function_id: location.inlined_function_id.map(|id| id.0),
                line: location.location.line(),
                column: location.location.column(),
            });
        }

        let inlined_functions = entry
            .code
            .inlined_functions()
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
        for (offset, site) in entry.code.lazy_compilation().entries() {
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

        for (offset, reloc_kind) in &entry.code.relocations().entries {
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

    let known_shapes = build_known_shapes(vm, &shape_interner);
    let shapes = encode_aot_shapes(vm, program, &symbols, &shape_interner);

    let function_info = synthetic_function_info(&mut strings, "dora_aot_trap_trampoline");
    aot_functions.push(compile_runtime_function_trampoline(
        vm,
        "dora_aot_trap_trampoline",
        "dora_native_trap",
        function_info,
        BytecodeTypeArray::one(BytecodeType::Int32),
        BytecodeType::Unit,
        NativeFctKind::TrapTrampoline,
        AotCodeKind::TrapTrampoline,
    ));
    let function_info = synthetic_function_info(&mut strings, "dora_aot_safepoint_trampoline");
    aot_functions.push(compile_runtime_function_trampoline(
        vm,
        "dora_aot_safepoint_trampoline",
        "dora_native_safepoint_slow",
        function_info,
        BytecodeTypeArray::empty(),
        BytecodeType::Unit,
        NativeFctKind::SafepointTrampoline,
        AotCodeKind::SafepointTrampoline,
    ));
    let function_info = synthetic_function_info(&mut strings, "dora_aot_gc_allocation_trampoline");
    aot_functions.push(compile_runtime_function_trampoline(
        vm,
        "dora_aot_gc_allocation_trampoline",
        "dora_native_gc_alloc",
        function_info,
        BytecodeTypeArray::new(vec![BytecodeType::Int64, BytecodeType::Bool]),
        BytecodeType::Ptr,
        NativeFctKind::GcAllocationTrampoline,
        AotCodeKind::AllocationFailureTrampoline,
    ));
    let unreachable_fct_id = vm
        .known
        .unreachable_fct_id
        .expect("unreachable function missing");
    let function_info = function_info_for_fct(program, &mut strings, unreachable_fct_id);
    aot_functions.push(compile_runtime_function_trampoline(
        vm,
        "dora_aot_unreachable_trampoline",
        "dora_native_unreachable",
        function_info,
        BytecodeTypeArray::empty(),
        BytecodeType::Unit,
        NativeFctKind::RuntimeEntryTrampoline(unreachable_fct_id),
        AotCodeKind::RuntimeEntryTrampoline,
    ));
    let fatal_error_fct_id = vm
        .known
        .fatal_error_fct_id
        .expect("fatalError function missing");
    let function_info = function_info_for_fct(program, &mut strings, fatal_error_fct_id);
    aot_functions.push(compile_runtime_function_trampoline(
        vm,
        "dora_aot_fatal_error_trampoline",
        "dora_native_fatal_error",
        function_info,
        BytecodeTypeArray::one(BytecodeType::Ptr),
        BytecodeType::Unit,
        NativeFctKind::RuntimeEntryTrampoline(fatal_error_fct_id),
        AotCodeKind::RuntimeEntryTrampoline,
    ));

    let collector_name = vm.flags.gc.unwrap_or(CollectorName::Swiper);

    AotCompilation {
        strings,
        functions: aot_functions,
        shapes,
        known_shapes,
        global_layout,
        collector_name,
    }
}

fn compile_runtime_function_trampoline(
    vm: &VM,
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
    let code = compile_runtime_entry_trampoline(vm, None, native_fct);
    let gcpoints = code.gcpoints().entries();
    assert_eq!(gcpoints.len(), 1);
    let (pc_offset, gcpoint) = &gcpoints[0];
    let gcpoints = vec![AotGcPoint {
        pc_offset: *pc_offset,
        offsets: gcpoint.offsets.clone(),
    }];

    let relocations = &code.relocations().entries;
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
        code: code.instruction_slice().to_vec(),
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

fn compute_global_layout(vm: &VM, program: &Program) -> GlobalLayout {
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

        let ty_size = size(vm, ty.clone()) as usize;
        let ty_align = align(vm, ty.clone()) as usize;

        let value_offset = mem::align_usize_up(memory_size, ty_align);
        add_ref_fields(vm, &mut references, value_offset as i32, ty);
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

fn encode_aot_shapes(
    vm: &VM,
    program: &Program,
    symbols: &AotSymbolMaps,
    interner: &AotShapeInterner,
) -> Vec<AotShape> {
    interner
        .keys()
        .iter()
        .enumerate()
        .map(|(idx, key)| {
            let id = u32::try_from(idx).expect("too many shapes in AOT shape table");
            let shape = shape_for_key(vm, key);
            encode_shape(vm, program, id, shape, symbols)
        })
        .collect()
}

fn intern_known_shapes(vm: &VM, interner: &mut AotShapeInterner) {
    for (_known_kind, key) in known_shape_keys(vm) {
        interner.intern(key);
    }
}

fn build_known_shapes(vm: &VM, interner: &AotShapeInterner) -> Vec<AotKnownShape> {
    known_shape_keys(vm)
        .into_iter()
        .map(|(kind, key)| AotKnownShape {
            kind,
            shape_id: interner.get(&key),
        })
        .collect()
}

fn known_shape_keys(vm: &VM) -> Vec<(AotKnownShapeKind, AotShapeKey)> {
    let array_class_id = vm.known.array_class_id();
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
            AotShapeKey::Class(vm.known.thread_class_id(), BytecodeTypeArray::empty()),
        ),
        (AotKnownShapeKind::FillerWord, AotShapeKey::FillerWord),
        (AotKnownShapeKind::FillerArray, AotShapeKey::FillerArray),
        (AotKnownShapeKind::FreeSpace, AotShapeKey::FreeSpace),
        (AotKnownShapeKind::Code, AotShapeKey::Code),
    ]
}

fn shape_for_key<'a>(vm: &'a VM, key: &AotShapeKey) -> &'a Shape {
    match key {
        AotShapeKey::FillerWord => vm.known.filler_word_shape(),
        AotShapeKey::FillerArray => vm.known.filler_array_shape(),
        AotShapeKey::FreeSpace => vm.known.free_space_shape(),
        AotShapeKey::Code => vm.known.code_shape(),
        AotShapeKey::String => vm.known.string_shape(),
        AotShapeKey::Class(class_id, type_params) | AotShapeKey::Array(class_id, type_params) => {
            vm.shape_for_class(*class_id, type_params)
        }
        AotShapeKey::EnumVariant {
            enum_id,
            type_params,
            variant_id,
        } => {
            let enum_instance_id = create_enum_instance(vm, *enum_id, type_params.clone());
            let enum_instance = vm.enum_instances.idx(enum_instance_id);
            vm.shape_for_enum_variant(&enum_instance, vm.enum_(*enum_id), *variant_id)
        }
        AotShapeKey::Lambda(fct_id, type_params) => {
            vm.shape_for_lambda(*fct_id, type_params.clone())
        }
        AotShapeKey::TraitObject {
            trait_ty,
            actual_object_ty,
        } => vm.shape_for_trait_object(trait_ty.clone(), actual_object_ty.clone()),
    }
}

fn encode_shape(
    vm: &VM,
    program: &Program,
    id: u32,
    shape: &Shape,
    symbols: &AotSymbolMaps,
) -> AotShape {
    let visitor = match shape.visitor {
        ShapeVisitor::Regular => 0,
        ShapeVisitor::PointerArray => 1,
        ShapeVisitor::RecordArray => 2,
        ShapeVisitor::None => 3,
        ShapeVisitor::Invalid => 4,
    };

    let kind = encode_shape_kind(vm, shape.kind());
    let fields = encode_shape_fields(vm, &shape.fields);

    // Trait object shapes use the same virtual-method ordering as runtime
    // vtables. Missing entries are kept as None as a defensive fallback.
    let vtable_entries = match shape.kind() {
        ShapeKind::TraitObject {
            trait_ty,
            actual_object_ty,
        } => {
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
        ShapeKind::Lambda(fct_id, type_params) => {
            vec![
                symbols
                    .functions
                    .get(&(*fct_id, type_params.clone()))
                    .cloned(),
            ]
        }
        _ => Vec::new(),
    };

    AotShape {
        id,
        name: display_shape_name(vm, shape),
        kind,
        fields,
        visitor,
        refs: shape.refs.clone(),
        instance_size: shape.instance_size as u64,
        element_size: shape.element_size as u64,
        vtable_entries,
    }
}

fn prepare_virtual_method_tables(vm: &VM, tc: &TransitiveClosure, ctc: &CompiledTransitiveClosure) {
    for shape in &tc.shapes {
        let shape = unsafe { &**shape };
        match shape.kind() {
            ShapeKind::Lambda(fct_id, type_params) => {
                let address = ctc
                    .function_addresses
                    .get(&(*fct_id, type_params.clone()))
                    .cloned()
                    .expect("missing function");
                shape.set_method_table_entry(0, address);
            }

            ShapeKind::TraitObject {
                trait_ty,
                actual_object_ty,
            } => {
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
