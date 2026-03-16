use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::time::Instant;

use dora_bytecode::{
    BytecodeFunction, BytecodeInstruction, BytecodeReader, BytecodeTraitType, BytecodeType,
    BytecodeTypeArray, ConstPoolEntry, ConstPoolIdx, FunctionId, FunctionKind, PackageId,
    display_fct_specialized,
};

use crate::cannon::codegen::{align, size};
use crate::compiler::codegen::{CompilerInvocation, compile_runtime_entry_trampoline};
use crate::compiler::{
    CompilationMode, NativeFct, NativeFctKind, NativeTarget, compile_fct_aot, trait_object_thunk,
};
use crate::gc::{Address, formatted_size};
use crate::mem;
use crate::os;
use crate::vm::{
    BytecodeTypeExt, Code, CodeKind, LazyCompilationSite, RelocationKind, RuntimeFunction,
    ShapeKind, VM, add_ref_fields, ensure_shape_for_lambda, ensure_shape_for_trait_object,
    execute_on_main, find_trait_impl, specialize_bty, specialize_bty_array, specialize_ty,
};
use crate::{Shape, ShapeVisitor, SpecializeSelf, get_bytecode};

pub fn compile_boots_aot(vm: &VM) {
    if vm.has_boots() {
        let package_id = vm
            .program
            .boots_package_id
            .expect("boots package is missing");
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
            display_fct_specialized(&vm.program, stage2_entry.fct_id, &stage2_entry.type_params)
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
    thunks: Vec<(FunctionId, BytecodeTypeArray, BytecodeType)>,
    shapes: Vec<*const Shape>,
}

struct TransitiveClosureComputation<'a> {
    vm: &'a VM,
    worklist: Vec<(FunctionId, BytecodeTypeArray)>,
    worklist_idx: usize,
    visited: HashSet<(FunctionId, BytecodeTypeArray)>,
    counter: usize,
    shapes: Vec<*const Shape>,
    thunks: Vec<(FunctionId, BytecodeTypeArray, BytecodeType)>,
}

impl<'a> TransitiveClosureComputation<'a> {
    fn new(vm: &VM) -> TransitiveClosureComputation<'_> {
        TransitiveClosureComputation {
            vm,
            worklist: Vec::new(),
            worklist_idx: 0,
            visited: HashSet::new(),
            shapes: Vec::new(),
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
            shapes: self.shapes,
        }
    }

    fn trace(&mut self, fct_id: FunctionId, type_params: BytecodeTypeArray) {
        let fct = &self.vm.fct(fct_id);

        if let Some((bytecode_function, specialize_self)) = get_bytecode(self.vm, fct) {
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
                    let ConstPoolEntry::Generic {
                        object_type,
                        trait_ty,
                        fct_id: callee_trait_fct_id,
                        fct_type_params: callee_fct_type_params,
                    } = bytecode_function.const_pool(fct)
                    else {
                        unreachable!()
                    };

                    let generic_ty = specialize_ty(
                        self.vm,
                        specialize_self.as_ref(),
                        object_type.clone(),
                        &type_params,
                    );
                    let callee_trait_type_params = trait_ty.type_params.clone();

                    let fct = self.vm.fct(*callee_trait_fct_id);

                    let trait_id = match fct.kind {
                        FunctionKind::Trait(trait_id) => trait_id,
                        _ => unreachable!(),
                    };

                    let callee_trait_type_params =
                        specialize_bty_array(&callee_trait_type_params, &type_params);

                    let trait_ty = BytecodeTraitType {
                        trait_id,
                        type_params: callee_trait_type_params.clone(),
                        bindings: Vec::new(),
                    };

                    let (callee_id, callee_container_bindings) =
                        find_trait_impl(self.vm, *callee_trait_fct_id, trait_ty, generic_ty);

                    let combined_type_params =
                        callee_container_bindings.connect(&callee_fct_type_params);
                    self.push(callee_id, combined_type_params);
                }

                BytecodeInstruction::NewLambda { idx, .. } => {
                    let (callee_id, callee_type_params) = match bytecode_function.const_pool(idx) {
                        ConstPoolEntry::Fct(fct_id, type_params) => (*fct_id, type_params),
                        _ => unreachable!(),
                    };

                    let callee_type_params =
                        specialize_bty_array(&callee_type_params, &type_params);
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

                    let trait_ty = specialize_bty(trait_ty, &type_params);
                    let actual_object_ty = specialize_bty(actual_object_ty, &type_params);

                    let shape = ensure_shape_for_trait_object(self.vm, trait_ty, actual_object_ty);
                    self.shapes.push(shape);
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
                        if impl_.trait_ty.is_trait_object_ty(&trait_object_ty) {
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

struct CompiledFunction {
    fct_id: FunctionId,
    type_params: BytecodeTypeArray,
    code: Arc<Code>,
}

struct CompiledTransitiveClosure {
    function_addresses: HashMap<(FunctionId, BytecodeTypeArray), Address>,
    functions: Vec<CompiledFunction>,
    counter: usize,
}

impl CompiledTransitiveClosure {
    fn new() -> CompiledTransitiveClosure {
        CompiledTransitiveClosure {
            function_addresses: HashMap::new(),
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
            fct_id,
            type_params,
            code,
        });
    } else if let Some(_) = get_bytecode(vm, fct) {
        let (_code_id, code) = compile_fct_aot(vm, fct_id, &type_params, compiler, mode);
        ctc.counter += 1;
        let existing = ctc
            .function_addresses
            .insert((fct_id, type_params.clone()), code.instruction_start());
        assert!(existing.is_none());
        ctc.functions.push(CompiledFunction {
            fct_id,
            type_params,
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
    for (trait_fct_id, trait_type_params, actual_ty) in &tc.thunks {
        let (_code_id, code) = trait_object_thunk::ensure_compiled_aot(
            vm,
            *trait_fct_id,
            trait_type_params.clone(),
            actual_ty.clone(),
            compiler,
            mode,
        );

        let combined_type_params = trait_type_params.append(actual_ty.clone());
        let existing = ctc.function_addresses.insert(
            (*trait_fct_id, combined_type_params.clone()),
            code.instruction_start(),
        );
        assert!(existing.is_none());

        ctc.functions.push(CompiledFunction {
            fct_id: *trait_fct_id,
            type_params: combined_type_params,
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
                                display_fct_specialized(
                                    &_vm.program,
                                    entry.fct_id,
                                    &entry.type_params
                                )
                            );
                            eprintln!(
                                " calls {} with {:?}",
                                display_fct_specialized(&_vm.program, *fct_id, type_params),
                                type_params
                            );
                            eprintln!("offset = {}", offset);
                            let has_native = _vm.native_methods.get(*fct_id).is_some();
                            let has_bytecode = get_bytecode(_vm, _vm.fct(*fct_id)).is_some();
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
    DoraEntryTrampoline,
}

pub struct AotGcPoint {
    pub pc_offset: u32,
    pub offsets: Vec<i32>,
}

pub struct AotStringRelocation {
    /// Offset of the RIP-relative disp32 in the string-load instruction.
    pub offset: u32,
    /// UTF-8 string payload referenced by this relocation.
    pub value: String,
}

pub struct AotShapeRelocation {
    pub offset: u32,
    pub shape_id: u32,
}

pub struct AotGlobalRelocation {
    /// Offset of the RIP-relative disp32 in the lea instruction.
    pub offset: u32,
    /// Byte offset into the global memory block.
    pub global_offset: usize,
}

pub struct AotFunction {
    pub name: String,
    pub fct_id: u32,
    pub kind: AotCodeKind,
    pub code: Vec<u8>,
    pub call_relocations: Vec<AotCallRelocation>,
    pub string_relocations: Vec<AotStringRelocation>,
    pub shape_relocations: Vec<AotShapeRelocation>,
    pub global_relocations: Vec<AotGlobalRelocation>,
    pub gcpoints: Vec<AotGcPoint>,
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

#[derive(Clone, Copy)]
pub enum AotShapeKind {
    Opaque,
    String,
}

pub struct AotShape {
    pub id: u32,
    pub kind: AotShapeKind,
    pub visitor: u8,
    pub refs: Vec<i32>,
    pub instance_size: u64,
    pub element_size: u64,
}

#[derive(Clone, Copy)]
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
    pub shape_id: u32,
}

pub struct AotCompilation {
    pub functions: Vec<AotFunction>,
    pub shapes: Vec<AotShape>,
    pub known_shapes: Vec<AotKnownShape>,
    pub global_layout: GlobalLayout,
    pub main_returns_unit: bool,
}

pub fn compile_program(vm: &VM) -> AotCompilation {
    let main_fct_id = vm.program.main_fct_id.expect("no main function");
    let package_id = vm.program.program_package_id;

    let boots_address = vm.known.boots_compile_fct_address();
    let compiler = CompilerInvocation::Boots(boots_address);

    let tc = compute_transitive_closure(vm, package_id, main_fct_id, &[]);
    let ctc = compile_transitive_closure(vm, &tc, compiler, CompilationMode::Aot);

    // Compute global memory layout (same logic as init_global_addresses in globals.rs).
    let global_layout = compute_global_layout(vm);

    // Collect all shape pointers from ClassPointer relocations and build the shape map.
    let (shapes, known_shapes, shape_ptr_to_id) = collect_shapes(vm, &ctc);

    let mut aot_functions = Vec::new();
    for entry in &ctc.functions {
        let kind = match entry.code.descriptor() {
            CodeKind::BaselineFct(_) => {
                panic!("baseline code object in AOT output is not supported")
            }
            CodeKind::OptimizedFct(_) => AotCodeKind::Optimized,
            CodeKind::RuntimeEntryTrampoline(_) => AotCodeKind::RuntimeEntryTrampoline,
            _ => unreachable!("unexpected code kind in AOT compilation output"),
        };

        let name = display_fct_specialized(&vm.program, entry.fct_id, &entry.type_params);
        let bytes = entry.code.instruction_slice().to_vec();
        let mut gcpoints = Vec::new();

        for (pc_offset, gcpoint) in entry.code.gcpoints().entries() {
            gcpoints.push(AotGcPoint {
                pc_offset: *pc_offset,
                offsets: gcpoint.offsets.clone(),
            });
        }

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
                    let target_name = display_fct_specialized(&vm.program, *fct_id, type_params);
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
                    let target_name = display_fct_specialized(&vm.program, *fct_id, type_params);
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
                    let value = resolve_string_relocation(vm, *owner_fct_id, *const_pool_idx);
                    string_relocations.push(AotStringRelocation {
                        offset: *offset,
                        value,
                    });
                }
                RelocationKind::Shape { address } => {
                    let shape_id = shape_ptr_to_id[address];
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
            name,
            fct_id: entry.fct_id.index_as_u32(),
            kind,
            code: bytes,
            call_relocations,
            string_relocations,
            shape_relocations,
            global_relocations,
            gcpoints,
        });
    }

    let main_returns_unit = vm.fct(main_fct_id).return_type.is_unit();

    AotCompilation {
        functions: aot_functions,
        shapes,
        known_shapes,
        global_layout,
        main_returns_unit,
    }
}

pub struct GlobalLayout {
    pub memory_size: usize,
    pub references: Vec<i32>,
    pub value_offsets: Vec<usize>,
    pub state_offsets: Vec<usize>,
}

fn compute_global_layout(vm: &VM) -> GlobalLayout {
    let number_globals = vm.program.globals.len();
    let mut memory_size = 0usize;
    let mut references = Vec::new();
    let mut value_offsets = Vec::with_capacity(number_globals);
    let mut state_offsets = Vec::with_capacity(number_globals);

    let initialized_field_size = 1;

    for global_var in &vm.program.globals {
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
    vm: &VM,
    owner_fct_id: FunctionId,
    const_pool_idx: ConstPoolIdx,
) -> String {
    let owner = vm.fct(owner_fct_id);
    let bytecode = if let Some(bytecode) = owner.bytecode.as_ref() {
        bytecode
    } else {
        let trait_method_id = owner
            .trait_method_impl
            .expect("missing trait method for relocation owner");
        vm.fct(trait_method_id)
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

fn runtime_function_symbol(runtime_function: RuntimeFunction) -> &'static str {
    match runtime_function {
        RuntimeFunction::TrapTrampoline => "dora_aot_trap_trampoline",
        RuntimeFunction::SafepointTrampoline => "dora_aot_safepoint_trampoline",
        RuntimeFunction::GcAllocationTrampoline => "dora_aot_gc_allocation_trampoline",
        RuntimeFunction::WriteBarrierSlowPath => "dora_aot_write_barrier_slow_path",
    }
}

fn collect_shapes(
    vm: &VM,
    ctc: &CompiledTransitiveClosure,
) -> (Vec<AotShape>, Vec<AotKnownShape>, HashMap<Address, u32>) {
    let (mut shapes, known_shapes, mut ptr_to_id) = collect_known_shapes(vm);

    for entry in &ctc.functions {
        for (_offset, reloc_kind) in &entry.code.relocations().entries {
            if let RelocationKind::Shape { address } = reloc_kind {
                if !ptr_to_id.contains_key(address) {
                    let shape = unsafe { &*address.to_ptr::<Shape>() };
                    let shape_id = shapes.len() as u32;
                    ptr_to_id.insert(*address, shape_id);
                    shapes.push(encode_shape(shape_id, shape));
                }
            }
        }
    }

    (shapes, known_shapes, ptr_to_id)
}

fn collect_known_shapes(vm: &VM) -> (Vec<AotShape>, Vec<AotKnownShape>, HashMap<Address, u32>) {
    let known_shape_ptrs = [
        (AotKnownShapeKind::ByteArray, vm.known.byte_array_shape),
        (AotKnownShapeKind::Int32Array, vm.known.int32_array_shape),
        (AotKnownShapeKind::String, vm.known.string_shape),
        (AotKnownShapeKind::Thread, vm.known.thread_shape),
        (AotKnownShapeKind::FillerWord, vm.known.filler_word_shape),
        (AotKnownShapeKind::FillerArray, vm.known.filler_array_shape),
        (AotKnownShapeKind::FreeSpace, vm.known.free_space_shape),
        (AotKnownShapeKind::Code, vm.known.code_shape),
    ];

    let mut ptr_to_id: HashMap<Address, u32> = HashMap::new();
    let mut shapes = Vec::new();
    let mut known_shapes = Vec::new();

    for (known_kind, shape_ptr) in known_shape_ptrs {
        assert!(!shape_ptr.is_null(), "known shape pointer is null");
        let key = Address::from_ptr(shape_ptr);

        let shape_id = if let Some(&shape_id) = ptr_to_id.get(&key) {
            shape_id
        } else {
            let shape = unsafe { &*shape_ptr };
            let shape_id = shapes.len() as u32;
            ptr_to_id.insert(key, shape_id);
            shapes.push(encode_shape(shape_id, shape));
            shape_id
        };

        known_shapes.push(AotKnownShape {
            kind: known_kind,
            shape_id,
        });
    }

    (shapes, known_shapes, ptr_to_id)
}

fn encode_shape(id: u32, shape: &Shape) -> AotShape {
    let visitor = match shape.visitor {
        ShapeVisitor::Regular => 0,
        ShapeVisitor::PointerArray => 1,
        ShapeVisitor::RecordArray => 2,
        ShapeVisitor::None => 3,
        ShapeVisitor::Invalid => 4,
    };

    let kind = match shape.kind() {
        ShapeKind::Array(..) => AotShapeKind::Opaque,
        ShapeKind::Class(..) => AotShapeKind::Opaque,
        ShapeKind::String => AotShapeKind::String,
        ShapeKind::Lambda(..) => unimplemented!("AOT shape serialization for lambda shapes"),
        ShapeKind::TraitObject { .. } => {
            unimplemented!("AOT shape serialization for trait object shapes")
        }
        ShapeKind::Enum(..) => AotShapeKind::Opaque,
        ShapeKind::Builtin => AotShapeKind::Opaque,
    };

    AotShape {
        id,
        kind,
        visitor,
        refs: shape.refs.clone(),
        instance_size: shape.instance_size as u64,
        element_size: shape.element_size as u64,
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
                let combined_type_params = trait_ty.type_params().append(actual_object_ty.clone());
                let trait_ = vm.trait_(trait_id);
                for (idx, &trait_fct_id) in trait_.methods.iter().enumerate() {
                    if let Some(address) = ctc
                        .function_addresses
                        .get(&(trait_fct_id, combined_type_params.clone()))
                        .cloned()
                    {
                        shape.set_method_table_entry(idx, address);
                    }
                }
            }

            _ => unreachable!(),
        }
    }
}
