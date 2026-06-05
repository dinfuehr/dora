use std::collections::HashMap;

use dora_bytecode::{
    BytecodeFunction, BytecodeType, BytecodeTypeArray, ClassId, ConstPoolEntry, ConstPoolIdx,
    EnumId, FunctionData, FunctionId, Location, PackageId, Program, display_fct,
    display_fct_specialized, display_ty, display_ty_array, lookup_fct, resolve_path,
};

use crate::runtime_entry_trampoline::{self, NativeFct, NativeFctKind, NativeTarget};
use crate::{
    AotCodeKind, AotCompilation, AotFunction, AotFunctionInfo, AotGcPoint,
    AotGlobalRelocationTarget, AotInlinedFunction, AotKnownShape, AotKnownShapeKind, AotLayout,
    AotLocation, AotRelocation, AotRelocationTarget, AotShape, AotShapeInterner, AotShapeKey,
    AotStringTable, AotTestFunction, BytecodeTypeExt, CodeDescriptor, CollectorName,
    CompilationData, FieldInstance, GlobalLayout, GlobalLayoutEntry, InstanceSize, Intrinsic,
    RelocationKind, RuntimeFunction, STDLIB_INTRINSICS, ShapeKind, ShapeVisitor, SpecializeSelf,
    TargetArch, TraitObjectThunk, TransitiveClosure, align_i32, align_usize_up,
    compute_transitive_closure, encode_shape_fields, generate_bytecode_for_trait_object_thunk,
    get_bytecode, native_function_symbol, object_header_size, ptr_width, specialize_ty_in_program,
};
use dora_symbol::mangle_name;

pub fn compile_program_aot(program: &Program, inputs: AotCompileInputs) -> AotCompilation {
    let main_fct_id = program.main_fct_id.expect("no main function");
    compile_program_entries_aot(program, &[main_fct_id], &[], inputs)
}

pub fn compile_test_runner(
    program: &Program,
    package_id: PackageId,
    inputs: AotCompileInputs,
) -> AotCompilation {
    let entries = test_fct_ids(program, package_id);
    compile_program_entries_aot(program, &entries, &entries, inputs)
}

fn compile_program_entries_aot(
    program: &Program,
    entries: &[FunctionId],
    test_function_ids: &[FunctionId],
    inputs: AotCompileInputs,
) -> AotCompilation {
    let tc = compute_transitive_closure(program, entries, inputs.emit_compiler);
    let ctx = Box::new(AotCodegenContext {
        program,
        layout: AotLayout::new(program),
        intrinsics: collect_aot_intrinsics(program),
        compiler_invocation: inputs.compiler_invocation,
        target_arch: inputs.target_arch,
        collector_name: inputs.collector_name,
        array_class_id: inputs.known_elements.classes.array_class_id,
        string_class_id: inputs.known_elements.classes.string_class_id,
        emit_graph: inputs.emit_graph.as_deref(),
        emit_graph_after_each_pass: inputs.emit_graph_after_each_pass,
    });
    let ctc = {
        let _active_aot_context = ctx.compiler_invocation.enter_context(ctx.as_ref());
        compile_transitive_closure(ctx.as_ref(), &tc)
    };
    let mut strings = AotStringTable::new();
    let runtime_functions = compile_aot_runtime_trampolines(&mut strings, inputs.target_arch);

    build_aot_compilation(
        ctx.as_ref(),
        &tc,
        ctc,
        inputs.known_elements,
        inputs.collector_name,
        strings,
        runtime_functions,
        test_function_ids,
    )
}

fn test_fct_ids(program: &Program, package_id: PackageId) -> Vec<FunctionId> {
    program
        .functions
        .iter()
        .enumerate()
        .filter_map(|(fct_id, fct)| {
            let fct_id: FunctionId = fct_id.into();
            if fct.package_id == package_id && fct.is_test {
                Some(fct_id)
            } else {
                None
            }
        })
        .collect()
}

pub fn compile_boots_compiler_aot(
    program: &Program,
    entry_id: FunctionId,
    inputs: AotCompileInputs,
) -> AotCompilation {
    let tc = compute_transitive_closure(program, &[entry_id], inputs.emit_compiler);
    let ctx = Box::new(AotCodegenContext {
        program,
        layout: AotLayout::new(program),
        intrinsics: collect_aot_intrinsics(program),
        compiler_invocation: inputs.compiler_invocation,
        target_arch: inputs.target_arch,
        collector_name: inputs.collector_name,
        array_class_id: inputs.known_elements.classes.array_class_id,
        string_class_id: inputs.known_elements.classes.string_class_id,
        emit_graph: inputs.emit_graph.as_deref(),
        emit_graph_after_each_pass: inputs.emit_graph_after_each_pass,
    });
    let ctc = {
        let _active_aot_context = ctx.compiler_invocation.enter_context(ctx.as_ref());
        compile_transitive_closure(ctx.as_ref(), &tc)
    };
    let mut strings = AotStringTable::new();
    let runtime_functions = compile_aot_runtime_trampolines(&mut strings, inputs.target_arch);

    build_aot_compilation(
        ctx.as_ref(),
        &tc,
        ctc,
        inputs.known_elements,
        inputs.collector_name,
        strings,
        runtime_functions,
        &[],
    )
}

#[derive(Clone)]
pub(super) enum CompiledFunctionTarget {
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

pub(super) struct CompiledFunction {
    pub(super) target: CompiledFunctionTarget,
    pub(super) code: CodeDescriptor,
    pub(super) code_kind: CompiledCodeKind,
}

#[derive(Clone, Copy)]
pub(super) enum CompiledCodeKind {
    OptimizedFct,
    RuntimeEntryTrampoline,
}

pub(super) struct CompiledTransitiveClosure {
    pub(super) functions: Vec<CompiledFunction>,
}

impl CompiledTransitiveClosure {
    pub(super) fn new() -> CompiledTransitiveClosure {
        CompiledTransitiveClosure {
            functions: Vec::new(),
        }
    }
}

fn collect_aot_intrinsics(program: &Program) -> HashMap<FunctionId, Intrinsic> {
    let mut intrinsics = HashMap::new();

    add_intrinsic_functions(program, STDLIB_INTRINSICS, &mut intrinsics);

    intrinsics
}

fn add_intrinsic_functions(
    program: &Program,
    functions: &[(&'static str, Intrinsic)],
    intrinsics: &mut HashMap<FunctionId, Intrinsic>,
) {
    for (path, intrinsic) in functions {
        if let Some(fct_id) = lookup_fct(program, path) {
            intrinsics.insert(fct_id, *intrinsic);
        }
    }
}

pub struct AotCodegenContext<'a> {
    pub(super) program: &'a Program,
    layout: AotLayout<'a>,
    intrinsics: HashMap<FunctionId, Intrinsic>,
    pub(super) compiler_invocation: CompilerInvocation,
    pub(super) target_arch: TargetArch,
    pub(super) collector_name: CollectorName,
    array_class_id: ClassId,
    string_class_id: ClassId,
    pub(super) emit_graph: Option<&'a str>,
    pub(super) emit_graph_after_each_pass: bool,
}

impl AotCodegenContext<'_> {
    pub fn program(&self) -> &Program {
        self.program
    }

    pub fn layout(&self) -> &AotLayout<'_> {
        &self.layout
    }

    pub fn intrinsics(&self) -> &HashMap<FunctionId, Intrinsic> {
        &self.intrinsics
    }

    pub fn intrinsic_for_function(&self, fct_id: FunctionId) -> Option<Intrinsic> {
        self.intrinsics.get(&fct_id).copied()
    }

    pub fn target_arch(&self) -> TargetArch {
        self.target_arch
    }

    pub fn needs_write_barrier(&self) -> bool {
        matches!(self.collector_name, CollectorName::Swiper)
    }

    pub fn array_class_id(&self) -> ClassId {
        self.array_class_id
    }

    pub fn string_class_id(&self) -> ClassId {
        self.string_class_id
    }
}

pub(super) fn compile_transitive_closure(
    ctx: &AotCodegenContext<'_>,
    tc: &TransitiveClosure,
) -> CompiledTransitiveClosure {
    let mut ctc = CompiledTransitiveClosure::new();

    for (fct_id, type_params) in &tc.functions {
        compile_function(ctx, *fct_id, type_params.clone(), &mut ctc);
    }

    for thunk in &tc.thunks {
        let (code, code_kind) = compile_trait_object_thunk(ctx, thunk);

        ctc.functions.push(CompiledFunction {
            target: CompiledFunctionTarget::TraitObjectThunk(thunk.clone()),
            code,
            code_kind,
        });
    }

    ctc
}

fn compile_function(
    ctx: &AotCodegenContext<'_>,
    fct_id: FunctionId,
    type_params: BytecodeTypeArray,
    ctc: &mut CompiledTransitiveClosure,
) {
    let fct = ctx.program.fct(fct_id);

    if fct.is_native {
        assert!(
            fct.bytecode.is_none(),
            "native function {} has bytecode",
            display_fct(ctx.program, fct_id)
        );

        // Method is implemented in native code. Create trampoline for invoking it.
        let internal_fct = NativeFct {
            target: NativeTarget::Symbol(native_function_symbol(ctx.program, fct_id)),
            args: BytecodeTypeArray::new(fct.params.clone()),
            return_type: fct.return_type.clone(),
            desc: NativeFctKind::RuntimeEntryTrampoline(fct_id),
        };

        let code = runtime_entry_trampoline::generate_aot(ctx.target_arch(), internal_fct, false);
        ctc.functions.push(CompiledFunction {
            target: CompiledFunctionTarget::Function {
                fct_id,
                type_params,
            },
            code,
            code_kind: CompiledCodeKind::RuntimeEntryTrampoline,
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
    ctx: &AotCodegenContext<'_>,
    fct_id: FunctionId,
    program_fct: &FunctionData,
    params: BytecodeTypeArray,
    return_type: BytecodeType,
    bytecode_fct: &BytecodeFunction,
    type_params: &BytecodeTypeArray,
    specialize_self: Option<SpecializeSelf>,
) -> (CodeDescriptor, CompiledCodeKind) {
    debug_assert!(type_params.iter().all(|ty| ty.is_concrete_type()));

    let (emit_graph, emit_html) = should_emit_graph(ctx.program, fct_id, ctx.emit_graph);
    let emit_final_graph = emit_graph;
    let emit_graph_after_each_pass = emit_graph && ctx.emit_graph_after_each_pass;

    let compilation_data = CompilationData {
        program: ctx.program,
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

    let code = ctx.compiler_invocation.compile(compilation_data, ctx);

    (code, CompiledCodeKind::OptimizedFct)
}

fn compile_trait_object_thunk(
    ctx: &AotCodegenContext<'_>,
    thunk: &TraitObjectThunk,
) -> (CodeDescriptor, CompiledCodeKind) {
    let trait_fct_id = thunk.trait_fct_id;
    let trait_type_params = thunk.trait_object_ty.type_params();
    let all_type_params = trait_type_params.append(thunk.actual_object_ty.clone());

    assert!(all_type_params.iter().all(|ty| ty.is_concrete_type()));

    let trait_object_type_param_id = all_type_params.len() - 1;
    assert_eq!(
        &all_type_params[trait_object_type_param_id],
        &thunk.actual_object_ty
    );

    let bytecode_fct = generate_bytecode_for_trait_object_thunk(
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

pub(super) fn should_emit_graph(
    program: &Program,
    fct_id: FunctionId,
    emit_graph: Option<&str>,
) -> (bool, bool) {
    if let Some(names) = emit_graph {
        let (matches, has_plus) = fct_pattern_match(program, fct_id, names);
        (matches && !has_plus, matches && has_plus)
    } else {
        (false, false)
    }
}

fn fct_pattern_match(program: &Program, fct_id: FunctionId, pattern: &str) -> (bool, bool) {
    if pattern == "all" || pattern == "*" {
        return (true, false);
    } else if pattern == "all+" || pattern == "*+" {
        return (true, true);
    }

    let fct_name = display_fct(program, fct_id);

    for part in pattern.split(';') {
        let (part, plus) = if let Some(part) = part.strip_suffix('+') {
            (part, true)
        } else {
            (part, false)
        };

        if fct_name.ends_with(part) {
            return (true, plus);
        }
    }

    (false, false)
}

pub type AotCompileFn =
    for<'a> fn(CompilationData<'a>, &HashMap<FunctionId, Intrinsic>) -> CodeDescriptor;

pub trait AotContextGuard {}

struct NoopAotContextGuard;

impl AotContextGuard for NoopAotContextGuard {}

pub trait AotBackend {
    fn enter_context<'ctx>(
        &self,
        _ctx: &'ctx AotCodegenContext<'_>,
    ) -> Box<dyn AotContextGuard + 'ctx> {
        Box::new(NoopAotContextGuard)
    }

    fn compile<'a>(
        &self,
        compilation_data: CompilationData<'a>,
        ctx: &AotCodegenContext<'_>,
    ) -> CodeDescriptor;
}

struct ExternalAotBackend {
    compile: AotCompileFn,
}

impl AotBackend for ExternalAotBackend {
    fn compile<'a>(
        &self,
        compilation_data: CompilationData<'a>,
        ctx: &AotCodegenContext<'_>,
    ) -> CodeDescriptor {
        (self.compile)(compilation_data, ctx.intrinsics())
    }
}

pub struct CompilerInvocation {
    backend: Box<dyn AotBackend>,
}

impl CompilerInvocation {
    pub fn new(backend: impl AotBackend + 'static) -> CompilerInvocation {
        CompilerInvocation {
            backend: Box::new(backend),
        }
    }

    pub fn external(compile: AotCompileFn) -> CompilerInvocation {
        CompilerInvocation::new(ExternalAotBackend { compile })
    }

    fn enter_context<'ctx>(
        &self,
        ctx: &'ctx AotCodegenContext<'_>,
    ) -> Box<dyn AotContextGuard + 'ctx> {
        self.backend.enter_context(ctx)
    }

    fn compile<'a>(
        &self,
        compilation_data: CompilationData<'a>,
        ctx: &AotCodegenContext<'_>,
    ) -> CodeDescriptor {
        self.backend.compile(compilation_data, ctx)
    }
}

pub struct AotCompileInputs {
    known_elements: AotKnownElements,
    compiler_invocation: CompilerInvocation,
    target_arch: TargetArch,
    collector_name: CollectorName,
    emit_compiler: bool,
    emit_graph: Option<String>,
    emit_graph_after_each_pass: bool,
}

pub trait AotCompileArgs {
    fn target_arch(&self) -> TargetArch;
    fn collector_name(&self) -> CollectorName;
    fn emit_graph(&self) -> Option<&str>;
    fn emit_graph_after_each_pass(&self) -> bool;
}

impl AotCompileInputs {
    pub fn from_program(
        program: &Program,
        args: &impl AotCompileArgs,
        compiler_invocation: CompilerInvocation,
    ) -> AotCompileInputs {
        AotCompileInputs {
            known_elements: AotKnownElements::from_program(program),
            compiler_invocation,
            target_arch: args.target_arch(),
            collector_name: args.collector_name(),
            emit_compiler: false,
            emit_graph: args.emit_graph().map(ToOwned::to_owned),
            emit_graph_after_each_pass: args.emit_graph_after_each_pass(),
        }
    }

    pub fn target_arch(&self) -> TargetArch {
        self.target_arch
    }
}

fn build_aot_compilation(
    ctx: &AotCodegenContext<'_>,
    tc: &TransitiveClosure,
    ctc: CompiledTransitiveClosure,
    known_elements: AotKnownElements,
    collector_name: CollectorName,
    mut strings: AotStringTable,
    runtime_functions: Vec<AotFunction>,
    test_function_ids: &[FunctionId],
) -> AotCompilation {
    let program = ctx.program();
    let layout = ctx.layout();

    // Compute global memory layout.
    let global_layout = compute_global_layout(layout, program);

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
            CompiledCodeKind::OptimizedFct => AotCodeKind::Optimized,
            CompiledCodeKind::RuntimeEntryTrampoline => AotCodeKind::RuntimeEntryTrampoline,
        };

        let name = aot_compiled_function_name(program, entry);
        let symbol_name = match &entry.code_kind {
            CompiledCodeKind::RuntimeEntryTrampoline => {
                mangle_name(&format!("{name}$runtime_entry"))
            }
            _ => mangle_name(&name),
        };

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

        let mut relocations = Vec::new();
        for reloc in &entry.code.relocations.entries {
            match &reloc.target {
                RelocationKind::DirectCall {
                    fct_id,
                    type_params,
                } => {
                    let target_name = display_fct_specialized(program, *fct_id, type_params);
                    let target = if program.fct(*fct_id).is_native {
                        mangle_name(&format!("{target_name}$runtime_entry"))
                    } else {
                        mangle_name(&target_name)
                    };
                    relocations.push(AotRelocation {
                        offset: reloc.offset,
                        target: AotRelocationTarget::Call(target),
                        form: reloc.form,
                    });
                }
                RelocationKind::NativeCall(symbol) => {
                    relocations.push(AotRelocation {
                        offset: reloc.offset,
                        target: AotRelocationTarget::Call(symbol.clone()),
                        form: reloc.form,
                    });
                }
                RelocationKind::RuntimeFunction(runtime_function) => {
                    relocations.push(AotRelocation {
                        offset: reloc.offset,
                        target: AotRelocationTarget::Call(
                            runtime_function_symbol(*runtime_function).to_string(),
                        ),
                        form: reloc.form,
                    });
                }
                RelocationKind::StringConst {
                    owner_fct_id,
                    const_pool_idx,
                } => {
                    let value = resolve_string_relocation(program, *owner_fct_id, *const_pool_idx);
                    relocations.push(AotRelocation {
                        offset: reloc.offset,
                        target: AotRelocationTarget::StringSlot(strings.intern(&value)),
                        form: reloc.form,
                    });
                }
                RelocationKind::Shape { key } => {
                    let shape_id = shape_interner.intern(key.clone());
                    relocations.push(AotRelocation {
                        offset: reloc.offset,
                        target: AotRelocationTarget::ShapeSlot(shape_id),
                        form: reloc.form,
                    });
                }
                RelocationKind::GlobalValueAddress { global_id } => {
                    relocations.push(AotRelocation {
                        offset: reloc.offset,
                        target: AotRelocationTarget::Global(AotGlobalRelocationTarget::Value(
                            *global_id,
                        )),
                        form: reloc.form,
                    });
                }
                RelocationKind::GlobalStateAddress { global_id } => {
                    relocations.push(AotRelocation {
                        offset: reloc.offset,
                        target: AotRelocationTarget::Global(AotGlobalRelocationTarget::State(
                            *global_id,
                        )),
                        form: reloc.form,
                    });
                }
                RelocationKind::JumpTableEntry(_) => {
                    unimplemented!("AOT jump table relocations");
                }
                RelocationKind::CodeTarget | RelocationKind::Object => {
                    unreachable!("unexpected unresolved relocation target in AOT");
                }
            }
        }

        aot_functions.push(AotFunction {
            symbol_name,
            fct_id: fct_id.index_as_u32(),
            function,
            kind,
            code: bytes,
            relocations,
            gcpoints,
            locations,
            inlined_functions,
        });
    }

    intern_shape_keys(&mut shape_interner, &tc.shape_keys);

    let test_functions = test_function_ids
        .iter()
        .map(|&fct_id| {
            let symbol_name = mangle_name(&aot_display_name(
                program,
                fct_id,
                &BytecodeTypeArray::empty(),
            ));
            AotTestFunction {
                symbol_name,
                fct_id: fct_id.index_as_u32(),
            }
        })
        .collect();

    let known_shapes = build_known_shapes(known_elements.classes, &shape_interner);
    let shapes = encode_aot_shapes(
        layout,
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
        collector_name,
        test_functions,
    }
}

fn compile_aot_runtime_trampolines(
    strings: &mut AotStringTable,
    target_arch: TargetArch,
) -> Vec<AotFunction> {
    let mut runtime_functions = Vec::new();

    let function_info = synthetic_function_info(strings, "dora_aot_trap_trampoline");
    runtime_functions.push(compile_runtime_function_trampoline(
        "dora_aot_trap_trampoline",
        "dora_native_trap".to_string(),
        function_info,
        BytecodeTypeArray::one(BytecodeType::Int32),
        BytecodeType::Unit,
        NativeFctKind::TrapTrampoline,
        AotCodeKind::TrapTrampoline,
        target_arch,
    ));
    let function_info = synthetic_function_info(strings, "dora_aot_safepoint_trampoline");
    runtime_functions.push(compile_runtime_function_trampoline(
        "dora_aot_safepoint_trampoline",
        "dora_native_safepoint_slow".to_string(),
        function_info,
        BytecodeTypeArray::empty(),
        BytecodeType::Unit,
        NativeFctKind::SafepointTrampoline,
        AotCodeKind::SafepointTrampoline,
        target_arch,
    ));
    let function_info = synthetic_function_info(strings, "dora_aot_gc_allocation_trampoline");
    runtime_functions.push(compile_runtime_function_trampoline(
        "dora_aot_gc_allocation_trampoline",
        "dora_native_gc_alloc".to_string(),
        function_info,
        BytecodeTypeArray::new(vec![BytecodeType::Int64, BytecodeType::Bool]),
        BytecodeType::Ptr,
        NativeFctKind::GcAllocationTrampoline,
        AotCodeKind::AllocationFailureTrampoline,
        target_arch,
    ));
    let function_info = synthetic_function_info(strings, "dora_aot_unreachable_trampoline");
    runtime_functions.push(compile_runtime_function_trampoline(
        "dora_aot_unreachable_trampoline",
        "dora_native_unreachable".to_string(),
        function_info,
        BytecodeTypeArray::empty(),
        BytecodeType::Unit,
        NativeFctKind::UnreachableTrampoline,
        AotCodeKind::UnreachableTrampoline,
        target_arch,
    ));
    let function_info = synthetic_function_info(strings, "dora_aot_fatal_error_trampoline");
    runtime_functions.push(compile_runtime_function_trampoline(
        "dora_aot_fatal_error_trampoline",
        "dora_native_fatal_error".to_string(),
        function_info,
        BytecodeTypeArray::one(BytecodeType::Ptr),
        BytecodeType::Unit,
        NativeFctKind::FatalErrorTrampoline,
        AotCodeKind::FatalErrorTrampoline,
        target_arch,
    ));

    runtime_functions
}

fn compile_runtime_function_trampoline(
    symbol_name: &str,
    target_symbol: String,
    function: AotFunctionInfo,
    args: BytecodeTypeArray,
    return_type: BytecodeType,
    desc: NativeFctKind,
    kind: AotCodeKind,
    target_arch: TargetArch,
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
    let code = runtime_entry_trampoline::generate_aot(target_arch, native_fct, false);
    let gcpoints = code.gcpoints.entries();
    assert_eq!(gcpoints.len(), 1);
    let (pc_offset, gcpoint) = &gcpoints[0];
    let gcpoints = vec![AotGcPoint {
        pc_offset: *pc_offset,
        offsets: gcpoint.offsets.clone(),
    }];

    let relocations = &code.relocations.entries;
    assert_eq!(relocations.len(), 1);
    let reloc = &relocations[0];
    let RelocationKind::NativeCall(symbol) = &reloc.target else {
        unreachable!("unexpected relocation in AOT GC allocation trampoline");
    };
    let relocations = vec![AotRelocation {
        offset: reloc.offset,
        target: AotRelocationTarget::Call(symbol.clone()),
        form: reloc.form,
    }];

    AotFunction {
        symbol_name: symbol_name.to_string(),
        fct_id,
        function,
        kind,
        code: code.code,
        relocations,
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

fn compute_global_layout(layout: &AotLayout<'_>, program: &Program) -> GlobalLayout {
    let number_globals = program.globals.len();
    let mut memory_size = 0usize;
    let mut references = Vec::new();
    let mut globals = Vec::with_capacity(number_globals);

    let initialized_field_size = 1;

    for global_var in &program.globals {
        let state_offset = memory_size;
        memory_size += initialized_field_size;

        let ty = global_var.ty.clone();
        assert!(ty.is_concrete_type());

        let ty_size = layout.size(ty.clone()) as usize;
        let ty_align = layout.align(ty.clone()) as usize;

        let value_offset = align_usize_up(memory_size, ty_align);
        layout.add_ref_fields(&mut references, value_offset as i32, ty);
        globals.push(GlobalLayoutEntry {
            state_offset,
            value_offset,
        });
        memory_size = value_offset + ty_size;
    }

    GlobalLayout {
        memory_size,
        references,
        globals,
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
pub(super) fn aot_compiled_function_name(program: &Program, entry: &CompiledFunction) -> String {
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
}

impl AotKnownElements {
    fn from_program(program: &Program) -> AotKnownElements {
        AotKnownElements {
            classes: AotKnownClasses::from_program(program),
        }
    }
}

#[derive(Clone, Copy)]
struct AotKnownClasses {
    array_class_id: ClassId,
    string_class_id: ClassId,
    thread_class_id: ClassId,
}

impl AotKnownClasses {
    fn from_program(program: &Program) -> AotKnownClasses {
        AotKnownClasses {
            array_class_id: resolve_path(program, "std::collections::Array")
                .expect("'std::collections::Array' not found")
                .class_id()
                .expect("class expected"),
            string_class_id: resolve_path(program, "std::string::String")
                .expect("'std::string::String' not found")
                .class_id()
                .expect("class expected"),
            thread_class_id: resolve_path(program, "std::thread::Thread")
                .expect("'std::thread::Thread' not found")
                .class_id()
                .expect("class expected"),
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
            encode_lambda_aot_shape(layout, program, id, *fct_id, type_params, symbols)
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
    let class = layout.class_layout(class_id, type_params);
    let size = InstanceSize::Fixed(class.size);

    AotShape {
        id,
        name: display_class_shape_name(program, class_id, type_params),
        kind: ShapeKind::Class(class_id, type_params.clone()),
        fields: encode_shape_fields(&class.fields),
        visitor: aot_shape_visitor(size),
        refs: class.refs,
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
    let enum_variant = layout.enum_variant_layout(enum_id, type_params, variant_id);
    let size = InstanceSize::Fixed(enum_variant.size);

    AotShape {
        id,
        name: display_enum_variant_shape_name(program, enum_id, type_params, variant_id),
        kind: ShapeKind::EnumVariant(enum_id, type_params.clone(), variant_id),
        fields: encode_shape_fields(&enum_variant.fields),
        visitor: aot_shape_visitor(size),
        refs: enum_variant.refs,
        instance_size: aot_instance_size(size),
        element_size: aot_element_size(size),
        vtable_entries: Vec::new(),
    }
}

fn encode_lambda_aot_shape(
    layout: &AotLayout<'_>,
    program: &Program,
    id: u32,
    fct_id: FunctionId,
    type_params: &BytecodeTypeArray,
    symbols: &AotSymbolMaps,
) -> AotShape {
    let lambda = layout.lambda_layout(fct_id, type_params);
    let size = InstanceSize::Fixed(lambda.size);

    AotShape {
        id,
        name: display_lambda_shape_name(program, fct_id, type_params),
        kind: ShapeKind::Lambda(fct_id, type_params.clone()),
        fields: encode_shape_fields(&lambda.fields),
        visitor: ShapeVisitor::Regular,
        refs: lambda.refs,
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
    let mut csize = object_header_size();

    debug_assert!(actual_object_ty.is_concrete_type());

    let field_size = layout.size(actual_object_ty.clone());
    let field_align = layout.align(actual_object_ty.clone());
    let offset = align_i32(csize, field_align);
    let fields = vec![FieldInstance {
        offset,
        ty: actual_object_ty.clone(),
    }];
    layout.add_ref_fields(&mut refs, offset, actual_object_ty.clone());
    csize = align_i32(offset + field_size, ptr_width());
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
