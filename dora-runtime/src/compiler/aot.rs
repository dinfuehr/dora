use std::collections::{HashMap, HashSet};

use dora_bytecode::{
    BytecodeFunction, BytecodeType, BytecodeTypeArray, ClassId, ConstPoolEntry, ConstPoolIdx,
    EnumId, FunctionData, FunctionId, Location, PackageId, Program, display_fct,
    display_fct_specialized, display_ty, display_ty_array, lookup_fct, resolve_path,
};

use crate::ShapeVisitor;
use crate::aot::layout::AotLayout;
use crate::boots::{self, BOOTS_FUNCTIONS};
use crate::cannon;
use crate::compiler::closure::{TraitObjectThunk, TransitiveClosure, compute_transitive_closure};
use crate::compiler::runtime_entry_trampoline;
use crate::compiler::{
    CompilationData, NativeFct, NativeFctKind, NativeTarget, SpecializeSelf, get_bytecode,
    trait_object_thunk,
};
use crate::gc::Address;
use crate::mangle_name;
use crate::mem;
use crate::mirror::Header;
use crate::size::InstanceSize;
use crate::startup::encode_shape_fields;
use crate::stdlib::STDLIB_FUNCTIONS;
use crate::stdlib::io::IO_FUNCTIONS;
use crate::vm::{
    AotShapeKey, BytecodeTypeExt, CodeDescriptor, CodeKind, FieldInstance, RelocationKind,
    RuntimeFunction, ShapeKind, TargetArch, specialize_ty_in_program,
};
use crate::vm::{CollectorName, FctImplementation, Intrinsic};

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
    let native_lookup = AotNativeLookup::from_program(program, &tc);
    let ctx = Box::new(AotCodegenContext {
        program,
        layout: AotLayout::new(program),
        intrinsics: collect_aot_intrinsics(program),
        native_lookup: &native_lookup,
        compiler_invocation: inputs.compiler_invocation,
        target_arch: inputs.target_arch,
        collector_name: inputs.collector_name,
        array_class_id: inputs.known_elements.classes.array_class_id,
        string_class_id: inputs.known_elements.classes.string_class_id,
        emit_graph: inputs.emit_graph.as_deref(),
        emit_graph_after_each_pass: inputs.emit_graph_after_each_pass,
    });
    let ctc = {
        let _active_aot_context = boots::set_active_aot_context(ctx.as_ref());
        compile_transitive_closure(ctx.as_ref(), &tc)
    };
    let mut strings = AotStringTable::new();
    let runtime_functions = compile_aot_runtime_trampolines(
        program,
        &mut strings,
        inputs.known_elements,
        inputs.target_arch,
    );

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
    let native_lookup = AotNativeLookup::from_program(program, &tc);
    let ctx = Box::new(AotCodegenContext {
        program,
        layout: AotLayout::new(program),
        intrinsics: collect_aot_intrinsics(program),
        native_lookup: &native_lookup,
        compiler_invocation: inputs.compiler_invocation,
        target_arch: inputs.target_arch,
        collector_name: inputs.collector_name,
        array_class_id: inputs.known_elements.classes.array_class_id,
        string_class_id: inputs.known_elements.classes.string_class_id,
        emit_graph: inputs.emit_graph.as_deref(),
        emit_graph_after_each_pass: inputs.emit_graph_after_each_pass,
    });
    let ctc = {
        let _active_aot_context = boots::set_active_aot_context(ctx.as_ref());
        compile_transitive_closure(ctx.as_ref(), &tc)
    };
    let mut strings = AotStringTable::new();
    let runtime_functions = compile_aot_runtime_trampolines(
        program,
        &mut strings,
        inputs.known_elements,
        inputs.target_arch,
    );

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
    pub(super) code_kind: CodeKind,
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

pub(super) struct AotNativeLookup {
    methods: HashMap<FunctionId, String>,
}

impl AotNativeLookup {
    pub(super) fn from_program(program: &Program, tc: &TransitiveClosure) -> AotNativeLookup {
        let tc_functions: HashSet<FunctionId> =
            tc.functions.iter().map(|(fct_id, _)| *fct_id).collect();
        let mut methods = HashMap::new();

        add_native_functions(program, &tc_functions, STDLIB_FUNCTIONS, &mut methods);
        add_native_functions(program, &tc_functions, IO_FUNCTIONS, &mut methods);

        if program.boots_package_id.is_some() {
            add_native_functions(program, &tc_functions, BOOTS_FUNCTIONS, &mut methods);
        }

        AotNativeLookup { methods }
    }

    pub(super) fn get_target(&self, fct_id: FunctionId) -> Option<NativeTarget> {
        self.methods
            .get(&fct_id)
            .map(|symbol| NativeTarget::Symbol(symbol.clone()))
    }

    pub(super) fn contains(&self, fct_id: FunctionId) -> bool {
        self.methods.contains_key(&fct_id)
    }
}

fn add_native_functions(
    program: &Program,
    tc_functions: &HashSet<FunctionId>,
    functions: &[(&'static str, FctImplementation)],
    methods: &mut HashMap<FunctionId, String>,
) {
    for (path, implementation) in functions {
        let FctImplementation::Native = implementation else {
            continue;
        };
        let fct_id = lookup_fct(program, path).unwrap_or_else(|| panic!("'{}' not found", path));

        if !tc_functions.contains(&fct_id) {
            continue;
        }

        assert!(
            program.fct(fct_id).is_internal,
            "native function {} is not marked @internal",
            display_fct(program, fct_id)
        );

        let existing = methods.insert(fct_id, mangle_name(path));
        assert!(existing.is_none());
    }
}

fn collect_aot_intrinsics(program: &Program) -> HashMap<FunctionId, Intrinsic> {
    let mut intrinsics = HashMap::new();

    add_intrinsic_functions(program, STDLIB_FUNCTIONS, &mut intrinsics);
    add_intrinsic_functions(program, IO_FUNCTIONS, &mut intrinsics);

    if program.boots_package_id.is_some() {
        add_intrinsic_functions(program, BOOTS_FUNCTIONS, &mut intrinsics);
    }

    intrinsics
}

fn add_intrinsic_functions(
    program: &Program,
    functions: &[(&'static str, FctImplementation)],
    intrinsics: &mut HashMap<FunctionId, Intrinsic>,
) {
    for (path, implementation) in functions {
        let FctImplementation::Intrinsic(intrinsic) = implementation else {
            continue;
        };

        if let Some(fct_id) = lookup_fct(program, path) {
            intrinsics.insert(fct_id, *intrinsic);
        }
    }
}

pub(crate) struct AotCodegenContext<'a> {
    pub(super) program: &'a Program,
    layout: AotLayout<'a>,
    intrinsics: HashMap<FunctionId, Intrinsic>,
    pub(super) native_lookup: &'a AotNativeLookup,
    pub(super) compiler_invocation: CompilerInvocation,
    pub(super) target_arch: TargetArch,
    pub(super) collector_name: CollectorName,
    array_class_id: ClassId,
    string_class_id: ClassId,
    pub(super) emit_graph: Option<&'a str>,
    pub(super) emit_graph_after_each_pass: bool,
}

impl AotCodegenContext<'_> {
    pub(crate) fn program(&self) -> &Program {
        self.program
    }

    pub(crate) fn layout(&self) -> &AotLayout<'_> {
        &self.layout
    }

    pub(crate) fn intrinsics(&self) -> &HashMap<FunctionId, Intrinsic> {
        &self.intrinsics
    }

    pub(crate) fn intrinsic_for_function(&self, fct_id: FunctionId) -> Option<Intrinsic> {
        self.intrinsics.get(&fct_id).copied()
    }

    pub(crate) fn target_arch(&self) -> TargetArch {
        self.target_arch
    }

    pub(crate) fn needs_write_barrier(&self) -> bool {
        matches!(self.collector_name, CollectorName::Swiper)
    }

    pub(crate) fn array_class_id(&self) -> ClassId {
        self.array_class_id
    }

    pub(crate) fn string_class_id(&self) -> ClassId {
        self.string_class_id
    }
}

pub(super) fn compile_transitive_closure(
    ctx: &AotCodegenContext<'_>,
    tc: &TransitiveClosure,
) -> CompiledTransitiveClosure {
    let mut ctc = CompiledTransitiveClosure::new();
    compile_functions(ctx, tc, &mut ctc);
    compile_thunks(ctx, tc, &mut ctc);
    ctc
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

    if let Some(target) = ctx.native_lookup.get_target(fct_id) {
        // Method is implemented in native code. Create trampoline for invoking it.
        let internal_fct = NativeFct {
            target,
            args: BytecodeTypeArray::new(fct.params.clone()),
            return_type: fct.return_type.clone(),
            desc: NativeFctKind::RuntimeEntryTrampoline(fct_id),
        };

        let code_kind = runtime_entry_trampoline::code_kind(&internal_fct.desc);
        let code = runtime_entry_trampoline::generate_aot(ctx.target_arch(), internal_fct, false);
        ctc.functions.push(CompiledFunction {
            target: CompiledFunctionTarget::Function {
                fct_id,
                type_params,
            },
            code,
            code_kind,
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
) -> (CodeDescriptor, CodeKind) {
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

    let code = match ctx.compiler_invocation {
        CompilerInvocation::Boots {
            dora_entry_trampoline_address,
            compile_function_address,
        } => boots::compile(
            Address::from_ptr(compile_function_address),
            Address::from_ptr(dora_entry_trampoline_address),
            compilation_data,
        ),
        CompilerInvocation::Cannon => cannon::compile(ctx, compilation_data),
    };

    (code, CodeKind::OptimizedFct(fct_id))
}

fn compile_thunks(
    ctx: &AotCodegenContext<'_>,
    tc: &TransitiveClosure,
    ctc: &mut CompiledTransitiveClosure,
) {
    for thunk in &tc.thunks {
        let (code, code_kind) = compile_trait_object_thunk(ctx, thunk);

        ctc.functions.push(CompiledFunction {
            target: CompiledFunctionTarget::TraitObjectThunk(thunk.clone()),
            code,
            code_kind,
        });
    }
}

fn compile_trait_object_thunk(
    ctx: &AotCodegenContext<'_>,
    thunk: &TraitObjectThunk,
) -> (CodeDescriptor, CodeKind) {
    let trait_fct_id = thunk.trait_fct_id;
    let trait_type_params = thunk.trait_object_ty.type_params();
    let all_type_params = trait_type_params.append(thunk.actual_object_ty.clone());

    assert!(all_type_params.iter().all(|ty| ty.is_concrete_type()));

    let trait_object_type_param_id = all_type_params.len() - 1;
    assert_eq!(
        &all_type_params[trait_object_type_param_id],
        &thunk.actual_object_ty
    );

    let bytecode_fct = trait_object_thunk::generate_bytecode_for_thunk(
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

pub struct AotTestFunction {
    pub symbol_name: String,
    pub fct_id: u32,
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
    pub test_functions: Vec<AotTestFunction>,
}

#[derive(Clone, Copy)]
pub enum CompilerInvocation {
    Boots {
        dora_entry_trampoline_address: *const u8,
        compile_function_address: *const u8,
    },
    Cannon,
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
            CodeKind::OptimizedFct(_) => AotCodeKind::Optimized,
            CodeKind::RuntimeEntryTrampoline(_) => AotCodeKind::RuntimeEntryTrampoline,
            _ => unreachable!("unexpected code kind in AOT compilation output"),
        };

        let name = aot_compiled_function_name(program, entry);
        let symbol_name = match &entry.code_kind {
            CodeKind::RuntimeEntryTrampoline(_) => mangle_name(&format!("{name}$runtime_entry")),
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

        let mut call_relocations = Vec::new();
        let mut string_relocations = Vec::new();
        let mut shape_relocations = Vec::new();
        let mut global_relocations = Vec::new();
        for (offset, reloc_kind) in &entry.code.relocations.entries {
            match reloc_kind {
                RelocationKind::DirectCall {
                    fct_id,
                    type_params,
                } => {
                    let target_name = display_fct_specialized(program, *fct_id, type_params);
                    let target = if ctx.native_lookup.contains(*fct_id) {
                        mangle_name(&format!("{target_name}$runtime_entry"))
                    } else {
                        mangle_name(&target_name)
                    };
                    call_relocations.push(AotCallRelocation {
                        offset: *offset,
                        target,
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
    program: &Program,
    strings: &mut AotStringTable,
    known_elements: AotKnownElements,
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
    let unreachable_fct_id = known_elements.unreachable_fct_id;
    let function_info = function_info_for_fct(program, strings, unreachable_fct_id);
    runtime_functions.push(compile_runtime_function_trampoline(
        "dora_aot_unreachable_trampoline",
        "dora_native_unreachable".to_string(),
        function_info,
        BytecodeTypeArray::empty(),
        BytecodeType::Unit,
        NativeFctKind::RuntimeEntryTrampoline(unreachable_fct_id),
        AotCodeKind::RuntimeEntryTrampoline,
        target_arch,
    ));
    let fatal_error_fct_id = known_elements.fatal_error_fct_id;
    let function_info = function_info_for_fct(program, strings, fatal_error_fct_id);
    runtime_functions.push(compile_runtime_function_trampoline(
        "dora_aot_fatal_error_trampoline",
        mangle_name("std::fatal_error"),
        function_info,
        BytecodeTypeArray::one(BytecodeType::Ptr),
        BytecodeType::Unit,
        NativeFctKind::RuntimeEntryTrampoline(fatal_error_fct_id),
        AotCodeKind::RuntimeEntryTrampoline,
        target_arch,
    ));

    runtime_functions
}

fn compile_runtime_function_trampoline(
    symbol_name: &'static str,
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
    unreachable_fct_id: FunctionId,
    fatal_error_fct_id: FunctionId,
}

impl AotKnownElements {
    fn from_program(program: &Program) -> AotKnownElements {
        AotKnownElements {
            classes: AotKnownClasses::from_program(program),
            unreachable_fct_id: resolve_path(program, "std::unreachable")
                .expect("'std::unreachable' not found")
                .function_id()
                .expect("function expected"),
            fatal_error_fct_id: resolve_path(program, "std::fatal_error")
                .expect("'std::fatal_error' not found")
                .function_id()
                .expect("function expected"),
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
