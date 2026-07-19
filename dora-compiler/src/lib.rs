use dora_bytecode::{
    BytecodeBody, BytecodeType, BytecodeTypeArray, ClassId, ConstPoolIdx, EnumId, FunctionData,
    FunctionId, FunctionKind, GlobalId, Location, Program,
};
use std::collections::HashSet;

mod abi;
mod aot;
mod aot_compile;
mod assembly;
mod calling_convention;
mod closure;
pub mod cpu;
pub mod dora_entry_trampoline;
mod extensions;
mod impls;
mod intrinsics;
pub mod layout;
mod native_lookup;
mod reg;
mod runtime_entry_trampoline;
mod specialize;
mod ty;
pub mod wire;

pub use abi::{
    Address, CODE_ALIGNMENT, DoraToNativeInfo, GLOBAL_INITIALIZED, GLOBAL_RUNNING,
    GLOBAL_UNINITIALIZED, Header, LARGE_OBJECT_SIZE, MAX_TLAB_OBJECT_SIZE, REMEMBERED_BIT_SHIFT,
    Shape, ThreadLocalData, ThreadState, Trap, thread_local_dtn_offset,
};
pub use aot::{
    AOT_CODE_KIND_ALLOCATION_FAILURE_TRAMPOLINE, AOT_CODE_KIND_DORA_ENTRY_TRAMPOLINE,
    AOT_CODE_KIND_FATAL_ERROR_TRAMPOLINE, AOT_CODE_KIND_OPTIMIZED,
    AOT_CODE_KIND_RUNTIME_ENTRY_TRAMPOLINE, AOT_CODE_KIND_SAFEPOINT_TRAMPOLINE,
    AOT_CODE_KIND_STACK_OVERFLOW_TRAMPOLINE, AOT_CODE_KIND_TRAP_TRAMPOLINE,
    AOT_CODE_KIND_UNREACHABLE_TRAMPOLINE, AOT_SHAPE_KIND_ARRAY, AOT_SHAPE_KIND_CLASS,
    AOT_SHAPE_KIND_CODE, AOT_SHAPE_KIND_ENUM_VARIANT, AOT_SHAPE_KIND_FILLER_ARRAY,
    AOT_SHAPE_KIND_FILLER_WORD, AOT_SHAPE_KIND_FREE_SPACE, AOT_SHAPE_KIND_LAMBDA,
    AOT_SHAPE_KIND_STRING, AOT_SHAPE_KIND_TRAIT_OBJECT, AOT_SHAPE_REFS_BITMAP_MAX_WORD,
    AOT_SHAPE_REFS_BITMAP_TAG, AOT_SHAPE_VISITOR_INVALID, AOT_SHAPE_VISITOR_NONE,
    AOT_SHAPE_VISITOR_POINTER_ARRAY, AOT_SHAPE_VISITOR_RECORD_ARRAY, AOT_SHAPE_VISITOR_REGULAR,
    AotCodeKind, AotCompilation, AotFunction, AotFunctionInfo, AotGcPoint,
    AotGlobalRelocationTarget, AotInlinedFunction, AotJumpTable, AotKnownShape, AotKnownShapeKind,
    AotLocation, AotRelocation, AotRelocationTarget, AotShape, AotShapeId, AotShapeInterner,
    AotStringId, AotStringTable, AotTestFunction, CollectorName, GlobalLayout, GlobalLayoutEntry,
    ShapeKind, ShapeVisitor, TargetArch, encode_shape_kind, parse_collector, parse_target_arch,
};
pub use aot_compile::{
    AotBackend, AotCodegenContext, AotCompileArgs, AotCompileInputs, AotContextGuard,
    CompilerInvocation, compile_boots_compiler_aot, compile_program_aot, compile_test_runner,
};
pub use assembly::{AotAssemblyKind, write_assembly};
pub use calling_convention::{ArgumentPassingMode, argument_passing_mode};
pub use closure::{TraitObjectThunk, TransitiveClosure, compute_transitive_closure};
pub use extensions::block_matches_ty_in_program;
pub use impls::{
    TypeParamBoundsIter, bounds_for_tp, find_impl_in_program, find_trait_impl_in_program,
    find_trait_ty_impl_in_program, tp_implements_trait, ty_implements_trait_in_program,
};
pub use intrinsics::{Intrinsic, STDLIB_INTRINSICS};
pub use layout::{
    AotEnumLayout, AotLayout, AotRecordLayout, FieldInstance, InstanceSize, MachineMode, align_i32,
    align_usize_up, array_header_size, fits_i32, object_header_size, ptr_width, ptr_width_usize,
};
pub use native_lookup::{native_function_path, native_function_symbol};
pub use reg::{AllocationSize, AnyReg, FReg, Reg};
pub use runtime_entry_trampoline::NativeFct;
pub use specialize::{
    specialize_bty, specialize_bty_array, specialize_bty_for_trait_object,
    specialize_bty_for_trait_object_array, specialize_trait_ty_in_program,
    specialize_ty_array_in_program, specialize_ty_in_program,
};
pub use ty::BytecodeTypeExt;

pub struct FunctionSignature {
    pub params: BytecodeTypeArray,
    pub return_type: BytecodeType,
    pub type_params: BytecodeTypeArray,
}

impl FunctionSignature {
    pub fn from_bytecode(
        bytecode_body: &BytecodeBody,
        function: &FunctionData,
        type_params: BytecodeTypeArray,
    ) -> FunctionSignature {
        let argument_count = function.params.len();
        let params = BytecodeTypeArray::new(bytecode_body.registers()[..argument_count].to_vec());

        FunctionSignature {
            params,
            return_type: function.return_type.clone(),
            type_params,
        }
    }
}

pub struct CompilationOptions {
    pub emit_debug: bool,
    pub emit_final_graph: bool,
    pub emit_graph_after_each_pass: bool,
    pub emit_html: bool,
    pub emit_code_comments: bool,
}

pub struct CompilationData<'a> {
    pub program: &'a Program,
    pub bytecode_body: &'a BytecodeBody,
    pub fct_id: FunctionId,
    pub signature: FunctionSignature,
    pub loc: Location,
    pub options: CompilationOptions,
}

pub struct TraitObjectThunkCompilationData<'a> {
    pub program: &'a Program,
    pub trait_fct_id: FunctionId,
    pub trait_object_ty: BytecodeType,
    pub actual_object_ty: BytecodeType,
    pub receiver_by_reference: bool,
    pub callee_fct_id: FunctionId,
    pub callee_type_params: BytecodeTypeArray,
    pub signature: FunctionSignature,
    pub loc: Location,
    pub options: CompilationOptions,
}

pub fn register_ty(ty: BytecodeType) -> BytecodeType {
    match ty {
        BytecodeType::Class(_, _) | BytecodeType::Lambda(..) => BytecodeType::Ptr,
        _ => ty,
    }
}

pub fn get_bytecode<'a>(
    program: &'a Program,
    program_fct: &'a FunctionData,
) -> Option<(&'a BytecodeBody, &'a FunctionData)> {
    match program_fct.bytecode.as_ref() {
        Some(bytecode_body) => Some((bytecode_body, program_fct)),
        None => {
            let trait_method_id = program_fct.trait_method_impl?;
            let trait_method = program.fct(trait_method_id);
            let bytecode_body = trait_method.bytecode.as_ref()?;
            Some((bytecode_body, trait_method))
        }
    }
}

pub fn bytecode_type_params(
    program: &Program,
    program_fct: &FunctionData,
    type_params: &BytecodeTypeArray,
) -> BytecodeTypeArray {
    if program_fct.bytecode.is_some() {
        return type_params.clone();
    }

    let trait_method_id = program_fct
        .trait_method_impl
        .expect("function without bytecode is not a default trait method");
    let trait_method = program.fct(trait_method_id);
    let impl_id = match program_fct.kind {
        FunctionKind::Impl(impl_id) => impl_id,
        _ => unreachable!(),
    };
    let impl_ = program.impl_(impl_id);
    let impl_container_count = impl_.type_params.type_param_count();
    let trait_container_count = impl_.trait_ty.type_params.len();
    let trait_method_type_param_count = trait_method.type_params.type_param_count();
    assert!(trait_method_type_param_count > trait_container_count);
    assert!(trait_method.has_bytecode_self_type_param());
    let method_type_param_count = trait_method_type_param_count - trait_container_count - 1;
    assert_eq!(
        program_fct.type_params.type_param_count(),
        impl_container_count + method_type_param_count
    );
    assert_eq!(
        type_params.len(),
        impl_container_count + method_type_param_count
    );

    // A default method body is emitted in trait coordinates, while its synthetic impl method is
    // instantiated in impl coordinates. Translate once into the body's ordinary indexed layout:
    // trait arguments, method arguments, and the final bytecode-only Self argument.
    let trait_type_params =
        specialize_ty_array_in_program(program, &impl_.trait_ty.type_params, type_params);
    let (_, method_type_params) = type_params.split(impl_container_count);
    let self_type = specialize_ty_in_program(program, impl_.extended_ty.clone(), type_params);

    trait_type_params
        .connect(&method_type_params)
        .append(self_type)
}

#[derive(Clone)]
pub struct CodeDescriptor {
    pub code: Vec<u8>,
    pub jump_tables: Vec<JumpTable>,
    pub gcpoints: GcPointTable,
    pub comments: CommentTable,
    pub positions: LocationTable,
    pub relocations: RelocationTable,
    pub inlined_functions: Vec<InlinedFunction>,
}

#[derive(Clone, Debug)]
pub struct JumpTable {
    pub targets: Vec<u32>,
}

#[derive(Clone, Debug)]
pub struct GcPointTable {
    entries: Vec<(u32, GcPoint)>,
}

impl GcPointTable {
    pub fn new() -> GcPointTable {
        GcPointTable {
            entries: Vec::new(),
        }
    }

    pub fn get(&self, offset: u32) -> Option<&GcPoint> {
        let result = self
            .entries
            .binary_search_by_key(&offset, |&(offset, _)| offset);

        match result {
            Ok(idx) => Some(&self.entries[idx].1),
            Err(_) => None,
        }
    }

    pub fn insert(&mut self, offset: u32, gcpoint: GcPoint) {
        if let Some(last) = self.entries.last_mut() {
            debug_assert!(offset > last.0);
        }

        self.entries.push((offset, gcpoint));
    }

    pub fn entries(&self) -> &[(u32, GcPoint)] {
        &self.entries
    }
}

#[derive(Clone, Debug)]
pub struct GcPoint {
    pub offsets: Vec<i32>,
}

impl GcPoint {
    pub fn new() -> GcPoint {
        GcPoint {
            offsets: Vec::new(),
        }
    }

    pub fn merge(lhs: GcPoint, rhs: GcPoint) -> GcPoint {
        let mut offsets = HashSet::new();

        for offset in lhs.offsets {
            offsets.insert(offset);
        }

        for offset in rhs.offsets {
            offsets.insert(offset);
        }

        GcPoint::from_offsets(offsets.drain().collect())
    }

    pub fn from_offsets(offsets: Vec<i32>) -> GcPoint {
        GcPoint { offsets }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct InlinedFunctionId(pub u32);

#[derive(Clone)]
pub struct InlinedFunction {
    pub fct_id: FunctionId,
    pub type_params: BytecodeTypeArray,
    pub inlined_location: InlinedLocation,
}

#[derive(Clone)]
pub struct CommentTable {
    entries: Vec<(u32, String)>,
}

impl CommentTable {
    pub fn new() -> CommentTable {
        CommentTable {
            entries: Vec::new(),
        }
    }

    pub fn get(&self, offset: u32) -> Vec<&String> {
        let result = self
            .entries
            .binary_search_by_key(&offset, |&(offset, _)| offset);

        match result {
            Ok(mut idx) => {
                let mut comments = Vec::new();

                // The index is not guaranteed to point to the first entry
                // with that offset.
                while idx > 0 && self.entries[idx - 1].0 == offset {
                    idx -= 1;
                }

                // Now find all entries with this offset.
                while idx < self.entries.len() && self.entries[idx].0 == offset {
                    comments.push(&self.entries[idx].1);
                    idx += 1;
                }

                comments
            }
            Err(_) => Vec::new(),
        }
    }

    pub fn insert(&mut self, offset: u32, comment: String) {
        if let Some(last) = self.entries.last_mut() {
            debug_assert!(offset >= last.0);
        }

        self.entries.push((offset, comment));
    }
}

#[derive(Clone, Debug)]
pub struct LocationTable {
    entries: Vec<(u32, InlinedLocation)>,
}

impl LocationTable {
    pub fn new() -> LocationTable {
        LocationTable {
            entries: Vec::new(),
        }
    }

    pub fn from_entries(entries: Vec<(u32, InlinedLocation)>) -> LocationTable {
        debug_assert!(entries.windows(2).all(|window| window[0].0 <= window[1].0));
        LocationTable { entries }
    }

    pub fn insert(&mut self, offset: u32, location: InlinedLocation) {
        if let Some(last) = self.entries.last() {
            debug_assert!(offset > last.0);
        }

        self.entries.push((offset, location));
    }

    pub fn get(&self, offset: u32) -> Option<InlinedLocation> {
        let result = self
            .entries
            .binary_search_by_key(&offset, |&(offset, _)| offset);

        match result {
            Ok(idx) => Some(self.entries[idx].1.clone()),
            Err(_) => None,
        }
    }

    pub fn entries(&self) -> &[(u32, InlinedLocation)] {
        &self.entries
    }
}

#[derive(Debug, Clone)]
pub struct InlinedLocation {
    pub location: Location,
    // Is Some(x) when the location is in some inlined function instead of
    // the top function.
    pub inlined_function_id: Option<InlinedFunctionId>,
}

impl InlinedLocation {
    pub fn is_inlined(&self) -> bool {
        self.inlined_function_id.is_some()
    }

    pub fn inlined_function_id(&self) -> InlinedFunctionId {
        self.inlined_function_id.expect("no id")
    }
}

#[derive(Clone, Debug)]
pub struct RelocationTable {
    pub entries: Vec<RelocationEntry>,
}

impl From<Vec<RelocationEntry>> for RelocationTable {
    fn from(entries: Vec<RelocationEntry>) -> Self {
        RelocationTable { entries }
    }
}

#[derive(Clone, Debug)]
pub struct RelocationEntry {
    pub offset: u32,
    pub target: RelocationKind,
    pub form: RelocationForm,
}

impl RelocationEntry {
    pub fn new(offset: u32, target: RelocationKind, form: RelocationForm) -> RelocationEntry {
        RelocationEntry {
            offset,
            target,
            form,
        }
    }
}

impl RelocationTable {
    pub fn new() -> RelocationTable {
        RelocationTable {
            entries: Vec::new(),
        }
    }

    pub fn insert(&mut self, offset: u32, target: RelocationKind, form: RelocationForm) {
        if let Some(last) = self.entries.last() {
            debug_assert!(offset >= last.offset);
        }

        self.entries
            .push(RelocationEntry::new(offset, target, form));
    }
}

#[derive(Clone, Copy, Debug)]
#[allow(dead_code)]
pub enum RuntimeFunction {
    TrapTrampoline,
    SafepointTrampoline,
    AllocationSlowTrampoline,
    WriteBarrierSlowPath,
    UnreachableTrampoline,
    FatalErrorTrampoline,
    StackOverflowTrampoline,
}

#[derive(Clone, Debug)]
#[allow(dead_code)]
pub enum RelocationKind {
    JumpTableAddress(u32),
    CodeTarget,
    Object,
    NativeCall(String),
    RuntimeFunction(RuntimeFunction),
    StringConst {
        owner_fct_id: FunctionId,
        const_pool_idx: ConstPoolIdx,
    },
    ShapeAddress {
        key: AotShapeKey,
    },
    ShapeBase,
    GlobalValueAddress {
        global_id: GlobalId,
    },
    GlobalStateAddress {
        global_id: GlobalId,
    },
    DirectCall {
        fct_id: FunctionId,
        type_params: BytecodeTypeArray,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RelocationForm {
    AbsoluteAddress,
    X64CallRel32,
    X64RipRelativeLoad64 {
        disp_offset: u8,
        dst_reg: u8,
    },
    X64RipRelativeLoad32 {
        disp_offset: u8,
        dst_reg: u8,
    },
    X64RipRelativeLea {
        disp_offset: u8,
        dst_reg: u8,
    },
    Arm64Branch26,
    Arm64AdrpLdr {
        page_reg: u8,
        base_reg: u8,
        dst_reg: u8,
        width: Arm64LoadWidth,
    },
    Arm64AdrpAdd {
        page_reg: u8,
        base_reg: u8,
        dst_reg: u8,
    },
}

impl RelocationForm {
    /// Number of machine-code bytes covered by this instruction relocation pattern.
    pub fn instruction_sequence_len(self) -> usize {
        match self {
            RelocationForm::AbsoluteAddress => {
                panic!("absolute address relocation has no instruction sequence length")
            }
            RelocationForm::X64CallRel32 => 5,
            RelocationForm::X64RipRelativeLoad64 { disp_offset, .. }
            | RelocationForm::X64RipRelativeLoad32 { disp_offset, .. }
            | RelocationForm::X64RipRelativeLea { disp_offset, .. } => usize::from(disp_offset) + 4,
            RelocationForm::Arm64Branch26 => 4,
            RelocationForm::Arm64AdrpLdr { .. } | RelocationForm::Arm64AdrpAdd { .. } => 8,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Arm64LoadWidth {
    U32,
    U64,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum AotShapeKey {
    FillerWord,
    FillerArray,
    FreeSpace,
    Code,
    String,
    Class(ClassId, BytecodeTypeArray),
    Array(ClassId, BytecodeTypeArray),
    EnumVariant {
        enum_id: EnumId,
        type_params: BytecodeTypeArray,
        variant_id: u32,
    },
    Lambda(FunctionId, BytecodeTypeArray),
    TraitObject {
        trait_ty: BytecodeType,
        actual_object_ty: BytecodeType,
    },
}
