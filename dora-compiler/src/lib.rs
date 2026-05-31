use dora_bytecode::{
    BytecodeFunction, BytecodeTraitType, BytecodeType, BytecodeTypeArray, ClassId, ConstPoolIdx,
    EnumId, FunctionData, FunctionId, FunctionKind, GlobalId, ImplId, Location, Program,
};
use std::collections::HashSet;

mod extensions;
mod impls;
pub mod layout;
mod specialize;
mod ty;
pub mod wire;

pub use extensions::block_matches_ty_in_program;
pub use impls::{
    TypeParamBoundsIter, bounds_for_tp, find_impl_in_program, find_trait_impl_in_program,
    find_trait_ty_impl_in_program, tp_implements_trait, ty_implements_trait_in_program,
};
pub use layout::{
    AotEnumLayout, AotLayout, AotRecordLayout, FieldInstance, InstanceSize, MachineMode, align_i32,
    array_header_size, object_header_size, ptr_width,
};
pub use specialize::{
    specialize_bty, specialize_bty_array, specialize_bty_for_trait_object,
    specialize_bty_for_trait_object_array, specialize_trait_ty_in_program,
    specialize_ty_array_in_program, specialize_ty_in_program,
};
pub use ty::BytecodeTypeExt;

pub struct SpecializeSelf {
    pub impl_id: ImplId,
    pub container_type_params: usize,
    pub trait_ty: BytecodeTraitType,
    pub extended_ty: BytecodeType,
}

pub struct CompilationData<'a> {
    pub program: &'a Program,
    pub bytecode_fct: &'a BytecodeFunction,
    pub params: BytecodeTypeArray,
    pub has_variadic_parameter: bool,
    pub return_type: BytecodeType,
    pub fct_id: FunctionId,
    pub type_params: BytecodeTypeArray,
    pub specialize_self: Option<SpecializeSelf>,
    pub loc: Location,

    pub emit_debug: bool,
    pub emit_code_comments: bool,
    pub emit_final_graph: bool,
    pub emit_graph_after_each_pass: bool,
    pub emit_html: bool,
}

pub fn register_ty(ty: BytecodeType) -> BytecodeType {
    match ty {
        BytecodeType::Class(_, _) | BytecodeType::Lambda(_, _) => BytecodeType::Ptr,
        _ => ty,
    }
}

pub fn get_bytecode<'a>(
    program: &'a Program,
    program_fct: &'a FunctionData,
) -> Option<(&'a BytecodeFunction, Option<SpecializeSelf>)> {
    match program_fct.bytecode.as_ref() {
        Some(bytecode_fct) => Some((bytecode_fct, None)),
        None => {
            let trait_method_id = program_fct.trait_method_impl?;
            let trait_method = program.fct(trait_method_id);

            let program_fct_impl_id = match program_fct.kind {
                FunctionKind::Impl(impl_id) => impl_id,
                _ => unreachable!(),
            };

            let bytecode_fct = trait_method.bytecode.as_ref()?;

            let program_fct_impl = program.impl_(program_fct_impl_id);

            let specialize_self = SpecializeSelf {
                impl_id: program_fct_impl_id,
                container_type_params: program_fct_impl.type_params.type_param_count(),
                trait_ty: program_fct_impl.trait_ty.clone(),
                extended_ty: program_fct_impl.extended_ty.clone(),
            };

            Some((bytecode_fct, Some(specialize_self)))
        }
    }
}

#[derive(Clone)]
pub struct CodeDescriptor {
    pub code: Vec<u8>,
    pub gcpoints: GcPointTable,
    pub comments: CommentTable,
    pub positions: LocationTable,
    pub relocations: RelocationTable,
    pub inlined_functions: Vec<InlinedFunction>,
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
    pub entries: Vec<(u32, RelocationKind)>,
}

impl From<Vec<(u32, RelocationKind)>> for RelocationTable {
    fn from(entries: Vec<(u32, RelocationKind)>) -> Self {
        RelocationTable { entries }
    }
}

impl RelocationTable {
    pub fn new() -> RelocationTable {
        RelocationTable {
            entries: Vec::new(),
        }
    }

    pub fn insert(&mut self, offset: u32, kind: RelocationKind) {
        if let Some(last) = self.entries.last() {
            debug_assert!(offset > last.0);
        }

        self.entries.push((offset, kind));
    }
}

#[derive(Clone, Copy, Debug)]
#[allow(dead_code)]
pub enum RuntimeFunction {
    TrapTrampoline,
    SafepointTrampoline,
    GcAllocationTrampoline,
    WriteBarrierSlowPath,
    UnreachableTrampoline,
    FatalErrorTrampoline,
}

#[derive(Clone, Debug)]
#[allow(dead_code)]
pub enum RelocationKind {
    JumpTableEntry(u32),
    CodeTarget,
    Object,
    NativeCall(String),
    RuntimeFunction(RuntimeFunction),
    StringConst {
        owner_fct_id: FunctionId,
        const_pool_idx: ConstPoolIdx,
    },
    Shape {
        key: AotShapeKey,
    },
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
