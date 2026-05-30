use dora_bytecode::{
    BytecodeType, BytecodeTypeArray, ClassId, ConstPoolIdx, EnumId, FunctionId, GlobalId, Location,
};
use std::collections::HashSet;

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
