use parking_lot::RwLock;

use std::collections::HashSet;
use std::fmt;
use std::ptr;
use std::sync::Arc;

use crate::cpu::flush_icache;
use crate::gc::Address;
use crate::mem;
use crate::mirror::Header;
use crate::os;
use crate::vm::VM;
use dora_bytecode::{BytecodeType, BytecodeTypeArray, FunctionId, Location};

pub const CODE_ALIGNMENT: usize = 16;

#[derive(Debug, Clone)]
pub enum CodeKind {
    DoraEntryTrampoline,

    BaselineFct(FunctionId),
    OptimizedFct(FunctionId),
    RuntimeEntryTrampoline(FunctionId),

    LazyCompilationStub,

    StackOverflowTrampoline,
    SafepointTrampoline,
    TrapTrampoline,
    AllocationFailureTrampoline,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct CodeId(usize);

impl CodeId {
    pub fn from(idx: usize) -> CodeId {
        CodeId(idx)
    }

    pub fn idx(self) -> usize {
        self.0
    }
}

impl From<usize> for CodeId {
    fn from(data: usize) -> CodeId {
        CodeId(data)
    }
}

pub fn install_code_stub(vm: &VM, code_descriptor: CodeDescriptor, kind: CodeKind) -> Arc<Code> {
    let code = install_code(vm, code_descriptor, kind);
    vm.add_code(code.clone());
    code
}

#[repr(C)]
pub struct ManagedCodeHeader {
    object_header: Header,
    length: usize,
    native_code_object: Address,
    padding: usize,
}

impl ManagedCodeHeader {
    pub fn drop_native_code_object(&mut self) -> Arc<Code> {
        let native_code = unsafe { Arc::from_raw(self.native_code_object.to_ptr::<Code>()) };
        self.native_code_object = Address::null();
        native_code
    }
}

pub fn install_code(vm: &VM, code_descriptor: CodeDescriptor, kind: CodeKind) -> Arc<Code> {
    let object_header_size = std::mem::size_of::<ManagedCodeHeader>();
    debug_assert!(object_header_size % CODE_ALIGNMENT == 0);
    debug_assert!(code_descriptor.constpool.size() as usize % CODE_ALIGNMENT == 0);
    debug_assert!(code_descriptor.code.len() as usize % CODE_ALIGNMENT == 0);

    let object_size =
        object_header_size + code_descriptor.constpool.size() as usize + code_descriptor.code.len();

    debug_assert!(object_size % CODE_ALIGNMENT == 0);

    let object_start = vm.gc.alloc_code(object_size);
    let object_end = object_start.offset(object_size);

    let array_length =
        (object_size - (Header::size() as usize) - mem::ptr_width_usize()) / mem::ptr_width_usize();

    let object_payload_start = object_start.offset(object_header_size);
    let instruction_start = object_payload_start.offset(code_descriptor.constpool.size() as usize);

    if object_start.is_null() {
        panic!("out of memory: not enough executable memory left!");
    }

    os::jit_writable();

    let code_header = object_start.to_mut_ptr::<ManagedCodeHeader>();
    let code_header = unsafe { &mut *code_header };
    code_header.object_header.setup_header_word(
        vm.known.code_shape().address(),
        vm.meta_space_start(),
        false,
        false,
    );
    code_header.length = array_length;
    code_header.native_code_object = Address::null();
    code_header.padding = 0;

    // Fill constant pool.
    code_descriptor
        .constpool
        .install(object_payload_start.to_ptr());

    // Copy machine code into object.
    unsafe {
        ptr::copy_nonoverlapping(
            code_descriptor.code.as_ptr(),
            instruction_start.to_mut_ptr(),
            code_descriptor.code.len(),
        );
    }

    // Initialize jump table entries.
    for (offset, reloc_kind) in &code_descriptor.relocations.entries {
        match reloc_kind {
            RelocationKind::JumpTableEntry(pos) => {
                let jump_target = instruction_start.add_ptr(*pos as usize);
                let address = object_payload_start.offset(*offset as usize);
                unsafe {
                    *address.to_mut_ptr::<Address>() = jump_target;
                }
            }
            _ => (),
        }
    }

    let native_code_object = Arc::new(Code {
        object_start,
        object_end,
        instruction_start,
        kind,
        lazy_compilation: code_descriptor.lazy_compilation,
        gcpoints: code_descriptor.gcpoints,
        comments: code_descriptor.comments,
        locations: code_descriptor.positions,
        relocations: code_descriptor.relocations,
        inlined_functions: code_descriptor.inlined_functions,
    });

    let code_header = object_start.to_mut_ptr::<ManagedCodeHeader>();
    let code_header = unsafe { &mut *code_header };
    code_header.native_code_object = Address::from_ptr(Arc::into_raw(native_code_object.clone()));

    os::jit_executable();

    flush_icache(object_start.to_ptr(), object_size);

    native_code_object
}

pub struct Code {
    object_start: Address,
    object_end: Address,

    // pointer to beginning of function
    instruction_start: Address,

    kind: CodeKind,

    lazy_compilation: LazyCompilationData,
    gcpoints: GcPointTable,
    comments: CommentTable,
    locations: LocationTable,
    relocations: RelocationTable,
    inlined_functions: Vec<InlinedFunction>,
}

impl Code {
    pub fn lazy_compilation(&self) -> &LazyCompilationData {
        &self.lazy_compilation
    }

    pub fn location_for_offset(&self, offset: u32) -> Option<InlinedLocation> {
        self.locations.get(offset)
    }

    pub fn gcpoint_for_offset(&self, offset: u32) -> Option<&GcPoint> {
        self.gcpoints.get(offset)
    }

    pub fn object_start(&self) -> Address {
        self.object_start
    }

    pub fn object_end(&self) -> Address {
        self.object_end
    }

    pub fn fct_id(&self) -> FunctionId {
        match self.kind {
            CodeKind::RuntimeEntryTrampoline(fct_id)
            | CodeKind::BaselineFct(fct_id)
            | CodeKind::OptimizedFct(fct_id) => fct_id,
            _ => panic!("no fctid found"),
        }
    }

    pub fn instruction_start(&self) -> Address {
        self.instruction_start
    }

    pub fn instruction_end(&self) -> Address {
        self.object_end
    }

    pub fn instruction_slice(&self) -> &[u8] {
        unsafe {
            std::slice::from_raw_parts(
                self.instruction_start().to_ptr::<u8>(),
                self.instruction_size(),
            )
        }
    }

    pub fn instruction_size(&self) -> usize {
        self.instruction_end().offset_from(self.instruction_start())
    }

    pub fn comments_for_offset(&self, offset: u32) -> Vec<&String> {
        self.comments.get(offset)
    }

    pub fn lazy_for_offset(&self, offset: u32) -> Option<&LazyCompilationSite> {
        self.lazy_compilation.get(offset)
    }

    pub fn descriptor(&self) -> CodeKind {
        self.kind.clone()
    }

    pub fn is_optimized(&self) -> bool {
        match self.kind {
            CodeKind::OptimizedFct(_) => true,
            _ => false,
        }
    }

    pub fn inlined_function(&self, id: InlinedFunctionId) -> &InlinedFunction {
        &self.inlined_functions[id.0 as usize]
    }
}

impl fmt::Debug for Code {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Code {{ start: {:?}, end: {:?}, desc: {:?} }}",
            self.object_start(),
            self.object_end(),
            self.kind,
        )
    }
}

pub struct CodeDescriptor {
    pub constpool: ConstPool,
    pub code: Vec<u8>,
    pub lazy_compilation: LazyCompilationData,
    pub gcpoints: GcPointTable,
    pub comments: CommentTable,
    pub positions: LocationTable,
    pub relocations: RelocationTable,
    pub inlined_functions: Vec<InlinedFunction>,
}

#[derive(Debug)]
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
}

#[derive(Debug)]
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

pub struct InlinedFunction {
    pub fct_id: FunctionId,
    pub type_params: BytecodeTypeArray,
    pub inlined_location: InlinedLocation,
}

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

#[derive(Debug)]
pub struct LocationTable {
    entries: Vec<(u32, InlinedLocation)>,
}

impl LocationTable {
    pub fn new() -> LocationTable {
        LocationTable {
            entries: Vec::new(),
        }
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
}

#[derive(Debug, Clone)]
pub struct InlinedLocation {
    pub location: Location,
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

#[derive(Debug)]
pub struct LazyCompilationData {
    entries: Vec<(u32, LazyCompilationSite)>,
}

impl LazyCompilationData {
    pub fn new() -> LazyCompilationData {
        LazyCompilationData {
            entries: Vec::new(),
        }
    }

    pub fn insert(&mut self, offset: u32, info: LazyCompilationSite) {
        if let Some(last) = self.entries.last() {
            debug_assert!(offset > last.0);
        }

        self.entries.push((offset, info));
    }

    pub fn get(&self, offset: u32) -> Option<&LazyCompilationSite> {
        let result = self
            .entries
            .binary_search_by_key(&offset, |&(offset, _)| offset);

        match result {
            Ok(idx) => Some(&self.entries[idx].1),
            Err(_) => None,
        }
    }

    pub fn entries(&self) -> &[(u32, LazyCompilationSite)] {
        &self.entries
    }
}

#[derive(Clone, Debug)]
pub enum LazyCompilationSite {
    Direct {
        fct_id: FunctionId,
        type_params: BytecodeTypeArray,
        const_pool_offset_from_ra: i32,
    },
    Virtual {
        receiver_is_first: bool,
        trait_object_ty: BytecodeType,
        vtable_index: u32,
    },
    Lambda {
        receiver_is_first: bool,
        params: BytecodeTypeArray,
        return_type: BytecodeType,
    },
}

#[derive(Debug)]
pub struct ConstPool {
    entries: Vec<ConstPoolEntry>,
    size: i32,
}

#[derive(Debug)]
pub struct ConstPoolEntry {
    pub disp: i32,
    pub value: ConstPoolValue,
}

#[derive(Debug, PartialEq)]
pub enum ConstPoolValue {
    Ptr(Address),
    Float32(f32),
    Float64(f64),
    Int128(i128),
}

impl ConstPoolValue {
    fn size(&self) -> i32 {
        match self {
            &ConstPoolValue::Ptr(_) => mem::ptr_width(),
            &ConstPoolValue::Float32(_) => std::mem::size_of::<f32>() as i32,
            &ConstPoolValue::Float64(_) => std::mem::size_of::<f64>() as i32,
            &ConstPoolValue::Int128(_) => std::mem::size_of::<i128>() as i32,
        }
    }
}

impl ConstPool {
    pub fn new() -> ConstPool {
        ConstPool {
            entries: Vec::new(),
            size: 0,
        }
    }

    pub fn size(&self) -> i32 {
        self.size
    }

    pub fn install(&self, ptr: *const u8) {
        for entry in &self.entries {
            let offset = self.size - entry.disp;

            unsafe {
                let entry_ptr = ptr.offset(offset as isize);

                match entry.value {
                    ConstPoolValue::Ptr(v) => {
                        *(entry_ptr as *mut Address) = v;
                    }

                    ConstPoolValue::Float32(v) => {
                        *(entry_ptr as *mut f32) = v;
                    }

                    ConstPoolValue::Float64(v) => {
                        *(entry_ptr as *mut f64) = v;
                    }

                    ConstPoolValue::Int128(v) => {
                        *(entry_ptr as *mut i128) = v;
                    }
                }
            }
        }
    }

    pub fn add_addr_reuse(&mut self, ptr: Address) -> i32 {
        for entry in &self.entries {
            if entry.value == ConstPoolValue::Ptr(ptr) {
                return entry.disp;
            }
        }

        self.add_addr(ptr)
    }

    pub fn add_addr(&mut self, ptr: Address) -> i32 {
        self.add_value(ConstPoolValue::Ptr(ptr))
    }

    pub fn add_f32(&mut self, value: f32) -> i32 {
        self.add_value(ConstPoolValue::Float32(value))
    }

    pub fn add_f64(&mut self, value: f64) -> i32 {
        self.add_value(ConstPoolValue::Float64(value))
    }

    pub fn add_i128(&mut self, value: i128) -> i32 {
        self.add_value(ConstPoolValue::Int128(value))
    }

    pub fn add_value(&mut self, value: ConstPoolValue) -> i32 {
        let size = value.size();
        self.size = mem::align_i32(self.size + size, size);

        let entry = ConstPoolEntry {
            disp: self.size,
            value,
        };

        self.entries.push(entry);

        self.size
    }

    pub fn align(&mut self, size: i32) -> i32 {
        assert!(size > 0);
        self.size = mem::align_i32(self.size, size);

        self.size
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add_addr() {
        let mut constpool = ConstPool::new();
        assert_eq!(mem::ptr_width(), constpool.add_addr(1.into()));
        assert_eq!(2 * mem::ptr_width(), constpool.add_addr(1.into()));
    }

    #[test]
    fn test_add_addr_reuse() {
        let mut constpool = ConstPool::new();
        assert_eq!(mem::ptr_width(), constpool.add_addr_reuse(1.into()));
        assert_eq!(mem::ptr_width(), constpool.add_addr_reuse(1.into()));
    }
}

#[derive(Debug)]
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
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum RelocationKind {
    JumpTableEntry(u32),
    CodeTarget,
    Object,
}

pub struct CodeObjects {
    data: RwLock<Vec<Arc<Code>>>,
}

impl CodeObjects {
    pub fn new() -> CodeObjects {
        CodeObjects {
            data: RwLock::new(Vec::new()),
        }
    }

    pub fn get(&self, id: CodeId) -> Arc<Code> {
        let data = self.data.read();
        data[id.idx()].clone()
    }

    pub fn add(&self, object: Arc<Code>) -> CodeId {
        let mut data = self.data.write();
        let code_id: CodeId = data.len().into();
        data.push(object);
        code_id
    }
}
