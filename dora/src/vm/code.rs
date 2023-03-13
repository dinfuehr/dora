use parking_lot::RwLock;

use std::collections::HashSet;
use std::fmt;
use std::ptr;
use std::sync::Arc;

use crate::cpu::flush_icache;
use crate::gc::Address;
use crate::masm::CodeDescriptor;
use crate::mem;
use crate::object::Header;
use crate::os;
use crate::vm::VM;
use crate::vtable::VTable;
use dora_frontend::bytecode::{BytecodeTypeArray, FunctionId, Location};

pub const CODE_ALIGNMENT: usize = 16;

#[derive(Debug, Clone)]
pub enum CodeKind {
    DoraFct(FunctionId),
    CompileStub,
    TrapStub,
    AllocStub,
    VerifyStub,
    NativeStub(FunctionId),
    DoraStub,
    GuardCheckStub,
    SafepointStub,
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

    let constpool_start = object_start.offset(object_header_size);
    let instruction_start = constpool_start.offset(code_descriptor.constpool.size() as usize);

    if object_start.is_null() {
        panic!("out of memory: not enough executable memory left!");
    }

    os::jit_writable();

    let clsid = vm.known_instances.code_class_instance();
    let cls = vm.class_instances.idx(clsid);
    let vtable = cls.vtable.read();
    let vtable: &VTable = vtable.as_ref().expect("missing vtable");

    let code_header = object_start.to_mut_ptr::<ManagedCodeHeader>();
    let code_header = unsafe { &mut *code_header };
    code_header
        .object_header
        .set_vtblptr(Address::from_ptr(vtable as *const VTable));
    code_header.object_header.clear_fwdptr();
    code_header.length = array_length;
    code_header.native_code_object = Address::null();

    code_descriptor.constpool.install(constpool_start.to_ptr());

    unsafe {
        ptr::copy_nonoverlapping(
            code_descriptor.code.as_ptr(),
            instruction_start.to_mut_ptr(),
            code_descriptor.code.len(),
        );
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
}

impl Code {
    pub fn location_for_offset(&self, offset: u32) -> Option<Location> {
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
            CodeKind::NativeStub(fct_id) => fct_id,
            CodeKind::DoraFct(fct_id) => fct_id,
            _ => panic!("no fctid found"),
        }
    }

    pub fn instruction_start(&self) -> Address {
        self.instruction_start
    }

    pub fn instruction_end(&self) -> Address {
        self.object_end
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
    entries: Vec<(u32, Location)>,
}

impl LocationTable {
    pub fn new() -> LocationTable {
        LocationTable {
            entries: Vec::new(),
        }
    }

    pub fn insert(&mut self, offset: u32, location: Location) {
        if let Some(last) = self.entries.last() {
            debug_assert!(offset > last.0);
        }

        self.entries.push((offset, location));
    }

    pub fn get(&self, offset: u32) -> Option<Location> {
        let result = self
            .entries
            .binary_search_by_key(&offset, |&(offset, _)| offset);

        match result {
            Ok(idx) => Some(self.entries[idx].1),
            Err(_) => None,
        }
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
}

#[derive(Clone, Debug)]
pub enum LazyCompilationSite {
    Direct(FunctionId, i32, BytecodeTypeArray),
    Virtual(bool, FunctionId, u32, BytecodeTypeArray),
    Lambda(bool),
}

#[derive(Debug)]
pub struct RelocationTable {
    #[allow(dead_code)]
    entries: Vec<(u32, RelocationKind)>,
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
