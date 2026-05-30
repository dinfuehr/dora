use parking_lot::RwLock;

use std::fmt;
use std::ptr;
use std::sync::Arc;

use crate::cpu::flush_icache;
use crate::gc::Address;
use crate::mem;
use crate::mirror::Header;
use crate::os;
use crate::vm::VM;
use dora_bytecode::{FunctionId, Location};
use dora_compiler::{
    CodeDescriptor, CommentTable, GcPoint, GcPointTable, InlinedFunction, InlinedFunctionId,
    InlinedLocation, LocationTable, RelocationKind, RelocationTable,
};

pub const CODE_ALIGNMENT: usize = 16;

#[derive(Debug, Clone)]
pub enum CodeKind {
    DoraEntryTrampoline,

    BaselineFct(FunctionId),
    OptimizedFct(FunctionId),
    RuntimeEntryTrampoline(FunctionId),

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

pub fn install_external_code_stub(
    vm: &VM,
    instruction_start: Address,
    instruction_end: Address,
    kind: CodeKind,
    gcpoints: GcPointTable,
    locations: LocationTable,
    function_info_aot: FunctionInfoAot,
    inlined_functions_aot: Vec<InlinedFunctionAot>,
) -> Arc<Code> {
    assert!(instruction_start < instruction_end);

    let code = Arc::new(Code {
        object_start: instruction_start,
        object_end: instruction_end,
        instruction_start,
        kind,
        gcpoints,
        comments: CommentTable::new(),
        locations,
        relocations: RelocationTable::new(),
        inlined_functions: Vec::new(),
        function_info_aot: Some(function_info_aot),
        inlined_functions_aot,
    });

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
    debug_assert!(code_descriptor.code.len() as usize % CODE_ALIGNMENT == 0);

    let object_size = object_header_size + code_descriptor.code.len();

    debug_assert!(object_size % CODE_ALIGNMENT == 0);

    let object_start = vm.gc.alloc_code(object_size);
    let object_end = object_start.offset(object_size);

    let array_length =
        (object_size - (Header::size() as usize) - mem::ptr_width_usize()) / mem::ptr_width_usize();

    let object_payload_start = object_start.offset(object_header_size);
    let instruction_start = object_payload_start;

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
        gcpoints: code_descriptor.gcpoints,
        comments: code_descriptor.comments,
        locations: code_descriptor.positions,
        relocations: code_descriptor.relocations,
        inlined_functions: code_descriptor.inlined_functions,
        function_info_aot: None,
        inlined_functions_aot: Vec::new(),
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

    gcpoints: GcPointTable,
    comments: CommentTable,
    relocations: RelocationTable,
    locations: LocationTable,
    inlined_functions: Vec<InlinedFunction>,
    function_info_aot: Option<FunctionInfoAot>,
    inlined_functions_aot: Vec<InlinedFunctionAot>,
}

impl Code {
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

    pub fn relocations(&self) -> &RelocationTable {
        &self.relocations
    }

    pub fn gcpoints(&self) -> &GcPointTable {
        &self.gcpoints
    }

    pub fn locations(&self) -> &LocationTable {
        &self.locations
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

    pub fn inlined_functions(&self) -> &[InlinedFunction] {
        &self.inlined_functions
    }

    pub fn function_info_aot(&self) -> Option<&FunctionInfoAot> {
        self.function_info_aot.as_ref()
    }

    pub fn inlined_function_aot(&self, id: InlinedFunctionId) -> &InlinedFunctionAot {
        &self.inlined_functions_aot[id.0 as usize]
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

#[derive(Clone)]
pub struct FunctionInfoAot {
    pub name: &'static str,
    pub file: &'static str,
    pub loc: Location,
}

#[derive(Clone)]
pub struct InlinedFunctionAot {
    pub function_info: FunctionInfoAot,
    pub inlined_location: InlinedLocation,
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
