use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::fmt;
use std::ptr;

use crate::gc::Address;
use crate::mem;
use crate::mirror::Header;
use crate::os;
use crate::vm::VM;
use dora_bytecode::{FunctionId, Location, display_fct};
use dora_compiler::{
    CodeDescriptor, CommentTable, GcPoint, GcPointTable, InlinedFunction, InlinedFunctionId,
    InlinedLocation, LocationTable, RelocationKind, RelocationTable,
};

pub use dora_compiler::CODE_ALIGNMENT;

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

pub struct CodeMap {
    entries: Vec<Code>,
    tree: BTreeMap<CodeSpan, CodeId>,
}

impl CodeMap {
    pub fn new() -> CodeMap {
        CodeMap {
            entries: Vec::new(),
            tree: BTreeMap::new(),
        }
    }

    pub fn dump(&self, vm: &VM) {
        println!("CodeMap {{");

        for (key, &code_id) in self.tree.iter() {
            print!("  {} - {} => ", key.start, key.end);
            let code = self.get_code(code_id);

            match code.descriptor() {
                CodeKind::BaselineFct(fct_id) => {
                    println!("dora {}", display_fct(&vm.program, fct_id));
                }
                CodeKind::OptimizedFct(fct_id) => {
                    println!("dora(opt) {}", display_fct(&vm.program, fct_id));
                }
                CodeKind::TrapTrampoline => println!("trap_stub"),
                CodeKind::AllocationFailureTrampoline => println!("alloc_stub"),
                CodeKind::RuntimeEntryTrampoline(fct_id) => {
                    println!("native stub {}", display_fct(&vm.program, fct_id));
                }
                CodeKind::DoraEntryTrampoline => println!("dora_stub"),
                CodeKind::StackOverflowTrampoline => println!("guard_check_stub"),
                CodeKind::SafepointTrampoline => println!("safepoint_stub"),
            }
        }

        println!("}}");
    }

    pub fn add(&mut self, code: Code) -> CodeId {
        let code_id: CodeId = self.entries.len().into();
        self.insert(code.object_start(), code.object_end(), code_id);
        self.entries.push(code);
        code_id
    }

    pub fn get_code(&self, id: CodeId) -> &Code {
        &self.entries[id.idx()]
    }

    fn insert(&mut self, start: Address, end: Address, code_id: CodeId) {
        let span = CodeSpan::new(start, end);
        assert!(self.tree.insert(span, code_id).is_none());
    }

    pub fn get(&self, ptr: Address) -> Option<CodeId> {
        let span = CodeSpan::new(ptr, ptr.offset(1));
        self.tree.get(&span).map(|el| *el)
    }
}

pub fn install_code_stub(vm: &mut VM, code_descriptor: CodeDescriptor, kind: CodeKind) -> CodeId {
    let code = install_code(vm, code_descriptor, kind);
    vm.add_code(code)
}

pub fn install_external_code_stub(
    vm: &mut VM,
    instruction_start: Address,
    instruction_end: Address,
    kind: CodeKind,
    gcpoints: GcPointTable,
    locations: LocationTable,
    function_info_aot: FunctionInfoAot,
    inlined_functions_aot: Vec<InlinedFunctionAot>,
) -> CodeId {
    assert!(instruction_start < instruction_end);

    let code = Code {
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
    };

    vm.add_code(code)
}

#[repr(C)]
struct ManagedCodeHeader {
    object_header: Header,
    length: usize,
}

pub fn install_code(vm: &mut VM, code_descriptor: CodeDescriptor, kind: CodeKind) -> Code {
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

    let code = Code {
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
    };

    os::jit_executable();

    code
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

#[derive(Copy, Clone, Debug)]
struct CodeSpan {
    start: Address,
    end: Address,
}

impl CodeSpan {
    fn new(start: Address, end: Address) -> CodeSpan {
        assert!(start < end);

        CodeSpan { start, end }
    }

    fn intersect(&self, other: &CodeSpan) -> bool {
        (self.start <= other.start && other.start < self.end)
            || (self.start < other.end && other.end <= self.end)
            || (other.start <= self.start && self.end <= other.end)
    }
}

impl PartialEq for CodeSpan {
    fn eq(&self, other: &CodeSpan) -> bool {
        self.intersect(other)
    }
}

impl Eq for CodeSpan {}

impl PartialOrd for CodeSpan {
    fn partial_cmp(&self, other: &CodeSpan) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for CodeSpan {
    fn cmp(&self, other: &CodeSpan) -> Ordering {
        if self.intersect(other) {
            Ordering::Equal
        } else if self.start >= other.end {
            Ordering::Greater
        } else {
            Ordering::Less
        }
    }
}

#[test]
#[should_panic]
fn test_new_fail() {
    span(7, 5);
}

#[test]
fn test_new() {
    span(5, 7);
}

#[test]
fn test_intersect() {
    assert!(span(5, 7).intersect(&span(1, 6)));
    assert!(!span(5, 7).intersect(&span(1, 5)));
    assert!(span(5, 7).intersect(&span(5, 7)));
    assert!(span(5, 7).intersect(&span(4, 7)));
    assert!(!span(5, 7).intersect(&span(7, 9)));
    assert!(!span(5, 7).intersect(&span(7, 8)));
}

#[cfg(test)]
fn span(v1: usize, v2: usize) -> CodeSpan {
    CodeSpan::new(v1.into(), v2.into())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_insert() {
        let mut map = CodeMap::new();

        map.insert(5.into(), 7.into(), 1.into());
        map.insert(7.into(), 9.into(), 2.into());

        assert_eq!(None, map.get(4.into()));
        assert_eq!(Some(1.into()), map.get(5.into()));
        assert_eq!(Some(1.into()), map.get(6.into()));
        assert_eq!(Some(2.into()), map.get(7.into()));
        assert_eq!(Some(2.into()), map.get(8.into()));
        assert_eq!(None, map.get(9.into()));
    }

    #[test]
    #[should_panic]
    fn test_insert_fails() {
        let mut map = CodeMap::new();

        map.insert(5.into(), 7.into(), 1.into());
        map.insert(6.into(), 7.into(), 2.into());
    }
}
