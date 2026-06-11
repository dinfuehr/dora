use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::fmt;

use crate::gc::Address;
use crate::vm::VM;
use dora_bytecode::{FunctionId, Location, display_fct};
use dora_compiler::{GcPoint, GcPointTable, InlinedFunctionId, InlinedLocation, LocationTable};

pub use dora_compiler::CODE_ALIGNMENT;

#[derive(Debug, Clone)]
pub enum CodeKind {
    DoraEntryTrampoline,

    OptimizedFct(FunctionId),
    RuntimeEntryTrampoline(FunctionId),

    SafepointTrampoline,
    TrapTrampoline,
    StackOverflowTrampoline,
    AllocationFailureTrampoline,
    UnreachableTrampoline,
    FatalErrorTrampoline,
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
                CodeKind::OptimizedFct(fct_id) => {
                    println!("dora(opt) {}", display_fct(&vm.program, fct_id));
                }
                CodeKind::TrapTrampoline => println!("trap_stub"),
                CodeKind::AllocationFailureTrampoline => println!("alloc_stub"),
                CodeKind::RuntimeEntryTrampoline(fct_id) => {
                    println!("native stub {}", display_fct(&vm.program, fct_id));
                }
                CodeKind::DoraEntryTrampoline => println!("dora_stub"),
                CodeKind::StackOverflowTrampoline => println!("stack_overflow_stub"),
                CodeKind::SafepointTrampoline => println!("safepoint_stub"),
                CodeKind::UnreachableTrampoline => println!("unreachable_stub"),
                CodeKind::FatalErrorTrampoline => println!("fatal_error_stub"),
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

pub fn install_external_code(
    vm: &mut VM,
    instruction_start: Address,
    instruction_end: Address,
    kind: CodeKind,
    gcpoints: GcPointTable,
    locations: LocationTable,
    function_info: FunctionInfoAot,
    inlined_functions: Vec<InlinedFunctionAot>,
) -> CodeId {
    assert!(instruction_start < instruction_end);

    let code = Code {
        object_start: instruction_start,
        object_end: instruction_end,
        instruction_start,
        kind,
        gcpoints,
        locations,
        function_info,
        inlined_functions,
    };

    vm.add_code(code)
}

pub struct Code {
    object_start: Address,
    object_end: Address,

    // pointer to beginning of function
    instruction_start: Address,

    kind: CodeKind,

    gcpoints: GcPointTable,
    locations: LocationTable,
    function_info: FunctionInfoAot,
    inlined_functions: Vec<InlinedFunctionAot>,
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
            CodeKind::RuntimeEntryTrampoline(fct_id) | CodeKind::OptimizedFct(fct_id) => fct_id,
            _ => panic!("no fctid found"),
        }
    }

    pub fn instruction_start(&self) -> Address {
        self.instruction_start
    }

    pub fn instruction_end(&self) -> Address {
        self.object_end
    }

    pub fn descriptor(&self) -> CodeKind {
        self.kind.clone()
    }

    pub fn inlined_function(&self, id: InlinedFunctionId) -> &InlinedFunctionAot {
        &self.inlined_functions[id.0 as usize]
    }

    pub fn function_info(&self) -> &FunctionInfoAot {
        &self.function_info
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
