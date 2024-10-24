use parking_lot::RwLock;

use std::cmp::Ordering;
use std::collections::BTreeMap;

use crate::gc::Address;
use crate::vm::{CodeId, CodeKind, VM};

use dora_bytecode::display_fct;

pub struct CodeMap {
    tree: RwLock<BTreeMap<CodeSpan, CodeId>>,
}

impl CodeMap {
    pub fn new() -> CodeMap {
        CodeMap {
            tree: RwLock::new(BTreeMap::new()),
        }
    }

    pub fn dump(&self, vm: &VM) {
        let tree = self.tree.read();
        println!("CodeMap {{");

        for (key, &code_id) in tree.iter() {
            print!("  {} - {} => ", key.start, key.end);
            let code = vm.code_objects.get(code_id);

            match code.descriptor() {
                CodeKind::BaselineFct(fct_id) => {
                    println!("dora {}", display_fct(&vm.program, fct_id));
                }
                CodeKind::OptimizedFct(fct_id) => {
                    println!("dora(opt) {}", display_fct(&vm.program, fct_id));
                }
                CodeKind::LazyCompilationStub => println!("compile_stub"),
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

    pub fn insert(&self, start: Address, end: Address, code_id: CodeId) {
        let mut tree = self.tree.write();
        let span = CodeSpan::new(start, end);
        assert!(tree.insert(span, code_id).is_none());
    }

    pub fn get(&self, ptr: Address) -> Option<CodeId> {
        let tree = self.tree.read();
        let span = CodeSpan::new(ptr, ptr.offset(1));
        tree.get(&span).map(|el| *el)
    }
}

#[derive(Copy, Clone, Debug)]
struct CodeSpan {
    start: Address,
    end: Address,
}

impl CodeSpan {
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

impl CodeSpan {
    fn new(start: Address, end: Address) -> CodeSpan {
        assert!(start < end);

        CodeSpan { start, end }
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
        let map = CodeMap::new();

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
        let map = CodeMap::new();

        map.insert(5.into(), 7.into(), 1.into());
        map.insert(6.into(), 7.into(), 2.into());
    }
}
