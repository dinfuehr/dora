use std::cmp::Ordering;
use std::collections::BTreeMap;

use baseline::fct::JitFctId;
use ctxt::VM;
use gc::Address;

pub struct CodeMap {
    tree: BTreeMap<CodeSpan, CodeDescriptor>,
}

impl CodeMap {
    pub fn new() -> CodeMap {
        CodeMap {
            tree: BTreeMap::new(),
        }
    }

    pub fn dump(&self, vm: &VM) {
        println!("CodeMap {{");

        for (key, data) in &self.tree {
            print!("  {} - {} => ", key.start, key.end);

            match data {
                &CodeDescriptor::DoraFct(jit_fct_id) => {
                    let jit_fct = vm.jit_fcts.idx(jit_fct_id);
                    let fct = vm.fcts.idx(jit_fct.fct_id());
                    let fct = fct.read();

                    println!("dora {}", fct.full_name(vm));
                }
                &CodeDescriptor::CompilerThunk => println!("compiler_thunk"),
                &CodeDescriptor::ThrowThunk => println!("throw_thunk"),
                &CodeDescriptor::TrapThunk => println!("trap_thunk"),
                &CodeDescriptor::AllocThunk => println!("alloc_thunk"),
                &CodeDescriptor::NativeThunk(jit_fct_id) => {
                    let jit_fct = vm.jit_fcts.idx(jit_fct_id);
                    let fct = vm.fcts.idx(jit_fct.fct_id());
                    let fct = fct.read();

                    println!("native {}", fct.full_name(vm));
                }
                &CodeDescriptor::DoraEntry => println!("dora_entry"),
            }
        }

        println!("}}");
    }

    pub fn insert(&mut self, start: Address, end: Address, data: CodeDescriptor) {
        let span = CodeSpan::new(start, end);
        assert!(self.tree.insert(span, data).is_none());
    }

    pub fn get(&self, ptr: Address) -> Option<CodeDescriptor> {
        let span = CodeSpan::new(ptr, ptr.offset(1));

        self.tree.get(&span).map(|el| *el)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum CodeDescriptor {
    DoraFct(JitFctId),
    CompilerThunk,
    ThrowThunk,
    TrapThunk,
    AllocThunk,
    NativeThunk(JitFctId),
    DoraEntry,
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

        CodeSpan {
            start: start,
            end: end,
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

        map.insert(5.into(), 7.into(), CodeDescriptor::DoraFct(1.into()));
        map.insert(7.into(), 9.into(), CodeDescriptor::DoraFct(2.into()));

        assert_eq!(None, map.get(4.into()));
        assert_eq!(Some(CodeDescriptor::DoraFct(1.into())), map.get(5.into()));
        assert_eq!(Some(CodeDescriptor::DoraFct(1.into())), map.get(6.into()));
        assert_eq!(Some(CodeDescriptor::DoraFct(2.into())), map.get(7.into()));
        assert_eq!(Some(CodeDescriptor::DoraFct(2.into())), map.get(8.into()));
        assert_eq!(None, map.get(9.into()));
    }

    #[test]
    #[should_panic]
    fn test_insert_fails() {
        let mut map = CodeMap::new();

        map.insert(5.into(), 7.into(), CodeDescriptor::DoraFct(1.into()));
        map.insert(6.into(), 7.into(), CodeDescriptor::DoraFct(2.into()));
    }
}
