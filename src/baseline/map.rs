use std::cmp::Ordering;
use std::collections::BTreeMap;

use ctxt::{Context, FctId};

pub struct CodeMap {
    tree: BTreeMap<CodeSpan, FctId>,
}

impl CodeMap {
    pub fn new() -> CodeMap {
        CodeMap { tree: BTreeMap::new() }
    }

    pub fn dump(&self, ctxt: &Context) {
        println!("CodeMap {{");

        for (key, &fctid) in &self.tree {
            let fct = ctxt.fct_by_id(fctid);
            let fct_name = fct.full_name(ctxt);

            println!("  {:?} - {:?} => {} (id {})", key.start, key.end, fct_name, fct.id.0);
        }

        println!("}}");
    }

    pub fn insert(&mut self, start: *const u8, end: *const u8, fct: FctId) {
        let span = CodeSpan::new(start, end);
        assert!(self.tree.insert(span, fct).is_none());
    }

    pub fn get(&self, ptr: *const u8) -> Option<FctId> {
        let span = CodeSpan::new(ptr, unsafe { ptr.offset(1) });

        self.tree.get(&span).map(|el| *el)
    }
}

#[derive(Copy, Clone, Debug)]
struct CodeSpan {
    start: *const u8,
    end: *const u8,
}

impl CodeSpan {
    fn intersect(&self, other: &CodeSpan) -> bool {
        (self.start <= other.start && other.start < self.end) ||
        (self.start < other.end && other.end <= self.end) ||
        (other.start <= self.start && self.end <= other.end)
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
    fn new(start: *const u8, end: *const u8) -> CodeSpan {
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
    CodeSpan::new(ptr(v1), ptr(v2))
}

#[cfg(test)]
pub fn ptr(val: usize) -> *const u8 {
    val as *const u8
}

#[cfg(test)]
mod tests {
    use super::*;
    use ctxt::FctId;

    #[test]
    fn test_insert() {
        let mut map = CodeMap::new();

        map.insert(ptr(5), ptr(7), FctId(1));
        map.insert(ptr(7), ptr(9), FctId(2));

        assert_eq!(None, map.get(ptr(4)));
        assert_eq!(Some(FctId(1)), map.get(ptr(5)));
        assert_eq!(Some(FctId(1)), map.get(ptr(6)));
        assert_eq!(Some(FctId(2)), map.get(ptr(7)));
        assert_eq!(Some(FctId(2)), map.get(ptr(8)));
        assert_eq!(None, map.get(ptr(9)));
    }

    #[test]
    #[should_panic]
    fn test_insert_fails() {
        let mut map = CodeMap::new();

        map.insert(ptr(5), ptr(7), FctId(1));
        map.insert(ptr(6), ptr(7), FctId(2));
    }
}
