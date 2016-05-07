use std::cmp::Ordering;
use std::collections::BTreeMap;

use ctxt::FctId;
use mem::Ptr;

pub struct CodeMap {
    tree: BTreeMap<PtrSpan, FctId>
}

impl CodeMap {
    pub fn new() -> CodeMap {
        CodeMap {
            tree: BTreeMap::new()
        }
    }

    pub fn dump(&self) {
        println!("CodeMap {{");

        for (key, fctid) in &self.tree {
            println!("  {:?} => {:?}", key, fctid);
        }

        println!("}}");
    }

    pub fn insert(&mut self, start: Ptr, end: Ptr, fct: FctId) {
        let span = PtrSpan::new(start, end);
        assert!(self.tree.insert(span, fct).is_none());
    }

    pub fn get(&self, ptr: Ptr) -> Option<FctId> {
        let span = PtrSpan::new(ptr, ptr.offset(1));

        self.tree.get(&span).map(|el| { *el })
    }
}

#[derive(Copy, Clone, Debug)]
struct PtrSpan {
    start: Ptr,
    end: Ptr,
}

impl PtrSpan {
    fn intersect(&self, other: &PtrSpan) -> bool {
        (self.start <= other.start && other.start < self.end) ||
            (self.start < other.end && other.end <= self.end) ||
            (other.start <= self.start && self.end <= other.end)
    }
}

impl PartialEq for PtrSpan {
    fn eq(&self, other: &PtrSpan) -> bool {
        self.intersect(other)
    }
}

impl Eq for PtrSpan {}

impl PartialOrd for PtrSpan {
    fn partial_cmp(&self, other: &PtrSpan) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for PtrSpan {
    fn cmp(&self, other: &PtrSpan) -> Ordering {
        if self.intersect(other) {
            Ordering::Equal

        } else if self.start >= other.end {
            Ordering::Greater

        } else {
            Ordering::Less
        }
    }
}

impl PtrSpan {
    fn new(start: Ptr, end: Ptr) -> PtrSpan {
        assert!(start < end);

        PtrSpan { start: start, end: end }
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
fn span(v1: usize, v2: usize) -> PtrSpan {
    PtrSpan::new(ptr(v1), ptr(v2))
}

#[cfg(test)]
pub fn ptr(val: usize) -> Ptr {
    use libc::c_void;

    Ptr::new(val as *mut c_void)
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
