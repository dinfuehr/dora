use std::hash::Hash;
use std::collections::HashSet;

pub struct HashSetStack<T> where T: Hash + Eq {
    stack: Vec<HashSet<T>>,
}

impl<T> HashSetStack<T> where T: Hash + Eq {
    pub fn new() -> HashSetStack<T> {
        HashSetStack { stack: Vec::new() }
    }

    pub fn contains(&self, ind: &T) -> bool {
        self.stack.iter().any(|e| e.contains(ind))
    }

    pub fn reset(&mut self) {
        self.stack.clear();
        self.stack.push(HashSet::new());
    }

    pub fn insert(&mut self, ind: T) {
        let last = self.stack.len() - 1;
        self.stack[last].insert(ind);
    }

    pub fn push(&mut self) {
        self.stack.push(HashSet::new());
    }

    pub fn push_hash(&mut self, hash: HashSet<T>) {
        self.stack.push(hash);
    }

    pub fn pop(&mut self) -> HashSet<T> {
        self.stack.pop().unwrap()
    }
}
