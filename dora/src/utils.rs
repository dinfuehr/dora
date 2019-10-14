use parking_lot::{Mutex, MutexGuard};
use std::sync::Arc;

pub struct GrowableVec<T> {
    elements: Mutex<Vec<Arc<T>>>,
}

impl<T> GrowableVec<T> {
    pub fn new() -> GrowableVec<T> {
        GrowableVec {
            elements: Mutex::new(Vec::new()),
        }
    }

    pub fn lock(&self) -> MutexGuard<Vec<Arc<T>>> {
        self.elements.lock()
    }

    pub fn push(&self, val: T) -> usize {
        let mut elements = self.elements.lock();
        let idx = elements.len();
        elements.push(Arc::new(val));

        idx
    }

    pub fn len(&self) -> usize {
        let elements = self.elements.lock();
        elements.len()
    }

    pub fn iter(&self) -> GrowableVecIter<T> {
        GrowableVecIter { vec: self, idx: 0 }
    }

    pub fn idx_usize(&self, idx: usize) -> Arc<T> {
        let elements = self.elements.lock();
        elements[idx].clone()
    }
}

pub struct GrowableVecIter<'a, T>
where
    T: 'a,
{
    vec: &'a GrowableVec<T>,
    idx: usize,
}

impl<'a, T> Iterator for GrowableVecIter<'a, T> {
    type Item = Arc<T>;

    fn next(&mut self) -> Option<Arc<T>> {
        let length = self.vec.len();

        if self.idx < length {
            let idx = self.idx;
            self.idx += 1;
            Some(self.vec.idx_usize(idx))
        } else {
            None
        }
    }
}

#[test]
fn test_push() {
    let vec: GrowableVec<Mutex<i32>> = GrowableVec::new();

    {
        vec.push(Mutex::new(1));
        vec.push(Mutex::new(2));

        let elem = vec.idx_usize(1);
        let mut elem = elem.lock();

        *elem = 10;
        for i in 3..8 {
            vec.push(Mutex::new(i));
        }
    }

    assert_eq!(7, vec.len());
}
