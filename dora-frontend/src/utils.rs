use std::sync::Arc;

pub struct GrowableVec<T> {
    elements: Vec<Arc<T>>,
}

impl<T> GrowableVec<T> {
    pub fn new() -> GrowableVec<T> {
        GrowableVec {
            elements: Vec::new(),
        }
    }

    pub fn push(&mut self, val: T) -> usize {
        let idx = self.elements.len();
        self.elements.push(Arc::new(val));

        idx
    }

    pub fn len(&self) -> usize {
        self.elements.len()
    }

    pub fn iter(&self) -> GrowableVecIter<T> {
        GrowableVecIter { vec: self, idx: 0 }
    }

    pub fn idx_usize(&self, idx: usize) -> Arc<T> {
        self.elements[idx].clone()
    }
}

impl<T: Id> GrowableVec<T> {
    pub fn idx(&self, idx: T::IdType) -> Arc<T> {
        self.idx_usize(T::id_to_usize(idx))
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

pub trait Id {
    type IdType: Copy + Clone;

    fn usize_to_id(value: usize) -> Self::IdType;
    fn id_to_usize(value: Self::IdType) -> usize;
    fn store_id(value: &mut Self, id: Self::IdType);
}

type ElementType<T> = Arc<T>;

pub struct SharedVecIter<'a, T>
where
    T: 'a,
{
    vec: &'a Vec<ElementType<T>>,
    idx: usize,
    len: usize,
}

impl<'a, T> Iterator for SharedVecIter<'a, T> {
    type Item = ElementType<T>;

    fn next(&mut self) -> Option<ElementType<T>> {
        if self.idx < self.len {
            let idx = self.idx;
            self.idx += 1;
            Some(self.vec[idx].clone())
        } else {
            None
        }
    }
}
