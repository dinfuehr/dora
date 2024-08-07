use parking_lot::RwLock;
use std::sync::Arc;

pub trait Id {
    type IdType: Copy + Clone;

    fn usize_to_id(value: usize) -> Self::IdType;
    fn id_to_usize(value: Self::IdType) -> usize;
    fn store_id(value: &mut Self, id: Self::IdType);
}

pub struct GrowableVecNonIter<T: Id> {
    elements: RwLock<Vec<Arc<T>>>,
}

impl<T: Id> GrowableVecNonIter<T> {
    pub fn new() -> GrowableVecNonIter<T> {
        GrowableVecNonIter {
            elements: RwLock::new(Vec::new()),
        }
    }

    pub fn push(&self, mut value: T) -> T::IdType {
        let mut elements = self.elements.write();
        let id = T::usize_to_id(elements.len());
        T::store_id(&mut value, id);
        elements.push(Arc::new(value));

        id
    }

    pub fn idx(&self, idx: T::IdType) -> Arc<T> {
        let elements = self.elements.read();
        elements[T::id_to_usize(idx)].clone()
    }
}
