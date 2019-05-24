use std::marker::PhantomData;
use std::ops::{Index, IndexMut};

pub struct VecMap<K, V>
where
    K: VecKey,
{
    data: Vec<V>,
    unused: PhantomData<K>,
}

impl<K, V> VecMap<K, V>
where
    K: VecKey,
{
    pub fn new() -> VecMap<K, V> {
        VecMap {
            data: Vec::new(),
            unused: PhantomData,
        }
    }

    pub fn push(&mut self, value: V) -> K {
        let idx = self.data.len();
        self.data.push(value);
        K::new(idx)
    }
}

impl<K, V> Index<K> for VecMap<K, V>
where
    K: VecKey,
{
    type Output = V;

    fn index(&self, k: K) -> &V {
        &self.data[k.index()]
    }
}

impl<K, V> IndexMut<K> for VecMap<K, V>
where
    K: VecKey,
{
    fn index_mut(&mut self, k: K) -> &mut V {
        &mut self.data[k.index()]
    }
}

pub trait VecKey {
    fn new(value: usize) -> Self;
    fn index(&self) -> usize;
}
