use dora_frontend::Id;
use parking_lot::RwLock;
use std::sync::Arc;

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

macro_rules! enumeration {
    (@step $idx:expr, $name:ident,) => {
        impl $name {
            pub fn from_u8(value: u8) -> Option<$name> {
                if value < $idx {
                    Some($name(value))
                } else {
                    None
                }
            }
        }
    };

    (@step $idx:expr, $name:ident, $head:ident, $($tail:ident,)*) => {
        impl $name {
            #[allow(non_upper_case_globals)]
            pub const $head: $name = $name($idx);
        }

        enumeration!(@step $idx + 1, $name, $($tail,)*);
    };

    ($name:ident { $($list:ident),+ }) => {
        #[derive(Copy, Clone, PartialEq, Eq)]
        pub struct $name(u8);

        impl $name {
            pub fn to_u8(self) -> u8 {
                self.0
            }
        }

        enumeration!(@step 0, $name, $($list,)*);
    };
}

pub(crate) use enumeration;
