use std::cell::RefCell;
use std::ops::Index;

pub struct GrowableVec<T> {
    elements: RefCell<Vec<Box<RefCell<T>>>>,
}

impl<T> GrowableVec<T> {
    pub fn new() -> GrowableVec<T> {
        GrowableVec {
            elements: RefCell::new(Vec::new()),
        }
    }

    pub fn push(&self, val: T) {
        self.elements.borrow_mut().push(Box::new(RefCell::new(val)));
    }

    pub fn len(&self) -> usize {
        self.elements.borrow().len()
    }

    pub fn iter(&self) -> GrowableVecIter<T> {
        GrowableVecIter { vec: self, idx: 0 }
    }
}

impl<T> Index<usize> for GrowableVec<T> {
    type Output = RefCell<T>;

    fn index(&self, idx: usize) -> &RefCell<T> {
        let elements = self.elements.borrow();
        let ptr = elements[idx].as_ref() as *const _;

        unsafe { &*ptr }
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
    type Item = &'a RefCell<T>;

    fn next(&mut self) -> Option<&'a RefCell<T>> {
        let length = self.vec.len();

        if self.idx < length {
            let idx = self.idx;
            self.idx += 1;
            Some(&self.vec[idx])
        } else {
            None
        }
    }
}

#[test]
fn test_push() {
    let vec = GrowableVec::<i32>::new();

    {
        vec.push(1);
        vec.push(2);
        let mut elem = vec[1].borrow_mut();

        *elem = 10;
        for i in 3..8 {
            vec.push(i);
        }
    }

    assert_eq!(7, vec.len());
}
