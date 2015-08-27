use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub struct Name(pub usize);

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct RcStr(Rc<String>);

impl fmt::Display for RcStr {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", &*self.0)
    }
}

impl fmt::Debug for RcStr {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", &*self.0)
    }
}

impl RcStr {
    fn new(value: String) -> RcStr {
        RcStr(Rc::new(value))
    }
}

impl Borrow<str> for RcStr {
    fn borrow(&self) -> &str {
        &self.0[..]
    }
}

impl Deref for RcStr {
    type Target = String;

    fn deref<'a>(&'a self) -> &'a String {
        &self.0
    }
}

pub struct Interner {
    map: RefCell<HashMap<RcStr, Name>>,
    vec: RefCell<Vec<RcStr>>
}

impl Interner {
    pub fn new() -> Interner {
        Interner {
            map: RefCell::new(HashMap::new()),
            vec: RefCell::new(Vec::new())
        }
    }

    pub fn intern(&self, name: &str) -> Name {
        if let Some(&val) = self.map.borrow().get(name) {
            return val;
        }

        let key = RcStr::new(String::from(name));
        let value = Name(self.vec.borrow().len());

        self.vec.borrow_mut().push(key.clone());
        self.map.borrow_mut().insert(key, value);

        value
    }

    pub fn str(&self, name: Name) -> RcStr {
        let elem = &self.vec.borrow()[name.0];

        elem.clone()
    }
}

#[test]
fn interner() {
    let mut interner = Interner::new();

    assert_eq!(Name(0), interner.intern("hello"));
    assert_eq!(Name(0), interner.intern("hello"));

    assert_eq!(Name(1), interner.intern("world"));
    assert_eq!(Name(1), interner.intern("world"));

    assert_eq!("hello", *interner.str(Name(0)));
    assert_eq!("world", *interner.str(Name(1)));

    assert_eq!(Name(2), interner.intern("keyword"));
    assert_eq!(Name(2), interner.intern("keyword"));

    assert_eq!("keyword", *interner.str(Name(2)));
}
