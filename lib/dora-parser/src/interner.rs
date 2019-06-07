use std::borrow::Borrow;

use std::collections::HashMap;
use std::fmt;
use std::ops::Deref;
use std::sync::Arc;
use parking_lot::Mutex;

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub struct Name(pub usize);

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ArcStr(Arc<String>);

impl fmt::Display for ArcStr {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", &*self.0)
    }
}

impl fmt::Debug for ArcStr {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", &*self.0)
    }
}

impl ArcStr {
    fn new(value: String) -> ArcStr {
        ArcStr(Arc::new(value))
    }
}

impl Borrow<str> for ArcStr {
    fn borrow(&self) -> &str {
        &self.0[..]
    }
}

impl Deref for ArcStr {
    type Target = String;

    fn deref<'a>(&'a self) -> &'a String {
        &self.0
    }
}

pub struct Interner {
    data: Mutex<Internal>,
}

struct Internal {
    map: HashMap<ArcStr, Name>,
    vec: Vec<ArcStr>,
}

impl Interner {
    pub fn new() -> Interner {
        Interner {
            data: Mutex::new(Internal {
                map: HashMap::new(),
                vec: Vec::new(),
            }),
        }
    }

    pub fn intern(&self, name: &str) -> Name {
        let mut data = self.data.lock();

        if let Some(&val) = data.map.get(name) {
            return val;
        }

        let key = ArcStr::new(String::from(name));
        let value = Name(data.vec.len());

        data.vec.push(key.clone());
        data.map.insert(key, value);

        value
    }

    pub fn str(&self, name: Name) -> ArcStr {
        let data = self.data.lock();
        data.vec[name.0].clone()
    }
}

#[test]
fn interner() {
    let interner = Interner::new();

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
