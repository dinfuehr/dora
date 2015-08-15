use std::borrow::Borrow;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub struct Name(pub usize);

#[derive(Clone, PartialEq, Eq, Hash)]
struct RcStr(Rc<String>);

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
    map: HashMap<RcStr, Name>,
    vec: Vec<RcStr>
}

impl Interner {
    pub fn new() -> Interner {
        Interner {
            map: HashMap::new(),
            vec: Vec::new()
        }
    }

    pub fn intern(&mut self, name: String) -> Name {
        if let Some(&val) = self.map.get(&name[..]) {
            return val;
        }

        let key = RcStr::new(name);
        let value = Name(self.vec.len());

        self.vec.push(key.clone());
        self.map.insert(key, value);

        value
    }

    pub fn str(&self, name: Name) -> &str {
        &self.vec[name.0]
    }
}

#[test]
fn interner() {
    let mut interner = Interner::new();

    assert_eq!(Name(0), interner.intern("hello".to_string()));
    assert_eq!(Name(0), interner.intern("hello".to_string()));

    assert_eq!(Name(1), interner.intern("world".to_string()));
    assert_eq!(Name(1), interner.intern("world".to_string()));

    assert_eq!("hello", interner.str(Name(0)));
    assert_eq!("world", interner.str(Name(1)));

    assert_eq!(Name(2), interner.intern("keyword".to_string()));
    assert_eq!(Name(2), interner.intern("keyword".to_string()));

    assert_eq!("keyword", interner.str(Name(2)));
}
