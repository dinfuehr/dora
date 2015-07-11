use std::collections::HashMap;
use std::collections::hash_map::Entry;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct InternStr(pub usize);

pub struct Interner {
    map: HashMap<String, InternStr>,
    vec: Vec<String>
}

impl Interner {
    pub fn new() -> Interner {
        Interner {
            map: HashMap::new(),
            vec: Vec::new()
        }
    }

    pub fn intern(&mut self, value: String) -> InternStr {
        let entry = self.map.entry(value.clone());

        match entry {
            Entry::Occupied(e) => *e.get(),
            Entry::Vacant(e) => {
                let name = InternStr(self.vec.len());

                self.vec.push(value);
                e.insert(name);

                name
            }
        }
    }

    pub fn str(&self, name: InternStr) -> &str {
        &self.vec[name.0]
    }
}

#[test]
fn interner() {
    let mut interner = Interner::new();

    assert_eq!(InternStr(0), interner.intern("hello".to_string()));
    assert_eq!(InternStr(0), interner.intern("hello".to_string()));

    assert_eq!(InternStr(1), interner.intern("world".to_string()));
    assert_eq!(InternStr(1), interner.intern("world".to_string()));

    assert_eq!("hello", interner.str(InternStr(0)));
    assert_eq!("world", interner.str(InternStr(1)));

    assert_eq!(InternStr(2), interner.intern("keyword".to_string()));
    assert_eq!(InternStr(2), interner.intern("keyword".to_string()));

    assert_eq!("keyword", interner.str(InternStr(2)));
}
