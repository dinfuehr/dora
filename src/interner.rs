use std::collections::HashMap;
use std::collections::hash_map::Entry;

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub struct Name(pub usize);

pub struct Interner {
    map: HashMap<String, Name>,
    vec: Vec<String>
}

impl Interner {
    pub fn new() -> Interner {
        Interner {
            map: HashMap::new(),
            vec: Vec::new()
        }
    }

    pub fn intern(&mut self, value: String) -> Name {
        let entry = self.map.entry(value.clone());

        match entry {
            Entry::Occupied(e) => *e.get(),
            Entry::Vacant(e) => {
                let name = Name(self.vec.len());

                self.vec.push(value);
                e.insert(name);

                name
            }
        }
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
