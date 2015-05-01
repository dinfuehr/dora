use std::collections::HashMap;

pub struct SymbolTable {
    map: HashMap<String,Sym>
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable { map: HashMap::new() }
    }

    pub fn get(&self, name: &str) -> Option<&Sym> {
        self.map.get(name)
    }

    pub fn insert(&mut self, name: String, sym: Sym) {
        let old = self.map.insert(name, sym);
        assert!(old.is_none());
    }
}

#[derive(Debug)]
pub enum Sym {
    SymLocalVar,
}
