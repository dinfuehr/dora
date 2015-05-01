use std::collections::HashMap;

pub struct SymbolTable {
    levels: Vec<Level>
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable { levels: vec![Level::new()] }
    }

    pub fn push_level(&mut self) {
        self.levels.push(Level::new());
    }

    pub fn pop_level(&mut self) {
        self.levels.pop();
    }

    pub fn get(&self, name: &str) -> Option<&Sym> {
        for level in self.levels.iter().rev() {
            let found = level.get(name);
            if found.is_some() { return found; }
        }

        None
    }

    pub fn insert(&mut self, name: String, sym: Sym) {
        self.levels.last_mut().unwrap().insert(name, sym);
    }
}

struct Level {
    map: HashMap<String,Sym>
}

impl Level {
    fn new() -> Level {
        Level { map: HashMap::new() }
    }

    fn get(&self, name: &str) -> Option<&Sym> {
        self.map.get(name)
    }

    fn insert(&mut self, name: String, sym: Sym) {
        let old = self.map.insert(name, sym);
        assert!(old.is_none());
    }
}

#[derive(Debug)]
pub enum Sym {
    SymLocalVar,
}
