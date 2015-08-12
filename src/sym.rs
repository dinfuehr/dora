use std::collections::HashMap;
use std::collections::hash_map::Entry::{Vacant, Occupied};

use ast::NodeId;
use ast::Type;

use interner::Name;

pub struct SymbolTable {
    levels: Vec<Level>
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            levels: vec![Level::new()]
        }
    }

    // adds another level to the symbol table
    pub fn push_level(&mut self) {
        self.levels.push(Level::new());
    }

    // pops the top level from the symbol table
    pub fn pop_level(&mut self) {
        self.levels.pop();
    }

    // finds symbol beginning from the current/last level down
    // to the lowest level
    pub fn find(&self, name: Name) -> Option<&Sym> {
        for level in self.levels.iter().rev() {
            let found = level.find(name);
            if found.is_some() { return found; }
        }

        None
    }

    // inserts symbol into the last/current level
    pub fn insert(&mut self, name: Name, sym: Sym) -> Result<(), ()> {
        self.levels.last_mut().unwrap().insert(name, sym)
    }
}

struct Level {
    map: HashMap<Name, Sym>
}

impl Level {
    fn new() -> Level {
        Level {
            map: HashMap::new()
        }
    }

    fn find(&self, name: Name) -> Option<&Sym> {
        self.map.get(&name)
    }

    fn insert(&mut self, name: Name, sym: Sym) -> Result<(), ()> {
        match self.map.entry(name) {
            Vacant(mut entry) => {
                entry.insert(sym);

                Ok(())
            }

            Occupied(_) => Err(()),
        }
    }
}

pub enum Sym {
    SymLocalVar(SymLocalVarType),
    SymFunction(SymFunctionType),

    // only for testing purposes
    SymDummy(u8),
}

struct SymFunctionType {
    name: Name,
    return_type: Type,
    params: Vec<Param>,
    body: NodeId,
}

struct Param {
    name: Name,
    data_type: Type
}

struct SymLocalVarType {
    name: Name,
    data_type: Type,
    expr: Option<NodeId>,
}

#[test]
fn test_insert_and_find_again() {
    let mut table = SymbolTable::new();

    assert!(table.insert(Name(1), Sym::SymDummy(12)).is_ok());

    match *table.find(Name(1)).unwrap() {
        Sym::SymDummy(12) => {},
        _ => unreachable!()
    }
}
