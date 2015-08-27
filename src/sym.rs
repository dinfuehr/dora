use std::collections::HashMap;
use std::collections::hash_map::IterMut;
use std::collections::hash_map::Entry::{Vacant, Occupied};

use self::Sym::*;

use parser::ast::NodeId;

use parser::interner::Name;

pub struct SymTable {
    levels: Vec<SymLevel>
}

impl SymTable {
    pub fn new() -> SymTable {
        SymTable {
            levels: vec![SymLevel::new()]
        }
    }

    pub fn push_level(&mut self) {
        self.levels.push(SymLevel::new());
    }

    pub fn pop_level(&mut self) {
        assert!(self.levels.len() > 1);

        self.levels.pop();
    }

    pub fn get(&self, name: Name) -> Option<&Sym> {
        for level in self.levels.iter().rev() {
            if let Some(val) = level.get(name) {
                return Some(val);
            }
        }

        None
    }

    pub fn get_type(&self, name: Name) -> Option<BuiltinType> {
        self.get(name).and_then(|n| n.to_type())
    }

    pub fn insert(&mut self, name: Name, sym: Sym) -> Option<Sym> {
        self.levels.last_mut().unwrap().insert(name, sym)
    }
}

#[derive(Debug)]
struct SymLevel {
    map: HashMap<Name, Sym>
}

impl SymLevel {
    // creates a new table
    fn new() -> SymLevel {
        SymLevel {
            map: HashMap::new()
        }
    }

    // finds symbol in table
    fn get(&self, name: Name) -> Option<&Sym> {
        self.map.get(&name)
    }

    fn insert(&mut self, name: Name, sym: Sym) -> Option<Sym> {
        self.map.insert(name, sym)
    }
}

#[derive(Debug)]
pub enum Sym {
    SymType(BuiltinType),
    SymFunction(NodeId),
    SymVar(NodeId),
}

impl Sym {
    pub fn to_type(&self) -> Option<BuiltinType> {
        match *self {
            SymType(builtin) => Some(builtin),
            _ => None
        }
    }

    pub fn to_function(&self) -> Option<NodeId> {
        match *self {
            SymFunction(id) => Some(id),
            _ => None
        }
    }

    pub fn to_var(&self) -> Option<NodeId> {
        match *self {
            SymVar(id) => Some(id),
            _ => None
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum BuiltinType {
    Int,
    Bool,
    Str,
}
