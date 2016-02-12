use std::collections::HashMap;
use std::string::ToString;

use self::Sym::*;

use class::ClassId;
use ctxt::*;
use interner::Name;
use ty::BuiltinType;

#[derive(Debug)]
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

    pub fn get(&self, name: Name) -> Option<Sym> {
        for level in self.levels.iter().rev() {
            if let Some(val) = level.get(name) {
                return Some(val.clone());
            }
        }

        None
    }

    pub fn get_var(&self, name: Name) -> Option<VarId> {
        self.get(name).and_then(|n| n.to_var())
    }

    pub fn get_type(&self, name: Name) -> Option<BuiltinType> {
        self.get(name).and_then(|n| n.to_type())
    }

    pub fn get_function(&self, name: Name) -> Option<FctId> {
        self.get(name).and_then(|n| n.to_function())
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

#[derive(Debug, Copy, Clone)]
pub enum SymEntry {
    Empty, Type, Function, Var
}

impl SymEntry {
    pub fn is_empty(self) -> bool {
        match self {
            SymEntry::Empty => true,
            _ => false
        }
    }

    pub fn is_type(self) -> bool {
        match self {
            SymEntry::Type => true,
            _ => false
        }
    }

    pub fn is_function(self) -> bool {
        match self {
            SymEntry::Function => true,
            _ => false
        }
    }

    pub fn is_var(self) -> bool {
        match self {
            SymEntry::Var => true,
            _ => false
        }
    }
}

#[derive(Debug, Clone)]
pub enum Sym {
    SymType(BuiltinType),
    SymFunction(FctId),
    SymVar(VarId),
}

impl Sym {
    pub fn is_type(&self) -> bool {
        match *self {
            SymType(_) => true,
            _ => false
        }
    }

    pub fn to_type(&self) -> Option<BuiltinType> {
        match *self {
            SymType(builtin) => Some(builtin),
            _ => None
        }
    }

    pub fn is_function(&self) -> bool {
        match *self {
            SymFunction(_) => true,
            _ => false
        }
    }

    pub fn to_function(&self) -> Option<FctId> {
        match *self {
            SymFunction(id) => Some(id),
            _ => None
        }
    }

    pub fn is_var(&self) -> bool {
        match *self {
            SymType(_) => true,
            _ => false
        }
    }

    pub fn to_var(&self) -> Option<VarId> {
        match *self {
            SymVar(id) => Some(id),
            _ => None
        }
    }
}
