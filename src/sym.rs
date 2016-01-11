use std::collections::HashMap;
use std::string::ToString;

use self::Sym::*;

use ctxt::*;
use interner::Name;

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

    pub fn get_var(&self, name: Name) -> Option<VarInfoId> {
        self.get(name).and_then(|n| n.to_var())
    }

    pub fn get_type(&self, name: Name) -> Option<BuiltinType> {
        self.get(name).and_then(|n| n.to_type())
    }

    pub fn get_function(&self, name: Name) -> Option<FctInfoId> {
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
    SymFunction(FctInfoId),
    SymVar(VarInfoId),
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

    pub fn to_function(&self) -> Option<FctInfoId> {
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

    pub fn to_var(&self) -> Option<VarInfoId> {
        match *self {
            SymVar(id) => Some(id),
            _ => None
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BuiltinType {
    Unit,
    Int,
    Bool,
    Str,
}

impl BuiltinType {
    pub fn size(&self) -> i32 {
        match *self {
            BuiltinType::Unit => 0,
            BuiltinType::Bool => 1,
            BuiltinType::Int => 4,
            BuiltinType::Str => 8,
        }
    }
}

impl ToString for BuiltinType {
    fn to_string(&self) -> String {
        let name = match *self {
            BuiltinType::Unit => "()",
            BuiltinType::Int => "int",
            BuiltinType::Bool => "bool",
            BuiltinType::Str => "str"
        };

        name.into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn type_size() {
        assert_eq!(0, BuiltinType::Unit.size());
        assert_eq!(1, BuiltinType::Bool.size());
        assert_eq!(4, BuiltinType::Int.size());
        assert_eq!(8, BuiltinType::Str.size());
    }
}
