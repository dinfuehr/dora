use std::collections::HashMap;
use std::collections::hash_map::IterMut;
use std::collections::hash_map::Entry::{Vacant, Occupied};

use parser::ast::NodeId;

use parser::interner::Name;

#[derive(Debug)]
pub struct SymTable {
    map: HashMap<Name, Sym>
}

impl SymTable {
    // creates a new table
    pub fn new() -> SymTable {
        SymTable {
            map: HashMap::new()
        }
    }

    // finds symbol in table
    pub fn find(&self, name: Name) -> Option<&Sym> {
        self.map.get(&name)
    }

    // inserts symbol into the table
    pub fn insert(& mut self, name: Name, sym: Sym) -> Result<(), &Sym> {
        match self.map.entry(name) {
            Vacant(entry) => {
                entry.insert(sym);

                Ok(())
            }

            Occupied(old) => Err(old.into_mut())
        }
    }
}

#[derive(Debug)]
pub enum Sym {
    SymType(BuiltinType),
    SymFunction(NodeId),
    SymVar(NodeId),
}

#[derive(Debug)]
pub enum BuiltinType {
    Int,
    Bool,
    Str,
}
