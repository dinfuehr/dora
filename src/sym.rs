use std::collections::HashMap;

use self::Sym::*;

use class::{ClassId, TypeParamId};
use ctxt::*;
use dora_parser::interner::Name;

#[derive(Debug)]
pub struct SymTable {
    levels: Vec<SymLevel>,
}

impl SymTable {
    pub fn new() -> SymTable {
        SymTable { levels: vec![SymLevel::new()] }
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

    pub fn get_class(&self, name: Name) -> Option<ClassId> {
        self.get(name).and_then(|n| n.to_class())
    }

    pub fn get_fct(&self, name: Name) -> Option<FctId> {
        self.get(name).and_then(|n| n.to_fct())
    }

    pub fn get_struct(&self, name: Name) -> Option<StructId> {
        self.get(name).and_then(|n| n.to_struct())
    }

    pub fn insert(&mut self, name: Name, sym: Sym) -> Option<Sym> {
        self.levels.last_mut().unwrap().insert(name, sym)
    }
}

#[derive(Debug)]
struct SymLevel {
    map: HashMap<Name, Sym>,
}

impl SymLevel {
    // creates a new table
    fn new() -> SymLevel {
        SymLevel { map: HashMap::new() }
    }

    // finds symbol in table
    fn get(&self, name: Name) -> Option<&Sym> {
        self.map.get(&name)
    }

    fn insert(&mut self, name: Name, sym: Sym) -> Option<Sym> {
        self.map.insert(name, sym)
    }
}

#[derive(Debug, Clone)]
pub enum Sym {
    SymFct(FctId),
    SymVar(VarId),
    SymClass(ClassId),
    SymStruct(StructId),
    SymTrait(TraitId),
    SymGlobal(GlobalId),
    SymTypeParam(TypeParamId),
}

impl Sym {
    pub fn is_fct(&self) -> bool {
        match *self {
            SymFct(_) => true,
            _ => false,
        }
    }

    pub fn to_fct(&self) -> Option<FctId> {
        match *self {
            SymFct(id) => Some(id),
            _ => None,
        }
    }

    pub fn is_var(&self) -> bool {
        match *self {
            SymVar(_) => true,
            _ => false,
        }
    }

    pub fn to_var(&self) -> Option<VarId> {
        match *self {
            SymVar(id) => Some(id),
            _ => None,
        }
    }

    pub fn is_class(&self) -> bool {
        match *self {
            SymClass(_) => true,
            _ => false,
        }
    }

    pub fn to_class(&self) -> Option<ClassId> {
        match *self {
            SymClass(id) => Some(id),
            _ => None,
        }
    }

    pub fn is_struct(&self) -> bool {
        match *self {
            SymStruct(_) => true,
            _ => false,
        }
    }

    pub fn to_struct(&self) -> Option<StructId> {
        match *self {
            SymStruct(id) => Some(id),
            _ => None,
        }
    }

    pub fn is_trait(&self) -> bool {
        match *self {
            SymTrait(_) => true,
            _ => false,
        }
    }

    pub fn to_trait(&self) -> Option<TraitId> {
        match *self {
            SymTrait(id) => Some(id),
            _ => None,
        }
    }

    pub fn is_global(&self) -> bool {
        match *self {
            SymGlobal(_) => true,
            _ => false,
        }
    }

    pub fn to_global(&self) -> Option<GlobalId> {
        match *self {
            SymGlobal(id) => Some(id),
            _ => None,
        }
    }

    pub fn is_type_param(&self) -> bool {
        match *self {
            SymTypeParam(_) => true,
            _ => false,
        }
    }

    pub fn to_type_param(&self) -> Option<TypeParamId> {
        match *self {
            SymTypeParam(id) => Some(id),
            _ => None,
        }
    }
}
