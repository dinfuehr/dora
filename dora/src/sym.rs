use std::collections::HashMap;

use self::Sym::*;

use crate::ty::TypeListId;
use crate::vm::module::ModuleId;
use crate::vm::{ClassId, ConstId, EnumId, FctId, FieldId, GlobalId, StructId, TraitId, VarId};
use dora_parser::interner::Name;

#[derive(Debug)]
pub struct SymTable {
    levels: Vec<SymLevel>,
}

impl SymTable {
    pub fn new() -> SymTable {
        SymTable {
            levels: vec![SymLevel::new()],
        }
    }

    pub fn push_level(&mut self) {
        self.levels.push(SymLevel::new());
    }

    pub fn pop_level(&mut self) {
        assert!(self.levels.len() > 1);

        self.levels.pop();
    }

    pub fn levels(&self) -> usize {
        self.levels.len()
    }

    pub fn get(&self, name: Name) -> Option<Sym> {
        for level in self.levels.iter().rev() {
            if let Some(val) = level.get(name) {
                return Some(val.clone());
            }
        }

        None
    }

    pub fn get_class(&self, name: Name) -> Option<ClassId> {
        self.get(name).and_then(|n| n.to_class())
    }

    pub fn get_const(&self, name: Name) -> Option<ConstId> {
        self.get(name).and_then(|n| n.to_const())
    }

    pub fn get_fct(&self, name: Name) -> Option<FctId> {
        self.get(name).and_then(|n| n.to_fct())
    }

    pub fn get_struct(&self, name: Name) -> Option<StructId> {
        self.get(name).and_then(|n| n.to_struct())
    }

    pub fn get_trait(&self, name: Name) -> Option<TraitId> {
        self.get(name).and_then(|n| n.to_trait())
    }

    pub fn get_module(&self, name: Name) -> Option<ModuleId> {
        self.get(name).and_then(|n| n.to_module())
    }

    pub fn get_enum(&self, name: Name) -> Option<EnumId> {
        self.get(name).and_then(|n| n.to_enum())
    }

    pub fn get_global(&self, name: Name) -> Option<GlobalId> {
        self.get(name).and_then(|n| n.to_global())
    }

    pub fn get_var(&self, name: Name) -> Option<VarId> {
        self.get(name).and_then(|n| n.to_var())
    }

    pub fn insert(&mut self, name: Name, sym: Sym) -> Option<Sym> {
        self.levels.last_mut().unwrap().insert(name, sym)
    }
}

#[derive(Debug)]
pub struct SymLevel {
    map: HashMap<Name, Sym>,
}

impl SymLevel {
    // creates a new table
    pub fn new() -> SymLevel {
        SymLevel {
            map: HashMap::new(),
        }
    }

    pub fn contains(&self, name: Name) -> bool {
        self.map.contains_key(&name)
    }

    // finds symbol in table
    pub fn get(&self, name: Name) -> Option<&Sym> {
        self.map.get(&name)
    }

    pub fn insert(&mut self, name: Name, sym: Sym) -> Option<Sym> {
        self.map.insert(name, sym)
    }
}

#[derive(Debug, Clone)]
pub enum Sym {
    SymField(FieldId),
    SymFct(FctId),
    SymVar(VarId),
    SymClass(ClassId),
    SymStruct(StructId),
    SymTrait(TraitId),
    SymModule(ModuleId),
    SymGlobal(GlobalId),
    SymClassTypeParam(ClassId, TypeListId),
    SymFctTypeParam(FctId, TypeListId),
    SymConst(ConstId),
    SymEnum(EnumId),
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

    pub fn is_module(&self) -> bool {
        match *self {
            SymModule(_) => true,
            _ => false,
        }
    }

    pub fn to_module(&self) -> Option<ModuleId> {
        match *self {
            SymModule(id) => Some(id),
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
            SymClassTypeParam(_, _) => true,
            SymFctTypeParam(_, _) => true,
            _ => false,
        }
    }

    pub fn is_const(&self) -> bool {
        match *self {
            SymConst(_) => true,
            _ => false,
        }
    }

    pub fn to_const(&self) -> Option<ConstId> {
        match *self {
            SymConst(id) => Some(id),
            _ => None,
        }
    }

    pub fn is_enum(&self) -> bool {
        match *self {
            SymEnum(_) => true,
            _ => false,
        }
    }

    pub fn to_enum(&self) -> Option<EnumId> {
        match *self {
            SymEnum(id) => Some(id),
            _ => None,
        }
    }
}
