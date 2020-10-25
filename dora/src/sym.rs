use parking_lot::RwLock;

use std::collections::HashMap;
use std::sync::Arc;

use self::TypeSym::*;

use crate::sym::TermSym::{
    SymClassConstructorAndModule, SymConst, SymFct, SymGlobal, SymModule,
    SymStructConstructorAndModule, SymVar,
};
use crate::ty::TypeListId;
use crate::vm::module::ModuleId;
use crate::vm::{
    ClassId, ConstId, EnumId, FctId, FieldId, GlobalId, NamespaceId, StructId, TraitId, VarId, VM,
};
use dora_parser::interner::Name;

#[derive(Debug)]
pub struct SymTables {
    namespace: Arc<RwLock<SymTable>>,
    levels: Vec<SymTable>,
}

impl SymTables {
    pub fn new(namespace: Arc<RwLock<SymTable>>) -> SymTables {
        SymTables {
            namespace,
            levels: Vec::new(),
        }
    }

    pub fn current(vm: &VM, namespace_id: Option<NamespaceId>) -> SymTables {
        let namespace = if let Some(namespace_id) = namespace_id {
            vm.namespaces[namespace_id.to_usize()].table.clone()
        } else {
            vm.global_namespace.clone()
        };

        SymTables {
            namespace,
            levels: Vec::new(),
        }
    }

    pub fn push_level(&mut self) {
        self.levels.push(SymTable::new());
    }

    pub fn pop_level(&mut self) {
        assert!(self.levels.len() >= 1);
        self.levels.pop();
    }

    pub fn get_type(&self, name: Name) -> Option<TypeSym> {
        for level in self.levels.iter().rev() {
            if let Some(val) = level.get_type(name) {
                return Some(val.clone());
            }
        }

        self.namespace.read().get_type(name)
    }

    pub fn get_term(&self, name: Name) -> Option<TermSym> {
        for level in self.levels.iter().rev() {
            if let Some(val) = level.get_term(name) {
                return Some(val.clone());
            }
        }

        self.namespace.read().get_term(name)
    }

    pub fn get_class(&self, name: Name) -> Option<ClassId> {
        self.get_type(name).and_then(|n| n.to_class())
    }

    pub fn get_const(&self, name: Name) -> Option<ConstId> {
        self.get_term(name).and_then(|n| n.to_const())
    }

    pub fn get_fct(&self, name: Name) -> Option<FctId> {
        self.get_term(name).and_then(|n| n.to_fct())
    }

    pub fn get_struct(&self, name: Name) -> Option<StructId> {
        self.get_type(name).and_then(|n| n.to_struct())
    }

    pub fn get_trait(&self, name: Name) -> Option<TraitId> {
        self.get_type(name).and_then(|n| n.to_trait())
    }

    pub fn get_module(&self, name: Name) -> Option<ModuleId> {
        self.get_term(name).and_then(|n| n.to_module())
    }

    pub fn get_enum(&self, name: Name) -> Option<EnumId> {
        self.get_type(name).and_then(|n| n.to_enum())
    }

    pub fn get_global(&self, name: Name) -> Option<GlobalId> {
        self.get_term(name).and_then(|n| n.to_global())
    }

    pub fn get_var(&self, name: Name) -> Option<VarId> {
        self.get_term(name).and_then(|n| n.to_var())
    }

    pub fn insert_type(&mut self, name: Name, sym: TypeSym) -> Option<TypeSym> {
        self.levels.last_mut().unwrap().insert_type(name, sym)
    }

    pub fn insert_term(&mut self, name: Name, sym: TermSym) -> Option<TermSym> {
        self.levels.last_mut().unwrap().insert_term(name, sym)
    }
}

#[derive(Debug)]
pub struct SymTable {
    types: HashMap<Name, TypeSym>,
    terms: HashMap<Name, TermSym>,
}

impl SymTable {
    // creates a new table
    pub fn new() -> SymTable {
        SymTable {
            types: HashMap::new(),
            terms: HashMap::new(),
        }
    }

    pub fn contains_type(&self, name: Name) -> bool {
        self.types.contains_key(&name)
    }

    pub fn get_type(&self, name: Name) -> Option<TypeSym> {
        self.types.get(&name).cloned()
    }

    pub fn insert_type(&mut self, name: Name, sym: TypeSym) -> Option<TypeSym> {
        self.types.insert(name, sym)
    }

    pub fn contains_term(&self, name: Name) -> bool {
        self.terms.contains_key(&name)
    }

    pub fn get_term(&self, name: Name) -> Option<TermSym> {
        self.terms.get(&name).cloned()
    }

    pub fn insert_term(&mut self, name: Name, sym: TermSym) -> Option<TermSym> {
        self.terms.insert(name, sym)
    }

    pub fn get_fct(&self, name: Name) -> Option<FctId> {
        self.get_term(name).and_then(|n| n.to_fct())
    }

    pub fn get_const(&self, name: Name) -> Option<ConstId> {
        self.get_term(name).and_then(|n| n.to_const())
    }

    pub fn get_class(&self, name: Name) -> Option<ClassId> {
        self.get_type(name).and_then(|n| n.to_class())
    }

    pub fn get_trait(&self, name: Name) -> Option<TraitId> {
        self.get_type(name).and_then(|n| n.to_trait())
    }

    pub fn get_module(&self, name: Name) -> Option<ModuleId> {
        self.get_term(name).and_then(|n| n.to_module())
    }

    pub fn get_enum(&self, name: Name) -> Option<EnumId> {
        self.get_type(name).and_then(|n| n.to_enum())
    }

    pub fn get_global(&self, name: Name) -> Option<GlobalId> {
        self.get_term(name).and_then(|n| n.to_global())
    }
}

#[derive(Debug, Clone)]
pub enum TypeSym {
    SymClass(ClassId),
    SymStruct(StructId),
    SymTrait(TraitId),
    SymTypeParam(TypeListId),
    SymEnum(EnumId),
}

#[derive(Debug, Clone)]
pub enum TermSym {
    SymField(FieldId),
    SymFct(FctId),
    SymVar(VarId),
    SymModule(ModuleId),
    SymClassConstructorAndModule(ClassId, ModuleId),
    SymStructConstructorAndModule(StructId, ModuleId),
    SymGlobal(GlobalId),
    SymConst(ConstId),
    SymClassConstructor(ClassId),
    SymStructConstructor(StructId),
    SymNamespace(NamespaceId),
}

impl TypeSym {
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

    pub fn is_type_param(&self) -> bool {
        match *self {
            SymTypeParam(_) => true,
            _ => false,
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

impl TermSym {
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

    pub fn is_module(&self) -> bool {
        match *self {
            SymModule(_) => true,
            _ => false,
        }
    }

    pub fn to_module(&self) -> Option<ModuleId> {
        match *self {
            SymModule(id) => Some(id),
            SymClassConstructorAndModule(_, id) => Some(id),
            SymStructConstructorAndModule(_, id) => Some(id),
            _ => None,
        }
    }
}
