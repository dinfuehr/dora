use std::collections::HashMap;

use self::TypeSym::*;

use crate::sym::TermSym::{
    ClassConstructorAndModule, Const, Fct, Global, Module, StructConstructorAndModule, Var,
};
use crate::vm::{
    ClassId, ConstId, EnumId, FctId, FieldId, GlobalId, ModuleId, NamespaceId, StructId, TraitId,
    TypeParamId, VarId, VM,
};
use dora_parser::interner::Name;

pub struct NestedSymTable<'a> {
    vm: &'a VM,
    namespace_id: NamespaceId,
    levels: Vec<SymTable>,
}

impl<'a> NestedSymTable<'a> {
    pub fn new(vm: &'a VM, namespace_id: NamespaceId) -> NestedSymTable {
        NestedSymTable {
            vm,
            namespace_id,
            levels: Vec::new(),
        }
    }

    pub fn namespace_id(&self) -> NamespaceId {
        self.namespace_id
    }

    pub fn push_level(&mut self) {
        self.levels.push(SymTable::new());
    }

    pub fn pop_level(&mut self) {
        assert!(self.levels.len() >= 1);
        self.levels.pop();
    }

    pub fn levels(&mut self) -> usize {
        self.levels.len()
    }

    pub fn get_type(&self, name: Name) -> Option<TypeSym> {
        for level in self.levels.iter().rev() {
            if let Some(val) = level.get_type(name) {
                return Some(val.clone());
            }
        }

        {
            let namespace = &self.vm.namespaces[self.namespace_id.to_usize()];

            if let Some(sym) = namespace.table.read().get_type(name) {
                return Some(sym.clone());
            }
        }

        {
            let namespace = &self.vm.namespaces[self.vm.prelude_namespace_id.to_usize()];

            if let Some(sym) = namespace.table.read().get_type(name) {
                return Some(sym.clone());
            }
        }

        None
    }

    pub fn get_term(&self, name: Name) -> Option<TermSym> {
        for level in self.levels.iter().rev() {
            if let Some(val) = level.get_term(name) {
                return Some(val.clone());
            }
        }

        {
            let namespace = &self.vm.namespaces[self.namespace_id.to_usize()];

            if let Some(sym) = namespace.table.read().get_term(name) {
                return Some(sym.clone());
            }
        }

        {
            let namespace = &self.vm.namespaces[self.vm.prelude_namespace_id.to_usize()];

            if let Some(sym) = namespace.table.read().get_term(name) {
                return Some(sym.clone());
            }
        }

        None
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
}

#[derive(Debug, Clone)]
pub enum TypeSym {
    Class(ClassId),
    Struct(StructId),
    Trait(TraitId),
    TypeParam(TypeParamId),
    Enum(EnumId),
}

#[derive(Debug, Clone)]
pub enum TermSym {
    Field(FieldId),
    Fct(FctId),
    Var(VarId),
    Module(ModuleId),
    ClassConstructorAndModule(ClassId, ModuleId),
    StructConstructorAndModule(StructId, ModuleId),
    Global(GlobalId),
    Const(ConstId),
    ClassConstructor(ClassId),
    StructConstructor(StructId),
    Namespace(NamespaceId),
    EnumValue(EnumId, usize),
}

impl TypeSym {
    pub fn is_class(&self) -> bool {
        match *self {
            Class(_) => true,
            _ => false,
        }
    }

    pub fn to_class(&self) -> Option<ClassId> {
        match *self {
            Class(id) => Some(id),
            _ => None,
        }
    }

    pub fn is_struct(&self) -> bool {
        match *self {
            Struct(_) => true,
            _ => false,
        }
    }

    pub fn to_struct(&self) -> Option<StructId> {
        match *self {
            Struct(id) => Some(id),
            _ => None,
        }
    }

    pub fn is_trait(&self) -> bool {
        match *self {
            Trait(_) => true,
            _ => false,
        }
    }

    pub fn to_trait(&self) -> Option<TraitId> {
        match *self {
            Trait(id) => Some(id),
            _ => None,
        }
    }

    pub fn is_type_param(&self) -> bool {
        match *self {
            TypeParam(_) => true,
            _ => false,
        }
    }

    pub fn is_enum(&self) -> bool {
        match *self {
            Enum(_) => true,
            _ => false,
        }
    }

    pub fn to_enum(&self) -> Option<EnumId> {
        match *self {
            Enum(id) => Some(id),
            _ => None,
        }
    }
}

impl TermSym {
    pub fn is_fct(&self) -> bool {
        match *self {
            Fct(_) => true,
            _ => false,
        }
    }

    pub fn to_fct(&self) -> Option<FctId> {
        match *self {
            Fct(id) => Some(id),
            _ => None,
        }
    }

    pub fn is_var(&self) -> bool {
        match *self {
            Var(_) => true,
            _ => false,
        }
    }

    pub fn to_var(&self) -> Option<VarId> {
        match *self {
            Var(id) => Some(id),
            _ => None,
        }
    }

    pub fn is_const(&self) -> bool {
        match *self {
            Const(_) => true,
            _ => false,
        }
    }

    pub fn to_const(&self) -> Option<ConstId> {
        match *self {
            Const(id) => Some(id),
            _ => None,
        }
    }

    pub fn is_global(&self) -> bool {
        match *self {
            Global(_) => true,
            _ => false,
        }
    }

    pub fn to_global(&self) -> Option<GlobalId> {
        match *self {
            Global(id) => Some(id),
            _ => None,
        }
    }

    pub fn is_module(&self) -> bool {
        match *self {
            Module(_) => true,
            _ => false,
        }
    }

    pub fn to_module(&self) -> Option<ModuleId> {
        match *self {
            Module(id) => Some(id),
            ClassConstructorAndModule(_, id) => Some(id),
            StructConstructorAndModule(_, id) => Some(id),
            _ => None,
        }
    }
}
