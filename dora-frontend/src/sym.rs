use parking_lot::RwLock;

use std::collections::HashMap;
use std::sync::Arc;

use self::SymbolKind::*;

use crate::interner::Name;
use crate::sema::{
    ClassDefinitionId, ConstDefinitionId, EnumDefinitionId, FctDefinitionId, FieldId,
    GlobalDefinitionId, ModuleDefinitionId, NestedVarId, Sema, StructDefinitionId,
    TraitDefinitionId, TypeParamId,
};

pub struct ModuleSymTable {
    module_id: ModuleDefinitionId,
    levels: Vec<SymTable>,
    outer: Arc<RwLock<SymTable>>,
    dependencies: Arc<RwLock<SymTable>>,
    prelude: Arc<RwLock<SymTable>>,
}

impl ModuleSymTable {
    pub fn new(sa: &Sema, module_id: ModuleDefinitionId) -> ModuleSymTable {
        let module = sa.modules.idx(module_id);
        let module = module.read();
        let outer = module.table.clone();

        let dependencies = sa.packages[module.package_id()].table.clone();

        let prelude = sa.modules[sa.prelude_module_id()].read().table.clone();

        ModuleSymTable {
            module_id,
            levels: Vec::new(),
            outer,
            dependencies,
            prelude,
        }
    }

    pub fn module_id(&self) -> ModuleDefinitionId {
        self.module_id
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

    pub fn get_string(&self, sa: &Sema, name: &str) -> Option<SymbolKind> {
        let interned_name = sa.interner.intern(name);
        self.get(interned_name)
    }

    pub fn get(&self, name: Name) -> Option<SymbolKind> {
        for level in self.levels.iter().rev() {
            if let Some(val) = level.get(name) {
                return Some(val.clone());
            }
        }

        if let Some(sym) = self.outer.read().get(name) {
            return Some(sym.clone());
        }

        if let Some(sym) = self.dependencies.read().get(name) {
            return Some(sym.clone());
        }

        if let Some(sym) = self.prelude.read().get(name) {
            return Some(sym.clone());
        }

        None
    }

    pub fn get_class(&self, name: Name) -> Option<ClassDefinitionId> {
        self.get(name).and_then(|n| n.to_class())
    }

    pub fn get_const(&self, name: Name) -> Option<ConstDefinitionId> {
        self.get(name).and_then(|n| n.to_const())
    }

    pub fn get_fct(&self, name: Name) -> Option<FctDefinitionId> {
        self.get(name).and_then(|n| n.to_fct())
    }

    pub fn get_struct(&self, name: Name) -> Option<StructDefinitionId> {
        self.get(name).and_then(|n| n.to_struct())
    }

    pub fn get_trait(&self, name: Name) -> Option<TraitDefinitionId> {
        self.get(name).and_then(|n| n.to_trait())
    }

    pub fn get_enum(&self, name: Name) -> Option<EnumDefinitionId> {
        self.get(name).and_then(|n| n.to_enum())
    }

    pub fn get_global(&self, name: Name) -> Option<GlobalDefinitionId> {
        self.get(name).and_then(|n| n.to_global())
    }

    pub fn get_var(&self, name: Name) -> Option<NestedVarId> {
        self.get(name).and_then(|n| n.to_var())
    }

    pub fn insert(&mut self, name: Name, sym: SymbolKind) -> Option<Symbol> {
        self.levels.last_mut().unwrap().insert(name, false, sym)
    }
}

#[derive(Debug)]
pub struct SymTable {
    table: HashMap<Name, Symbol>,
}

impl SymTable {
    // creates a new table
    pub fn new() -> SymTable {
        SymTable {
            table: HashMap::new(),
        }
    }

    pub fn get(&self, name: Name) -> Option<SymbolKind> {
        self.table.get(&name).map(|sym| sym.kind.clone())
    }

    pub fn insert(&mut self, name: Name, is_exported: bool, kind: SymbolKind) -> Option<Symbol> {
        let symbol = Symbol { is_exported, kind };
        self.table.insert(name, symbol)
    }

    pub fn get_fct(&self, name: Name) -> Option<FctDefinitionId> {
        self.get(name).and_then(|n| n.to_fct())
    }

    pub fn get_const(&self, name: Name) -> Option<ConstDefinitionId> {
        self.get(name).and_then(|n| n.to_const())
    }

    pub fn get_class(&self, name: Name) -> Option<ClassDefinitionId> {
        self.get(name).and_then(|n| n.to_class())
    }

    pub fn get_struct(&self, name: Name) -> Option<StructDefinitionId> {
        self.get(name).and_then(|n| n.to_struct())
    }

    pub fn get_trait(&self, name: Name) -> Option<TraitDefinitionId> {
        self.get(name).and_then(|n| n.to_trait())
    }

    pub fn get_enum(&self, name: Name) -> Option<EnumDefinitionId> {
        self.get(name).and_then(|n| n.to_enum())
    }

    pub fn get_global(&self, name: Name) -> Option<GlobalDefinitionId> {
        self.get(name).and_then(|n| n.to_global())
    }

    pub fn dump(&self, sa: &Sema) {
        for (key, symbol) in &self.table {
            println!("{} -> {:?}", sa.interner.str(*key), symbol.kind);
        }
    }
}

#[derive(Debug, Clone)]
pub struct Symbol {
    is_exported: bool,
    kind: SymbolKind,
}

impl Symbol {
    pub fn is_exported(&self) -> bool {
        self.is_exported
    }

    pub fn kind(&self) -> &SymbolKind {
        &self.kind
    }
}

#[derive(Debug, Clone)]
pub enum SymbolKind {
    Class(ClassDefinitionId),
    Struct(StructDefinitionId),
    Trait(TraitDefinitionId),
    TypeParam(TypeParamId),
    Enum(EnumDefinitionId),
    Field(FieldId),
    Fct(FctDefinitionId),
    Var(NestedVarId),
    Global(GlobalDefinitionId),
    Const(ConstDefinitionId),
    Module(ModuleDefinitionId),
    EnumVariant(EnumDefinitionId, u32),
}

impl SymbolKind {
    pub fn is_class(&self) -> bool {
        match *self {
            Class(_) => true,
            _ => false,
        }
    }

    pub fn to_class(&self) -> Option<ClassDefinitionId> {
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

    pub fn to_struct(&self) -> Option<StructDefinitionId> {
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

    pub fn to_trait(&self) -> Option<TraitDefinitionId> {
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

    pub fn to_enum(&self) -> Option<EnumDefinitionId> {
        match *self {
            Enum(id) => Some(id),
            _ => None,
        }
    }

    pub fn is_fct(&self) -> bool {
        match *self {
            Fct(_) => true,
            _ => false,
        }
    }

    pub fn to_fct(&self) -> Option<FctDefinitionId> {
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

    pub fn to_var(&self) -> Option<NestedVarId> {
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

    pub fn to_const(&self) -> Option<ConstDefinitionId> {
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

    pub fn to_global(&self) -> Option<GlobalDefinitionId> {
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

    pub fn to_module(&self) -> Option<ModuleDefinitionId> {
        match *self {
            Module(id) => Some(id),
            _ => None,
        }
    }
}
