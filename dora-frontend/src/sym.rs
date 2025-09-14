use parking_lot::RwLock;

use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::sync::Arc;

use self::SymbolKind::*;

use crate::interner::Name;
use crate::sema::{
    AliasDefinitionId, ClassDefinitionId, ConstDefinitionId, EnumDefinitionId, FctDefinitionId,
    FieldIndex, GlobalDefinitionId, ModuleDefinitionId, NestedVarId, Sema, StructDefinitionId,
    TraitDefinitionId, TypeParamId, Visibility,
};

pub struct ModuleSymTable {
    module_id: ModuleDefinitionId,
    levels: Vec<SymTable>,
    outer: Rc<SymTable>,
    dependencies: Arc<RwLock<SymTable>>,
    prelude: Rc<SymTable>,
}

impl ModuleSymTable {
    pub fn new(sa: &Sema, module_id: ModuleDefinitionId) -> ModuleSymTable {
        let module = sa.module(module_id);
        let outer = module.table();
        let dependencies = sa.packages[module.package_id()].table.clone();
        let prelude = sa.module(sa.prelude_module_id()).table();

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

        if let Some(sym) = self.outer.get(name) {
            return Some(sym.clone());
        }

        if let Some(sym) = self.dependencies.read().get(name) {
            return Some(sym.clone());
        }

        if let Some(sym) = self.prelude.get(name) {
            return Some(sym.clone());
        }

        None
    }

    pub fn contains_trait(&self, trait_id: TraitDefinitionId) -> bool {
        for level in self.levels.iter().rev() {
            if level.contains_trait(trait_id) {
                return true;
            }
        }

        if self.outer.contains_trait(trait_id) {
            return true;
        }

        if self.prelude.contains_trait(trait_id) {
            return true;
        }

        false
    }

    pub fn insert(&mut self, name: Name, sym: SymbolKind) -> Option<Symbol> {
        self.levels.last_mut().unwrap().insert(name, sym)
    }
}

#[derive(Debug)]
pub struct SymTable {
    pub table: HashMap<Name, Symbol>,
    traits: HashSet<TraitDefinitionId>,
}

impl SymTable {
    // creates a new table
    pub fn new() -> SymTable {
        SymTable {
            table: HashMap::new(),
            traits: HashSet::new(),
        }
    }

    pub fn get(&self, name: Name) -> Option<SymbolKind> {
        self.table.get(&name).map(|sym| sym.kind.clone())
    }

    pub fn get_sym(&self, name: Name) -> Option<&Symbol> {
        self.table.get(&name)
    }

    pub fn contains_trait(&self, trait_id: TraitDefinitionId) -> bool {
        self.traits.contains(&trait_id)
    }

    pub fn insert(&mut self, name: Name, kind: SymbolKind) -> Option<Symbol> {
        let symbol = Symbol {
            visibility: None,
            kind,
        };
        self.table.insert(name, symbol)
    }

    pub fn insert_use(
        &mut self,
        name: Name,
        visibility: Visibility,
        kind: SymbolKind,
    ) -> Option<Symbol> {
        let symbol = Symbol {
            visibility: Some(visibility),
            kind,
        };
        if let SymbolKind::Trait(trait_id) = kind {
            self.traits.insert(trait_id);
        }
        self.table.insert(name, symbol)
    }

    pub fn dump(&self, sa: &Sema) {
        for (key, symbol) in &self.table {
            println!("{} -> {:?}", sa.interner.str(*key), symbol.kind);
        }
    }
}

#[derive(Debug, Clone)]
pub struct Symbol {
    visibility: Option<Visibility>,
    kind: SymbolKind,
}

impl Symbol {
    pub fn visibility(&self) -> Option<&Visibility> {
        self.visibility.as_ref()
    }

    pub fn kind(&self) -> &SymbolKind {
        &self.kind
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SymbolKind {
    Class(ClassDefinitionId),
    Struct(StructDefinitionId),
    Trait(TraitDefinitionId),
    TypeParam(TypeParamId),
    Enum(EnumDefinitionId),
    Field(FieldIndex),
    Fct(FctDefinitionId),
    Var(NestedVarId),
    Global(GlobalDefinitionId),
    Const(ConstDefinitionId),
    Module(ModuleDefinitionId),
    EnumVariant(EnumDefinitionId, u32),
    Alias(AliasDefinitionId),
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
