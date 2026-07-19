use parking_lot::RwLock;

use std::cell::Cell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::sync::Arc;

use dora_parser::Span;

use self::SymbolKind::*;

use crate::interner::Name;
use crate::sema::{
    AliasDefinitionId, ClassDefinitionId, ConstDefinitionId, EnumDefinitionId, FctDefinitionId,
    FieldIndex, GlobalDefinitionId, ModuleDefinitionId, NestedVarId, Sema, SourceFileId,
    StructDefinitionId, TraitDefinitionId, TypeParamId, Visibility,
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

    pub fn get_unmarked(&self, name: Name) -> Option<SymbolKind> {
        for level in self.levels.iter().rev() {
            if let Some(val) = level.get_unmarked(name) {
                return Some(val);
            }
        }

        if let Some(sym) = self.outer.get_unmarked(name) {
            return Some(sym);
        }

        if let Some(sym) = self.dependencies.read().get_unmarked(name) {
            return Some(sym);
        }

        if let Some(sym) = self.prelude.get_unmarked(name) {
            return Some(sym);
        }

        None
    }

    pub fn mark_used(&self, name: Name) {
        for level in self.levels.iter().rev() {
            if level.mark_used(name) {
                return;
            }
        }

        if self.outer.mark_used(name) {
            return;
        }

        if self.dependencies.read().mark_used(name) {
            return;
        }

        self.prelude.mark_used(name);
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

    pub fn mark_trait_used(&self, trait_id: TraitDefinitionId) {
        for level in self.levels.iter().rev() {
            if level.mark_trait_used(trait_id) {
                return;
            }
        }

        if self.outer.mark_trait_used(trait_id) {
            return;
        }

        self.prelude.mark_trait_used(trait_id);
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
        self.table.get(&name).map(|sym| {
            sym.mark_used();
            sym.kind
        })
    }

    pub fn get_unmarked(&self, name: Name) -> Option<SymbolKind> {
        self.table.get(&name).map(|sym| sym.kind)
    }

    fn mark_used(&self, name: Name) -> bool {
        let Some(symbol) = self.table.get(&name) else {
            return false;
        };

        symbol.mark_used();
        true
    }

    pub fn get_sym(&self, name: Name) -> Option<&Symbol> {
        let sym = self.table.get(&name)?;
        sym.mark_used();
        Some(sym)
    }

    pub fn contains_trait(&self, trait_id: TraitDefinitionId) -> bool {
        self.traits.contains(&trait_id)
    }

    fn mark_trait_used(&self, trait_id: TraitDefinitionId) -> bool {
        if !self.contains_trait(trait_id) {
            return false;
        }

        // A trait can be imported under multiple names. Mark only the first import as used so
        // redundant aliases still produce unused-use warnings.
        let mut first_symbol: Option<&Symbol> = None;

        for symbol in self.table.values() {
            if symbol.kind != SymbolKind::Trait(trait_id) {
                continue;
            }

            match first_symbol {
                Some(first) if first.use_order() <= symbol.use_order() => {}
                _ => first_symbol = Some(symbol),
            }
        }

        let Some(symbol) = first_symbol else {
            return false;
        };

        symbol.mark_used();

        true
    }

    pub fn insert(&mut self, name: Name, kind: SymbolKind) -> Option<Symbol> {
        let symbol = Symbol {
            visibility: None,
            kind,
            use_info: None,
        };
        self.insert_symbol(name, symbol)
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
            use_info: None,
        };
        self.insert_symbol(name, symbol)
    }

    pub fn insert_source_use(
        &mut self,
        name: Name,
        visibility: Visibility,
        kind: SymbolKind,
        file_id: SourceFileId,
        span: Span,
    ) -> Option<Symbol> {
        let symbol = Symbol {
            visibility: Some(visibility),
            kind,
            use_info: Some(Rc::new(UseInfo {
                file_id,
                span,
                used: Cell::new(false),
            })),
        };
        self.insert_symbol(name, symbol)
    }

    fn insert_symbol(&mut self, name: Name, symbol: Symbol) -> Option<Symbol> {
        let new_trait_id = if symbol.visibility.is_some() {
            symbol.kind.to_trait()
        } else {
            None
        };
        let old_symbol = self.table.insert(name, symbol);

        if let Some(old_trait_id) = old_symbol
            .as_ref()
            .and_then(|symbol| symbol.kind.to_trait())
        {
            let still_imported = self.table.values().any(|symbol| {
                symbol.visibility.is_some() && symbol.kind == SymbolKind::Trait(old_trait_id)
            });

            if !still_imported {
                self.traits.remove(&old_trait_id);
            }
        }

        if let Some(new_trait_id) = new_trait_id {
            self.traits.insert(new_trait_id);
        }

        old_symbol
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
    use_info: Option<Rc<UseInfo>>,
}

impl Symbol {
    pub fn visibility(&self) -> Option<&Visibility> {
        self.visibility.as_ref()
    }

    pub fn kind(&self) -> &SymbolKind {
        &self.kind
    }

    fn mark_used(&self) {
        if let Some(use_info) = &self.use_info {
            use_info.used.set(true);
        }
    }

    fn use_order(&self) -> Option<(usize, u32)> {
        self.use_info
            .as_ref()
            .map(|use_info| (use_info.file_id.index(), use_info.span.start()))
    }

    pub fn unused_use(&self) -> Option<(SourceFileId, Span)> {
        let use_info = self.use_info.as_ref()?;

        if self
            .visibility
            .expect("missing import visibility")
            .is_public()
            || use_info.used.get()
        {
            None
        } else {
            Some((use_info.file_id, use_info.span))
        }
    }
}

#[derive(Debug)]
struct UseInfo {
    file_id: SourceFileId,
    span: Span,
    used: Cell<bool>,
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
