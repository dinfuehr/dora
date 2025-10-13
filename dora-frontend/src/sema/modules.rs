use std::cell::OnceCell;
use std::rc::Rc;

use crate::SourceFileId;
use crate::SourceType;
use crate::element_collector::Annotations;
use crate::sema::{Element, ElementId, PackageDefinitionId, Sema, TypeParamDefinition, Visibility};
use crate::sym::SymTable;

use crate::interner::Name;
use dora_parser::{Span, ast};
use id_arena::Id;

pub type ModuleDefinitionId = Id<ModuleDefinition>;

#[derive(Debug)]
pub struct ModuleDefinition {
    pub id: Option<ModuleDefinitionId>,
    pub package_id: Option<PackageDefinitionId>,
    pub parent_module_id: Option<ModuleDefinitionId>,
    pub file_id: Option<SourceFileId>,
    pub span: Option<Span>,
    pub ast_id: Option<ast::AstId>,
    pub name: Option<Name>,
    pub table: OnceCell<Rc<SymTable>>,
    pub children: OnceCell<Vec<ElementId>>,
    pub visibility: Visibility,
    pub parents: Vec<ModuleDefinitionId>,
    pub depth: usize,
}

impl ModuleDefinition {
    pub(crate) fn new_top_level(name: Option<Name>) -> ModuleDefinition {
        ModuleDefinition {
            id: None,
            package_id: None,
            ast_id: None,
            file_id: None,
            span: None,
            parent_module_id: None,
            name,
            table: OnceCell::new(),
            children: OnceCell::new(),
            visibility: Visibility::Public,
            parents: Vec::new(),
            depth: 0,
        }
    }

    pub(crate) fn new_inner(
        sa: &mut Sema,
        package_id: PackageDefinitionId,
        parent_id: ModuleDefinitionId,
        file_id: SourceFileId,
        span: Span,
        ast_id: ast::AstId,
        modifiers: Annotations,
        name: Name,
    ) -> ModuleDefinition {
        let parent = sa.module(parent_id);
        let mut parents = parent.parents.clone();
        parents.push(parent_id);

        let depth = parents.len();

        ModuleDefinition {
            id: None,
            package_id: Some(package_id),
            ast_id: Some(ast_id),
            file_id: Some(file_id),
            span: Some(span),
            parent_module_id: Some(parent_id),
            name: Some(name),
            table: OnceCell::new(),
            children: OnceCell::new(),
            visibility: modifiers.visibility(),
            parents,
            depth,
        }
    }

    pub fn id(&self) -> ModuleDefinitionId {
        self.id.expect("missing id")
    }

    pub fn ast<'a>(&self, sa: &'a Sema) -> &'a ast::Module {
        sa.file(self.file_id.expect("file expected"))
            .node(self.ast_id.expect("missing ast"))
            .to_module()
            .expect("module expected")
    }

    pub fn package_id(&self) -> PackageDefinitionId {
        self.package_id.expect("uninitialized package_id")
    }

    pub fn table(&self) -> Rc<SymTable> {
        self.table.get().cloned().expect("missing table")
    }

    pub fn name(&self, sa: &Sema) -> String {
        let mut path = String::new();

        for &module_id in &self.parents {
            let module = sa.module(module_id);

            if let Some(name) = module.name {
                if !path.is_empty() {
                    path.push_str("::");
                }

                path.push_str(&sa.interner.str(name));
            }
        }

        if let Some(name) = self.name {
            if !path.is_empty() {
                path.push_str("::");
            }

            path.push_str(&sa.interner.str(name));
        }

        path
    }
}

pub fn module_package(sa: &Sema, module_id: ModuleDefinitionId) -> ModuleDefinitionId {
    let module = sa.module(module_id);

    if let Some(&global_id) = module.parents.first() {
        global_id
    } else {
        module_id
    }
}

pub fn module_path(sa: &Sema, module_id: ModuleDefinitionId, name: Name) -> String {
    let module = sa.module(module_id);
    let mut result = module.name(sa);

    if !result.is_empty() {
        result.push_str("::");
    }

    result.push_str(&sa.interner.str(name));
    result
}

impl Element for ModuleDefinition {
    fn element_id(&self) -> ElementId {
        ElementId::Module(self.id())
    }

    fn file_id(&self) -> SourceFileId {
        self.file_id.expect("missing file_id")
    }

    fn span(&self) -> Span {
        self.span.expect("missing span")
    }

    fn module_id(&self) -> ModuleDefinitionId {
        unreachable!()
    }

    fn package_id(&self) -> PackageDefinitionId {
        self.package_id.expect("missing package_id")
    }

    fn type_param_definition(&self) -> &Rc<TypeParamDefinition> {
        unreachable!()
    }

    fn self_ty(&self, _sa: &Sema) -> Option<SourceType> {
        unreachable!()
    }

    fn visibility(&self) -> Visibility {
        self.visibility
    }

    fn children(&self) -> &[ElementId] {
        self.children.get().expect("missing children")
    }
}
