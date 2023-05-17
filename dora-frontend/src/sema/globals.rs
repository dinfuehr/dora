use std::convert::TryInto;
use std::sync::Arc;

use crate::sema::{
    module_path, AnalysisData, FctDefinitionId, ModuleDefinitionId, PackageDefinitionId, Sema,
    SourceFileId, Visibility,
};
use crate::ty::SourceType;
use crate::Id;

use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::Span;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct GlobalDefinitionId(pub u32);

impl Id for GlobalDefinition {
    type IdType = GlobalDefinitionId;

    fn id_to_usize(id: GlobalDefinitionId) -> usize {
        id.0 as usize
    }

    fn usize_to_id(value: usize) -> GlobalDefinitionId {
        GlobalDefinitionId(value.try_into().unwrap())
    }

    fn store_id(value: &mut GlobalDefinition, id: GlobalDefinitionId) {
        value.id = Some(id);
    }
}

impl GlobalDefinitionId {
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug)]
pub struct GlobalDefinition {
    pub id: Option<GlobalDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub ast: Arc<ast::Global>,
    pub span: Span,
    pub visibility: Visibility,
    pub ty: SourceType,
    pub mutable: bool,
    pub name: Name,
    pub initializer: Option<FctDefinitionId>,
    pub analysis: Option<AnalysisData>,
}

impl GlobalDefinition {
    pub fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        node: &Arc<ast::Global>,
        name: Name,
    ) -> GlobalDefinition {
        GlobalDefinition {
            id: None,
            package_id,
            module_id,
            file_id,
            ast: node.clone(),
            span: node.span,
            name,
            visibility: Visibility::from_ast(node.visibility),
            ty: SourceType::Unit,
            mutable: node.mutable,
            initializer: None,
            analysis: None,
        }
    }

    pub fn id(&self) -> GlobalDefinitionId {
        self.id.expect("id missing")
    }

    pub fn has_initializer(&self) -> bool {
        self.initializer.is_some()
    }

    pub fn name(&self, sa: &Sema) -> String {
        module_path(sa, self.module_id, self.name)
    }
}
