use std::cell::OnceCell;
use std::convert::TryInto;
use std::sync::Arc;

use crate::interner::Name;
use crate::program_parser::ParsedModifierList;
use crate::sema::{
    module_path, AnalysisData, FctDefinitionId, ModuleDefinitionId, PackageDefinitionId, Sema,
    SourceFileId, Visibility,
};
use crate::ty::SourceType;
use crate::Id;
use dora_bytecode::BytecodeFunction;
use dora_parser::ast;
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
    pub ty: OnceCell<SourceType>,
    pub mutable: bool,
    pub name: Name,
    pub initializer: OnceCell<FctDefinitionId>,
    pub analysis: OnceCell<AnalysisData>,
    pub bytecode: OnceCell<BytecodeFunction>,
}

impl GlobalDefinition {
    pub(crate) fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        node: &Arc<ast::Global>,
        modifiers: ParsedModifierList,
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
            visibility: modifiers.visibility(),
            ty: OnceCell::new(),
            mutable: node.mutable,
            initializer: OnceCell::new(),
            analysis: OnceCell::new(),
            bytecode: OnceCell::new(),
        }
    }

    pub fn id(&self) -> GlobalDefinitionId {
        self.id.expect("id missing")
    }

    pub fn has_initial_value(&self) -> bool {
        self.ast.initial_value.is_some()
    }

    pub fn initial_value_expr(&self) -> &ast::Expr {
        self.ast.initial_value.as_ref().expect("missing expr")
    }

    pub fn name(&self, sa: &Sema) -> String {
        module_path(sa, self.module_id, self.name)
    }

    pub fn ty(&self) -> SourceType {
        self.ty.get().expect("missing type").clone()
    }

    pub fn analysis(&self) -> &AnalysisData {
        self.analysis.get().expect("missing analysis")
    }

    pub fn bytecode(&self) -> &BytecodeFunction {
        self.bytecode.get().expect("missing bytecode")
    }
}
