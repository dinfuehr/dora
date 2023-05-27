use std::sync::Arc;

use crate::interner::Name;
use crate::program_parser::ParsedModifiers;
use dora_parser::ast;
use dora_parser::Span;

use crate::sema::{
    module_path, ModuleDefinitionId, PackageDefinitionId, Sema, SourceFileId, Visibility,
};
use crate::ty::SourceType;
use crate::Id;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ConstDefinitionId(usize);

impl Id for ConstDefinition {
    type IdType = ConstDefinitionId;

    fn id_to_usize(id: ConstDefinitionId) -> usize {
        id.0
    }

    fn usize_to_id(value: usize) -> ConstDefinitionId {
        ConstDefinitionId(value)
    }

    fn store_id(value: &mut ConstDefinition, id: ConstDefinitionId) {
        value.id = Some(id);
    }
}

#[derive(Clone, Debug)]
pub struct ConstDefinition {
    pub id: Option<ConstDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub ast: Arc<ast::Const>,
    pub visibility: Visibility,
    pub span: Span,
    pub name: Name,
    pub ty: SourceType,
    pub expr: ast::Expr,
    pub value: ConstValue,
}

impl ConstDefinition {
    pub(crate) fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        node: &Arc<ast::Const>,
        modifiers: ParsedModifiers,
        name: Name,
    ) -> ConstDefinition {
        ConstDefinition {
            id: None,
            package_id,
            module_id,
            file_id,
            ast: node.clone(),
            span: node.span,
            name,
            visibility: modifiers.visibility(),
            ty: SourceType::Error,
            expr: node.expr.clone(),
            value: ConstValue::None,
        }
    }

    pub fn id(&self) -> ConstDefinitionId {
        self.id.expect("id missing")
    }

    pub fn name(&self, sa: &Sema) -> String {
        module_path(sa, self.module_id, self.name)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ConstValue {
    None,
    Bool(bool),
    Char(char),
    Int(i64),
    Float(f64),
}

impl ConstValue {
    pub fn to_bool(&self) -> bool {
        match self {
            &ConstValue::Bool(b) => b,
            _ => unreachable!(),
        }
    }

    pub fn to_char(&self) -> char {
        match self {
            &ConstValue::Char(c) => c,
            _ => unreachable!(),
        }
    }

    pub fn to_int(&self) -> i64 {
        match self {
            &ConstValue::Int(i) => i,
            _ => unreachable!(),
        }
    }

    pub fn to_float(&self) -> f64 {
        match self {
            &ConstValue::Float(f) => f,
            _ => unreachable!(),
        }
    }
}
