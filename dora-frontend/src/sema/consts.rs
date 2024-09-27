use id_arena::Id;
use std::cell::OnceCell;
use std::sync::Arc;

use crate::interner::Name;
use crate::program_parser::ParsedModifierList;
use dora_parser::ast;
use dora_parser::Span;

use crate::sema::{
    module_path, ModuleDefinitionId, PackageDefinitionId, Sema, SourceFileId, Visibility,
};
use crate::ty::SourceType;
use crate::ParsedType;

pub type ConstDefinitionId = Id<ConstDefinition>;

#[derive(Debug)]
pub struct ConstDefinition {
    pub id: Option<ConstDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub ast: Arc<ast::Const>,
    pub visibility: Visibility,
    pub span: Span,
    pub name: Name,
    pub parsed_ty: Box<ParsedType>,
    pub expr: ast::Expr,
    pub value: OnceCell<ConstValue>,
}

impl ConstDefinition {
    pub(crate) fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        node: &Arc<ast::Const>,
        modifiers: ParsedModifierList,
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
            parsed_ty: ParsedType::new_ast(node.data_type.clone()),
            expr: node.expr.clone(),
            value: OnceCell::new(),
        }
    }

    pub fn id(&self) -> ConstDefinitionId {
        self.id.expect("id missing")
    }

    pub fn name(&self, sa: &Sema) -> String {
        module_path(sa, self.module_id, self.name)
    }

    pub fn ty(&self) -> SourceType {
        self.parsed_ty().ty()
    }

    pub fn parsed_ty(&self) -> &ParsedType {
        &self.parsed_ty
    }

    pub fn value(&self) -> &ConstValue {
        self.value.get().expect("uninitialized")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ConstValue {
    None,
    Bool(bool),
    Char(char),
    Int(i64),
    Float(f64),
    String(String),
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

    pub fn to_i64(&self) -> Option<i64> {
        match self {
            &ConstValue::Int(i) => Some(i),
            _ => None,
        }
    }

    pub fn to_f64(&self) -> Option<f64> {
        match self {
            &ConstValue::Float(f) => Some(f),
            _ => None,
        }
    }

    pub fn to_string(&self) -> Option<&String> {
        match self {
            &ConstValue::String(ref v) => Some(v),
            _ => None,
        }
    }
}
