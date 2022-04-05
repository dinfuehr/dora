use parking_lot::RwLock;
use std::sync::Arc;

use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

use crate::language::sem_analysis::{namespace_path, NamespaceId};
use crate::language::ty::SourceType;
use crate::utils::GrowableVec;
use crate::vm::{FileId, VM};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ConstDefinitionId(usize);

impl From<usize> for ConstDefinitionId {
    fn from(data: usize) -> ConstDefinitionId {
        ConstDefinitionId(data)
    }
}

impl GrowableVec<RwLock<ConstDefinition>> {
    pub fn idx(&self, index: ConstDefinitionId) -> Arc<RwLock<ConstDefinition>> {
        self.idx_usize(index.0 as usize)
    }
}

#[derive(Clone, Debug)]
pub struct ConstDefinition {
    pub id: ConstDefinitionId,
    pub file_id: FileId,
    pub ast: Arc<ast::Const>,
    pub namespace_id: NamespaceId,
    pub is_pub: bool,
    pub pos: Position,
    pub name: Name,
    pub ty: SourceType,
    pub expr: Box<ast::Expr>,
    pub value: ConstValue,
}

impl ConstDefinition {
    pub fn name(&self, vm: &VM) -> String {
        namespace_path(vm, self.namespace_id, self.name)
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
