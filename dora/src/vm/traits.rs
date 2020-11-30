use parking_lot::RwLock;
use std::ops::Index;
use std::sync::Arc;

use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

use crate::ty::{SourceType, SourceTypeArray};
use crate::vm::{
    accessible_from, namespace_path, FctId, FileId, NamespaceId, TypeParam, TypeParamId, VM,
};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct TraitId(u32);

impl From<u32> for TraitId {
    fn from(data: u32) -> TraitId {
        TraitId(data)
    }
}

#[derive(Debug)]
pub struct TraitData {
    pub id: TraitId,
    pub file_id: FileId,
    pub namespace_id: NamespaceId,
    pub is_pub: bool,
    pub ast: Arc<ast::Trait>,
    pub pos: Position,
    pub name: Name,
    pub type_params: Vec<TypeParam>,
    pub methods: Vec<FctId>,
}

impl TraitData {
    pub fn name(&self, vm: &VM) -> String {
        namespace_path(vm, self.namespace_id, self.name)
    }

    pub fn find_method(&self, vm: &VM, name: Name, is_static: bool) -> Option<FctId> {
        for &method in &self.methods {
            let method = vm.fcts.idx(method);
            let method = method.read();

            if method.name == name && method.is_static == is_static {
                return Some(method.id);
            }
        }

        None
    }

    pub fn find_method_with_replace(
        &self,
        vm: &VM,
        is_static: bool,
        name: Name,
        replace: Option<SourceType>,
        args: &[SourceType],
    ) -> Option<FctId> {
        for &method in &self.methods {
            let method = vm.fcts.idx(method);
            let method = method.read();

            if method.name == name
                && method.is_static == is_static
                && params_match(replace.clone(), method.params_without_self(), args)
            {
                return Some(method.id);
            }
        }

        None
    }

    pub fn type_param(&self, id: TypeParamId) -> &TypeParam {
        &self.type_params[id.to_usize()]
    }
}

struct TraitType {
    trait_id: TraitId,
    type_params: SourceTypeArray,
}

fn params_match(
    replace: Option<SourceType>,
    trait_args: &[SourceType],
    args: &[SourceType],
) -> bool {
    if trait_args.len() != args.len() {
        return false;
    }

    for (ind, ty) in trait_args.iter().enumerate() {
        let ty = ty.clone();
        let other = args[ind].clone();

        let found = if ty.is_self() {
            replace.is_none() || replace.clone().unwrap() == other
        } else {
            ty == other
        };

        if !found {
            return false;
        }
    }

    true
}

impl Index<TraitId> for Vec<RwLock<TraitData>> {
    type Output = RwLock<TraitData>;

    fn index(&self, index: TraitId) -> &RwLock<TraitData> {
        &self[index.0 as usize]
    }
}

pub fn trait_accessible_from(vm: &VM, trait_id: TraitId, namespace_id: NamespaceId) -> bool {
    let xtrait = vm.traits[trait_id].read();

    accessible_from(vm, xtrait.namespace_id, xtrait.is_pub, namespace_id)
}
