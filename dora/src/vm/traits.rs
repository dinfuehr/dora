use parking_lot::RwLock;
use std::ops::Index;

use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

use crate::ty::BuiltinType;
use crate::vm::{FctId, FileId, VM};

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
    pub file: FileId,
    pub pos: Position,
    pub name: Name,
    pub methods: Vec<FctId>,
}

impl TraitData {
    pub fn find_method(&self, vm: &VM, name: Name) -> Option<FctId> {
        for &method in &self.methods {
            let method = vm.fcts.idx(method);
            let method = method.read();

            if method.name == name {
                return Some(method.id);
            }
        }

        None
    }

    pub fn find_method_with_replace(
        &self,
        vm: &VM,
        name: Name,
        replace: Option<BuiltinType>,
        args: &[BuiltinType],
    ) -> Option<FctId> {
        for &method in &self.methods {
            let method = vm.fcts.idx(method);
            let method = method.read();

            if method.name == name
                && params_match(replace, method.params_without_self(), args)
            {
                return Some(method.id);
            }
        }

        None
    }
}

fn params_match(
    replace: Option<BuiltinType>,
    trait_args: &[BuiltinType],
    args: &[BuiltinType],
) -> bool {
    if trait_args.len() != args.len() {
        return false;
    }

    for (ind, &ty) in trait_args.iter().enumerate() {
        let other = args[ind];

        let found = if ty == BuiltinType::This {
            replace.is_none() || replace.unwrap() == other
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
