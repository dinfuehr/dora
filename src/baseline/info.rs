use std::collections::HashMap;

use crate::ctxt::{
     CallSite, Intrinsic, NodeMap, Store,
 VarId,
};
use crate::mem;
use crate::ty::BuiltinType;
use dora_parser::ast::*;

pub struct JitInfo<'ast> {
    pub tempsize: i32,                // size of temporary variables on stack
    pub localsize: i32,               // size of local variables on stack
    pub argsize: i32,                 // size of arguments on stack (need to be on bottom)
    pub leaf: bool,                   // false if fct calls other functions
    pub eh_return_value: Option<i32>, // stack slot for return value storage

    pub map_stores: NodeMap<Store>,
    pub map_csites: NodeMap<CallSite<'ast>>,
    pub map_offsets: NodeMap<i32>,
    pub map_var_offsets: HashMap<VarId, i32>,
    pub map_var_types: HashMap<VarId, BuiltinType>,
    pub map_intrinsics: NodeMap<Intrinsic>,
    pub map_fors: NodeMap<ForInfo<'ast>>,
}

impl<'ast> JitInfo<'ast> {
    pub fn get_store(&self, id: NodeId) -> Store {
        match self.map_stores.get(id) {
            Some(store) => *store,
            None => Store::Reg,
        }
    }

    pub fn stacksize(&self) -> i32 {
        mem::align_i32(self.tempsize + self.localsize + self.argsize, 16)
    }

    pub fn offset(&self, var_id: VarId) -> i32 {
        *self
            .map_var_offsets
            .get(&var_id)
            .expect("no offset found for var")
    }

    pub fn ty(&self, var_id: VarId) -> BuiltinType {
        *self
            .map_var_types
            .get(&var_id)
            .expect("no type found for var")
    }

    pub fn new() -> JitInfo<'ast> {
        JitInfo {
            tempsize: 0,
            localsize: 0,
            argsize: 0,
            leaf: false,
            eh_return_value: None,

            map_stores: NodeMap::new(),
            map_csites: NodeMap::new(),
            map_offsets: NodeMap::new(),
            map_var_offsets: HashMap::new(),
            map_var_types: HashMap::new(),
            map_intrinsics: NodeMap::new(),
            map_fors: NodeMap::new(),
        }
    }
}


#[derive(Clone)]
pub struct ForInfo<'ast> {
    pub make_iterator: CallSite<'ast>,
    pub has_next: CallSite<'ast>,
    pub next: CallSite<'ast>,
}