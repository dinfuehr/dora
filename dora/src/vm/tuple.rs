use std::cmp::max;
use std::collections::HashMap;
use std::convert::TryInto;
use std::sync::Arc;

use crate::mem;
use crate::ty::BuiltinType;
use crate::vm::VM;

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct TupleId(u32);

pub struct Tuple {
    args: Arc<Vec<BuiltinType>>,
    offsets: Vec<i32>,
    references: Vec<i32>,
    size: i32,
    align: i32,
}

impl Tuple {
    pub fn offsets(&self) -> &[i32] {
        &self.offsets
    }

    pub fn contains_references(&self) -> bool {
        !self.references.is_empty()
    }

    pub fn references(&self) -> &[i32] {
        &self.references
    }

    pub fn size(&self) -> i32 {
        self.size
    }

    pub fn align(&self) -> i32 {
        self.size
    }
}

pub struct Tuples {
    all: Vec<Tuple>,
    map: HashMap<Arc<Vec<BuiltinType>>, TupleId>,
}

impl Tuples {
    pub fn new() -> Tuples {
        Tuples {
            all: Vec::new(),
            map: HashMap::new(),
        }
    }

    pub fn get_at(&self, id: TupleId, idx: usize) -> (BuiltinType, i32) {
        let tuple = self.get_tuple(id);
        (tuple.args[idx], tuple.offsets[idx])
    }

    pub fn get_tuple(&self, id: TupleId) -> &Tuple {
        &self.all[id.0 as usize]
    }

    pub fn get(&self, id: TupleId) -> Arc<Vec<BuiltinType>> {
        self.all[id.0 as usize].args.clone()
    }

    pub fn insert(&mut self, vm: &VM, args: Vec<BuiltinType>) -> TupleId {
        let args = Arc::new(args);

        if let Some(&tuple_id) = self.map.get(&args) {
            return tuple_id;
        }

        let (offsets, references, size, align) = determine_tuple_size(vm, &*args);

        self.all.push(Tuple {
            args: args.clone(),
            offsets,
            references,
            size,
            align,
        });

        let id = TupleId((self.all.len() - 1).try_into().unwrap());
        self.map.insert(args, id);

        id
    }
}

fn determine_tuple_size<'ast>(vm: &VM, subtypes: &[BuiltinType]) -> (Vec<i32>, Vec<i32>, i32, i32) {
    let mut size = 0;
    let mut offsets = Vec::new();
    let mut references = Vec::new();
    let mut align = 0;

    for ty in subtypes {
        if !ty.is_concrete_type(vm) {
            return (Vec::new(), Vec::new(), 0, 0);
        }

        let element_size = ty.size(vm);
        let element_align = ty.align(vm);

        let element_offset = mem::align_i32(size, element_align);
        offsets.push(element_offset);

        if ty.reference_type() {
            references.push(element_offset);
        }

        size = element_offset + element_size;
        align = max(align, element_align);
    }

    size = mem::align_i32(size, align);
    (offsets, references, size, align)
}
