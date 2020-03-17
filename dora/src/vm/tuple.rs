use std::cmp::max;
use std::collections::HashMap;
use std::convert::TryInto;
use std::sync::Arc;

use crate::mem;
use crate::ty::BuiltinType;
use crate::vm::VM;

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct TupleId(u32);

pub struct ConcreteTuple {
    offsets: Vec<i32>,
    references: Vec<i32>,
    size: i32,
    align: i32,
}

pub struct Tuple {
    args: Arc<Vec<BuiltinType>>,
    concrete: Option<ConcreteTuple>,
}

impl Tuple {
    pub fn is_concrete_type(&self) -> bool {
        self.concrete.is_some()
    }

    pub fn args(&self) -> Arc<Vec<BuiltinType>> {
        self.args.clone()
    }

    pub fn offsets(&self) -> &[i32] {
        &self.concrete.as_ref().unwrap().offsets
    }

    pub fn contains_references(&self) -> bool {
        !self.concrete.as_ref().unwrap().references.is_empty()
    }

    pub fn references(&self) -> &[i32] {
        &self.concrete.as_ref().unwrap().references
    }

    pub fn size(&self) -> i32 {
        self.concrete.as_ref().unwrap().size
    }

    pub fn align(&self) -> i32 {
        self.concrete.as_ref().unwrap().align
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
        (
            tuple.args[idx],
            tuple.concrete.as_ref().unwrap().offsets[idx],
        )
    }

    pub fn get_tuple(&self, id: TupleId) -> &Tuple {
        &self.all[id.0 as usize]
    }

    pub fn get(&self, id: TupleId) -> Arc<Vec<BuiltinType>> {
        self.all[id.0 as usize].args.clone()
    }
}

pub fn ensure_tuple(vm: &VM, args: Vec<BuiltinType>) -> TupleId {
    let args = Arc::new(args);

    if let Some(&tuple_id) = vm.tuples.lock().map.get(&args) {
        return tuple_id;
    }

    let concrete = determine_tuple_size(vm, &*args);

    let mut tuples = vm.tuples.lock();

    if let Some(&tuple_id) = tuples.map.get(&args) {
        return tuple_id;
    }

    tuples.all.push(Tuple {
        args: args.clone(),
        concrete,
    });

    let id = TupleId((tuples.all.len() - 1).try_into().unwrap());
    tuples.map.insert(args, id);

    id
}

fn determine_tuple_size<'ast>(vm: &VM, subtypes: &[BuiltinType]) -> Option<ConcreteTuple> {
    let mut size = 0;
    let mut offsets = Vec::new();
    let mut references = Vec::new();
    let mut align = 0;

    for ty in subtypes {
        if !ty.is_concrete_type(vm) {
            return None;
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
    Some(ConcreteTuple {
        offsets,
        references,
        size,
        align,
    })
}
