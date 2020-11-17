use std::cmp::max;
use std::collections::HashMap;
use std::convert::TryInto;
use std::sync::Arc;

use crate::mem;
use crate::semck::specialize::specialize_enum_id_params;
use crate::ty::SourceType;
use crate::vm::{EnumLayout, VM};

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct TupleId(u32);

impl From<u32> for TupleId {
    fn from(value: u32) -> TupleId {
        TupleId(value)
    }
}

impl TupleId {
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

pub struct ConcreteTuple {
    offsets: Vec<i32>,
    references: Vec<i32>,
    size: i32,
    align: i32,
}

pub struct Tuple {
    args: Arc<Vec<SourceType>>,
    concrete: Option<ConcreteTuple>,
}

impl Tuple {
    pub fn is_concrete_type(&self) -> bool {
        self.concrete.is_some()
    }

    pub fn is_defined_type(&self, vm: &VM) -> bool {
        for arg in self.args.as_ref() {
            if !arg.is_defined_type(vm) {
                return false;
            }
        }

        true
    }

    pub fn args(&self) -> Arc<Vec<SourceType>> {
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
    map: HashMap<Arc<Vec<SourceType>>, TupleId>,
}

impl Tuples {
    pub fn new() -> Tuples {
        Tuples {
            all: Vec::new(),
            map: HashMap::new(),
        }
    }

    pub fn get_ty(&self, id: TupleId, idx: usize) -> SourceType {
        let tuple = self.get_tuple(id);
        tuple.args[idx].clone()
    }

    pub fn get_ty_and_offset(&self, id: TupleId, idx: usize) -> (SourceType, i32) {
        let tuple = self.get_tuple(id);
        (
            tuple.args[idx].clone(),
            tuple
                .concrete
                .as_ref()
                .expect("should be concrete tuple")
                .offsets[idx],
        )
    }

    pub fn get_tuple(&self, id: TupleId) -> &Tuple {
        &self.all[id.0 as usize]
    }

    pub fn get(&self, id: TupleId) -> Arc<Vec<SourceType>> {
        self.all[id.0 as usize].args.clone()
    }
}

pub fn ensure_tuple(vm: &VM, args: Vec<SourceType>) -> TupleId {
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

fn determine_tuple_size(vm: &VM, subtypes: &[SourceType]) -> Option<ConcreteTuple> {
    let mut size = 0;
    let mut offsets = Vec::new();
    let mut references = Vec::new();
    let mut align = 0;

    for ty in subtypes {
        if !ty.is_concrete_type(vm) {
            return None;
        }

        let element_size;
        let element_align;
        let element_ty;

        if let Some(tuple_id) = ty.tuple_id() {
            let tuples = vm.tuples.lock();
            let tpl = tuples.get_tuple(tuple_id);
            let concrete = tpl.concrete.as_ref().expect("concrete tuple expected");

            element_size = concrete.size;
            element_align = concrete.align;

            let element_offset = mem::align_i32(size, element_align);
            offsets.push(element_offset);

            for &ref_offset in &concrete.references {
                offsets.push(element_offset + ref_offset);
            }

            size = element_offset + element_size;
            align = max(align, element_align);

            continue;
        } else if let SourceType::Enum(enum_id, type_params_id) = ty {
            let type_params = vm.lists.lock().get(*type_params_id);
            let edef_id = specialize_enum_id_params(vm, *enum_id, type_params);
            let edef = vm.enum_defs.idx(edef_id);

            match edef.layout {
                EnumLayout::Int => {
                    element_size = 4;
                    element_align = 4;
                    element_ty = SourceType::Int32;
                }
                EnumLayout::Ptr | EnumLayout::Tagged => {
                    element_size = mem::ptr_width();
                    element_align = mem::ptr_width();
                    element_ty = SourceType::Ptr;
                }
            }
        } else {
            element_size = ty.size(vm);
            element_align = ty.align(vm);
            element_ty = ty.clone();
        }

        let element_offset = mem::align_i32(size, element_align);
        offsets.push(element_offset);

        if element_ty.reference_type() {
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
