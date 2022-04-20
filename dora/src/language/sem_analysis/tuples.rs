use std::collections::HashMap;

use crate::language::sem_analysis::SemAnalysis;
use crate::language::ty::{SourceType, SourceTypeArray};

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct TupleId(u32);

impl TupleId {
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

pub struct Tuples {
    all: Vec<SourceTypeArray>,
    map: HashMap<SourceTypeArray, TupleId>,
}

impl Tuples {
    pub fn new() -> Tuples {
        Tuples {
            all: Vec::new(),
            map: HashMap::new(),
        }
    }

    fn get_subtypes_array(&self, id: TupleId) -> SourceTypeArray {
        self.all[id.0 as usize].clone()
    }
}

pub fn create_tuple(sa: &SemAnalysis, args: Vec<SourceType>) -> SourceType {
    let mut tuples = sa.tuples.lock();
    let source_type_array = SourceTypeArray::with(args);

    if let Some(&tuple_id) = tuples.map.get(&source_type_array) {
        return SourceType::Tuple(tuple_id);
    }

    tuples.all.push(source_type_array.clone());

    let id = TupleId((tuples.all.len() - 1).try_into().unwrap());
    tuples.map.insert(source_type_array, id);

    SourceType::Tuple(id)
}

pub fn get_tuple_subtypes(sa: &SemAnalysis, id: TupleId) -> SourceTypeArray {
    sa.tuples.lock().get_subtypes_array(id)
}
