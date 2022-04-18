use std::collections::HashMap;
use std::sync::Arc;

use crate::language::sem_analysis::SemAnalysis;
use crate::language::ty::SourceType;

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

pub struct Tuples {
    all: Vec<Arc<Vec<SourceType>>>,
    map: HashMap<Arc<Vec<SourceType>>, TupleId>,
}

impl Tuples {
    pub fn new() -> Tuples {
        Tuples {
            all: Vec::new(),
            map: HashMap::new(),
        }
    }

    fn get_subtypes(&self, id: TupleId) -> Arc<Vec<SourceType>> {
        self.all[id.0 as usize].clone()
    }
}

pub fn ensure_tuple(sa: &SemAnalysis, args: Vec<SourceType>) -> TupleId {
    let args = Arc::new(args);

    let mut tuples = sa.tuples.lock();

    if let Some(&tuple_id) = tuples.map.get(&args) {
        return tuple_id;
    }

    tuples.all.push(args.clone());

    let id = TupleId((tuples.all.len() - 1).try_into().unwrap());
    tuples.map.insert(args, id);

    id
}

pub fn get_tuple_subtypes(sa: &SemAnalysis, id: TupleId) -> Arc<Vec<SourceType>> {
    sa.tuples.lock().get_subtypes(id)
}
