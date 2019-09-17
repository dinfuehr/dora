use std::ops::Index;
use std::sync::Arc;

use crate::ty::BuiltinType;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum TypeParams {
    Empty,
    List(Arc<Vec<BuiltinType>>),
}

impl TypeParams {
    pub fn empty() -> TypeParams {
        TypeParams::Empty
    }

    pub fn with(type_params: Vec<BuiltinType>) -> TypeParams {
        if type_params.len() == 0 {
            TypeParams::Empty
        } else {
            TypeParams::List(Arc::new(type_params))
        }
    }

    pub fn len(&self) -> usize {
        match self {
            &TypeParams::Empty => 0,
            &TypeParams::List(ref params) => params.len(),
        }
    }

    pub fn iter(&self) -> TypeParamsIter {
        TypeParamsIter {
            params: self,
            idx: 0,
        }
    }
}

impl Index<usize> for TypeParams {
    type Output = BuiltinType;

    fn index(&self, idx: usize) -> &BuiltinType {
        match self {
            &TypeParams::Empty => panic!("out-of-bounds"),
            &TypeParams::List(ref params) => &params[idx],
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct TypeParamId(usize);

impl TypeParamId {
    pub fn idx(self) -> usize {
        self.0
    }
}

impl From<usize> for TypeParamId {
    fn from(data: usize) -> TypeParamId {
        TypeParamId(data)
    }
}

pub struct TypeParamsIter<'a> {
    params: &'a TypeParams,
    idx: usize,
}

impl<'a> Iterator for TypeParamsIter<'a> {
    type Item = BuiltinType;

    fn next(&mut self) -> Option<BuiltinType> {
        match self.params {
            &TypeParams::Empty => None,

            &TypeParams::List(ref params) => {
                if self.idx < params.len() {
                    let ret = params[self.idx];
                    self.idx += 1;

                    Some(ret)
                } else {
                    None
                }
            }
        }
    }
}

impl From<Vec<BuiltinType>> for TypeParams {
    fn from(val: Vec<BuiltinType>) -> TypeParams {
        TypeParams::with(val)
    }
}
