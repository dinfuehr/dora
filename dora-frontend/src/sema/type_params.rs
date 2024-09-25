use std::cell::OnceCell;

use crate::{Name, ParsedType, SourceType};

#[derive(Clone, Debug)]
pub struct TypeParamDefinition {
    type_params: Vec<TypeParam>,
    bounds: Vec<Bound>,
    container_type_params: OnceCell<usize>,
}

impl TypeParamDefinition {
    pub fn new() -> TypeParamDefinition {
        TypeParamDefinition {
            type_params: Vec::new(),
            bounds: Vec::new(),
            container_type_params: OnceCell::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.type_params.len()
    }

    pub fn name(&self, id: TypeParamId) -> Name {
        self.type_params[id.to_usize()].name
    }

    pub fn container_type_params(&self) -> usize {
        self.container_type_params
            .get()
            .cloned()
            .expect("uninitialized field")
    }

    pub fn set_container_type_params(&self) -> usize {
        let container_type_params = self.type_params.len();
        assert!(self
            .container_type_params
            .set(container_type_params)
            .is_ok());
        container_type_params
    }

    pub fn fct_type_params_len(&self) -> usize {
        self.len() - self.container_type_params()
    }

    pub fn has_fct_type_params(&self) -> bool {
        self.len() > self.container_type_params()
    }

    pub fn add_type_param(&mut self, name: Name) -> TypeParamId {
        let id = TypeParamId(self.type_params.len());
        self.type_params.push(TypeParam { name });
        id
    }

    pub fn add_bound(&mut self, id: TypeParamId, trait_ty: Box<ParsedType>) {
        self.bounds.push(Bound {
            ty: ParsedType::new(SourceType::TypeParam(id)),
            trait_ty,
        });
    }

    pub fn add_where_bound(&mut self, ty: Box<ParsedType>, trait_ty: Box<ParsedType>) {
        self.bounds.push(Bound { ty, trait_ty });
    }

    pub fn implements_trait(&self, id: TypeParamId, trait_ty: SourceType) -> bool {
        for bound in &self.bounds {
            if bound.ty() == SourceType::TypeParam(id) && bound.trait_ty() == trait_ty {
                return true;
            }
        }
        false
    }

    pub fn bounds(&self) -> &[Bound] {
        &self.bounds
    }

    pub fn bounds_for_type_param(&self, id: TypeParamId) -> TypeParamBoundsIter {
        TypeParamBoundsIter {
            bounds: &self.bounds,
            current: 0,
            id,
        }
    }

    pub fn append(&mut self, other: &TypeParamDefinition) {
        assert_eq!(self.type_params.len(), 0);
        assert_eq!(self.bounds.len(), 0);

        self.type_params = other.type_params.clone();
        self.bounds = other.bounds.clone();
    }

    pub fn is_empty(&self) -> bool {
        self.type_params.is_empty()
    }

    pub fn names(&self) -> TypeParamNameIter {
        TypeParamNameIter {
            data: self,
            current: 0,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Bound {
    pub ty: Box<ParsedType>,
    pub trait_ty: Box<ParsedType>,
}

impl Bound {
    pub fn ty(&self) -> SourceType {
        self.ty.ty()
    }

    pub fn trait_ty(&self) -> SourceType {
        self.trait_ty.ty()
    }
}

pub struct TypeParamBoundsIter<'a> {
    bounds: &'a [Bound],
    current: usize,
    id: TypeParamId,
}

impl<'a> Iterator for TypeParamBoundsIter<'a> {
    type Item = SourceType;

    fn next(&mut self) -> Option<SourceType> {
        while self.current < self.bounds.len() {
            let bound = &self.bounds[self.current];
            if bound.ty() == SourceType::TypeParam(self.id) {
                self.current += 1;
                return Some(bound.trait_ty());
            }

            self.current += 1;
        }

        None
    }
}

pub struct TypeParamNameIter<'a> {
    data: &'a TypeParamDefinition,
    current: usize,
}

impl<'a> Iterator for TypeParamNameIter<'a> {
    type Item = (TypeParamId, Name);

    fn next(&mut self) -> Option<(TypeParamId, Name)> {
        if self.current < self.data.len() {
            let current = TypeParamId(self.current);
            self.current += 1;
            Some((current, self.data.name(current)))
        } else {
            None
        }
    }
}

#[derive(Clone, Debug)]
struct TypeParam {
    name: Name,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct TypeParamId(pub usize);

impl TypeParamId {
    pub fn to_usize(self) -> usize {
        self.0
    }
}
