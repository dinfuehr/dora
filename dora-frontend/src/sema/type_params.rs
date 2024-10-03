use std::rc::Rc;

use dora_parser::ast;

use crate::{Name, ParsedTraitType, ParsedType, SourceType, SourceTypeArray, TraitType};

#[derive(Clone, Debug)]
pub struct TypeParamDefinition {
    parent: Option<Rc<TypeParamDefinition>>,
    type_params: Vec<TypeParam>,
    bounds: Vec<Bound>,
    container_type_params: usize,
    container_bounds: usize,
}

impl TypeParamDefinition {
    pub fn new(parent: Option<Rc<TypeParamDefinition>>) -> TypeParamDefinition {
        let container_type_params;
        let container_bounds;

        if let Some(ref parent) = parent {
            container_type_params = parent.type_params.len();
            container_bounds = parent.bounds.len();
        } else {
            container_type_params = 0;
            container_bounds = 0;
        }

        TypeParamDefinition {
            parent,
            type_params: Vec::new(),
            bounds: Vec::new(),
            container_type_params,
            container_bounds,
        }
    }

    pub fn empty() -> Rc<TypeParamDefinition> {
        Rc::new(TypeParamDefinition::new(None))
    }

    pub fn name(&self, id: TypeParamId) -> Name {
        self.type_param(id).name
    }

    fn type_param(&self, id: TypeParamId) -> &TypeParam {
        let id = id.index();

        if id < self.container_type_params() {
            let parent = self.parent.as_ref().expect("parent missing");
            &parent.type_params[id]
        } else {
            &self.type_params[id - self.container_type_params()]
        }
    }

    fn bound(&self, idx: usize) -> &Bound {
        if idx < self.container_bounds {
            let parent = self.parent.as_ref().expect("parent missing");
            &parent.bounds[idx]
        } else {
            &self.bounds[idx - self.container_bounds]
        }
    }

    pub fn container_type_params(&self) -> usize {
        self.container_type_params
    }

    pub fn own_type_params_len(&self) -> usize {
        self.type_param_count() - self.container_type_params()
    }

    pub fn has_own_type_params(&self) -> bool {
        self.type_param_count() > self.container_type_params()
    }

    pub fn add_type_param(&mut self, name: Name) -> TypeParamId {
        let id = self.container_type_params + self.type_params.len();
        self.type_params.push(TypeParam { name });
        TypeParamId(id)
    }

    pub fn add_bound(&mut self, id: TypeParamId, ast_trait_ty: ast::Type) {
        let bound = Bound::new(
            ParsedType::new_ty(SourceType::TypeParam(id)),
            ParsedTraitType::new_ast(ast_trait_ty.clone()),
        );

        self.bounds.push(bound);
    }

    pub fn add_where_bound(&mut self, ast_ty: ast::Type, ast_trait_ty: ast::Type) {
        let bound = Bound::new(
            ParsedType::new_ast(ast_ty.clone()),
            ParsedTraitType::new_ast(ast_trait_ty.clone()),
        );

        self.bounds.push(bound);
    }

    pub fn implements_trait(&self, id: TypeParamId, trait_ty: TraitType) -> bool {
        for bound in self.bounds() {
            if let Some(bound_trait_ty) = bound.trait_ty() {
                if bound.ty() == SourceType::TypeParam(id) && bound_trait_ty == trait_ty {
                    return true;
                }
            }
        }
        false
    }

    pub fn bounds(&self) -> BoundsIter {
        BoundsIter {
            data: self,
            current: 0,
            total: self.container_bounds + self.bounds.len(),
        }
    }

    pub fn own_bounds(&self) -> BoundsIter {
        BoundsIter {
            data: self,
            current: self.container_bounds,
            total: self.container_bounds + self.bounds.len(),
        }
    }

    pub fn bounds_for_type_param<'a>(
        &'a self,
        id: TypeParamId,
    ) -> impl Iterator<Item = TraitType> + 'a {
        self.bounds()
            .filter(move |b| b.ty() == SourceType::TypeParam(id) && b.trait_ty().is_some())
            .map(|b| b.trait_ty().expect("trait type expected"))
    }

    pub fn type_param_count(&self) -> usize {
        self.container_type_params + self.type_params.len()
    }

    pub fn is_empty(&self) -> bool {
        self.type_param_count() == 0
    }

    pub fn names(&self) -> TypeParamNameIter {
        TypeParamNameIter {
            data: self,
            current: 0,
            total: self.type_param_count(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Bound {
    parsed_ty: ParsedType,
    parsed_trait_ty: ParsedTraitType,
}

impl Bound {
    pub fn new(parsed_ty: ParsedType, parsed_trait_ty: ParsedTraitType) -> Bound {
        Bound {
            parsed_ty,
            parsed_trait_ty,
        }
    }

    pub fn ty(&self) -> SourceType {
        self.parsed_ty().ty()
    }

    pub fn parsed_ty(&self) -> &ParsedType {
        &self.parsed_ty
    }

    pub fn trait_ty(&self) -> Option<TraitType> {
        self.parsed_trait_ty().ty()
    }

    pub fn parsed_trait_ty(&self) -> &ParsedTraitType {
        &self.parsed_trait_ty
    }
}

pub struct BoundsIter<'a> {
    data: &'a TypeParamDefinition,
    current: usize,
    total: usize,
}

impl<'a> Iterator for BoundsIter<'a> {
    type Item = &'a Bound;

    fn next(&mut self) -> Option<&'a Bound> {
        if self.current < self.total {
            let bound = self.data.bound(self.current);
            self.current += 1;
            Some(bound)
        } else {
            None
        }
    }
}

pub struct TypeParamNameIter<'a> {
    data: &'a TypeParamDefinition,
    current: usize,
    total: usize,
}

impl<'a> Iterator for TypeParamNameIter<'a> {
    type Item = (TypeParamId, Name);

    fn next(&mut self) -> Option<(TypeParamId, Name)> {
        if self.current < self.total {
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
    pub fn index(self) -> usize {
        self.0
    }
}

pub fn new_identity_type_params(number_type_params: usize) -> SourceTypeArray {
    let type_params = (0..number_type_params)
        .into_iter()
        .map(|id| SourceType::TypeParam(TypeParamId(id)))
        .collect::<Vec<_>>();
    SourceTypeArray::with(type_params)
}
