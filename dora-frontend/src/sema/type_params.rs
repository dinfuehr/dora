use dora_parser::ast;
use id_arena::Id;

use crate::sema::{Element, ImplDefinition, Sema, SourceFileId, TypeRefArenaBuilder, lower_type};
use crate::{
    Name, ParsedTraitType, ParsedType, SourceType, SourceTypeArray, TraitType,
    specialize_trait_type_generic,
};

pub type TypeParamDefinitionId = Id<TypeParamDefinition>;

#[derive(Clone, Debug)]
pub struct TypeParamDefinition {
    parent: Option<TypeParamDefinitionId>,
    type_params: Vec<TypeParam>,
    bounds: Vec<Bound>,
    container_type_params: usize,
    container_bounds: usize,
}

impl TypeParamDefinition {
    pub fn new(sa: &Sema, parent: Option<TypeParamDefinitionId>) -> TypeParamDefinition {
        let container_type_params;
        let container_bounds;

        if let Some(parent) = parent {
            let parent = sa.type_param_definition(parent);
            container_type_params = parent.type_param_count();
            container_bounds = parent.bounds_count();
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

    pub fn specialize_for_default_trait_method<S>(
        &self,
        sa: &Sema,
        impl_: &ImplDefinition,
        specialize: &S,
    ) -> TypeParamDefinition
    where
        S: Fn(SourceType) -> SourceType,
    {
        let parent = impl_.type_param_definition_id();
        let parent_definition = sa.type_param_definition(parent);
        let container_type_params = parent_definition.type_param_count();
        let container_bounds = parent_definition.bounds_count();

        let mut new_bounds = Vec::with_capacity(self.bounds.len());

        for bound in &self.bounds {
            let ty = specialize(bound.ty());
            let trait_ty = if let Some(trait_ty) = bound.trait_ty() {
                Some(specialize_trait_type_generic(sa, trait_ty, specialize))
            } else {
                None
            };
            let bound = Bound {
                parsed_ty: ParsedType::new_ty(ty),
                parsed_trait_ty: ParsedTraitType::new_ty(trait_ty),
            };
            new_bounds.push(bound);
        }

        TypeParamDefinition {
            parent: Some(parent),
            type_params: self.type_params.clone(),
            bounds: new_bounds,
            container_type_params,
            container_bounds,
        }
    }

    pub fn empty() -> TypeParamDefinition {
        TypeParamDefinition {
            parent: None,
            type_params: Vec::new(),
            bounds: Vec::new(),
            container_type_params: 0,
            container_bounds: 0,
        }
    }

    pub fn name(&self, sa: &Sema, id: TypeParamId) -> Name {
        self.type_param(sa, id).name
    }

    fn type_param<'a>(&'a self, sa: &'a Sema, id: TypeParamId) -> &'a TypeParam {
        let id = id.index();

        if id < self.container_type_params() {
            let parent = self.parent.expect("parent missing");
            sa.type_param_definition(parent)
                .type_param(sa, TypeParamId(id))
        } else {
            &self.type_params[id - self.container_type_params()]
        }
    }

    fn bound<'a>(&'a self, sa: &'a Sema, idx: usize) -> &'a Bound {
        if idx < self.container_bounds {
            let parent = self.parent.expect("parent missing");
            sa.type_param_definition(parent).bound(sa, idx)
        } else {
            &self.bounds[idx - self.container_bounds]
        }
    }

    pub fn container_type_params(&self) -> usize {
        self.container_type_params
    }

    pub fn container_bounds(&self) -> usize {
        self.container_bounds
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

    pub fn add_type_param_bound(
        &mut self,
        sa: &mut Sema,
        type_ref_arena: &mut TypeRefArenaBuilder,
        file_id: SourceFileId,
        id: TypeParamId,
        ast_trait_ty: ast::AstType,
    ) {
        let type_ref_id = lower_type(sa, type_ref_arena, file_id, ast_trait_ty);
        let bound = Bound::new(
            ParsedType::new_ty(SourceType::TypeParam(id)),
            ParsedTraitType::new(type_ref_id),
        );

        self.bounds.push(bound);
    }

    pub fn add_self_bound(
        &mut self,
        sa: &mut Sema,
        type_ref_arena: &mut TypeRefArenaBuilder,
        file_id: SourceFileId,
        ast_trait_ty: ast::AstType,
    ) {
        let type_ref_id = lower_type(sa, type_ref_arena, file_id, ast_trait_ty);
        let bound = Bound::new(
            ParsedType::new_ty(SourceType::This),
            ParsedTraitType::new(type_ref_id),
        );

        self.bounds.push(bound);
    }

    pub fn add_where_bound(&mut self, ast_ty: ParsedType, ast_trait_ty: ParsedTraitType) {
        let bound = Bound::new(ast_ty, ast_trait_ty);
        self.bounds.push(bound);
    }

    pub fn implements_trait(&self, sa: &Sema, id: TypeParamId, trait_ty: TraitType) -> bool {
        for bound_trait_ty in self.bounds_for_type_param(sa, id) {
            if bound_trait_ty.implements_trait(sa, &trait_ty) {
                return true;
            }
        }

        false
    }

    pub fn bounds<'a>(&'a self, sa: &'a Sema) -> BoundsIter<'a> {
        BoundsIter {
            sa,
            data: self,
            current: 0,
            total: self.container_bounds + self.bounds.len(),
        }
    }

    pub fn own_bounds<'a>(&'a self, sa: &'a Sema) -> BoundsIter<'a> {
        BoundsIter {
            sa,
            data: self,
            current: self.container_bounds,
            total: self.container_bounds + self.bounds.len(),
        }
    }

    pub fn bounds_for_type_param<'a>(
        &'a self,
        sa: &'a Sema,
        id: TypeParamId,
    ) -> impl Iterator<Item = TraitType> + 'a {
        self.bounds(sa)
            .filter(move |b| b.ty() == SourceType::TypeParam(id) && b.trait_ty().is_some())
            .map(|b| b.trait_ty().expect("trait type expected"))
    }

    pub fn bounds_for_self<'a>(&'a self, sa: &'a Sema) -> impl Iterator<Item = TraitType> + 'a {
        self.bounds(sa)
            .filter(move |b| {
                b.parsed_ty().maybe_ty() == Some(SourceType::This) && b.trait_ty().is_some()
            })
            .map(|b| b.trait_ty().expect("trait type expected"))
    }

    pub fn type_param_count(&self) -> usize {
        self.container_type_params + self.type_params.len()
    }

    pub fn bounds_count(&self) -> usize {
        self.container_bounds + self.bounds.len()
    }

    pub fn is_empty(&self) -> bool {
        self.type_param_count() == 0
    }

    pub fn names<'a>(&'a self, sa: &'a Sema) -> TypeParamNameIter<'a> {
        TypeParamNameIter {
            sa,
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
    sa: &'a Sema,
    data: &'a TypeParamDefinition,
    current: usize,
    total: usize,
}

impl<'a> Iterator for BoundsIter<'a> {
    type Item = &'a Bound;

    fn next(&mut self) -> Option<&'a Bound> {
        if self.current < self.total {
            let bound = self.data.bound(self.sa, self.current);
            self.current += 1;
            Some(bound)
        } else {
            None
        }
    }
}

pub struct TypeParamNameIter<'a> {
    sa: &'a Sema,
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
            Some((current, self.data.name(self.sa, current)))
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

pub fn new_identity_type_params(start: usize, number_type_params: usize) -> SourceTypeArray {
    let type_params = (start..start + number_type_params)
        .into_iter()
        .map(|id| SourceType::TypeParam(TypeParamId(id)))
        .collect::<Vec<_>>();
    SourceTypeArray::with(type_params)
}
