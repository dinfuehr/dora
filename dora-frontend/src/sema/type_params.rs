use dora_parser::ast;
use id_arena::Id;

use crate::sema::{Element, ImplDefinition, Sema, SourceFileId, TypeRefArenaBuilder, lower_type};
use crate::{
    Name, ParsedTraitType, ParsedType, SourceType, SourceTypeArray, TraitType,
    specialize_trait_type_generic,
};

pub type TypeParamDefinitionId = Id<TypeParamDefinition>;
pub type TypeParamId = Id<TypeParam>;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TypeParamKind {
    Container(TypeParamIdx),
    Own(TypeParamIdx),
}

#[derive(Clone, Debug)]
pub struct TypeParamDefinition {
    parent: Option<TypeParamDefinitionId>,
    type_params: Vec<TypeParamId>,
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

    pub fn name(&self, sa: &Sema, id: TypeParamIdx) -> Name {
        self.type_param(sa, id).name()
    }

    pub fn type_param_id(&self, sa: &Sema, id: TypeParamIdx) -> TypeParamId {
        let id = id.index();

        if id < self.container_type_params() {
            let parent = self.parent.expect("parent missing");
            sa.type_param_definition(parent)
                .type_param_id(sa, TypeParamIdx(id))
        } else {
            self.type_params[id - self.container_type_params()]
        }
    }

    pub fn type_param_idx(&self, sa: &Sema, id: TypeParamId) -> Option<TypeParamIdx> {
        self.classify_type_param(sa, id).map(|kind| match kind {
            TypeParamKind::Container(idx) | TypeParamKind::Own(idx) => idx,
        })
    }

    pub fn classify_type_param(&self, sa: &Sema, id: TypeParamId) -> Option<TypeParamKind> {
        if let Some(parent) = self.parent {
            if let Some(idx) = sa.type_param_definition(parent).type_param_idx(sa, id) {
                return Some(TypeParamKind::Container(idx));
            }
        }

        self.type_params
            .iter()
            .position(|&type_param_id| type_param_id == id)
            .map(|idx| TypeParamKind::Own(TypeParamIdx(self.container_type_params + idx)))
    }

    fn type_param<'a>(&'a self, sa: &'a Sema, id: TypeParamIdx) -> &'a TypeParam {
        sa.type_param(self.type_param_id(sa, id))
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

    pub fn add_type_param(&mut self, sa: &mut Sema, name: Name) -> TypeParamId {
        let id = sa.type_params.alloc(TypeParam { name });
        self.type_params.push(id);
        id
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

    pub fn identity_type_params(&self, sa: &Sema) -> SourceTypeArray {
        self.container_identity_type_params(sa)
            .connect(&self.own_identity_type_params())
    }

    pub fn container_identity_type_params(&self, sa: &Sema) -> SourceTypeArray {
        self.parent
            .map(|parent| sa.type_param_definition(parent).identity_type_params(sa))
            .unwrap_or_else(SourceTypeArray::empty)
    }

    pub fn own_identity_type_params(&self) -> SourceTypeArray {
        source_types_for_type_params(self.type_params.iter().copied())
    }
}

fn source_types_for_type_params(
    type_param_ids: impl Iterator<Item = TypeParamId>,
) -> SourceTypeArray {
    SourceTypeArray::with(type_param_ids.map(SourceType::TypeParam).collect())
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
            let current = self.data.type_param_id(self.sa, TypeParamIdx(self.current));
            self.current += 1;
            Some((current, self.sa.type_param(current).name()))
        } else {
            None
        }
    }
}

#[derive(Clone, Debug)]
pub struct TypeParam {
    name: Name,
}

impl TypeParam {
    pub fn name(&self) -> Name {
        self.name
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct TypeParamIdx(pub usize);

impl TypeParamIdx {
    pub fn index(self) -> usize {
        self.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::sema::FctParent;
    use crate::tests::ok;

    #[test]
    fn nested_method_reuses_container_type_param_ids() {
        let sa = ok("class A[X]\nimpl[T] A[T] { fn test[U]() {} }");
        let (_, method) = sa
            .fcts
            .iter()
            .find(|(_, fct)| sa.interner.str(fct.name).as_str() == "test")
            .expect("method not found");
        let FctParent::Extension(extension_id) = method.parent else {
            panic!("extension method expected");
        };
        let extension = sa.extension(extension_id);
        let impl_params = sa.type_param_definition(extension.type_param_definition_id);
        let method_params = sa.type_param_definition(method.type_param_definition_id);
        let container_id = impl_params.type_param_id(&sa, TypeParamIdx(0));
        let method_id = method_params.type_param_id(&sa, TypeParamIdx(1));

        assert_eq!(
            method_params
                .names(&sa)
                .map(|(id, _)| id)
                .collect::<Vec<_>>(),
            vec![container_id, method_id]
        );
        assert_eq!(
            method_params.type_param_id(&sa, TypeParamIdx(0)),
            container_id
        );
        assert_eq!(
            method_params.type_param_idx(&sa, container_id),
            Some(TypeParamIdx(0))
        );
        assert_eq!(
            method_params.type_param_idx(&sa, method_id),
            Some(TypeParamIdx(1))
        );
        assert_eq!(
            method_params.classify_type_param(&sa, container_id),
            Some(TypeParamKind::Container(TypeParamIdx(0)))
        );
        assert_eq!(
            method_params.classify_type_param(&sa, method_id),
            Some(TypeParamKind::Own(TypeParamIdx(1)))
        );
    }
}
