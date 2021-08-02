use crate::ty::SourceType;
use crate::utils::GrowableVec;
use crate::vm::{FileId, NamespaceId, TypeParam, VM};
use dora_parser::ast::{AnnotationParam, AnnotationUsages, InternalAnnotation};
use dora_parser::interner::Name;
use dora_parser::Position;
use parking_lot::RwLock;
use std::sync::Arc;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct AnnotationId(usize);

impl AnnotationId {
    pub fn max() -> AnnotationId {
        AnnotationId(usize::max_value())
    }
}

impl From<AnnotationId> for usize {
    fn from(data: AnnotationId) -> usize {
        data.0
    }
}

impl From<usize> for AnnotationId {
    fn from(data: usize) -> AnnotationId {
        AnnotationId(data)
    }
}

impl GrowableVec<RwLock<Annotation>> {
    pub fn idx(&self, index: AnnotationId) -> Arc<RwLock<Annotation>> {
        self.idx_usize(index.0)
    }
}

impl Annotation {
    pub fn is_abstract(annotation_usages: &AnnotationUsages, vm: &VM) -> bool {
        let name = vm
            .annotations
            .idx(vm.known.annotations.abstract_)
            .read()
            .name;
        annotation_usages.contains(name)
    }
    pub fn is_final(annotation_usages: &AnnotationUsages, vm: &VM) -> bool {
        let name = vm.annotations.idx(vm.known.annotations.final_).read().name;
        annotation_usages.contains(name)
    }
    pub fn is_internal(annotation_usages: &AnnotationUsages, vm: &VM) -> bool {
        let name = vm
            .annotations
            .idx(vm.known.annotations.internal)
            .read()
            .name;
        annotation_usages.contains(name)
    }
    pub fn is_open(annotation_usages: &AnnotationUsages, vm: &VM) -> bool {
        let name = vm.annotations.idx(vm.known.annotations.open).read().name;
        annotation_usages.contains(name)
    }
    pub fn is_override(annotation_usages: &AnnotationUsages, vm: &VM) -> bool {
        let name = vm
            .annotations
            .idx(vm.known.annotations.override_)
            .read()
            .name;
        annotation_usages.contains(name)
    }
    pub fn is_pub(annotation_usages: &AnnotationUsages, vm: &VM) -> bool {
        let name = vm.annotations.idx(vm.known.annotations.pub_).read().name;
        annotation_usages.contains(name)
    }
    pub fn is_static(annotation_usages: &AnnotationUsages, vm: &VM) -> bool {
        let name = vm.annotations.idx(vm.known.annotations.static_).read().name;
        annotation_usages.contains(name)
    }

    pub fn is_test(annotation_usages: &AnnotationUsages, vm: &VM) -> bool {
        let name = vm.annotations.idx(vm.known.annotations.test).read().name;
        annotation_usages.contains(name)
    }

    pub fn is_cannon(annotation_usages: &AnnotationUsages, vm: &VM) -> bool {
        let name = vm.annotations.idx(vm.known.annotations.cannon).read().name;
        annotation_usages.contains(name)
    }
    pub fn is_optimize_immediately(annotation_usages: &AnnotationUsages, vm: &VM) -> bool {
        let name = vm
            .annotations
            .idx(vm.known.annotations.optimize_immediately)
            .read()
            .name;
        annotation_usages.contains(name)
    }
}

#[derive(Debug)]
pub struct Annotation {
    pub id: AnnotationId,
    pub file_id: FileId,
    pub pos: Position,
    pub name: Name,
    pub namespace_id: NamespaceId,
    pub ty: SourceType,
    pub internal: bool,
    pub internal_annotation: Option<InternalAnnotation>,

    pub type_params: Option<Vec<TypeParam>>,
    pub term_params: Option<Vec<AnnotationParam>>,
}
