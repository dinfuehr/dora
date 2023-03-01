use std::convert::TryInto;

use dora_parser::ast::{AnnotationUsages, Modifier};
use dora_parser::interner::Name;
use dora_parser::Position;

use crate::language::sem_analysis::{
    ModuleDefinitionId, PackageDefinitionId, SemAnalysis, SourceFileId,
};
use crate::language::ty::SourceType;
use crate::Id;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct AnnotationDefinitionId(usize);

impl AnnotationDefinitionId {
    pub fn max() -> AnnotationDefinitionId {
        AnnotationDefinitionId(usize::max_value())
    }
}

impl Id for AnnotationDefinition {
    type IdType = AnnotationDefinitionId;

    fn id_to_usize(id: AnnotationDefinitionId) -> usize {
        id.0 as usize
    }

    fn usize_to_id(value: usize) -> AnnotationDefinitionId {
        AnnotationDefinitionId(value.try_into().unwrap())
    }

    fn store_id(value: &mut AnnotationDefinition, id: AnnotationDefinitionId) {
        value.id = Some(id);
    }
}

#[derive(Debug)]
pub struct AnnotationDefinition {
    pub id: Option<AnnotationDefinitionId>,
    pub package_id: PackageDefinitionId,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub pos: Position,
    pub name: Name,
    pub ty: SourceType,
    pub internal_annotation: Option<Modifier>,
}

impl AnnotationDefinition {
    pub fn new(
        package_id: PackageDefinitionId,
        module_id: ModuleDefinitionId,
        file_id: SourceFileId,
        pos: Position,
        name: Name,
    ) -> AnnotationDefinition {
        AnnotationDefinition {
            id: None,
            package_id,
            module_id,
            file_id,
            pos,
            name,
            ty: SourceType::Error,
            internal_annotation: None,
        }
    }

    pub fn id(&self) -> AnnotationDefinitionId {
        self.id.expect("id missing")
    }

    pub fn internal(&self) -> bool {
        self.internal_annotation.is_some()
    }

    pub fn is_internal(annotation_usages: &AnnotationUsages, sa: &SemAnalysis) -> bool {
        let name = sa
            .annotations
            .idx(sa.known.annotations.internal())
            .read()
            .name;
        annotation_usages.contains(name)
    }

    pub fn is_pub(annotation_usages: &AnnotationUsages, sa: &SemAnalysis) -> bool {
        let name = sa.annotations.idx(sa.known.annotations.pub_()).read().name;
        annotation_usages.contains(name)
    }
    pub fn is_static(annotation_usages: &AnnotationUsages, sa: &SemAnalysis) -> bool {
        let name = sa
            .annotations
            .idx(sa.known.annotations.static_())
            .read()
            .name;
        annotation_usages.contains(name)
    }

    pub fn is_test(annotation_usages: &AnnotationUsages, sa: &SemAnalysis) -> bool {
        let name = sa.annotations.idx(sa.known.annotations.test()).read().name;
        annotation_usages.contains(name)
    }

    pub fn is_cannon(annotation_usages: &AnnotationUsages, sa: &SemAnalysis) -> bool {
        let name = sa
            .annotations
            .idx(sa.known.annotations.cannon())
            .read()
            .name;
        annotation_usages.contains(name)
    }
    pub fn is_optimize_immediately(annotation_usages: &AnnotationUsages, sa: &SemAnalysis) -> bool {
        let name = sa
            .annotations
            .idx(sa.known.annotations.optimize_immediately())
            .read()
            .name;
        annotation_usages.contains(name)
    }
}
