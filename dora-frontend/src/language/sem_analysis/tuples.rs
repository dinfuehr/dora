use crate::language::sem_analysis::SemAnalysis;
use crate::language::ty::{SourceType, SourceTypeArray};

pub fn create_tuple(_sa: &SemAnalysis, args: Vec<SourceType>) -> SourceType {
    SourceType::Tuple(SourceTypeArray::with(args))
}
