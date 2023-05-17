use crate::language::sema::Sema;
use crate::language::ty::{SourceType, SourceTypeArray};

pub fn create_tuple(_sa: &Sema, args: Vec<SourceType>) -> SourceType {
    SourceType::Tuple(SourceTypeArray::with(args))
}
