use crate::sema::Sema;
use crate::ty::{SourceType, SourceTypeArray};

pub fn create_tuple(_sa: &Sema, args: Vec<SourceType>) -> SourceType {
    SourceType::Tuple(SourceTypeArray::with(args))
}
