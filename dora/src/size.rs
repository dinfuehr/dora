#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum InstanceSize {
    Fixed(i32),
    Array(i32),
    ObjArray,
    TupleArray,
    FreeArray,
    Str,
}
