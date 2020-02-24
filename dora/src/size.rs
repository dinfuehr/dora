#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum InstanceSize {
    Fixed(i32),
    Array(i32),
    ObjArray,
    UnitArray,
    TupleArray(i32),
    FreeArray,
    Str,
}
