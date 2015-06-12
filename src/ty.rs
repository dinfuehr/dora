#[derive(PartialEq, Eq)]
pub enum Ty {
    TyUnit,
    TyInt,
}

impl Ty {
    pub fn is_int(&self) -> bool {
        *self == Ty::TyInt
    }

    pub fn is_unit(&self) -> bool {
        *self == Ty::TyUnit
    }
}
