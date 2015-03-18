use std::fmt;

#[derive(PartialEq,Eq,Debug,Copy)]
pub enum DataType {
    Unit, Int, Bool, Str
}

impl DataType {
    pub fn is_unit(&self) -> bool {
        *self == DataType::Unit
    }

    fn display(&self) -> &'static str {
        match *self {
            DataType::Int => "int",
            DataType::Bool => "bool",
            DataType::Str => "str",
            DataType::Unit => "()",
        }
    }
}

impl fmt::Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.display())
    }
}
