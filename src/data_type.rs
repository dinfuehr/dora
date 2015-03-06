use std::fmt;

#[derive(PartialEq,Eq,Debug,Copy)]
pub enum DataType {
    Int, Bool, Str
}

impl DataType {
    fn display(&self) -> &'static str {
        match *self {
            DataType::Int => "int",
            DataType::Bool => "bool",
            DataType::Str => "str",
        }
    }
}

impl fmt::Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.display())
    }
}
