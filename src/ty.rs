#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BuiltinType {
    Unit,
    Int,
    Bool,
    Str,
}

impl BuiltinType {
    pub fn size(&self) -> i32 {
        match *self {
            BuiltinType::Unit => 0,
            BuiltinType::Bool => 1,
            BuiltinType::Int => 4,
            BuiltinType::Str => 8,
        }
    }
}

impl ToString for BuiltinType {
    fn to_string(&self) -> String {
        let name = match *self {
            BuiltinType::Unit => "()",
            BuiltinType::Int => "int",
            BuiltinType::Bool => "bool",
            BuiltinType::Str => "str"
        };

        name.into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn type_size() {
        assert_eq!(0, BuiltinType::Unit.size());
        assert_eq!(1, BuiltinType::Bool.size());
        assert_eq!(4, BuiltinType::Int.size());
        assert_eq!(8, BuiltinType::Str.size());
    }
}
