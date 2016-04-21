use class::ClassId;
use ctxt::Context;
use mem;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BuiltinType {
    // type with only one value: ()
    Unit,

    // value types
    Int, Bool,

    // type Nil, only used in typeck until final type is known
    Nil,

    // pointer to object, only used internally
    Ptr,

    // String type
    Str,

    // some class
    Class(ClassId),
}

impl BuiltinType {
    pub fn is_unit(&self) -> bool {
        match *self {
            BuiltinType::Unit => true,
            _ => false,
        }
    }

    pub fn is_nil(&self) -> bool {
        match *self {
            BuiltinType::Nil => true,
            _ => false
        }
    }

    pub fn is_cls(&self) -> bool {
        match *self {
            BuiltinType::Class(_) => true,
            _ => false,
        }
    }

    pub fn is_str(&self) -> bool {
        match *self {
            BuiltinType::Str => true,
            _ => false
        }
    }

    pub fn cls_id(&self) -> ClassId {
        match *self {
            BuiltinType::Class(cls_id) => cls_id,
            _ => panic!()
        }
    }

    pub fn name(&self, ctxt: &Context) -> String {
        match *self {
            BuiltinType::Unit => "()".into(),
            BuiltinType::Int => "int".into(),
            BuiltinType::Bool => "bool".into(),
            BuiltinType::Nil => "nil".into(),
            BuiltinType::Ptr => panic!("type Ptr only for internal use."),
            BuiltinType::Str => "str".into(),
            BuiltinType::Class(cid) => {
                let cls = ctxt.cls_by_id(cid);

                ctxt.interner.str(cls.name).to_string()
            }
        }
    }

    pub fn allows(&self, other: BuiltinType) -> bool {
        match *self {
            BuiltinType::Unit
                | BuiltinType::Bool
                | BuiltinType::Int
                | BuiltinType::Nil => *self == other,
            BuiltinType::Ptr => panic!("ptr does not allow any other types"),
            BuiltinType::Str
                | BuiltinType::Class(_) => *self == other || other.is_nil()
        }
    }

    pub fn if_nil(&self, other: BuiltinType) -> BuiltinType {
        if self.is_nil() {
            other
        } else {
            *self
        }
    }

    pub fn size(&self) -> i32 {
        match *self {
            BuiltinType::Unit => 0,
            BuiltinType::Bool => 1,
            BuiltinType::Int => 4,
            BuiltinType::Nil => panic!("type Nil does not have size."),
            BuiltinType::Str
                | BuiltinType::Class(_)
                | BuiltinType::Ptr => mem::ptr_width(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use mem;

    #[test]
    fn type_size() {
        assert_eq!(0, BuiltinType::Unit.size());
        assert_eq!(1, BuiltinType::Bool.size());
        assert_eq!(4, BuiltinType::Int.size());
        assert_eq!(mem::ptr_width(), BuiltinType::Str.size());
    }
}
