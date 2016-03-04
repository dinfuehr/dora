use class::ClassId;
use ctxt::Context;
use mem;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BuiltinType {
    Unit,
    Int,
    Bool,
    Ptr,
    Str,
    Class(ClassId),
}

impl BuiltinType {
    pub fn is_unit(&self) -> bool {
        match *self {
            BuiltinType::Unit => true,
            _ => false,
        }
    }

    pub fn is_cls(&self) -> bool {
        match *self {
            BuiltinType::Class(_) => true,
            _ => false,
        }
    }

    pub fn cls(&self) -> ClassId {
        match *self {
            BuiltinType::Class(clsid) => clsid,
            _ => panic!()
        }
    }

    pub fn name(&self, ctxt: &Context) -> String {
        match *self {
            BuiltinType::Unit => "()".into(),
            BuiltinType::Int => "int".into(),
            BuiltinType::Bool => "bool".into(),
            BuiltinType::Ptr => panic!("type Ptr only for internal use."),
            BuiltinType::Str => "str".into(),
            BuiltinType::Class(cid) => {
                let cls = ctxt.cls_by_id(cid);

                ctxt.interner.str(cls.name).to_string()
            }
        }
    }

    pub fn size(&self) -> i32 {
        match *self {
            BuiltinType::Unit => 0,
            BuiltinType::Bool => 1,
            BuiltinType::Int => 4,
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
