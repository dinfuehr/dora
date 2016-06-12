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

    // Array types
    IntArray,

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

    pub fn cls_id(&self, ctxt: &Context) -> ClassId {
        match *self {
            BuiltinType::Class(cls_id) => cls_id,
            BuiltinType::Str => ctxt.primitive_classes.str_class,
            BuiltinType::IntArray => ctxt.primitive_classes.int_array,

            _ => panic!()
        }
    }

    pub fn reference_type(&self) -> bool {
        !self.value_type()
    }

    pub fn value_type(&self) -> bool {
        match *self {
            BuiltinType::Unit
                | BuiltinType::Bool
                | BuiltinType::Int => true,
            _ => false
        }
    }

    pub fn subclass_from(&self, ctxt: &Context, ty: BuiltinType) -> bool {
        if !self.reference_type() { return false; }
        if !ty.reference_type() { return false; }

        let cls = ctxt.cls_by_id(self.cls_id(ctxt));
        cls.subclass_from(ctxt, ty.cls_id(ctxt))
    }

    pub fn name(&self, ctxt: &Context) -> String {
        match *self {
            BuiltinType::Unit => "()".into(),
            BuiltinType::Int => "int".into(),
            BuiltinType::Bool => "bool".into(),
            BuiltinType::Nil => "nil".into(),
            BuiltinType::Ptr => panic!("type Ptr only for internal use."),
            BuiltinType::Str => "Str".into(),
            BuiltinType::IntArray => "IntArray".into(),
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
                | BuiltinType::IntArray
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
            BuiltinType::Nil => panic!("no size for nil."),
            BuiltinType::Str
                | BuiltinType::IntArray
                | BuiltinType::Class(_)
                | BuiltinType::Ptr => mem::ptr_width(),
        }
    }

    pub fn mode(&self) -> MachineMode {
        match *self {
            BuiltinType::Unit => panic!("no machine mode for ()."),
            BuiltinType::Bool => MachineMode::Int8,
            BuiltinType::Int => MachineMode::Int32,
            BuiltinType::Nil => panic!("no machine mode for nil."),
            BuiltinType::Str
                | BuiltinType::IntArray
                | BuiltinType::Class(_)
                | BuiltinType::Ptr => MachineMode::Ptr,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MachineMode {
    Int8, Int32, Ptr
}

impl MachineMode {
    pub fn size(self) -> i32 {
        match self {
            MachineMode::Int8 => 1,
            MachineMode::Int32 => 4,
            MachineMode::Ptr => mem::ptr_width()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use mem;

    #[test]
    fn mode_size() {
        assert_eq!(1, MachineMode::Int8.size());
        assert_eq!(4, MachineMode::Int32.size());
        assert_eq!(mem::ptr_width(), MachineMode::Ptr.size());
    }

    #[test]
    fn mode_for_types() {
        assert_eq!(MachineMode::Int8, BuiltinType::Bool.mode());
        assert_eq!(MachineMode::Int32, BuiltinType::Int.mode());
        assert_eq!(MachineMode::Ptr, BuiltinType::Ptr.mode());
        assert_eq!(MachineMode::Ptr, BuiltinType::IntArray.mode());
        assert_eq!(MachineMode::Ptr, BuiltinType::Str.mode());
    }

    #[test]
    #[should_panic]
    fn mode_for_nil() {
        assert_eq!(MachineMode::Ptr, BuiltinType::Nil.mode());
    }

    #[test]
    #[should_panic]
    fn mode_for_unit() {
        assert_eq!(MachineMode::Ptr, BuiltinType::Unit.mode());
    }
}
