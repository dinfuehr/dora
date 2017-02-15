use class::ClassId;
use ctxt::{Context, StructId};
use mem;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BuiltinType {
    // type with only one value: ()
    Unit,

    // value types
    Byte,
    Int,
    Long,

    Float,
    Double,

    Bool,

    // type Nil, only used in typeck until final type is known
    Nil,

    // pointer to object, only used internally
    Ptr,

    // String type
    Str,

    // Array types
    ByteArray,
    IntArray,
    LongArray,

    // some class
    Class(ClassId),

    // some struct
    Struct(StructId),
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
            _ => false,
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
            _ => false,
        }
    }

    pub fn cls_id(&self, ctxt: &Context) -> ClassId {
        match *self {
            BuiltinType::Class(cls_id) => cls_id,
            BuiltinType::Str => ctxt.primitive_classes.str_class,
            BuiltinType::ByteArray => ctxt.primitive_classes.byte_array,
            BuiltinType::IntArray => ctxt.primitive_classes.int_array,
            BuiltinType::LongArray => ctxt.primitive_classes.long_array,

            _ => panic!(),
        }
    }

    pub fn reference_type(&self) -> bool {
        !self.value_type()
    }

    pub fn value_type(&self) -> bool {
        match *self {
            BuiltinType::Unit | BuiltinType::Bool | BuiltinType::Byte | BuiltinType::Int |
            BuiltinType::Long => true,
            _ => false,
        }
    }

    pub fn subclass_from(&self, ctxt: &Context, ty: BuiltinType) -> bool {
        if !self.reference_type() {
            return false;
        }
        if !ty.reference_type() {
            return false;
        }

        let cls_id = self.cls_id(ctxt);
        let cls = ctxt.classes[cls_id].borrow();
        cls.subclass_from(ctxt, ty.cls_id(ctxt))
    }

    pub fn name(&self, ctxt: &Context) -> String {
        match *self {
            BuiltinType::Unit => "()".into(),
            BuiltinType::Byte => "byte".into(),
            BuiltinType::Int => "int".into(),
            BuiltinType::Long => "long".into(),
            BuiltinType::Float => "float".into(),
            BuiltinType::Double => "float".into(),
            BuiltinType::Bool => "bool".into(),
            BuiltinType::Nil => "nil".into(),
            BuiltinType::Ptr => panic!("type Ptr only for internal use."),
            BuiltinType::Str => "Str".into(),
            BuiltinType::ByteArray => "ByteArray".into(),
            BuiltinType::IntArray => "IntArray".into(),
            BuiltinType::LongArray => "LongArray".into(),
            BuiltinType::Class(cid) => {
                let cls = ctxt.classes[cid].borrow();
                ctxt.interner.str(cls.name).to_string()
            }
            BuiltinType::Struct(sid) => {
                let name = ctxt.structs[sid].borrow().name;
                ctxt.interner.str(name).to_string()
            }
        }
    }

    pub fn allows(&self, ctxt: &Context, other: BuiltinType) -> bool {
        match *self {
            BuiltinType::Unit |
            BuiltinType::Bool |
            BuiltinType::Byte |
            BuiltinType::Struct(_) => *self == other,
            BuiltinType::Int => *self == other,
            BuiltinType::Long => *self == other,
            BuiltinType::Float | BuiltinType::Double => *self == other,
            BuiltinType::Nil => panic!("nil does not allow any other types"),
            BuiltinType::Ptr => panic!("ptr does not allow any other types"),
            BuiltinType::Str |
            BuiltinType::ByteArray |
            BuiltinType::IntArray |
            BuiltinType::LongArray |
            BuiltinType::Class(_) => {
                if *self == other || other.is_nil() {
                    return true;
                }

                other.subclass_from(ctxt, *self)
            }
        }
    }

    pub fn if_nil(&self, other: BuiltinType) -> BuiltinType {
        if self.is_nil() { other } else { *self }
    }

    pub fn size(&self, ctxt: &Context) -> i32 {
        match *self {
            BuiltinType::Unit => 0,
            BuiltinType::Bool => 1,
            BuiltinType::Byte => 1,
            BuiltinType::Int => 4,
            BuiltinType::Long => 8,
            BuiltinType::Float => 4,
            BuiltinType::Double => 8,
            BuiltinType::Nil => panic!("no size for nil."),
            BuiltinType::Str |
            BuiltinType::ByteArray |
            BuiltinType::IntArray |
            BuiltinType::LongArray |
            BuiltinType::Class(_) |
            BuiltinType::Ptr => mem::ptr_width(),
            BuiltinType::Struct(id) => ctxt.structs[id].borrow().size,
        }
    }

    pub fn align(&self, ctxt: &Context) -> i32 {
        match *self {
            BuiltinType::Unit => 0,
            BuiltinType::Bool => 1,
            BuiltinType::Byte => 1,
            BuiltinType::Int => 4,
            BuiltinType::Long => 8,
            BuiltinType::Float => 4,
            BuiltinType::Double => 8,
            BuiltinType::Nil => panic!("no size for nil."),
            BuiltinType::Str |
            BuiltinType::ByteArray |
            BuiltinType::IntArray |
            BuiltinType::LongArray |
            BuiltinType::Class(_) |
            BuiltinType::Ptr => mem::ptr_width(),
            BuiltinType::Struct(id) => ctxt.structs[id].borrow().align,
        }
    }

    pub fn mode(&self) -> MachineMode {
        match *self {
            BuiltinType::Unit => panic!("no machine mode for ()."),
            BuiltinType::Bool => MachineMode::Int8,
            BuiltinType::Byte => MachineMode::Int8,
            BuiltinType::Int => MachineMode::Int32,
            BuiltinType::Long => MachineMode::Int64,
            BuiltinType::Float => MachineMode::Float32,
            BuiltinType::Double => MachineMode::Float64,
            BuiltinType::Nil => panic!("no machine mode for nil."),
            BuiltinType::Str |
            BuiltinType::ByteArray |
            BuiltinType::IntArray |
            BuiltinType::LongArray |
            BuiltinType::Class(_) |
            BuiltinType::Ptr => MachineMode::Ptr,
            BuiltinType::Struct(_) => unimplemented!(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MachineMode {
    Int8,
    Int32,
    Int64,
    Float32,
    Float64,
    Ptr,
}

impl MachineMode {
    pub fn size(self) -> i32 {
        match self {
            MachineMode::Int8 => 1,
            MachineMode::Int32 => 4,
            MachineMode::Int64 => 8,
            MachineMode::Ptr => mem::ptr_width(),
            MachineMode::Float32 => 4,
            MachineMode::Float64 => 8,
        }
    }

    pub fn is_float(self) -> bool {
        match self {
            MachineMode::Float32 |
            MachineMode::Float64 => true,
            _ => false,
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
        assert_eq!(MachineMode::Ptr, BuiltinType::ByteArray.mode());
        assert_eq!(MachineMode::Ptr, BuiltinType::IntArray.mode());
        assert_eq!(MachineMode::Ptr, BuiltinType::LongArray.mode());
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
