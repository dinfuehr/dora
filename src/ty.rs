use std::collections::HashMap;
use std::rc::Rc;

use class::{ClassId, TypeParamId, TypeParams};
use ctxt::{FctId, SemContext, StructId, TraitId};
use mem;
use semck;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum BuiltinType {
    // couldn't determine type because of error
    Error,

    // type with only one value: ()
    Unit,

    // value types
    Char,
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

    // self type
    This,

    // some class
    Class(ClassId, TypeListId),

    // some struct
    Struct(StructId, TypeListId),

    // some trait
    Trait(TraitId),

    // some type variable
    FctTypeParam(FctId, TypeParamId),
    ClassTypeParam(ClassId, TypeParamId),

    // some lambda
    Lambda(LambdaId),
}

impl BuiltinType {
    pub fn is_error(&self) -> bool {
        match *self {
            BuiltinType::Error => true,
            _ => false,
        }
    }

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
            BuiltinType::Class(_, _) => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            &BuiltinType::Float |
            &BuiltinType::Double => true,
            _ => false,
        }
    }

    pub fn is_type_param(&self) -> bool {
        match self {
            &BuiltinType::ClassTypeParam(_, _) => true,
            &BuiltinType::FctTypeParam(_, _) => true,
            _ => false,
        }
    }

    pub fn cls_id(&self, ctxt: &SemContext) -> Option<ClassId> {
        match *self {
            BuiltinType::Class(cls_id, _) => Some(cls_id),
            BuiltinType::Bool => Some(ctxt.vips.bool_class),
            BuiltinType::Byte => Some(ctxt.vips.byte_class),
            BuiltinType::Char => Some(ctxt.vips.char_class),
            BuiltinType::Int => Some(ctxt.vips.int_class),
            BuiltinType::Long => Some(ctxt.vips.long_class),
            BuiltinType::Float => Some(ctxt.vips.float_class),
            BuiltinType::Double => Some(ctxt.vips.double_class),
            _ => None,
        }
    }

    pub fn implements_trait(&self, ctxt: &SemContext, trait_id: TraitId) -> bool {
        if let Some(cls_id) = self.cls_id(ctxt) {
            let cls = ctxt.classes[cls_id].borrow();
            return cls.traits.contains(&trait_id);
        }

        false
    }

    pub fn type_params(&self, ctxt: &SemContext) -> TypeParams {
        match self {
            &BuiltinType::Class(_, list_id) => ctxt.lists.borrow().get(list_id),

            _ => TypeParams::empty(),
        }
    }

    pub fn contains_type_param(&self, ctxt: &SemContext) -> bool {
        match self {
            &BuiltinType::ClassTypeParam(_, _) => true,
            &BuiltinType::FctTypeParam(_, _) => true,

            &BuiltinType::Class(_, list_id) => {
                let params = ctxt.lists.borrow().get(list_id);
                params.iter().any(|t| t.contains_type_param(ctxt))
            }

            &BuiltinType::Lambda(_) => unimplemented!(),

            _ => false,
        }
    }

    pub fn reference_type(&self) -> bool {
        !self.value_type()
    }

    pub fn value_type(&self) -> bool {
        match *self {
            BuiltinType::Unit | BuiltinType::Bool | BuiltinType::Byte | BuiltinType::Int |
            BuiltinType::Long | BuiltinType::Float | BuiltinType::Double => true,
            _ => false,
        }
    }

    pub fn subclass_from(&self, ctxt: &SemContext, ty: BuiltinType) -> bool {
        if !self.is_cls() {
            return false;
        }
        if !ty.is_cls() {
            return false;
        }

        let cls_id = self.cls_id(ctxt).unwrap();
        let cls = ctxt.classes[cls_id].borrow();
        cls.subclass_from(ctxt, ty.cls_id(ctxt).unwrap())
    }

    pub fn name(&self, ctxt: &SemContext) -> String {
        match *self {
            BuiltinType::Error => "<error>".into(),
            BuiltinType::Unit => "()".into(),
            BuiltinType::Byte => "byte".into(),
            BuiltinType::Char => "char".into(),
            BuiltinType::Int => "int".into(),
            BuiltinType::Long => "long".into(),
            BuiltinType::Float => "float".into(),
            BuiltinType::Double => "float".into(),
            BuiltinType::Bool => "bool".into(),
            BuiltinType::Nil => "nil".into(),
            BuiltinType::Ptr => panic!("type Ptr only for internal use."),
            BuiltinType::This => "Self".into(),
            BuiltinType::Class(id, list_id) => {
                let params = ctxt.lists.borrow().get(list_id);
                let cls = ctxt.classes[id].borrow();
                let base = ctxt.interner.str(cls.name);

                if params.len() == 0 {
                    base.to_string()
                } else {
                    let params = params
                        .iter()
                        .map(|ty| ty.name(ctxt))
                        .collect::<Vec<_>>()
                        .join(", ");

                    format!("{}<{}>", base, params)
                }
            }
            BuiltinType::Struct(sid, list_id) => {
                let name = ctxt.structs[sid].borrow().name;
                let name = ctxt.interner.str(name).to_string();

                let params = ctxt.lists.borrow().get(list_id);

                if params.len() == 0 {
                    name
                } else {
                    let params = params
                        .iter()
                        .map(|ty| ty.name(ctxt))
                        .collect::<Vec<_>>()
                        .join(", ");

                    format!("{}<{}>", name, params)
                }
            }
            BuiltinType::Trait(tid) => {
                let name = ctxt.traits[tid].borrow().name;
                ctxt.interner.str(name).to_string()
            }
            BuiltinType::ClassTypeParam(cid, id) => {
                let cls = ctxt.classes[cid].borrow();
                ctxt.interner
                    .str(cls.type_params[id.idx()].name)
                    .to_string()
            }

            BuiltinType::FctTypeParam(fid, id) => {
                let fct = ctxt.fcts[fid].borrow();
                ctxt.interner
                    .str(fct.type_params[id.idx()].name)
                    .to_string()
            }

            BuiltinType::Lambda(id) => {
                let lambda = ctxt.lambda_types.borrow().get(id);
                let params = lambda
                    .params
                    .iter()
                    .map(|ty| ty.name(ctxt))
                    .collect::<Vec<_>>()
                    .join(", ");
                let ret = lambda.ret.name(ctxt);

                format!("({}) -> {}", params, ret)
            }
        }
    }

    pub fn allows(&self, ctxt: &SemContext, other: BuiltinType) -> bool {
        match *self {
            BuiltinType::Error => true,
            BuiltinType::Unit |
            BuiltinType::Bool |
            BuiltinType::Byte |
            BuiltinType::Char |
            BuiltinType::Struct(_, _) => *self == other,
            BuiltinType::Int => *self == other,
            BuiltinType::Long => *self == other,
            BuiltinType::Float | BuiltinType::Double => *self == other,
            BuiltinType::Nil => panic!("nil does not allow any other types"),
            BuiltinType::Ptr => panic!("ptr does not allow any other types"),
            BuiltinType::This => unreachable!(),
            BuiltinType::Class(_, _) => {
                *self == other || other.is_nil() || other.subclass_from(ctxt, *self)
            }
            BuiltinType::Trait(_) => unimplemented!(),

            BuiltinType::ClassTypeParam(_, _) => *self == other,
            BuiltinType::FctTypeParam(_, _) => *self == other,

            BuiltinType::Lambda(_) => {
                // for now expect the exact same params and return types
                // possible improvement: allow super classes for params,
                //                             sub class for return type
                *self == other
            }
        }
    }

    pub fn if_nil(&self, other: BuiltinType) -> BuiltinType {
        if self.is_nil() { other } else { *self }
    }

    pub fn size(&self, ctxt: &SemContext) -> i32 {
        match *self {
            BuiltinType::Error => panic!("no size for error."),
            BuiltinType::Unit => 0,
            BuiltinType::Bool => 1,
            BuiltinType::Byte => 1,
            BuiltinType::Char => 4,
            BuiltinType::Int => 4,
            BuiltinType::Long => 8,
            BuiltinType::Float => 4,
            BuiltinType::Double => 8,
            BuiltinType::Nil => panic!("no size for nil."),
            BuiltinType::This => panic!("no size for Self."),
            BuiltinType::Class(_, _) |
            BuiltinType::Lambda(_) |
            BuiltinType::Ptr => mem::ptr_width(),
            BuiltinType::Struct(sid, list_id) => {
                let params = ctxt.lists.borrow().get(list_id);
                let sid = semck::specialize::specialize_struct_id_params(ctxt, sid, params);
                let struc = ctxt.struct_defs[sid].borrow();

                struc.size
            }
            BuiltinType::Trait(_) => 2 * mem::ptr_width(),
            BuiltinType::ClassTypeParam(_, _) |
            BuiltinType::FctTypeParam(_, _) => panic!("no size for type variable."),
        }
    }

    pub fn align(&self, ctxt: &SemContext) -> i32 {
        match *self {
            BuiltinType::Error => panic!("no alignment for error."),
            BuiltinType::Unit => 0,
            BuiltinType::Bool => 1,
            BuiltinType::Byte => 1,
            BuiltinType::Char => 4,
            BuiltinType::Int => 4,
            BuiltinType::Long => 8,
            BuiltinType::Float => 4,
            BuiltinType::Double => 8,
            BuiltinType::Nil => panic!("no alignment for nil."),
            BuiltinType::This => panic!("no alignment for Self."),
            BuiltinType::Class(_, _) |
            BuiltinType::Lambda(_) |
            BuiltinType::Ptr => mem::ptr_width(),
            BuiltinType::Struct(sid, list_id) => {
                let params = ctxt.lists.borrow().get(list_id);
                let sid = semck::specialize::specialize_struct_id_params(ctxt, sid, params);
                let struc = ctxt.struct_defs[sid].borrow();

                struc.align
            }
            BuiltinType::Trait(_) => mem::ptr_width(),
            BuiltinType::ClassTypeParam(_, _) |
            BuiltinType::FctTypeParam(_, _) => panic!("no alignment for type variable."),
        }
    }

    pub fn mode(&self) -> MachineMode {
        match *self {
            BuiltinType::Error => panic!("no machine mode for error."),
            BuiltinType::Unit => panic!("no machine mode for ()."),
            BuiltinType::Bool => MachineMode::Int8,
            BuiltinType::Byte => MachineMode::Int8,
            BuiltinType::Char => MachineMode::Int32,
            BuiltinType::Int => MachineMode::Int32,
            BuiltinType::Long => MachineMode::Int64,
            BuiltinType::Float => MachineMode::Float32,
            BuiltinType::Double => MachineMode::Float64,
            BuiltinType::Nil => panic!("no machine mode for nil."),
            BuiltinType::This => panic!("no machine mode for Self."),
            BuiltinType::Class(_, _) |
            BuiltinType::Lambda(_) |
            BuiltinType::Ptr => MachineMode::Ptr,
            BuiltinType::Struct(_, _) => panic!("no machine mode for struct."),
            BuiltinType::Trait(_) => unimplemented!(),
            BuiltinType::ClassTypeParam(_, _) |
            BuiltinType::FctTypeParam(_, _) => panic!("no machine mode for type variable."),
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

pub struct TypeLists {
    lists: HashMap<TypeParams, TypeListId>,
    values: Vec<TypeParams>,
    next_id: usize,
}

impl TypeLists {
    pub fn new() -> TypeLists {
        TypeLists {
            lists: HashMap::new(),
            values: Vec::new(),
            next_id: 0,
        }
    }

    pub fn insert(&mut self, list: TypeParams) -> TypeListId {
        if let Some(&val) = self.lists.get(&list) {
            return val;
        }

        let id = TypeListId(self.next_id);
        self.lists.insert(list.clone(), id);

        self.values.push(list);

        self.next_id += 1;

        id
    }

    pub fn get(&self, id: TypeListId) -> TypeParams {
        self.values[id.0].clone()
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TypeListId(usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct LambdaId(usize);

impl From<usize> for LambdaId {
    fn from(val: usize) -> LambdaId {
        LambdaId(val)
    }
}

pub struct LambdaTypes {
    types: HashMap<Rc<LambdaType>, LambdaId>,
    values: Vec<Rc<LambdaType>>,
    next_lambda_id: usize,
}

impl LambdaTypes {
    pub fn new() -> LambdaTypes {
        LambdaTypes {
            types: HashMap::new(),
            values: Vec::new(),
            next_lambda_id: 0,
        }
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }

    pub fn insert(&mut self, params: Vec<BuiltinType>, ret: BuiltinType) -> LambdaId {
        let ty = LambdaType {
            params: params,
            ret: ret,
        };

        if let Some(&val) = self.types.get(&ty) {
            return val;
        }

        let id = LambdaId(self.next_lambda_id);
        let ty = Rc::new(ty);
        self.types.insert(ty.clone(), id);

        self.values.push(ty);

        self.next_lambda_id += 1;

        id
    }

    pub fn get(&self, id: LambdaId) -> Rc<LambdaType> {
        self.values[id.0].clone()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LambdaType {
    params: Vec<BuiltinType>,
    ret: BuiltinType,
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
