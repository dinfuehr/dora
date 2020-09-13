use std::collections::HashMap;
use std::ops::Index;
use std::sync::Arc;

use crate::mem;
use crate::semck;
use crate::vm::module::ModuleId;
use crate::vm::VM;
use crate::vm::{ClassId, EnumId, EnumLayout, FctId, StructId, TraitId, TupleId};

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum BuiltinType {
    // couldn't determine type because of error
    Error,

    // Allow any type here, used for type inference
    Any,

    // type with only one value: ()
    Unit,

    // value types
    Bool,

    Char,
    UInt8,
    Int32,
    Int64,

    Float32,
    Float64,

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

    // some tuple
    Tuple(TupleId),

    // some trait object
    Trait(TraitId),

    // some module
    Module(ModuleId),

    // some type variable
    FctTypeParam(FctId, TypeListId),
    ClassTypeParam(ClassId, TypeListId),

    // some lambda
    Lambda(LambdaId),

    // some enum
    Enum(EnumId, TypeListId),
}

impl BuiltinType {
    pub fn is_error(&self) -> bool {
        match *self {
            BuiltinType::Error => true,
            _ => false,
        }
    }

    pub fn is_enum(&self) -> bool {
        match *self {
            BuiltinType::Enum(_, _) => true,
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

    pub fn is_module(&self) -> bool {
        match *self {
            BuiltinType::Module(_) => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            &BuiltinType::Float32 | &BuiltinType::Float64 => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            &BuiltinType::Bool => true,
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

    pub fn is_tuple(&self) -> bool {
        match self {
            &BuiltinType::Tuple(_) => true,
            _ => false,
        }
    }

    pub fn is_tuple_or_unit(&self) -> bool {
        match self {
            &BuiltinType::Tuple(_) => true,
            &BuiltinType::Unit => true,
            _ => false,
        }
    }

    pub fn cls_id(&self, vm: &VM) -> Option<ClassId> {
        match *self {
            BuiltinType::Class(cls_id, _) => Some(cls_id),
            BuiltinType::Unit => Some(vm.vips.unit_class),
            BuiltinType::Bool => Some(vm.vips.bool_class),
            BuiltinType::UInt8 => Some(vm.vips.uint8_class),
            BuiltinType::Char => Some(vm.vips.char_class),
            BuiltinType::Int32 => Some(vm.vips.int32_class),
            BuiltinType::Int64 => Some(vm.vips.int64_class),
            BuiltinType::Float32 => Some(vm.vips.float32_class),
            BuiltinType::Float64 => Some(vm.vips.float64_class),
            _ => None,
        }
    }

    pub fn from_cls(cls_id: ClassId, vm: &VM) -> BuiltinType {
        let list_id = vm.lists.lock().insert(TypeList::empty());
        BuiltinType::Class(cls_id, list_id)
    }

    pub fn module_id(&self) -> Option<ModuleId> {
        match *self {
            BuiltinType::Module(module_id) => Some(module_id),
            _ => None,
        }
    }

    pub fn enum_id(&self) -> Option<EnumId> {
        match *self {
            BuiltinType::Enum(enum_id, _) => Some(enum_id),
            _ => None,
        }
    }

    pub fn tuple_id(&self) -> Option<TupleId> {
        match *self {
            BuiltinType::Tuple(tuple_id) => Some(tuple_id),
            _ => None,
        }
    }

    pub fn implements_trait(&self, vm: &VM, trait_id: TraitId) -> bool {
        if let Some(cls_id) = self.cls_id(vm) {
            let cls = vm.classes.idx(cls_id);
            let cls = cls.read();
            return cls.implements_trait(vm, trait_id);
        }

        false
    }

    pub fn type_params(&self, vm: &VM) -> TypeList {
        match self {
            &BuiltinType::Class(_, list_id)
            | &BuiltinType::Enum(_, list_id)
            | &BuiltinType::Struct(_, list_id) => vm.lists.lock().get(list_id),
            _ => TypeList::empty(),
        }
    }

    pub fn contains_type_param(&self, vm: &VM) -> bool {
        match self {
            &BuiltinType::ClassTypeParam(_, _) => true,
            &BuiltinType::FctTypeParam(_, _) => true,

            &BuiltinType::Class(_, list_id) => {
                let params = vm.lists.lock().get(list_id);
                params.iter().any(|t| t.contains_type_param(vm))
            }

            &BuiltinType::Lambda(_) => unimplemented!(),

            _ => false,
        }
    }

    pub fn reference_type(&self) -> bool {
        match *self {
            BuiltinType::Ptr => true,
            BuiltinType::Class(_, _) => true,
            BuiltinType::Trait(_) => true,
            _ => false,
        }
    }

    pub fn value_type(&self) -> bool {
        match *self {
            BuiltinType::Unit
            | BuiltinType::Bool
            | BuiltinType::UInt8
            | BuiltinType::Int32
            | BuiltinType::Int64
            | BuiltinType::Float32
            | BuiltinType::Float64 => true,
            _ => false,
        }
    }

    pub fn subclass_from(&self, vm: &VM, ty: BuiltinType) -> bool {
        if !self.is_cls() {
            return false;
        }
        if !ty.is_cls() {
            return false;
        }

        let cls_id = self.cls_id(vm).unwrap();
        let cls = vm.classes.idx(cls_id);
        let cls = cls.read();
        cls.subclass_from(vm, ty.cls_id(vm).unwrap())
    }

    pub fn name(&self, vm: &VM) -> String {
        match *self {
            BuiltinType::Error => "<error>".into(),
            BuiltinType::Any => "Any".into(),
            BuiltinType::Unit => "()".into(),
            BuiltinType::UInt8 => "UInt8".into(),
            BuiltinType::Char => "Char".into(),
            BuiltinType::Int32 => "Int32".into(),
            BuiltinType::Int64 => "Int64".into(),
            BuiltinType::Float32 => "Float32".into(),
            BuiltinType::Float64 => "Float64".into(),
            BuiltinType::Bool => "Bool".into(),
            BuiltinType::Nil => "nil".into(),
            BuiltinType::Ptr => panic!("type Ptr only for internal use."),
            BuiltinType::This => "Self".into(),
            BuiltinType::Class(id, list_id) => {
                let params = vm.lists.lock().get(list_id);
                let cls = vm.classes.idx(id);
                let cls = cls.read();
                let base = vm.interner.str(cls.name);

                if params.len() == 0 {
                    base.to_string()
                } else {
                    let params = params
                        .iter()
                        .map(|ty| ty.name(vm))
                        .collect::<Vec<_>>()
                        .join(", ");

                    format!("{}[{}]", base, params)
                }
            }
            BuiltinType::Struct(sid, list_id) => {
                let struc = vm.structs.idx(sid);
                let struc = struc.lock();
                let name = struc.name;
                let name = vm.interner.str(name).to_string();

                let params = vm.lists.lock().get(list_id);

                if params.len() == 0 {
                    name
                } else {
                    let params = params
                        .iter()
                        .map(|ty| ty.name(vm))
                        .collect::<Vec<_>>()
                        .join(", ");

                    format!("{}[{}]", name, params)
                }
            }
            BuiltinType::Trait(tid) => {
                let xtrait = vm.traits[tid].read();
                vm.interner.str(xtrait.name).to_string()
            }
            BuiltinType::Enum(id, list_id) => {
                let xenum = vm.enums[id].read();
                let name = vm.interner.str(xenum.name).to_string();

                let params = vm.lists.lock().get(list_id);

                if params.len() == 0 {
                    name
                } else {
                    let params = params
                        .iter()
                        .map(|ty| ty.name(vm))
                        .collect::<Vec<_>>()
                        .join(", ");

                    format!("{}[{}]", name, params)
                }
            }
            BuiltinType::Module(id) => {
                let module = vm.modules.idx(id);
                let module = module.read();
                vm.interner.str(module.name).to_string()
            }
            BuiltinType::ClassTypeParam(cid, id) => {
                let cls = vm.classes.idx(cid);
                let cls = cls.read();
                vm.interner.str(cls.type_params[id.idx()].name).to_string()
            }

            BuiltinType::FctTypeParam(fid, id) => {
                let fct = vm.fcts.idx(fid);
                let fct = fct.read();
                vm.interner.str(fct.type_params[id.idx()].name).to_string()
            }

            BuiltinType::Lambda(id) => {
                let lambda = vm.lambda_types.lock().get(id);
                let params = lambda
                    .params
                    .iter()
                    .map(|ty| ty.name(vm))
                    .collect::<Vec<_>>()
                    .join(", ");
                let ret = lambda.ret.name(vm);

                format!("({}) -> {}", params, ret)
            }

            BuiltinType::Tuple(tuple_id) => {
                let types = vm.tuples.lock().get(tuple_id);

                let types = types
                    .iter()
                    .map(|ty| ty.name(vm))
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("({})", types)
            }
        }
    }

    pub fn allows(&self, vm: &VM, other: BuiltinType) -> bool {
        match *self {
            // allow all types for Error, there is already an error,
            // don't report too many messages for the same error
            BuiltinType::Error => true,

            // Any allows all other types
            BuiltinType::Any => true,

            BuiltinType::Unit
            | BuiltinType::Bool
            | BuiltinType::UInt8
            | BuiltinType::Char
            | BuiltinType::Struct(_, _) => *self == other,
            BuiltinType::Int32
            | BuiltinType::Int64
            | BuiltinType::Float32
            | BuiltinType::Float64 => *self == other,
            BuiltinType::Nil => panic!("nil does not allow any other types"),
            BuiltinType::Ptr => panic!("ptr does not allow any other types"),
            BuiltinType::This => unreachable!(),
            BuiltinType::Class(_, _) => {
                *self == other || other.is_nil() || other.subclass_from(vm, *self)
            }
            BuiltinType::Tuple(tuple_id) => match other {
                BuiltinType::Tuple(other_tuple_id) => {
                    if tuple_id == other_tuple_id {
                        return true;
                    }

                    let subtypes = vm.tuples.lock().get(tuple_id);
                    let other_subtypes = vm.tuples.lock().get(other_tuple_id);

                    if subtypes.len() != other_subtypes.len() {
                        return false;
                    }

                    let len = subtypes.len();

                    for idx in 0..len {
                        let ty = subtypes[idx];
                        let other_ty = other_subtypes[idx];

                        if !ty.allows(vm, other_ty) {
                            return false;
                        }
                    }

                    true
                }

                _ => false,
            },
            BuiltinType::Trait(_) => unimplemented!(),
            BuiltinType::Module(_) => *self == other,
            BuiltinType::Enum(_, _) => *self == other,

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
        if self.is_nil() {
            other
        } else {
            *self
        }
    }

    pub fn size(&self, vm: &VM) -> i32 {
        match *self {
            BuiltinType::Error => panic!("no size for error."),
            BuiltinType::Unit => 0,
            BuiltinType::Bool => 1,
            BuiltinType::UInt8 => 1,
            BuiltinType::Char => 4,
            BuiltinType::Int32 => 4,
            BuiltinType::Int64 => 8,
            BuiltinType::Float32 => 4,
            BuiltinType::Float64 => 8,
            BuiltinType::Enum(eid, list_id) => {
                let params = vm.lists.lock().get(list_id);
                let enum_def_id = semck::specialize::specialize_enum_id_params(vm, eid, params);
                let xenum = vm.enum_defs.idx(enum_def_id);
                let xenum = xenum.read();

                match xenum.layout {
                    EnumLayout::Int => BuiltinType::Int32.size(vm),
                    EnumLayout::Ptr | EnumLayout::Tagged => BuiltinType::Ptr.size(vm),
                }
            }
            BuiltinType::Nil => panic!("no size for nil."),
            BuiltinType::This => panic!("no size for Self."),
            BuiltinType::Any => panic!("no size for Any."),
            BuiltinType::Class(_, _)
            | BuiltinType::Module(_)
            | BuiltinType::Lambda(_)
            | BuiltinType::Ptr => mem::ptr_width(),
            BuiltinType::Struct(sid, list_id) => {
                let params = vm.lists.lock().get(list_id);
                let sid = semck::specialize::specialize_struct_id_params(vm, sid, params);
                let struc = vm.struct_defs.idx(sid);
                let struc = struc.lock();

                struc.size
            }
            BuiltinType::Trait(_) => mem::ptr_width(),
            BuiltinType::ClassTypeParam(_, _) | BuiltinType::FctTypeParam(_, _) => {
                panic!("no size for type variable.")
            }
            BuiltinType::Tuple(tuple_id) => vm.tuples.lock().get_tuple(tuple_id).size(),
        }
    }

    pub fn align(&self, vm: &VM) -> i32 {
        match *self {
            BuiltinType::Error => panic!("no alignment for error."),
            BuiltinType::Unit => 0,
            BuiltinType::Bool => 1,
            BuiltinType::UInt8 => 1,
            BuiltinType::Char => 4,
            BuiltinType::Int32 => 4,
            BuiltinType::Int64 => 8,
            BuiltinType::Float32 => 4,
            BuiltinType::Float64 => 8,
            BuiltinType::Nil => panic!("no alignment for nil."),
            BuiltinType::This => panic!("no alignment for Self."),
            BuiltinType::Any => panic!("no alignment for Any."),
            BuiltinType::Enum(eid, list_id) => {
                let params = vm.lists.lock().get(list_id);
                let enum_def_id = semck::specialize::specialize_enum_id_params(vm, eid, params);
                let xenum = vm.enum_defs.idx(enum_def_id);
                let xenum = xenum.read();

                match xenum.layout {
                    EnumLayout::Int => BuiltinType::Int32.align(vm),
                    EnumLayout::Ptr | EnumLayout::Tagged => BuiltinType::Ptr.align(vm),
                }
            }
            BuiltinType::Class(_, _)
            | BuiltinType::Module(_)
            | BuiltinType::Lambda(_)
            | BuiltinType::Ptr => mem::ptr_width(),
            BuiltinType::Struct(sid, list_id) => {
                let params = vm.lists.lock().get(list_id);
                let sid = semck::specialize::specialize_struct_id_params(vm, sid, params);
                let struc = vm.struct_defs.idx(sid);
                let struc = struc.lock();

                struc.align
            }
            BuiltinType::Trait(_) => mem::ptr_width(),
            BuiltinType::ClassTypeParam(_, _) | BuiltinType::FctTypeParam(_, _) => {
                panic!("no alignment for type variable.")
            }
            BuiltinType::Tuple(tuple_id) => vm.tuples.lock().get_tuple(tuple_id).align(),
        }
    }

    pub fn mode(&self) -> MachineMode {
        match *self {
            BuiltinType::Error => panic!("no machine mode for error."),
            BuiltinType::Unit => panic!("no machine mode for ()."),
            BuiltinType::Bool => MachineMode::Int8,
            BuiltinType::UInt8 => MachineMode::Int8,
            BuiltinType::Char => MachineMode::Int32,
            BuiltinType::Int32 => MachineMode::Int32,
            BuiltinType::Int64 => MachineMode::Int64,
            BuiltinType::Float32 => MachineMode::Float32,
            BuiltinType::Float64 => MachineMode::Float64,
            BuiltinType::Enum(_, _) => MachineMode::Int32,
            BuiltinType::Nil => panic!("no machine mode for nil."),
            BuiltinType::This => panic!("no machine mode for Self."),
            BuiltinType::Any => panic!("no machine mode for Any."),
            BuiltinType::Class(_, _)
            | BuiltinType::Module(_)
            | BuiltinType::Lambda(_)
            | BuiltinType::Ptr => MachineMode::Ptr,
            BuiltinType::Struct(_, _) => panic!("no machine mode for struct."),
            BuiltinType::Trait(_) => MachineMode::Ptr,
            BuiltinType::ClassTypeParam(_, _) | BuiltinType::FctTypeParam(_, _) => {
                panic!("no machine mode for type variable.")
            }
            BuiltinType::Tuple(_) => unimplemented!(),
        }
    }

    pub fn is_defined_type(&self, vm: &VM) -> bool {
        match *self {
            BuiltinType::Error
            | BuiltinType::This
            | BuiltinType::Any
            | BuiltinType::Ptr
            | BuiltinType::Nil => false,
            BuiltinType::Unit
            | BuiltinType::Bool
            | BuiltinType::UInt8
            | BuiltinType::Char
            | BuiltinType::Int32
            | BuiltinType::Int64
            | BuiltinType::Float32
            | BuiltinType::Float64
            | BuiltinType::Enum(_, _)
            | BuiltinType::Module(_)
            | BuiltinType::Trait(_)
            | BuiltinType::Lambda(_)
            | BuiltinType::ClassTypeParam(_, _)
            | BuiltinType::FctTypeParam(_, _) => true,
            BuiltinType::Class(_, list_id) | BuiltinType::Struct(_, list_id) => {
                let params = vm.lists.lock().get(list_id);

                for param in params.iter() {
                    if !param.is_defined_type(vm) {
                        return false;
                    }
                }

                true
            }
            BuiltinType::Tuple(tuple_id) => {
                let subtypes = vm.tuples.lock().get(tuple_id);

                for ty in subtypes.iter() {
                    if !ty.is_defined_type(vm) {
                        return false;
                    }
                }

                true
            }
        }
    }

    pub fn is_concrete_type(&self, vm: &VM) -> bool {
        match *self {
            BuiltinType::Error | BuiltinType::This | BuiltinType::Any => false,
            BuiltinType::Unit
            | BuiltinType::Bool
            | BuiltinType::UInt8
            | BuiltinType::Char
            | BuiltinType::Int32
            | BuiltinType::Int64
            | BuiltinType::Float32
            | BuiltinType::Float64
            | BuiltinType::Enum(_, _)
            | BuiltinType::Module(_)
            | BuiltinType::Ptr
            | BuiltinType::Trait(_)
            | BuiltinType::Nil => true,
            BuiltinType::Class(_, list_id) => {
                let params = vm.lists.lock().get(list_id);

                for param in params.iter() {
                    if !param.is_concrete_type(vm) {
                        return false;
                    }
                }

                true
            }
            BuiltinType::Tuple(tuple_id) => vm.tuples.lock().get_tuple(tuple_id).is_concrete_type(),
            BuiltinType::Lambda(_) | BuiltinType::Struct(_, _) => unimplemented!(),
            BuiltinType::ClassTypeParam(_, _) | BuiltinType::FctTypeParam(_, _) => false,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MachineMode {
    Int8,
    Int32,
    Int64,
    IntPtr,
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
            MachineMode::IntPtr | MachineMode::Ptr => mem::ptr_width(),
            MachineMode::Float32 => 4,
            MachineMode::Float64 => 8,
        }
    }

    pub fn is_int8(self) -> bool {
        match self {
            MachineMode::Int8 => true,
            _ => false,
        }
    }

    pub fn is_float(self) -> bool {
        match self {
            MachineMode::Float32 | MachineMode::Float64 => true,
            _ => false,
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct TypeListId(u32);

impl TypeListId {
    pub fn idx(self) -> usize {
        self.0 as usize
    }
}

impl From<usize> for TypeListId {
    fn from(data: usize) -> TypeListId {
        assert!(data < u32::max_value() as usize);
        TypeListId(data as u32)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum TypeList {
    Empty,
    List(Arc<Vec<BuiltinType>>),
}

impl TypeList {
    pub fn empty() -> TypeList {
        TypeList::Empty
    }

    pub fn single(ty: BuiltinType) -> TypeList {
        TypeList::List(Arc::new(vec![ty]))
    }

    pub fn with(type_params: Vec<BuiltinType>) -> TypeList {
        if type_params.len() == 0 {
            TypeList::Empty
        } else {
            TypeList::List(Arc::new(type_params))
        }
    }

    pub fn len(&self) -> usize {
        match self {
            &TypeList::Empty => 0,
            &TypeList::List(ref params) => params.len(),
        }
    }

    pub fn iter(&self) -> TypeListIter {
        TypeListIter {
            params: self,
            idx: 0,
        }
    }
}

impl Index<usize> for TypeList {
    type Output = BuiltinType;

    fn index(&self, idx: usize) -> &BuiltinType {
        match self {
            &TypeList::Empty => panic!("type list index out-of-bounds"),
            &TypeList::List(ref params) => &params[idx],
        }
    }
}

pub struct TypeListIter<'a> {
    params: &'a TypeList,
    idx: usize,
}

impl<'a> Iterator for TypeListIter<'a> {
    type Item = BuiltinType;

    fn next(&mut self) -> Option<BuiltinType> {
        match self.params {
            &TypeList::Empty => None,

            &TypeList::List(ref params) => {
                if self.idx < params.len() {
                    let ret = params[self.idx];
                    self.idx += 1;

                    Some(ret)
                } else {
                    None
                }
            }
        }
    }
}

pub struct TypeLists {
    lists: HashMap<TypeList, TypeListId>,
    values: Vec<TypeList>,
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

    pub fn insert(&mut self, list: TypeList) -> TypeListId {
        if let Some(&val) = self.lists.get(&list) {
            return val;
        }

        let id: TypeListId = self.next_id.into();
        self.lists.insert(list.clone(), id);

        self.values.push(list);

        self.next_id += 1;

        id
    }

    pub fn get(&self, id: TypeListId) -> TypeList {
        self.values[id.idx()].clone()
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct LambdaId(usize);

impl From<usize> for LambdaId {
    fn from(val: usize) -> LambdaId {
        LambdaId(val)
    }
}

pub struct LambdaTypes {
    types: HashMap<Arc<LambdaType>, LambdaId>,
    values: Vec<Arc<LambdaType>>,
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
        let ty = LambdaType { params, ret };

        if let Some(&val) = self.types.get(&ty) {
            return val;
        }

        let id = LambdaId(self.next_lambda_id);
        let ty = Arc::new(ty);
        self.types.insert(ty.clone(), id);

        self.values.push(ty);

        self.next_lambda_id += 1;

        id
    }

    pub fn get(&self, id: LambdaId) -> Arc<LambdaType> {
        self.values[id.0].clone()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LambdaType {
    params: Vec<BuiltinType>,
    ret: BuiltinType,
}

#[derive(Debug, Copy, Clone)]
pub enum TypeParamId {
    Fct(TypeListId),
    Class(TypeListId),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mem;

    #[test]
    fn mode_size() {
        assert_eq!(1, MachineMode::Int8.size());
        assert_eq!(4, MachineMode::Int32.size());
        assert_eq!(mem::ptr_width(), MachineMode::Ptr.size());
    }

    #[test]
    fn mode_for_types() {
        assert_eq!(MachineMode::Int8, BuiltinType::Bool.mode());
        assert_eq!(MachineMode::Int32, BuiltinType::Int32.mode());
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
