use std::collections::HashMap;
use std::ops::Index;
use std::sync::Arc;

use crate::mem;
use crate::semck;
use crate::vm::module::ModuleId;
use crate::vm::VM;
use crate::vm::{Class, ClassId, EnumId, EnumLayout, Fct, StructId, TraitId, TupleId};

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
    TraitObject(TraitId),

    // some module
    Module(ModuleId),

    // some type variable
    TypeParam(TypeListId),

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

    pub fn is_self(&self) -> bool {
        match *self {
            BuiltinType::This => true,
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
            &BuiltinType::TypeParam(_) => true,
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
            BuiltinType::Unit => Some(vm.known.classes.unit),
            BuiltinType::Bool => Some(vm.known.classes.bool),
            BuiltinType::UInt8 => Some(vm.known.classes.uint8),
            BuiltinType::Char => Some(vm.known.classes.char),
            BuiltinType::Int32 => Some(vm.known.classes.int32),
            BuiltinType::Int64 => Some(vm.known.classes.int64),
            BuiltinType::Float32 => Some(vm.known.classes.float32),
            BuiltinType::Float64 => Some(vm.known.classes.float64),
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
            &BuiltinType::TypeParam(_) => true,

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
            BuiltinType::TraitObject(_) => true,
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
        let writer = BuiltinTypePrinter {
            vm,
            use_fct: None,
            use_class: None,
        };

        writer.name(*self)
    }

    pub fn name_fct<'ast>(&self, vm: &VM<'ast>, fct: &Fct<'ast>) -> String {
        let writer = BuiltinTypePrinter {
            vm,
            use_fct: Some(fct),
            use_class: None,
        };

        writer.name(*self)
    }

    pub fn name_cls(&self, vm: &VM, cls: &Class) -> String {
        let writer = BuiltinTypePrinter {
            vm,
            use_fct: None,
            use_class: Some(cls),
        };

        writer.name(*self)
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
            BuiltinType::TraitObject(_) => unimplemented!(),
            BuiltinType::Module(_) => *self == other,
            BuiltinType::Enum(_, _) => *self == other,

            BuiltinType::TypeParam(_) => *self == other,

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
            BuiltinType::TraitObject(_) => mem::ptr_width(),
            BuiltinType::TypeParam(_) => panic!("no size for type variable."),
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
            BuiltinType::TraitObject(_) => mem::ptr_width(),
            BuiltinType::TypeParam(_) => panic!("no alignment for type variable."),
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
            BuiltinType::TraitObject(_) => MachineMode::Ptr,
            BuiltinType::TypeParam(_) => panic!("no machine mode for type variable."),
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
            | BuiltinType::TraitObject(_)
            | BuiltinType::Lambda(_)
            | BuiltinType::TypeParam(_) => true,
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
            | BuiltinType::TraitObject(_)
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
            BuiltinType::TypeParam(_) => false,
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
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

impl From<usize> for TypeListId {
    fn from(data: usize) -> TypeListId {
        assert!(data < u32::max_value() as usize);
        TypeListId(data as u32)
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct CombinedTypeListId(u32);

impl CombinedTypeListId {
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

impl From<usize> for CombinedTypeListId {
    fn from(data: usize) -> CombinedTypeListId {
        assert!(data < u32::max_value() as usize);
        CombinedTypeListId(data as u32)
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

    pub fn append(&self, other: &TypeList) -> TypeList {
        if self.is_empty() {
            return other.clone();
        }

        if other.is_empty() {
            return self.clone();
        }

        let mut params = self.types().to_vec();
        params.extend_from_slice(other.types());

        TypeList::List(Arc::new(params))
    }

    pub fn types(&self) -> &[BuiltinType] {
        match self {
            TypeList::Empty => &[],
            TypeList::List(ref params) => (**params).as_slice(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
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
        self.values[id.to_usize()].clone()
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

struct BuiltinTypePrinter<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,
    use_fct: Option<&'a Fct<'ast>>,
    use_class: Option<&'a Class>,
}

impl<'a, 'ast> BuiltinTypePrinter<'a, 'ast> {
    pub fn name(&self, ty: BuiltinType) -> String {
        match ty {
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
                let params = self.vm.lists.lock().get(list_id);
                let cls = self.vm.classes.idx(id);
                let cls = cls.read();
                let base = self.vm.interner.str(cls.name);

                if params.len() == 0 {
                    base.to_string()
                } else {
                    let params = params
                        .iter()
                        .map(|ty| self.name(ty))
                        .collect::<Vec<_>>()
                        .join(", ");

                    format!("{}[{}]", base, params)
                }
            }
            BuiltinType::Struct(sid, list_id) => {
                let struc = self.vm.structs.idx(sid);
                let struc = struc.lock();
                let name = struc.name;
                let name = self.vm.interner.str(name).to_string();

                let params = self.vm.lists.lock().get(list_id);

                if params.len() == 0 {
                    name
                } else {
                    let params = params
                        .iter()
                        .map(|ty| self.name(ty))
                        .collect::<Vec<_>>()
                        .join(", ");

                    format!("{}[{}]", name, params)
                }
            }
            BuiltinType::TraitObject(tid) => {
                let xtrait = self.vm.traits[tid].read();
                self.vm.interner.str(xtrait.name).to_string()
            }
            BuiltinType::Enum(id, list_id) => {
                let xenum = self.vm.enums[id].read();
                let name = self.vm.interner.str(xenum.name).to_string();

                let params = self.vm.lists.lock().get(list_id);

                if params.len() == 0 {
                    name
                } else {
                    let params = params
                        .iter()
                        .map(|ty| self.name(ty))
                        .collect::<Vec<_>>()
                        .join(", ");

                    format!("{}[{}]", name, params)
                }
            }
            BuiltinType::Module(id) => {
                let module = self.vm.modules.idx(id);
                let module = module.read();
                self.vm.interner.str(module.name).to_string()
            }
            BuiltinType::TypeParam(idx) => {
                if let Some(fct) = self.use_fct {
                    fct.type_param_ty(self.vm, ty, |tp, _| {
                        self.vm.interner.str(tp.name).to_string()
                    })
                } else if let Some(cls) = self.use_class {
                    self.vm.interner.str(cls.type_param_ty(ty).name).to_string()
                } else {
                    format!("TypeParam({})", idx.to_usize())
                }
            }

            BuiltinType::Lambda(id) => {
                let lambda = self.vm.lambda_types.lock().get(id);
                let params = lambda
                    .params
                    .iter()
                    .map(|&ty| self.name(ty))
                    .collect::<Vec<_>>()
                    .join(", ");
                let ret = self.name(lambda.ret);

                format!("({}) -> {}", params, ret)
            }

            BuiltinType::Tuple(tuple_id) => {
                let types = self.vm.tuples.lock().get(tuple_id);

                let types = types
                    .iter()
                    .map(|&ty| self.name(ty))
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("({})", types)
            }
        }
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

    #[test]
    fn append_type_lists() {
        let e1 = TypeList::empty();
        let e2 = TypeList::single(BuiltinType::Int32);
        assert_eq!(e1.append(&e2).types(), &[BuiltinType::Int32]);

        let e1 = TypeList::single(BuiltinType::Float32);
        let e2 = TypeList::single(BuiltinType::Int32);
        assert_eq!(
            e1.append(&e2).types(),
            &[BuiltinType::Float32, BuiltinType::Int32]
        );
    }
}
