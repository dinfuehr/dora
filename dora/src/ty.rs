use std::collections::HashMap;
use std::ops::Index;
use std::sync::Arc;

use crate::mem;
use crate::semck;
use crate::vm::VM;
use crate::vm::{
    impl_matches, Class, ClassId, EnumData, EnumId, EnumLayout, Fct, ImplId, ModuleId, StructId,
    TraitId, TupleId, TypeParam, TypeParamId,
};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum SourceType {
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

    // pointer to object, only used internally
    Ptr,

    // self type
    This,

    // some class
    Class(ClassId, SourceTypeArrayId),

    // some struct
    Struct(StructId, SourceTypeArrayId),

    // some tuple
    Tuple(TupleId),

    // some trait object
    TraitObject(TraitId),

    // some module
    Module(ModuleId),

    // some type variable
    TypeParam(TypeParamId),

    // some lambda
    Lambda(LambdaId),

    // some enum
    Enum(EnumId, SourceTypeArrayId),
}

impl SourceType {
    pub fn is_error(&self) -> bool {
        match *self {
            SourceType::Error => true,
            _ => false,
        }
    }

    pub fn is_enum(&self) -> bool {
        match *self {
            SourceType::Enum(_, _) => true,
            _ => false,
        }
    }

    pub fn is_unit(&self) -> bool {
        match *self {
            SourceType::Unit => true,
            _ => false,
        }
    }

    pub fn is_self(&self) -> bool {
        match *self {
            SourceType::This => true,
            _ => false,
        }
    }

    pub fn is_cls(&self) -> bool {
        match *self {
            SourceType::Class(_, _) => true,
            _ => false,
        }
    }

    pub fn is_module(&self) -> bool {
        match *self {
            SourceType::Module(_) => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            &SourceType::Float32 | &SourceType::Float64 => true,
            _ => false,
        }
    }

    pub fn is_int32(&self) -> bool {
        match self {
            &SourceType::Int32 => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            &SourceType::Bool => true,
            _ => false,
        }
    }

    pub fn is_type_param(&self) -> bool {
        match self {
            &SourceType::TypeParam(_) => true,
            _ => false,
        }
    }

    pub fn is_tuple(&self) -> bool {
        match self {
            &SourceType::Tuple(_) => true,
            _ => false,
        }
    }

    pub fn is_struct(&self) -> bool {
        match self {
            &SourceType::Struct(_, _) => true,
            _ => false,
        }
    }

    pub fn is_primitive(&self) -> bool {
        match self {
            &SourceType::Bool
            | &SourceType::UInt8
            | &SourceType::Char
            | &SourceType::Int32
            | &SourceType::Int64
            | &SourceType::Float32
            | &SourceType::Float64 => true,
            _ => false,
        }
    }

    pub fn is_tuple_or_unit(&self) -> bool {
        match self {
            &SourceType::Tuple(_) => true,
            &SourceType::Unit => true,
            _ => false,
        }
    }

    pub fn cls_id(&self) -> Option<ClassId> {
        match *self {
            SourceType::Class(cls_id, _) => Some(cls_id),
            _ => None,
        }
    }

    pub fn primitive_struct_id(&self, vm: &VM) -> Option<StructId> {
        match *self {
            SourceType::Bool => Some(vm.known.structs.bool),
            SourceType::UInt8 => Some(vm.known.structs.uint8),
            SourceType::Char => Some(vm.known.structs.char),
            SourceType::Int32 => Some(vm.known.structs.int32),
            SourceType::Int64 => Some(vm.known.structs.int64),
            SourceType::Float32 => Some(vm.known.structs.float32),
            SourceType::Float64 => Some(vm.known.structs.float64),
            _ => None,
        }
    }

    pub fn from_cls(cls_id: ClassId, vm: &VM) -> SourceType {
        let list_id = vm
            .source_type_arrays
            .lock()
            .insert(SourceTypeArray::empty());
        SourceType::Class(cls_id, list_id)
    }

    pub fn module_id(&self) -> Option<ModuleId> {
        match *self {
            SourceType::Module(module_id) => Some(module_id),
            _ => None,
        }
    }

    pub fn enum_id(&self) -> Option<EnumId> {
        match *self {
            SourceType::Enum(enum_id, _) => Some(enum_id),
            _ => None,
        }
    }

    pub fn struct_id(&self) -> Option<StructId> {
        match *self {
            SourceType::Struct(struct_id, _) => Some(struct_id),
            _ => None,
        }
    }

    pub fn tuple_id(&self) -> Option<TupleId> {
        match *self {
            SourceType::Tuple(tuple_id) => Some(tuple_id),
            _ => None,
        }
    }

    pub fn type_param_id(&self) -> Option<TypeParamId> {
        match *self {
            SourceType::TypeParam(id) => Some(id),
            _ => None,
        }
    }

    pub fn type_params(&self, vm: &VM) -> SourceTypeArray {
        match self {
            &SourceType::Class(_, list_id)
            | &SourceType::Enum(_, list_id)
            | &SourceType::Struct(_, list_id) => vm.source_type_arrays.lock().get(list_id),
            _ => SourceTypeArray::empty(),
        }
    }

    pub fn contains_type_param(&self, vm: &VM) -> bool {
        match self {
            &SourceType::TypeParam(_) => true,

            &SourceType::Class(_, list_id) | &SourceType::Struct(_, list_id) => {
                let params = vm.source_type_arrays.lock().get(list_id);
                params.iter().any(|t| t.contains_type_param(vm))
            }

            &SourceType::Lambda(_) => unimplemented!(),

            _ => false,
        }
    }

    pub fn reference_type(&self) -> bool {
        match *self {
            SourceType::Ptr => true,
            SourceType::Class(_, _) => true,
            SourceType::TraitObject(_) => true,
            _ => false,
        }
    }

    pub fn value_type(&self) -> bool {
        match *self {
            SourceType::Unit
            | SourceType::Bool
            | SourceType::UInt8
            | SourceType::Int32
            | SourceType::Int64
            | SourceType::Float32
            | SourceType::Float64 => true,
            _ => false,
        }
    }

    pub fn subclass_from(&self, vm: &VM, ty: SourceType) -> bool {
        if !self.is_cls() {
            return false;
        }
        if !ty.is_cls() {
            return false;
        }

        let cls_id = self.cls_id().unwrap();
        let cls = vm.classes.idx(cls_id);
        let cls = cls.read();
        cls.subclass_from(vm, ty.cls_id().unwrap())
    }

    pub fn name(&self, vm: &VM) -> String {
        let writer = SourceTypePrinter {
            vm,
            type_params: None,
        };

        writer.name(self.clone())
    }

    pub fn name_with_params(&self, vm: &VM, type_params: &[TypeParam]) -> String {
        let writer = SourceTypePrinter {
            vm,
            type_params: Some(type_params),
        };

        writer.name(self.clone())
    }

    pub fn name_fct(&self, vm: &VM, fct: &Fct) -> String {
        let writer = SourceTypePrinter {
            vm,
            type_params: Some(&fct.type_params),
        };

        writer.name(self.clone())
    }

    pub fn name_cls(&self, vm: &VM, cls: &Class) -> String {
        let writer = SourceTypePrinter {
            vm,
            type_params: Some(&cls.type_params),
        };

        writer.name(self.clone())
    }

    pub fn name_enum(&self, vm: &VM, xenum: &EnumData) -> String {
        let writer = SourceTypePrinter {
            vm,
            type_params: Some(&xenum.type_params),
        };

        writer.name(self.clone())
    }

    pub fn allows(&self, vm: &VM, other: SourceType) -> bool {
        match *self {
            // allow all types for Error, there is already an error,
            // don't report too many messages for the same error
            SourceType::Error => true,

            // Any allows all other types
            SourceType::Any => true,

            SourceType::Unit
            | SourceType::Bool
            | SourceType::UInt8
            | SourceType::Char
            | SourceType::Struct(_, _) => *self == other,
            SourceType::Int32 | SourceType::Int64 | SourceType::Float32 | SourceType::Float64 => {
                *self == other
            }
            SourceType::Ptr => panic!("ptr does not allow any other types"),
            SourceType::This => unreachable!(),
            SourceType::Class(_, _) => *self == other || other.subclass_from(vm, self.clone()),
            SourceType::Tuple(tuple_id) => match other {
                SourceType::Tuple(other_tuple_id) => {
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
                        let ty = subtypes[idx].clone();
                        let other_ty = other_subtypes[idx].clone();

                        if !ty.allows(vm, other_ty) {
                            return false;
                        }
                    }

                    true
                }

                _ => false,
            },
            SourceType::TraitObject(_) => unimplemented!(),
            SourceType::Module(_) => *self == other,
            SourceType::Enum(enum_id, type_params_id) => match other {
                SourceType::Enum(other_enum_id, other_type_params_id) => {
                    enum_id == other_enum_id && type_params_id == other_type_params_id
                }

                _ => false,
            },

            SourceType::TypeParam(_) => *self == other,

            SourceType::Lambda(_) => {
                // for now expect the exact same params and return types
                // possible improvement: allow super classes for params,
                //                             sub class for return type
                *self == other
            }
        }
    }

    pub fn size(&self, vm: &VM) -> i32 {
        match *self {
            SourceType::Error => panic!("no size for error."),
            SourceType::Unit => 0,
            SourceType::Bool => 1,
            SourceType::UInt8 => 1,
            SourceType::Char => 4,
            SourceType::Int32 => 4,
            SourceType::Int64 => 8,
            SourceType::Float32 => 4,
            SourceType::Float64 => 8,
            SourceType::Enum(eid, list_id) => {
                let params = vm.source_type_arrays.lock().get(list_id);
                let enum_def_id = semck::specialize::specialize_enum_id_params(vm, eid, params);
                let xenum = vm.enum_defs.idx(enum_def_id);

                match xenum.layout {
                    EnumLayout::Int => SourceType::Int32.size(vm),
                    EnumLayout::Ptr | EnumLayout::Tagged => SourceType::Ptr.size(vm),
                }
            }
            SourceType::This => panic!("no size for Self."),
            SourceType::Any => panic!("no size for Any."),
            SourceType::Class(_, _)
            | SourceType::Module(_)
            | SourceType::Lambda(_)
            | SourceType::Ptr => mem::ptr_width(),
            SourceType::Struct(sid, list_id) => {
                let params = vm.source_type_arrays.lock().get(list_id);
                let sid = semck::specialize::specialize_struct_id_params(vm, sid, params);
                let struc = vm.struct_defs.idx(sid);

                struc.size
            }
            SourceType::TraitObject(_) => mem::ptr_width(),
            SourceType::TypeParam(_) => panic!("no size for type variable."),
            SourceType::Tuple(tuple_id) => vm.tuples.lock().get_tuple(tuple_id).size(),
        }
    }

    pub fn align(&self, vm: &VM) -> i32 {
        match *self {
            SourceType::Error => panic!("no alignment for error."),
            SourceType::Unit => 0,
            SourceType::Bool => 1,
            SourceType::UInt8 => 1,
            SourceType::Char => 4,
            SourceType::Int32 => 4,
            SourceType::Int64 => 8,
            SourceType::Float32 => 4,
            SourceType::Float64 => 8,
            SourceType::This => panic!("no alignment for Self."),
            SourceType::Any => panic!("no alignment for Any."),
            SourceType::Enum(eid, list_id) => {
                let params = vm.source_type_arrays.lock().get(list_id);
                let enum_def_id = semck::specialize::specialize_enum_id_params(vm, eid, params);
                let xenum = vm.enum_defs.idx(enum_def_id);

                match xenum.layout {
                    EnumLayout::Int => SourceType::Int32.align(vm),
                    EnumLayout::Ptr | EnumLayout::Tagged => SourceType::Ptr.align(vm),
                }
            }
            SourceType::Class(_, _)
            | SourceType::Module(_)
            | SourceType::Lambda(_)
            | SourceType::Ptr => mem::ptr_width(),
            SourceType::Struct(sid, list_id) => {
                let params = vm.source_type_arrays.lock().get(list_id);
                let sid = semck::specialize::specialize_struct_id_params(vm, sid, params);
                let struc = vm.struct_defs.idx(sid);

                struc.align
            }
            SourceType::TraitObject(_) => mem::ptr_width(),
            SourceType::TypeParam(_) => panic!("no alignment for type variable."),
            SourceType::Tuple(tuple_id) => vm.tuples.lock().get_tuple(tuple_id).align(),
        }
    }

    pub fn mode(&self) -> MachineMode {
        match *self {
            SourceType::Error => panic!("no machine mode for error."),
            SourceType::Unit => panic!("no machine mode for ()."),
            SourceType::Bool => MachineMode::Int8,
            SourceType::UInt8 => MachineMode::Int8,
            SourceType::Char => MachineMode::Int32,
            SourceType::Int32 => MachineMode::Int32,
            SourceType::Int64 => MachineMode::Int64,
            SourceType::Float32 => MachineMode::Float32,
            SourceType::Float64 => MachineMode::Float64,
            SourceType::Enum(_, _) => MachineMode::Int32,
            SourceType::This => panic!("no machine mode for Self."),
            SourceType::Any => panic!("no machine mode for Any."),
            SourceType::Class(_, _)
            | SourceType::Module(_)
            | SourceType::Lambda(_)
            | SourceType::Ptr => MachineMode::Ptr,
            SourceType::Struct(_, _) => panic!("no machine mode for struct."),
            SourceType::TraitObject(_) => MachineMode::Ptr,
            SourceType::TypeParam(_) => panic!("no machine mode for type variable."),
            SourceType::Tuple(_) => unimplemented!(),
        }
    }

    pub fn is_defined_type(&self, vm: &VM) -> bool {
        match *self {
            SourceType::Error | SourceType::This | SourceType::Any | SourceType::Ptr => false,
            SourceType::Unit
            | SourceType::Bool
            | SourceType::UInt8
            | SourceType::Char
            | SourceType::Int32
            | SourceType::Int64
            | SourceType::Float32
            | SourceType::Float64
            | SourceType::Module(_)
            | SourceType::TraitObject(_)
            | SourceType::Lambda(_)
            | SourceType::TypeParam(_) => true,
            SourceType::Enum(_, list_id)
            | SourceType::Class(_, list_id)
            | SourceType::Struct(_, list_id) => {
                let params = vm.source_type_arrays.lock().get(list_id);

                for param in params.iter() {
                    if !param.is_defined_type(vm) {
                        return false;
                    }
                }

                true
            }
            SourceType::Tuple(tuple_id) => {
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
            SourceType::Error | SourceType::This | SourceType::Any => false,
            SourceType::Unit
            | SourceType::Bool
            | SourceType::UInt8
            | SourceType::Char
            | SourceType::Int32
            | SourceType::Int64
            | SourceType::Float32
            | SourceType::Float64
            | SourceType::Module(_)
            | SourceType::Ptr
            | SourceType::TraitObject(_) => true,
            SourceType::Class(_, list_id)
            | SourceType::Enum(_, list_id)
            | SourceType::Struct(_, list_id) => {
                let params = vm.source_type_arrays.lock().get(list_id);

                for param in params.iter() {
                    if !param.is_concrete_type(vm) {
                        return false;
                    }
                }

                true
            }
            SourceType::Tuple(tuple_id) => vm.tuples.lock().get_tuple(tuple_id).is_concrete_type(),
            SourceType::Lambda(_) => unimplemented!(),
            SourceType::TypeParam(_) => false,
        }
    }
}

pub fn type_names(vm: &VM, types: &[SourceType]) -> String {
    let mut result = String::new();
    result.push('[');
    let mut first = true;
    for ty in types {
        if !first {
            result.push_str(", ");
        }
        result.push_str(&ty.name(vm));
        first = false;
    }
    result.push(']');
    result
}

pub fn implements_trait(
    vm: &VM,
    check_ty: SourceType,
    check_type_param_defs: &[TypeParam],
    trait_id: TraitId,
) -> bool {
    match check_ty {
        SourceType::Tuple(_)
        | SourceType::Unit
        | SourceType::Module(_)
        | SourceType::TraitObject(_)
        | SourceType::Lambda(_) => false,

        SourceType::Enum(enum_id, _) => {
            let xenum = vm.enums[enum_id].read();
            check_impls(vm, check_ty, check_type_param_defs, trait_id, &xenum.impls).is_some()
        }

        SourceType::Bool
        | SourceType::UInt8
        | SourceType::Char
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64 => {
            if vm.known.traits.zero == trait_id {
                return true;
            }

            let struct_id = check_ty
                .primitive_struct_id(vm)
                .expect("primitive expected");
            let xstruct = vm.structs.idx(struct_id);
            let xstruct = xstruct.read();

            check_impls(
                vm,
                check_ty,
                check_type_param_defs,
                trait_id,
                &xstruct.impls,
            )
            .is_some()
        }

        SourceType::Struct(struct_id, _) => {
            let xstruct = vm.structs.idx(struct_id);
            let xstruct = xstruct.read();

            check_impls(
                vm,
                check_ty,
                check_type_param_defs,
                trait_id,
                &xstruct.impls,
            )
            .is_some()
        }

        SourceType::Class(_, _) => {
            if vm.known.traits.zero == trait_id && !check_ty.is_cls() {
                return true;
            }

            let cls_id = check_ty.cls_id().expect("class expected");
            let cls = vm.classes.idx(cls_id);
            let cls = cls.read();

            check_impls(
                vm,
                check_ty.clone(),
                check_type_param_defs,
                trait_id,
                &cls.impls,
            )
            .is_some()
        }

        SourceType::TypeParam(tp_id) => {
            let tp = &check_type_param_defs[tp_id.to_usize()];
            tp.trait_bounds.contains(&trait_id)
        }

        SourceType::Error | SourceType::Ptr | SourceType::This | SourceType::Any => unreachable!(),
    }
}

pub fn find_impl(
    vm: &VM,
    check_ty: SourceType,
    check_type_param_defs: &[TypeParam],
    trait_id: TraitId,
) -> Option<ImplId> {
    match check_ty {
        SourceType::Tuple(_)
        | SourceType::Unit
        | SourceType::Module(_)
        | SourceType::TraitObject(_)
        | SourceType::Lambda(_) => None,

        SourceType::Enum(enum_id, _) => {
            let xenum = vm.enums[enum_id].read();
            check_impls(vm, check_ty, check_type_param_defs, trait_id, &xenum.impls)
        }

        SourceType::Bool
        | SourceType::UInt8
        | SourceType::Char
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64 => {
            let struct_id = check_ty
                .primitive_struct_id(vm)
                .expect("primitive expected");
            let xstruct = vm.structs.idx(struct_id);
            let xstruct = xstruct.read();

            check_impls(
                vm,
                check_ty,
                check_type_param_defs,
                trait_id,
                &xstruct.impls,
            )
        }

        SourceType::Struct(struct_id, _) => {
            let xstruct = vm.structs.idx(struct_id);
            let xstruct = xstruct.read();

            check_impls(
                vm,
                check_ty,
                check_type_param_defs,
                trait_id,
                &xstruct.impls,
            )
        }

        SourceType::Class(_, _) => {
            let cls_id = check_ty.cls_id().expect("class expected");
            let cls = vm.classes.idx(cls_id);
            let cls = cls.read();

            check_impls(
                vm,
                check_ty.clone(),
                check_type_param_defs,
                trait_id,
                &cls.impls,
            )
        }

        SourceType::TypeParam(_) => unreachable!(),
        SourceType::Error | SourceType::Ptr | SourceType::This | SourceType::Any => unreachable!(),
    }
}

pub fn check_impls(
    vm: &VM,
    check_ty: SourceType,
    check_type_param_defs: &[TypeParam],
    trait_id: TraitId,
    impls: &[ImplId],
) -> Option<ImplId> {
    for &impl_id in impls {
        let ximpl = &vm.impls[impl_id];
        let ximpl = ximpl.read();

        if ximpl.trait_id != Some(trait_id) {
            continue;
        }

        if impl_matches(vm, check_ty.clone(), check_type_param_defs, impl_id).is_some() {
            return Some(impl_id);
        }
    }

    None
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
pub struct SourceTypeArrayId(u32);

impl SourceTypeArrayId {
    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

impl From<usize> for SourceTypeArrayId {
    fn from(data: usize) -> SourceTypeArrayId {
        assert!(data < u32::max_value() as usize);
        SourceTypeArrayId(data as u32)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum SourceTypeArray {
    Empty,
    List(Arc<Vec<SourceType>>),
}

impl SourceTypeArray {
    pub fn empty() -> SourceTypeArray {
        SourceTypeArray::Empty
    }

    pub fn single(ty: SourceType) -> SourceTypeArray {
        SourceTypeArray::List(Arc::new(vec![ty]))
    }

    pub fn with(type_params: Vec<SourceType>) -> SourceTypeArray {
        if type_params.len() == 0 {
            SourceTypeArray::Empty
        } else {
            SourceTypeArray::List(Arc::new(type_params))
        }
    }

    pub fn connect(&self, other: &SourceTypeArray) -> SourceTypeArray {
        if self.is_empty() {
            return other.clone();
        }

        if other.is_empty() {
            return self.clone();
        }

        let mut params = self.types().to_vec();
        params.extend_from_slice(other.types());

        SourceTypeArray::List(Arc::new(params))
    }

    pub fn types(&self) -> &[SourceType] {
        match self {
            SourceTypeArray::Empty => &[],
            SourceTypeArray::List(ref params) => (**params).as_slice(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        match self {
            &SourceTypeArray::Empty => 0,
            &SourceTypeArray::List(ref params) => params.len(),
        }
    }

    pub fn iter(&self) -> SourceTypeArrayIter {
        SourceTypeArrayIter {
            params: self,
            idx: 0,
        }
    }

    pub fn name(&self, vm: &VM) -> String {
        let mut result = String::new();
        let mut first = true;
        result.push('[');

        for ty in self.iter() {
            if !first {
                result.push_str(", ");
            }
            result.push_str(&ty.name(vm));
            first = false;
        }

        result.push(']');

        result
    }
}

impl Index<usize> for SourceTypeArray {
    type Output = SourceType;

    fn index(&self, idx: usize) -> &SourceType {
        match self {
            &SourceTypeArray::Empty => panic!("type list index out-of-bounds"),
            &SourceTypeArray::List(ref params) => &params[idx],
        }
    }
}

pub struct SourceTypeArrayIter<'a> {
    params: &'a SourceTypeArray,
    idx: usize,
}

impl<'a> Iterator for SourceTypeArrayIter<'a> {
    type Item = SourceType;

    fn next(&mut self) -> Option<SourceType> {
        match self.params {
            &SourceTypeArray::Empty => None,

            &SourceTypeArray::List(ref params) => {
                if self.idx < params.len() {
                    let ret = params[self.idx].clone();
                    self.idx += 1;

                    Some(ret)
                } else {
                    None
                }
            }
        }
    }
}

pub struct SourceTypeArrays {
    lists: HashMap<SourceTypeArray, SourceTypeArrayId>,
    values: Vec<SourceTypeArray>,
    next_id: usize,
}

impl SourceTypeArrays {
    pub fn new() -> SourceTypeArrays {
        SourceTypeArrays {
            lists: HashMap::new(),
            values: Vec::new(),
            next_id: 0,
        }
    }

    pub fn insert(&mut self, list: SourceTypeArray) -> SourceTypeArrayId {
        if let Some(&val) = self.lists.get(&list) {
            return val;
        }

        let id: SourceTypeArrayId = self.next_id.into();
        self.lists.insert(list.clone(), id);

        self.values.push(list);

        self.next_id += 1;

        id
    }

    pub fn get(&self, id: SourceTypeArrayId) -> SourceTypeArray {
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

    pub fn insert(&mut self, params: Vec<SourceType>, ret: SourceType) -> LambdaId {
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

struct SourceTypePrinter<'a> {
    vm: &'a VM,
    type_params: Option<&'a [TypeParam]>,
}

impl<'a> SourceTypePrinter<'a> {
    pub fn name(&self, ty: SourceType) -> String {
        match ty {
            SourceType::Error => "<error>".into(),
            SourceType::Any => "Any".into(),
            SourceType::Unit => "()".into(),
            SourceType::UInt8 => "UInt8".into(),
            SourceType::Char => "Char".into(),
            SourceType::Int32 => "Int32".into(),
            SourceType::Int64 => "Int64".into(),
            SourceType::Float32 => "Float32".into(),
            SourceType::Float64 => "Float64".into(),
            SourceType::Bool => "Bool".into(),
            SourceType::Ptr => panic!("type Ptr only for internal use."),
            SourceType::This => "Self".into(),
            SourceType::Class(id, list_id) => {
                let params = self.vm.source_type_arrays.lock().get(list_id);
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
            SourceType::Struct(sid, list_id) => {
                let struc = self.vm.structs.idx(sid);
                let struc = struc.read();
                let name = struc.name;
                let name = self.vm.interner.str(name).to_string();

                let params = self.vm.source_type_arrays.lock().get(list_id);

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
            SourceType::TraitObject(tid) => {
                let xtrait = self.vm.traits[tid].read();
                self.vm.interner.str(xtrait.name).to_string()
            }
            SourceType::Enum(id, list_id) => {
                let xenum = self.vm.enums[id].read();
                let name = self.vm.interner.str(xenum.name).to_string();

                let params = self.vm.source_type_arrays.lock().get(list_id);

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
            SourceType::Module(id) => {
                let module = self.vm.modules.idx(id);
                let module = module.read();
                self.vm.interner.str(module.name).to_string()
            }
            SourceType::TypeParam(idx) => {
                if let Some(type_params) = self.type_params {
                    self.vm
                        .interner
                        .str(type_params[idx.to_usize()].name)
                        .to_string()
                } else {
                    format!("TypeParam({})", idx.to_usize())
                }
            }

            SourceType::Lambda(id) => {
                let lambda = self.vm.lambda_types.lock().get(id);
                let params = lambda
                    .params
                    .iter()
                    .map(|ty| self.name(ty.clone()))
                    .collect::<Vec<_>>()
                    .join(", ");
                let ret = self.name(lambda.ret.clone());

                format!("({}) -> {}", params, ret)
            }

            SourceType::Tuple(tuple_id) => {
                let types = self.vm.tuples.lock().get(tuple_id);

                let types = types
                    .iter()
                    .map(|ty| self.name(ty.clone()))
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("({})", types)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LambdaType {
    params: Vec<SourceType>,
    ret: SourceType,
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
        assert_eq!(MachineMode::Int8, SourceType::Bool.mode());
        assert_eq!(MachineMode::Int32, SourceType::Int32.mode());
        assert_eq!(MachineMode::Ptr, SourceType::Ptr.mode());
    }

    #[test]
    #[should_panic]
    fn mode_for_unit() {
        assert_eq!(MachineMode::Ptr, SourceType::Unit.mode());
    }

    #[test]
    fn append_type_lists() {
        let e1 = SourceTypeArray::empty();
        let e2 = SourceTypeArray::single(SourceType::Int32);
        assert_eq!(e1.connect(&e2).types(), &[SourceType::Int32]);

        let e1 = SourceTypeArray::single(SourceType::Float32);
        let e2 = SourceTypeArray::single(SourceType::Int32);
        assert_eq!(
            e1.connect(&e2).types(),
            &[SourceType::Float32, SourceType::Int32]
        );
    }
}
