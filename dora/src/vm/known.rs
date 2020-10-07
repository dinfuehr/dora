use parking_lot::Mutex;

use crate::semck::specialize::{specialize_class_id, specialize_class_id_params};
use crate::ty::{BuiltinType, TypeList};
use crate::vm::module::ModuleId;
use crate::vm::{ClassDefId, ClassId, EnumId, FctId, TraitId, VM};

#[derive(Debug)]
pub struct KnownElements {
    pub classes: KnownClasses,
    pub modules: KnownModules,
    pub traits: KnownTraits,
    pub functions: KnownFunctions,
    pub enums: KnownEnums,

    pub byte_array_def: Mutex<Option<ClassDefId>>,
    pub int_array_def: Mutex<Option<ClassDefId>>,
    pub str_class_def: Mutex<Option<ClassDefId>>,
    pub obj_class_def: Mutex<Option<ClassDefId>>,
    pub ste_class_def: Mutex<Option<ClassDefId>>,
    pub ex_class_def: Mutex<Option<ClassDefId>>,

    pub free_object_class_def: ClassDefId,
    pub free_array_class_def: ClassDefId,
}

#[derive(Debug)]
pub struct KnownEnums {
    pub new_option: EnumId,
}

#[derive(Debug)]
pub struct KnownClasses {
    pub unit: ClassId,
    pub bool: ClassId,
    pub uint8: ClassId,
    pub char: ClassId,
    pub int32: ClassId,
    pub int64: ClassId,
    pub float32: ClassId,
    pub float64: ClassId,
    pub object: ClassId,
    pub array: ClassId,
    pub string: ClassId,
    pub string_buffer: ClassId,
    pub testing: ClassId,
    pub stacktrace: ClassId,
    pub stacktrace_element: ClassId,
}

#[derive(Debug)]
pub struct KnownModules {
    pub array: ModuleId,
    pub string: ModuleId,
    pub string_buffer: ModuleId,
}

#[derive(Debug)]
pub struct KnownTraits {
    pub equals: TraitId,
    pub comparable: TraitId,
    pub stringable: TraitId,
    pub iterator: TraitId,
    pub zero: TraitId,
}

#[derive(Debug)]
pub struct KnownFunctions {
    pub string_buffer_empty: FctId,
    pub string_buffer_append: FctId,
    pub string_buffer_to_string: FctId,
}

impl KnownElements {
    pub fn array_ty(&self, vm: &VM, element: BuiltinType) -> BuiltinType {
        let list = TypeList::single(element);
        let list_id = vm.lists.lock().insert(list);
        BuiltinType::Class(self.classes.array, list_id)
    }

    pub fn byte_array(&self, vm: &VM) -> ClassDefId {
        let mut byte_array_def = self.byte_array_def.lock();

        if let Some(cls_id) = *byte_array_def {
            cls_id
        } else {
            let type_args = TypeList::single(BuiltinType::UInt8);
            let cls_id = specialize_class_id_params(vm, self.classes.array, &type_args);
            *byte_array_def = Some(cls_id);
            cls_id
        }
    }

    pub fn int_array(&self, vm: &VM) -> ClassDefId {
        let mut int_array_def = self.int_array_def.lock();

        if let Some(cls_id) = *int_array_def {
            cls_id
        } else {
            let type_args = TypeList::single(BuiltinType::Int32);
            let cls_id = specialize_class_id_params(vm, self.classes.array, &type_args);
            *int_array_def = Some(cls_id);
            cls_id
        }
    }

    pub fn str(&self, vm: &VM) -> ClassDefId {
        let mut str_class_def = self.str_class_def.lock();

        if let Some(cls_id) = *str_class_def {
            cls_id
        } else {
            let cls_id = specialize_class_id(vm, self.classes.string);
            *str_class_def = Some(cls_id);
            cls_id
        }
    }

    pub fn obj(&self, vm: &VM) -> ClassDefId {
        let mut obj_class_def = self.obj_class_def.lock();

        if let Some(cls_id) = *obj_class_def {
            cls_id
        } else {
            let cls_id = specialize_class_id(vm, self.classes.object);
            *obj_class_def = Some(cls_id);
            cls_id
        }
    }

    pub fn stack_trace_element(&self, vm: &VM) -> ClassDefId {
        let mut ste_class_def = self.ste_class_def.lock();

        if let Some(cls_id) = *ste_class_def {
            cls_id
        } else {
            let cls_id = specialize_class_id(vm, self.classes.stacktrace_element);
            *ste_class_def = Some(cls_id);
            cls_id
        }
    }

    pub fn find_class(&self, ty: BuiltinType) -> Option<ClassId> {
        match ty {
            BuiltinType::Bool => Some(self.classes.bool),
            BuiltinType::UInt8 => Some(self.classes.uint8),
            BuiltinType::Char => Some(self.classes.char),
            BuiltinType::Int32 => Some(self.classes.int32),
            BuiltinType::Int64 => Some(self.classes.int64),
            BuiltinType::Float32 => Some(self.classes.float32),
            BuiltinType::Float64 => Some(self.classes.float64),
            _ => None,
        }
    }
}
