use crate::language::sem_analysis::TypeParamDefinition;
use crate::language::ty::{SourceType, SourceTypeArray};
use crate::mem;
use crate::mode::MachineMode;
use crate::vm::{
    class_definition_name, enum_definition_name, get_concrete_tuple_ty, specialize_enum_id_params,
    specialize_struct_id_params, EnumLayout, FctDefinition, StructDefinitionId, VM,
};

impl SourceType {
    pub fn name_vm(&self, vm: &VM) -> String {
        let writer = SourceTypePrinter {
            vm,
            type_params: None,
        };

        writer.name(self.clone())
    }

    pub fn name_fct_vm(&self, vm: &VM, fct: &FctDefinition) -> String {
        let writer = SourceTypePrinter {
            vm,
            type_params: Some(&fct.type_params),
        };

        writer.name(self.clone())
    }

    pub fn primitive_struct_id_vm(&self, sa: &VM) -> Option<StructDefinitionId> {
        match self {
            SourceType::Bool => Some(sa.known.structs.bool()),
            SourceType::UInt8 => Some(sa.known.structs.uint8()),
            SourceType::Char => Some(sa.known.structs.char()),
            SourceType::Int32 => Some(sa.known.structs.int32()),
            SourceType::Int64 => Some(sa.known.structs.int64()),
            SourceType::Float32 => Some(sa.known.structs.float32()),
            SourceType::Float64 => Some(sa.known.structs.float64()),
            _ => None,
        }
    }

    pub fn size(&self, vm: &VM) -> i32 {
        match self {
            SourceType::Error => panic!("no size for error."),
            SourceType::Unit => 0,
            SourceType::Bool => 1,
            SourceType::UInt8 => 1,
            SourceType::Char => 4,
            SourceType::Int32 => 4,
            SourceType::Int64 => 8,
            SourceType::Float32 => 4,
            SourceType::Float64 => 8,
            SourceType::Enum(eid, params) => {
                let enum_def_id = specialize_enum_id_params(vm, *eid, params.clone());
                let enum_ = vm.enum_instances.idx(enum_def_id);

                match enum_.layout {
                    EnumLayout::Int => SourceType::Int32.size(vm),
                    EnumLayout::Ptr | EnumLayout::Tagged => SourceType::Ptr.size(vm),
                }
            }
            SourceType::This => panic!("no size for Self."),
            SourceType::Any => panic!("no size for Any."),
            SourceType::Class(_, _) | SourceType::Lambda(_, _) | SourceType::Ptr => {
                mem::ptr_width()
            }
            SourceType::Struct(sid, params) => {
                let sid = specialize_struct_id_params(vm, *sid, params.clone());
                let struc = vm.struct_instances.idx(sid);

                struc.size
            }
            SourceType::Trait(_, _) => mem::ptr_width(),
            SourceType::TypeParam(_) => panic!("no size for type variable."),
            SourceType::Tuple(_) => get_concrete_tuple_ty(vm, self).size(),
        }
    }

    pub fn align(&self, vm: &VM) -> i32 {
        match self {
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
            SourceType::Enum(eid, params) => {
                let enum_def_id = specialize_enum_id_params(vm, *eid, params.clone());
                let enum_ = vm.enum_instances.idx(enum_def_id);

                match enum_.layout {
                    EnumLayout::Int => SourceType::Int32.align(vm),
                    EnumLayout::Ptr | EnumLayout::Tagged => SourceType::Ptr.align(vm),
                }
            }
            SourceType::Class(_, _) | SourceType::Lambda(_, _) | SourceType::Ptr => {
                mem::ptr_width()
            }
            SourceType::Struct(sid, params) => {
                let sid = specialize_struct_id_params(vm, *sid, params.clone());
                let struc = vm.struct_instances.idx(sid);

                struc.align
            }
            SourceType::Trait(_, _) => mem::ptr_width(),
            SourceType::TypeParam(_) => panic!("no alignment for type variable."),
            SourceType::Tuple(_) => get_concrete_tuple_ty(vm, self).align(),
        }
    }

    pub fn mode(&self) -> MachineMode {
        match self {
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
            SourceType::Class(_, _) | SourceType::Lambda(_, _) | SourceType::Ptr => {
                MachineMode::Ptr
            }
            SourceType::Struct(_, _) => panic!("no machine mode for struct."),
            SourceType::Trait(_, _) => MachineMode::Ptr,
            SourceType::TypeParam(_) => panic!("no machine mode for type variable."),
            SourceType::Tuple(_) => unimplemented!(),
        }
    }
}

impl SourceTypeArray {
    pub fn tuple_name_vm(&self, vm: &VM) -> String {
        let mut result = String::new();
        let mut first = true;
        result.push('(');

        for ty in self.iter() {
            if !first {
                result.push_str(", ");
            }
            result.push_str(&ty.name_vm(vm));
            first = false;
        }

        result.push(')');

        result
    }
}

pub fn path_for_type(vm: &VM, ty: SourceType) -> String {
    if let Some(enum_id) = ty.enum_id() {
        let enum_ = &vm.enums[enum_id];
        let enum_ = enum_.read();
        enum_definition_name(&*enum_, vm)
    } else if let Some(cls_id) = ty.cls_id() {
        let cls = vm.classes.idx(cls_id);
        let cls = cls.read();
        class_definition_name(&*cls, vm)
    } else if let Some(struct_id) = ty.struct_id() {
        let struct_ = vm.structs.idx(struct_id);
        let struct_ = struct_.read();
        struct_.name_vm(vm)
    } else if let Some(struct_id) = ty.primitive_struct_id_vm(vm) {
        let struct_ = vm.structs.idx(struct_id);
        let struct_ = struct_.read();
        struct_.name_vm(vm)
    } else if ty.is_tuple_or_unit() {
        unimplemented!()
    } else {
        unreachable!()
    }
}

struct SourceTypePrinter<'a> {
    vm: &'a VM,
    type_params: Option<&'a TypeParamDefinition>,
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
            SourceType::Class(id, type_params) => {
                let cls = self.vm.classes.idx(id);
                let cls = cls.read();
                let base = self.vm.interner.str(cls.name);

                if type_params.len() == 0 {
                    base.to_string()
                } else {
                    let params = type_params
                        .iter()
                        .map(|ty| self.name(ty))
                        .collect::<Vec<_>>()
                        .join(", ");

                    format!("{}[{}]", base, params)
                }
            }
            SourceType::Struct(sid, type_params) => {
                let struc = self.vm.structs.idx(sid);
                let struc = struc.read();
                let name = struc.name;
                let name = self.vm.interner.str(name).to_string();

                if type_params.len() == 0 {
                    name
                } else {
                    let params = type_params
                        .iter()
                        .map(|ty| self.name(ty))
                        .collect::<Vec<_>>()
                        .join(", ");

                    format!("{}[{}]", name, params)
                }
            }
            SourceType::Trait(tid, type_params) => {
                let trait_ = self.vm.traits[tid].read();
                let name = self.vm.interner.str(trait_.name).to_string();

                if type_params.len() == 0 {
                    name
                } else {
                    let params = type_params
                        .iter()
                        .map(|ty| self.name(ty))
                        .collect::<Vec<_>>()
                        .join(", ");

                    format!("{}[{}]", name, params)
                }
            }
            SourceType::Enum(id, type_params) => {
                let enum_ = self.vm.enums[id].read();
                let name = self.vm.interner.str(enum_.name).to_string();

                if type_params.len() == 0 {
                    name
                } else {
                    let params = type_params
                        .iter()
                        .map(|ty| self.name(ty))
                        .collect::<Vec<_>>()
                        .join(", ");

                    format!("{}[{}]", name, params)
                }
            }

            SourceType::TypeParam(idx) => {
                if let Some(type_params) = self.type_params {
                    self.vm.interner.str(type_params.name(idx)).to_string()
                } else {
                    format!("TypeParam({})", idx.to_usize())
                }
            }

            SourceType::Lambda(params, return_type) => {
                let params = params
                    .iter()
                    .map(|ty| self.name(ty.clone()))
                    .collect::<Vec<_>>()
                    .join(", ");
                let ret = self.name(*return_type);

                format!("({}) -> {}", params, ret)
            }

            SourceType::Tuple(subtypes) => {
                let types = subtypes
                    .iter()
                    .map(|ty| self.name(ty.clone()))
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("({})", types)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::language::ty::SourceType;
    use crate::mem;
    use crate::mode::MachineMode;

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
}
