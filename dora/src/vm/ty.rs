use crate::language::ty::SourceType;
use crate::mem;
use crate::mode::MachineMode;
use crate::vm::{get_concrete_tuple_ty, VM};
use crate::vm::{specialize_enum_id_params, specialize_value_id_params, EnumLayout};

impl SourceType {
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
            SourceType::Value(sid, params) => {
                let sid = specialize_value_id_params(vm, *sid, params.clone());
                let struc = vm.value_instances.idx(sid);

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
            SourceType::Value(sid, params) => {
                let sid = specialize_value_id_params(vm, *sid, params.clone());
                let struc = vm.value_instances.idx(sid);

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
            SourceType::Unit => panic!("no machine mode for unit."),
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
            SourceType::Value(_, _) => panic!("no machine mode for value type."),
            SourceType::Trait(_, _) => MachineMode::Ptr,
            SourceType::TypeParam(_) => panic!("no machine mode for type variable."),
            SourceType::Tuple(_) => unimplemented!(),
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
