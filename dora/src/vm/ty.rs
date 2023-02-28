use crate::bytecode::{BytecodeType, BytecodeTypeArray};
use crate::language::sem_analysis::{TypeParamDefinition, TypeParamId};
use crate::vm::{class_definition_name, enum_definition_name, StructDefinitionId, VM};

fn primitive_struct_id(sa: &VM, ty: &BytecodeType) -> Option<StructDefinitionId> {
    match ty {
        BytecodeType::Bool => Some(sa.known.structs.bool()),
        BytecodeType::UInt8 => Some(sa.known.structs.uint8()),
        BytecodeType::Char => Some(sa.known.structs.char()),
        BytecodeType::Int32 => Some(sa.known.structs.int32()),
        BytecodeType::Int64 => Some(sa.known.structs.int64()),
        BytecodeType::Float32 => Some(sa.known.structs.float32()),
        BytecodeType::Float64 => Some(sa.known.structs.float64()),
        _ => None,
    }
}

pub fn path_for_type(vm: &VM, ty: BytecodeType) -> String {
    if let BytecodeType::Enum(enum_id, _) = ty {
        let enum_ = &vm.enums[enum_id];
        let enum_ = enum_.read();
        enum_definition_name(&*enum_, vm)
    } else if let BytecodeType::Class(cls_id, _) = ty {
        let cls = vm.classes.idx(cls_id);
        let cls = cls.read();
        class_definition_name(&*cls, vm)
    } else if let BytecodeType::Struct(struct_id, _) = ty {
        let struct_ = vm.structs.idx(struct_id);
        let struct_ = struct_.read();
        struct_.name_vm(vm)
    } else if let Some(struct_id) = primitive_struct_id(vm, &ty) {
        let struct_ = vm.structs.idx(struct_id);
        let struct_ = struct_.read();
        struct_.name_vm(vm)
    } else if ty.is_tuple() || ty.is_unit() {
        unimplemented!()
    } else {
        unreachable!()
    }
}

pub fn display_ty(vm: &VM, ty: &BytecodeType) -> String {
    let printer = BytecodeTypePrinter {
        vm,
        type_params: None,
    };

    printer.name(ty.clone())
}

pub fn display_tuple(vm: &VM, types: &BytecodeTypeArray) -> String {
    let mut result = String::new();
    let mut first = true;
    result.push('(');

    for ty in types.iter() {
        if !first {
            result.push_str(", ");
        }
        result.push_str(&display_ty(vm, &ty));
        first = false;
    }

    result.push(')');

    result
}

struct BytecodeTypePrinter<'a> {
    vm: &'a VM,
    type_params: Option<&'a TypeParamDefinition>,
}

impl<'a> BytecodeTypePrinter<'a> {
    fn name(&self, ty: BytecodeType) -> String {
        match ty {
            BytecodeType::Unit => "()".into(),
            BytecodeType::UInt8 => "UInt8".into(),
            BytecodeType::Char => "Char".into(),
            BytecodeType::Int32 => "Int32".into(),
            BytecodeType::Int64 => "Int64".into(),
            BytecodeType::Float32 => "Float32".into(),
            BytecodeType::Float64 => "Float64".into(),
            BytecodeType::Bool => "Bool".into(),
            BytecodeType::Ptr => panic!("type Ptr only for internal use."),
            BytecodeType::Class(id, type_params) => {
                let cls = self.vm.classes.idx(id);
                let cls = cls.read();
                let name = self.vm.interner.str(cls.name).to_string();
                self.with_type_params(name, &type_params)
            }
            BytecodeType::Struct(sid, type_params) => {
                let struc = self.vm.structs.idx(sid);
                let struc = struc.read();
                let name = struc.name;
                let name = self.vm.interner.str(name).to_string();
                self.with_type_params(name, &type_params)
            }
            BytecodeType::Trait(tid, type_params) => {
                let trait_ = self.vm.traits[tid].read();
                let name = self.vm.interner.str(trait_.name).to_string();
                self.with_type_params(name, &type_params)
            }
            BytecodeType::Enum(id, type_params) => {
                let enum_ = self.vm.enums[id].read();
                let name = self.vm.interner.str(enum_.name).to_string();
                self.with_type_params(name, &type_params)
            }

            BytecodeType::TypeParam(idx) => {
                if let Some(type_params) = self.type_params {
                    self.vm
                        .interner
                        .str(type_params.name(TypeParamId(idx as usize)))
                        .to_string()
                } else {
                    format!("TypeParam({})", idx)
                }
            }

            BytecodeType::Lambda(params, return_type) => {
                let params = self.type_list(&params);
                let ret = self.name(*return_type);
                format!("({}) -> {}", params, ret)
            }

            BytecodeType::Tuple(subtypes) => {
                let types = self.type_list(&subtypes);
                format!("({})", types)
            }
        }
    }

    fn with_type_params(&self, name: String, types: &BytecodeTypeArray) -> String {
        if types.is_empty() {
            return name;
        }

        let params = types
            .iter()
            .map(|ty| self.name(ty))
            .collect::<Vec<_>>()
            .join(", ");

        format!("{}[{}]", &name, params)
    }

    fn type_list(&self, types: &BytecodeTypeArray) -> String {
        types
            .iter()
            .map(|ty| self.name(ty))
            .collect::<Vec<_>>()
            .join(", ")
    }
}
