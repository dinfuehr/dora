use crate::bytecode::{BytecodeType, BytecodeTypeArray};
use crate::language::sem_analysis::{TypeParamDefinition, TypeParamId};
use crate::vm::{StructDefinitionId, VM};

pub fn display_ty(vm: &VM, ty: &BytecodeType) -> String {
    let printer = BytecodeTypePrinter {
        vm,
        type_params: None,
        ty: ty.clone(),
    };

    printer.string()
}

struct BytecodeTypePrinter<'a> {
    vm: &'a VM,
    type_params: Option<&'a TypeParamDefinition>,
    ty: BytecodeType,
}

impl<'a> BytecodeTypePrinter<'a> {
    fn string(&self) -> String {
        format!("{}", self)
    }

    fn name(&self, ty: &BytecodeType, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match ty {
            BytecodeType::Unit => write!(fmt, "()"),
            BytecodeType::UInt8 => write!(fmt, "UInt8"),
            BytecodeType::Char => write!(fmt, "Char"),
            BytecodeType::Int32 => write!(fmt, "Int32"),
            BytecodeType::Int64 => write!(fmt, "Int64"),
            BytecodeType::Float32 => write!(fmt, "Float32"),
            BytecodeType::Float64 => write!(fmt, "Float64"),
            BytecodeType::Bool => write!(fmt, "Bool"),
            BytecodeType::Ptr => write!(fmt, "Ptr"),
            BytecodeType::Class(id, type_params) => {
                let cls = self.vm.classes.idx(*id);
                let cls = cls.read();
                let name = self.vm.interner.str(cls.name).to_string();
                write!(fmt, "{}", name)?;
                self.type_params(type_params, fmt)
            }
            BytecodeType::Struct(sid, type_params) => {
                let struc = self.vm.structs.idx(*sid);
                let struc = struc.read();
                let name = struc.name;
                let name = self.vm.interner.str(name).to_string();
                write!(fmt, "{}", name)?;
                self.type_params(type_params, fmt)
            }
            BytecodeType::Trait(tid, type_params) => {
                let trait_ = self.vm.traits.idx(*tid);
                let trait_ = trait_.read();
                let name = self.vm.interner.str(trait_.name).to_string();
                write!(fmt, "{}", name)?;
                self.type_params(type_params, fmt)
            }
            BytecodeType::Enum(id, type_params) => {
                let enum_ = self.vm.enums.idx(*id);
                let enum_ = enum_.read();
                let name = self.vm.interner.str(enum_.name).to_string();
                write!(fmt, "{}", name)?;
                self.type_params(type_params, fmt)
            }

            BytecodeType::TypeParam(idx) => {
                if let Some(type_params) = self.type_params {
                    let name = self
                        .vm
                        .interner
                        .str(type_params.name(TypeParamId(*idx as usize)))
                        .to_string();
                    write!(fmt, "{}", name)
                } else {
                    write!(fmt, "TypeParam({})", idx)
                }
            }

            BytecodeType::Lambda(params, return_type) => {
                write!(fmt, "(")?;
                self.type_list(params, fmt)?;
                write!(fmt, ") -> ")?;
                self.name(return_type, fmt)
            }

            BytecodeType::Tuple(subtypes) => {
                write!(fmt, "(")?;
                self.type_list(subtypes, fmt)?;
                write!(fmt, ") -> ")
            }
        }
    }

    fn type_params(
        &self,
        types: &BytecodeTypeArray,
        fmt: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        if types.is_empty() {
            return Ok(());
        }

        write!(fmt, "[")?;
        self.type_list(types, fmt)?;
        write!(fmt, "]")
    }

    fn type_list(
        &self,
        types: &BytecodeTypeArray,
        fmt: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        let mut first = true;
        for ty in types.iter() {
            if first {
                write!(fmt, ", ")?;
            }
            self.name(&ty, fmt)?;
            first = false;
        }

        Ok(())
    }
}

impl<'a> std::fmt::Display for BytecodeTypePrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.name(&self.ty, f)
    }
}

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
