use crate::vm::VM;
use dora_frontend::bytecode::{BytecodeType, BytecodeTypeArray};
use dora_frontend::language::sem_analysis::{TypeParamDefinition, TypeParamId};

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
                let cls = &self.vm.program.classes[id.0 as usize];
                write!(fmt, "{}", cls.name)?;
                self.type_params(type_params, fmt)
            }
            BytecodeType::Struct(sid, type_params) => {
                let struct_ = &self.vm.program.structs[sid.0 as usize];
                write!(fmt, "{}", struct_.name)?;
                self.type_params(type_params, fmt)
            }
            BytecodeType::Trait(tid, type_params) => {
                let trait_ = &self.vm.program.traits[tid.0 as usize];
                write!(fmt, "{}", trait_.name)?;
                self.type_params(type_params, fmt)
            }
            BytecodeType::Enum(id, type_params) => {
                let enum_ = &self.vm.program.enums[id.0 as usize];
                write!(fmt, "{}", enum_.name)?;
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
