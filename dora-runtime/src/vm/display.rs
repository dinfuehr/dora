use crate::vm::{module_path, module_path_name, VM};
use dora_bytecode::{BytecodeType, BytecodeTypeArray, FunctionId, FunctionKind, TypeParamData};

pub fn display_fct(vm: &VM, fct_id: FunctionId) -> String {
    let fct = &vm.program.functions[fct_id.0 as usize];
    let mut repr = match fct.kind {
        FunctionKind::Trait(trait_id) => {
            let trait_ = &vm.program.traits[trait_id.0 as usize];
            module_path_name(vm, trait_.module_id, &trait_.name)
        }

        FunctionKind::Method => {
            let mut result = module_path(vm, fct.module_id);
            if result.is_empty() {
                result.push_str("::");
            }
            result.push_str("<extension block>");
            result
        }

        FunctionKind::Impl(impl_id) => {
            let impl_ = &vm.program.impls[impl_id.0 as usize];
            let mut result = module_path(vm, fct.module_id);
            if !result.is_empty() {
                result.push_str("::");
            }
            result.push_str("<impl");

            if !impl_.type_params.names.is_empty() {
                result.push_str("[");
                let mut first = true;
                for name in &impl_.type_params.names {
                    if !first {
                        result.push_str(", ");
                    }
                    result.push_str(name);
                    first = false;
                }
                result.push_str("]");
            }

            result.push_str(" ");
            result.push_str(&display_ty_with_type_params(
                vm,
                &impl_.trait_ty,
                &impl_.type_params,
            ));
            result.push_str(" for ");
            result.push_str(&display_ty_with_type_params(
                vm,
                &impl_.extended_ty,
                &impl_.type_params,
            ));
            result.push_str(">");
            result
        }

        FunctionKind::Function => return module_path_name(vm, fct.module_id, &fct.name),

        FunctionKind::Lambda => "lamba".into(),
    };

    repr.push_str("::");
    repr.push_str(&fct.name);
    repr
}

pub fn display_ty(vm: &VM, ty: &BytecodeType) -> String {
    let printer = BytecodeTypePrinter {
        vm,
        type_params: None,
        ty: ty.clone(),
    };

    printer.string()
}

pub fn display_ty_with_type_params(
    vm: &VM,
    ty: &BytecodeType,
    type_params: &TypeParamData,
) -> String {
    let printer = BytecodeTypePrinter {
        vm,
        type_params: Some(type_params),
        ty: ty.clone(),
    };

    printer.string()
}

struct BytecodeTypePrinter<'a> {
    vm: &'a VM,
    type_params: Option<&'a TypeParamData>,
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
                    write!(fmt, "{}", type_params.names[*idx as usize])
                } else {
                    write!(fmt, "TypeParam({})", idx)
                }
            }

            BytecodeType::This => write!(fmt, "Self"),

            BytecodeType::TypeAlias(..) => unimplemented!(),

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
            if !first {
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
