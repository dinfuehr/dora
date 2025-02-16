use crate::{
    BytecodeTraitType, BytecodeType, BytecodeTypeArray, FunctionId, FunctionKind, ModuleId,
    Program, TypeParamData,
};

pub fn display_fct(prog: &Program, fct_id: FunctionId) -> String {
    let fct = prog.fct(fct_id);
    let mut container_type_params = 0;
    let mut repr = match fct.kind {
        FunctionKind::Trait(trait_id) => {
            let trait_ = prog.trait_(trait_id);
            module_path_name(prog, trait_.module_id, &trait_.name)
        }

        FunctionKind::Extension(extension_id) => {
            let extension = prog.extension(extension_id);
            container_type_params = extension.type_params.names.len();
            let mut result = module_path(prog, fct.module_id);
            if !result.is_empty() {
                result.push_str("::");
            }
            result.push_str("<impl");

            if !extension.type_params.names.is_empty() {
                result.push_str("[");
                let mut first = true;
                for name in &extension.type_params.names {
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
                prog,
                &extension.extended_ty,
                &extension.type_params,
            ));
            result.push_str(">");

            result
        }

        FunctionKind::Impl(impl_id) => {
            let impl_ = prog.impl_(impl_id);
            container_type_params = impl_.type_params.names.len();
            let mut result = module_path(prog, fct.module_id);
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
            result.push_str(&display_trait_ty_with_type_params(
                prog,
                &impl_.trait_ty,
                &impl_.type_params,
            ));
            result.push_str(" for ");
            result.push_str(&display_ty_with_type_params(
                prog,
                &impl_.extended_ty,
                &impl_.type_params,
            ));
            result.push_str(">");
            result
        }

        FunctionKind::Function => return module_path_name(prog, fct.module_id, &fct.name),

        FunctionKind::Lambda => "lamba".into(),
    };

    repr.push_str("::");
    repr.push_str(&fct.name);

    if fct.type_params.names.len() > container_type_params {
        repr.push_str("[");
        let mut first = true;
        for name in fct.type_params.names.iter().skip(container_type_params) {
            if !first {
                repr.push_str(", ");
            }
            repr.push_str(name);
            first = false;
        }
        repr.push_str("]");
    }

    repr
}

pub fn display_ty(prog: &Program, ty: &BytecodeType) -> String {
    format!("{}", fmt_ty(prog, ty, TypeParamMode::Unknown))
}

pub fn display_ty_array(prog: &Program, array: &BytecodeTypeArray) -> String {
    format!("{}", fmt_type_params(prog, array, TypeParamMode::Unknown))
}

pub fn display_ty_without_type_params(prog: &Program, ty: &BytecodeType) -> String {
    format!("{}", fmt_ty(prog, ty, TypeParamMode::None))
}

pub fn fmt_ty<'a>(
    prog: &'a Program,
    ty: &'a BytecodeType,
    type_params: TypeParamMode<'a>,
) -> BytecodeTypePrinter<'a> {
    BytecodeTypePrinter {
        prog,
        type_params,
        ty: ty.clone(),
    }
}

pub fn fmt_ty_with_type_params<'a>(
    prog: &'a Program,
    ty: &'a BytecodeType,
    type_params: &'a TypeParamData,
) -> BytecodeTypePrinter<'a> {
    BytecodeTypePrinter {
        prog,
        type_params: TypeParamMode::TypeParams(type_params),
        ty: ty.clone(),
    }
}

pub fn fmt_type_params<'a>(
    prog: &'a Program,
    array: &'a BytecodeTypeArray,
    type_params: TypeParamMode<'a>,
) -> TypeParamsPrinter<'a> {
    TypeParamsPrinter {
        prog,
        type_params,
        array,
    }
}

pub fn fmt_trait_ty<'a>(
    prog: &'a Program,
    trait_ty: &'a BytecodeTraitType,
    type_params: TypeParamMode<'a>,
) -> BytecodeTraitTypePrinter<'a> {
    BytecodeTraitTypePrinter {
        prog,
        type_params,
        trait_ty,
    }
}

pub fn display_ty_with_type_params(
    prog: &Program,
    ty: &BytecodeType,
    type_params: &TypeParamData,
) -> String {
    format!("{}", fmt_ty_with_type_params(prog, ty, type_params))
}

pub fn fmt_trait_ty_with_type_params<'a>(
    prog: &'a Program,
    trait_ty: &'a BytecodeTraitType,
    type_params: &'a TypeParamData,
) -> BytecodeTraitTypePrinter<'a> {
    BytecodeTraitTypePrinter {
        prog,
        type_params: TypeParamMode::TypeParams(type_params),
        trait_ty,
    }
}

pub fn display_trait_ty_with_type_params(
    prog: &Program,
    trait_ty: &BytecodeTraitType,
    type_params: &TypeParamData,
) -> String {
    format!(
        "{}",
        fmt_trait_ty_with_type_params(prog, trait_ty, type_params)
    )
}

#[derive(Clone, Copy)]
pub enum TypeParamMode<'a> {
    None,
    Unknown,
    TypeParams(&'a TypeParamData),
}

pub struct BytecodeTypePrinter<'a> {
    prog: &'a Program,
    type_params: TypeParamMode<'a>,
    ty: BytecodeType,
}

impl<'a> std::fmt::Display for BytecodeTypePrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.ty {
            BytecodeType::Unit => write!(f, "()"),
            BytecodeType::UInt8 => write!(f, "UInt8"),
            BytecodeType::Char => write!(f, "Char"),
            BytecodeType::Int32 => write!(f, "Int32"),
            BytecodeType::Int64 => write!(f, "Int64"),
            BytecodeType::Float32 => write!(f, "Float32"),
            BytecodeType::Float64 => write!(f, "Float64"),
            BytecodeType::Bool => write!(f, "Bool"),
            BytecodeType::Ptr => write!(f, "Ptr"),
            BytecodeType::Class(id, type_params) => {
                let cls = self.prog.class(*id);
                write!(
                    f,
                    "{}{}",
                    cls.name,
                    fmt_type_params(self.prog, &type_params, self.type_params)
                )
            }
            BytecodeType::Struct(sid, type_params) => {
                let struct_ = self.prog.struct_(*sid);
                write!(
                    f,
                    "{}{}",
                    struct_.name,
                    fmt_type_params(self.prog, &type_params, self.type_params)
                )
            }
            BytecodeType::TraitObject(tid, type_params, bindings) => {
                let trait_ = self.prog.trait_(*tid);
                write!(f, "{}", trait_.name)?;
                if !type_params.is_empty() {
                    write!(
                        f,
                        "[{}]",
                        fmt_type_list(self.prog, &type_params, self.type_params)
                    )?;
                }

                if !bindings.is_empty() {
                    write!(
                        f,
                        "[{}]",
                        fmt_type_list(self.prog, &type_params, self.type_params)
                    )?;
                }

                Ok(())
            }
            BytecodeType::Enum(id, type_params) => {
                let enum_ = self.prog.enum_(*id);
                write!(
                    f,
                    "{}{}",
                    enum_.name,
                    fmt_type_params(self.prog, &type_params, self.type_params)
                )
            }

            BytecodeType::TypeParam(idx) => match self.type_params {
                TypeParamMode::None => panic!("type should not have type param"),
                TypeParamMode::TypeParams(type_params) => {
                    write!(f, "{}", type_params.names[*idx as usize])
                }
                TypeParamMode::Unknown => write!(f, "T#{}", idx),
            },

            BytecodeType::This => write!(f, "Self"),

            BytecodeType::Assoc(assoc_id, type_params) => {
                let alias = self.prog.alias(*assoc_id);
                write!(f, "Self::{}", alias.name)?;
                if !type_params.is_empty() {
                    write!(
                        f,
                        "[{}]",
                        fmt_type_list(self.prog, &type_params, self.type_params)
                    )
                } else {
                    Ok(())
                }
            }

            BytecodeType::TypeAlias(..) | BytecodeType::GenericAssoc { .. } => unimplemented!(),

            BytecodeType::Lambda(params, return_type) => {
                write!(
                    f,
                    "({}): {}",
                    fmt_type_list(self.prog, &params, self.type_params),
                    fmt_ty(self.prog, &return_type, self.type_params)
                )
            }

            BytecodeType::Tuple(subtypes) => {
                write!(
                    f,
                    "({})",
                    fmt_type_list(self.prog, &subtypes, self.type_params)
                )
            }
        }
    }
}

pub struct TypeParamsPrinter<'a> {
    prog: &'a Program,
    type_params: TypeParamMode<'a>,
    array: &'a BytecodeTypeArray,
}

impl<'a> std::fmt::Display for TypeParamsPrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.array.is_empty() {
            Ok(())
        } else {
            write!(
                f,
                "[{}]",
                fmt_type_list(self.prog, self.array, self.type_params)
            )
        }
    }
}

pub struct BytecodeTraitTypePrinter<'a> {
    prog: &'a Program,
    type_params: TypeParamMode<'a>,
    trait_ty: &'a BytecodeTraitType,
}

impl<'a> std::fmt::Display for BytecodeTraitTypePrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let trait_id = self.trait_ty.trait_id;
        let trait_ = self.prog.trait_(trait_id);
        write!(f, "{}", trait_.name)?;

        if !self.trait_ty.type_params.is_empty() || !self.trait_ty.bindings.is_empty() {
            let mut first = true;
            write!(f, "[")?;

            for ty in self.trait_ty.type_params.iter() {
                if !first {
                    write!(f, ", ")?;
                }

                write!(f, "{}", fmt_ty(&self.prog, &ty, self.type_params.clone()))?;
                first = false;
            }

            for (alias_id, ty) in self.trait_ty.bindings.iter() {
                if !first {
                    write!(f, ", ")?;
                }

                let alias = self.prog.alias(*alias_id);
                write!(
                    f,
                    "{}={}",
                    alias.name,
                    fmt_ty(&self.prog, &ty, self.type_params.clone())
                )?;
                first = false;
            }

            write!(f, "]")?;
        }

        Ok(())
    }
}

fn fmt_type_list<'a>(
    prog: &'a Program,
    array: &'a BytecodeTypeArray,
    type_params: TypeParamMode<'a>,
) -> TypeListPrinter<'a> {
    TypeListPrinter {
        prog,
        type_params,
        array,
    }
}

pub struct TypeListPrinter<'a> {
    prog: &'a Program,
    type_params: TypeParamMode<'a>,
    array: &'a BytecodeTypeArray,
}

impl<'a> std::fmt::Display for TypeListPrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut first = true;
        for ty in self.array.iter() {
            if !first {
                write!(f, ", ")?;
            }
            write!(f, "{}", fmt_ty(&self.prog, &ty, self.type_params.clone()))?;
            first = false;
        }

        Ok(())
    }
}

pub fn module_path_name(prog: &Program, module_id: ModuleId, name: &str) -> String {
    let mut result = module_path(prog, module_id);

    if !result.is_empty() {
        result.push_str("::");
    }

    result.push_str(name);
    result
}

pub fn module_path(prog: &Program, module_id: ModuleId) -> String {
    let mut path = String::new();

    let current_id = module_id;

    // Do not print name for the top program module.
    if current_id == prog.program_module_id() {
        return "".into();
    }

    let module = prog.module(current_id);
    path.push_str(&module.name);

    let mut module_id = module.parent_id;

    while let Some(current_id) = module_id {
        // Do not print name for the top program module.
        if current_id == prog.program_module_id() {
            break;
        }

        let module = prog.module(current_id);
        assert_ne!("<root>", module.name);
        path.insert_str(0, "::");
        path.insert_str(0, &module.name);
        module_id = module.parent_id;
    }

    path
}
