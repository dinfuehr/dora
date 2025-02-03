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
    let printer = BytecodeTypePrinter {
        prog,
        type_params: TypeParamMode::Unknown,
        ty: ty.clone(),
    };

    printer.string()
}

pub fn display_ty_array(prog: &Program, array: &BytecodeTypeArray) -> String {
    let mut result = "[".to_string();
    for (idx, ty) in array.iter().enumerate() {
        if idx > 0 {
            result.push_str(", ");
        }
        result.push_str(&display_ty(prog, &ty));
    }
    result.push(']');
    result
}

pub fn display_ty_without_type_params(prog: &Program, ty: &BytecodeType) -> String {
    let printer = BytecodeTypePrinter {
        prog,
        type_params: TypeParamMode::None,
        ty: ty.clone(),
    };

    printer.string()
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

pub fn display_ty_with_type_params(
    prog: &Program,
    ty: &BytecodeType,
    type_params: &TypeParamData,
) -> String {
    fmt_ty_with_type_params(prog, ty, type_params).string()
}

pub fn fmt_trait_ty_with_type_params<'a>(
    prog: &'a Program,
    trait_ty: &'a BytecodeTraitType,
    type_params: &'a TypeParamData,
) -> BytecodeTraitTypePrinter<'a> {
    BytecodeTraitTypePrinter {
        prog,
        type_params,
        trait_ty,
    }
}

pub fn display_trait_ty_with_type_params(
    prog: &Program,
    trait_ty: &BytecodeTraitType,
    type_params: &TypeParamData,
) -> String {
    fmt_trait_ty_with_type_params(prog, trait_ty, type_params).string()
}

enum TypeParamMode<'a> {
    None,
    Unknown,
    TypeParams(&'a TypeParamData),
}

pub struct BytecodeTypePrinter<'a> {
    prog: &'a Program,
    type_params: TypeParamMode<'a>,
    ty: BytecodeType,
}

impl<'a> BytecodeTypePrinter<'a> {
    pub fn string(&self) -> String {
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
                let cls = self.prog.class(*id);
                write!(fmt, "{}", cls.name)?;
                self.type_params(type_params, fmt)
            }
            BytecodeType::Struct(sid, type_params) => {
                let struct_ = self.prog.struct_(*sid);
                write!(fmt, "{}", struct_.name)?;
                self.type_params(type_params, fmt)
            }
            BytecodeType::TraitObject(tid, type_params, bindings) => {
                let trait_ = self.prog.trait_(*tid);
                write!(fmt, "{}", trait_.name)?;
                self.type_params(type_params, fmt)?;
                self.type_params(bindings, fmt)
            }
            BytecodeType::Enum(id, type_params) => {
                let enum_ = self.prog.enum_(*id);
                write!(fmt, "{}", enum_.name)?;
                self.type_params(type_params, fmt)
            }

            BytecodeType::TypeParam(idx) => match self.type_params {
                TypeParamMode::None => panic!("type should not have type param"),
                TypeParamMode::TypeParams(type_params) => {
                    write!(fmt, "{}", type_params.names[*idx as usize])
                }
                TypeParamMode::Unknown => write!(fmt, "TypeParam({})", idx),
            },

            BytecodeType::This => write!(fmt, "Self"),

            BytecodeType::TypeAlias(..)
            | BytecodeType::Assoc(..)
            | BytecodeType::GenericAssoc { .. } => unimplemented!(),

            BytecodeType::Lambda(params, return_type) => {
                write!(fmt, "(")?;
                self.type_list(params, fmt)?;
                write!(fmt, ") -> ")?;
                self.name(return_type, fmt)
            }

            BytecodeType::Tuple(subtypes) => {
                write!(fmt, "(")?;
                self.type_list(subtypes, fmt)?;
                write!(fmt, ")")
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

pub struct BytecodeTraitTypePrinter<'a> {
    prog: &'a Program,
    type_params: &'a TypeParamData,
    trait_ty: &'a BytecodeTraitType,
}

impl<'a> BytecodeTraitTypePrinter<'a> {
    pub fn string(&self) -> String {
        format!("{}", self)
    }

    pub fn name(
        &self,
        trait_ty: &BytecodeTraitType,
        fmt: &mut std::fmt::Formatter,
    ) -> std::fmt::Result {
        let trait_id = trait_ty.trait_id;
        let trait_ = self.prog.trait_(trait_id);
        write!(fmt, "{}", trait_.name)?;

        if !trait_ty.type_params.is_empty() || !trait_ty.bindings.is_empty() {
            let mut first = false;
            write!(fmt, "[")?;

            for ty in trait_ty.type_params.iter() {
                if !first {
                    write!(fmt, ", ")?;
                }

                write!(
                    fmt,
                    "{}",
                    fmt_ty_with_type_params(&self.prog, &ty, self.type_params)
                )?;
                first = false;
            }

            for (alias_id, ty) in trait_ty.bindings.iter() {
                if !first {
                    write!(fmt, ", ")?;
                }

                let alias = self.prog.alias(*alias_id);
                write!(
                    fmt,
                    "{}={}",
                    alias.name,
                    fmt_ty_with_type_params(&self.prog, &ty, self.type_params)
                )?;
                first = false;
            }

            write!(fmt, "]")?;
        }

        Ok(())
    }
}

impl<'a> std::fmt::Display for BytecodeTraitTypePrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.name(&self.trait_ty, f)
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
