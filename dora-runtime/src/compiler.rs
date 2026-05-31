pub use crate::compiler::runtime_entry_trampoline::*;
use crate::cpu::{FReg, Reg};
use dora_bytecode::{
    BytecodeFunction, BytecodeTraitType, BytecodeType, BytecodeTypeArray, FunctionData, FunctionId,
    FunctionKind, ImplId, Location, Program,
};

pub mod aot;
pub mod closure;
pub mod dora_entry_trampoline;
pub mod runtime_entry_trampoline;
pub mod trait_object_thunk;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum AnyReg {
    Reg(Reg),
    FReg(FReg),
}

impl AnyReg {
    pub fn is_reg(&self) -> bool {
        match self {
            &AnyReg::Reg(_) => true,
            _ => false,
        }
    }

    #[allow(dead_code)]
    pub fn is_freg(&self) -> bool {
        match self {
            &AnyReg::FReg(_) => true,
            _ => false,
        }
    }

    pub fn reg(&self) -> Reg {
        match self {
            &AnyReg::Reg(reg) => reg,
            _ => panic!("fp-register accessed as gp-register."),
        }
    }

    pub fn freg(&self) -> FReg {
        match self {
            &AnyReg::FReg(reg) => reg,
            _ => panic!("gp-register accessed as fp-register."),
        }
    }
}

impl From<Reg> for AnyReg {
    fn from(reg: Reg) -> AnyReg {
        AnyReg::Reg(reg)
    }
}

impl From<FReg> for AnyReg {
    fn from(reg: FReg) -> AnyReg {
        AnyReg::FReg(reg)
    }
}

pub enum AllocationSize {
    Fixed(usize),
    Dynamic(Reg),
}

pub struct SpecializeSelf {
    pub impl_id: ImplId,
    pub container_type_params: usize,
    pub trait_ty: BytecodeTraitType,
    pub extended_ty: BytecodeType,
}

pub struct CompilationData<'a> {
    pub program: &'a Program,
    pub bytecode_fct: &'a BytecodeFunction,
    pub params: BytecodeTypeArray,
    pub has_variadic_parameter: bool,
    pub return_type: BytecodeType,
    pub fct_id: FunctionId,
    pub type_params: BytecodeTypeArray,
    pub specialize_self: Option<SpecializeSelf>,
    pub loc: Location,

    pub emit_debug: bool,
    pub emit_code_comments: bool,
    pub emit_final_graph: bool,
    pub emit_graph_after_each_pass: bool,
    pub emit_html: bool,
}

pub fn register_ty(ty: BytecodeType) -> BytecodeType {
    match ty {
        BytecodeType::Class(_, _) | BytecodeType::Lambda(_, _) => BytecodeType::Ptr,
        _ => ty,
    }
}

pub fn get_bytecode<'a>(
    program: &'a Program,
    program_fct: &'a FunctionData,
) -> Option<(&'a BytecodeFunction, Option<SpecializeSelf>)> {
    match program_fct.bytecode.as_ref() {
        Some(bytecode_fct) => Some((bytecode_fct, None)),
        None => {
            let trait_method_id = program_fct.trait_method_impl?;
            let trait_method = program.fct(trait_method_id);

            let program_fct_impl_id = match program_fct.kind {
                FunctionKind::Impl(impl_id) => impl_id,
                _ => unreachable!(),
            };

            let bytecode_fct = trait_method.bytecode.as_ref()?;

            let program_fct_impl = program.impl_(program_fct_impl_id);

            let specialize_self = SpecializeSelf {
                impl_id: program_fct_impl_id,
                container_type_params: program_fct_impl.type_params.type_param_count(),
                trait_ty: program_fct_impl.trait_ty.clone(),
                extended_ty: program_fct_impl.extended_ty.clone(),
            };

            Some((bytecode_fct, Some(specialize_self)))
        }
    }
}
