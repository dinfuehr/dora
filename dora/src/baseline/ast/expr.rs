use crate::baseline::asm::BaselineAssembler;
use crate::baseline::ast::info::JitInfo;
use crate::baseline::codegen::{
    self, ensure_native_stub, register_for_mode, AllocationSize, CondCode, ExprStore, Scopes,
    TempOffsets,
};
use crate::baseline::dora_native::{InternalFct, InternalFctDescriptor};
use crate::baseline::fct::{CatchType, Comment, GcPoint};
use crate::class::{ClassDefId, ClassSize, FieldId};
use crate::cpu::{
    FReg, Mem, Reg, FREG_PARAMS, FREG_RESULT, FREG_TMP1, REG_PARAMS, REG_RESULT, REG_TMP1, REG_TMP2,
};
use crate::gc::Address;
use crate::mem;
use crate::object::{Header, Str};
use crate::os::signal::Trap;
use crate::ty::{BuiltinType, MachineMode, TypeList};
use crate::vm::VM;
use crate::vm::*;
use crate::vtable::{VTable, DISPLAY_SIZE};
use dora_parser::ast::Expr::*;
use dora_parser::ast::*;
use dora_parser::lexer::position::Position;
use dora_parser::lexer::token::{FloatSuffix, IntSuffix};

pub struct ExprGen<'a, 'b, 'ast>
where
    'ast: 'a,
    'ast: 'b,
{
    vm: &'a VM<'ast>,
    fct: &'a Fct<'ast>,
    src: &'a mut FctSrc,
    ast: &'ast Function,
    asm: &'a mut BaselineAssembler<'b, 'ast>,
    scopes: &'a mut Scopes,
    tempsize: i32,
    temps: TempOffsets,
    jit_info: &'a JitInfo<'ast>,
    cls_type_params: &'a TypeList,
    fct_type_params: &'a TypeList,
}

impl<'a, 'b, 'ast> ExprGen<'a, 'b, 'ast>
where
    'ast: 'a,
    'ast: 'b,
{
    pub fn new(
        vm: &'a VM<'ast>,
        fct: &'a Fct<'ast>,
        src: &'a mut FctSrc,
        ast: &'ast Function,
        asm: &'a mut BaselineAssembler<'b, 'ast>,
        scopes: &'a mut Scopes,
        jit_info: &'a JitInfo<'ast>,
        cls_type_params: &'a TypeList,
        fct_type_params: &'a TypeList,
    ) -> ExprGen<'a, 'b, 'ast> {
        ExprGen {
            vm,
            fct,
            src,
            ast,
            asm,
            tempsize: 0,
            scopes,
            temps: TempOffsets::new(),
            jit_info,
            cls_type_params,
            fct_type_params,
        }
    }

    pub fn generate(mut self, e: &'ast Expr, dest: ExprStore) {
        self.emit_expr(e, dest);

        if !self.temps.is_empty() {
            panic!("temporary variables are not fully freed!");
        }
    }
}

fn result_reg(mode: MachineMode) -> ExprStore {
    if mode.is_float() {
        FREG_RESULT.into()
    } else {
        REG_RESULT.into()
    }
}

fn check_for_nil(ty: BuiltinType) -> bool {
    match ty {
        BuiltinType::Error => panic!("error shouldn't occur in code generation."),
        BuiltinType::Unit => false,
        BuiltinType::Byte
        | BuiltinType::Char
        | BuiltinType::Int
        | BuiltinType::Long
        | BuiltinType::Float
        | BuiltinType::Double
        | BuiltinType::Bool
        | BuiltinType::Enum(_) => false,
        BuiltinType::Nil | BuiltinType::Ptr => true,
        BuiltinType::Class(_, _) => true,
        BuiltinType::Struct(_, _) => false,
        BuiltinType::Trait(_) => false,
        BuiltinType::This => unreachable!(),
        BuiltinType::ClassTypeParam(_, _) => unreachable!(),
        BuiltinType::FctTypeParam(_, _) => unreachable!(),
        BuiltinType::Lambda(_) => true,
    }
}

fn ensure_jit_or_stub_ptr<'ast>(
    src: &mut FctSrc,
    vm: &VM,
    cls_type_params: TypeList,
    fct_type_params: TypeList,
) -> Address {
    let specials = src.specializations.read();
    let key = (cls_type_params, fct_type_params);

    if let Some(&jit_fct_id) = specials.get(&key) {
        let jit_fct = vm.jit_fcts.idx(jit_fct_id);
        return jit_fct.fct_ptr();
    }

    vm.compiler_thunk()
}

fn to_cond_code(cmp: CmpOp) -> CondCode {
    match cmp {
        CmpOp::Eq => CondCode::Equal,
        CmpOp::Ne => CondCode::NotEqual,
        CmpOp::Gt => CondCode::Greater,
        CmpOp::Ge => CondCode::GreaterEq,
        CmpOp::Lt => CondCode::Less,
        CmpOp::Le => CondCode::LessEq,
        CmpOp::Is => CondCode::Equal,
        CmpOp::IsNot => CondCode::NotEqual,
    }
}
