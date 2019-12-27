use crate::compiler::fct::JitFct;
use crate::ty::TypeList;
use crate::vm::{Fct, FctSrc, VM};

pub(super) fn compile<'a, 'ast: 'a>(
    _vm: &'a VM<'ast>,
    _fct: &Fct<'ast>,
    _src: &'a FctSrc,
    _cls_type_params: &TypeList,
    _fct_type_params: &TypeList,
) -> JitFct {
    unimplemented!()
}
