use class::TypeParams;
use ctxt::{Fct, FctId, FctSrc, SemContext};

pub fn generate<'ast>(
    ctxt: &SemContext<'ast>,
    id: FctId,
    cls_type_params: &TypeParams,
    fct_type_params: &TypeParams,
) -> *const u8 {
    let fct = ctxt.fcts[id].borrow();
    let src = fct.src();
    let mut src = src.borrow_mut();

    generate_fct(ctxt, &fct, &mut src, cls_type_params, fct_type_params)
}

pub fn generate_fct<'ast>(
    _ctxt: &SemContext<'ast>,
    _fct: &Fct<'ast>,
    _src: &mut FctSrc,
    _cls_type_params: &TypeParams,
    _fct_type_params: &TypeParams,
) -> *const u8 {
    unimplemented!()
}