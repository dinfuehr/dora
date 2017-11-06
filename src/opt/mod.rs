use std::ffi::CString;
use std::ptr;

use class::TypeParams;
use ctxt::{Fct, FctId, FctSrc, SemContext};

use dora_parser::ast::*;

use llvm;
use llvm::core::*;
// use llvm::execution_engine::*;
// use llvm::target::*;

pub fn generate<'ast>(
    ctxt: &SemContext<'ast>,
    id: FctId,
    cls_type_params: &TypeParams,
    fct_type_params: &TypeParams,
) -> Result<*const u8, ()> {
    let fct = ctxt.fcts[id].borrow();
    let src = fct.src();
    let mut src = src.borrow_mut();

    generate_fct(ctxt, &fct, &mut src, cls_type_params, fct_type_params)
}

pub fn generate_fct<'ast>(
    ctxt: &SemContext<'ast>,
    fct: &Fct<'ast>,
    src: &mut FctSrc,
    cls_type_params: &TypeParams,
    fct_type_params: &TypeParams,
) -> Result<*const u8, ()> {
    let name = fct.full_name(ctxt);

    let mut cg = CodeGen {
        ctxt: ctxt,
        fct: fct,
        ast: fct.ast,
        src: src,
        cls_type_params,
        fct_type_params,

        fct_name: CString::new(name).unwrap(),

        context: ptr::null_mut(),
        module: ptr::null_mut(),
        builder: ptr::null_mut(),
        function: ptr::null_mut(),
    };

    cg.generate()
}

struct CodeGen<'a, 'ast: 'a> {
    ctxt: &'a SemContext<'ast>,
    fct: &'a Fct<'ast>,
    ast: &'ast Function,
    src: &'a mut FctSrc,
    cls_type_params: &'a TypeParams,
    fct_type_params: &'a TypeParams,

    fct_name: CString,

    context: *mut llvm::LLVMContext,
    module: *mut llvm::LLVMModule,
    builder: *mut llvm::LLVMBuilder,
    function: *mut llvm::LLVMValue,
}

impl<'a, 'ast> CodeGen<'a, 'ast>
where
    'ast: 'a,
{
    fn generate(&mut self) -> Result<*const u8, ()> {
        self.init();
        self.create_function();

        Err(())
    }

    fn init(&mut self) {
        unsafe {
            self.context = LLVMContextCreate();
            self.module = LLVMModuleCreateWithNameInContext(
                self.fct_name.as_ptr(),
                self.context,
            );
            self.builder = LLVMCreateBuilderInContext(self.context);
        }
    }

    fn create_function(&mut self) {
        unsafe {
            let i64t = LLVMInt64TypeInContext(self.context);
            let mut argts = [i64t, i64t, i64t];
            let function_type = LLVMFunctionType(
                i64t,
                argts.as_mut_ptr(),
                argts.len() as u32,
                0);

            self.function = LLVMAddFunction(self.module, self.fct_name.as_ptr(), function_type);
        }
    }
}
