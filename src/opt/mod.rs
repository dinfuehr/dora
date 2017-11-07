use std::ffi::CString;
use std::ptr;

use class::TypeParams;
use ctxt::{Fct, FctId, FctParent, FctSrc, SemContext};
use ty::{BuiltinType, MachineMode};

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
        self.create_function()?;

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

    fn create_function(&mut self) -> Result<(), ()> {
        let mut params = Vec::with_capacity(self.ast.params.len());

        for param in &self.ast.params {
            let ty = self.ty(param.id);
            params.push(self.llvm_ty(ty));
        }

        let return_type = self.specialize_type(self.fct.return_type);
        let llvm_return_type = self.llvm_ty(return_type);

        unsafe {
            let function_type = LLVMFunctionType(
                llvm_return_type,
                params.as_mut_ptr(),
                params.len() as u32,
                0);

            self.function = LLVMAddFunction(self.module, self.fct_name.as_ptr(), function_type);
        }

        Err(())
    }

    fn ty(&self, id: NodeId) -> BuiltinType {
        let ty = self.src.ty(id);
        self.specialize_type(ty)
    }

    fn specialize_type(&self, ty: BuiltinType) -> BuiltinType {
        match ty {
            BuiltinType::ClassTypeParam(cls_id, id) => {
                assert!(self.fct.parent == FctParent::Class(cls_id));
                self.cls_type_params[id.idx()]
            }

            BuiltinType::FctTypeParam(fct_id, id) => {
                assert!(self.fct.id == fct_id);
                self.fct_type_params[id.idx()]
            }

            BuiltinType::Class(cls_id, list_id) => {
                let params = self.ctxt.lists.borrow().get(list_id);

                let params: Vec<_> = params.iter().map(|t| self.specialize_type(t)).collect();

                let list_id = self.ctxt.lists.borrow_mut().insert(params.into());

                BuiltinType::Class(cls_id, list_id)
            }

            BuiltinType::Lambda(_) => unimplemented!(),

            _ => ty,
        }
    }

    fn llvm_ty(&self, ty: BuiltinType) -> *mut llvm::LLVMType {
        let mode = ty.mode();

        unsafe {
            match mode {
                MachineMode::Int64 => LLVMInt64TypeInContext(self.context),
                MachineMode::Int32 => LLVMInt32TypeInContext(self.context),
                MachineMode::Int8 => LLVMInt8TypeInContext(self.context),
                MachineMode::Float32 => LLVMFloatTypeInContext(self.context),
                MachineMode::Float64 => LLVMDoubleTypeInContext(self.context),
                MachineMode::Ptr => {
                    let int8 = LLVMInt8TypeInContext(self.context);
                    LLVMPointerType(int8, 1)
                }
            }
        }
    }
}
