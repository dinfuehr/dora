use std::ffi::CString;
use std::ptr;

use class::TypeParams;
use ctxt::{Fct, FctParent, FctSrc, SemContext};
use ty::{BuiltinType, MachineMode};

use dora_parser::ast::*;
use dora_parser::ast::Expr::*;
use dora_parser::ast::Stmt::*;
use dora_parser::lexer::token::{FloatSuffix, IntSuffix};

use llvm;
use llvm::analysis::*;
use llvm::core::*;
// use llvm::execution_engine::*;
// use llvm::target::*;

pub mod util;

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

type EmitResult<T> = Result<T, ()>;

fn fail<T>() -> EmitResult<T> {
    Err(())
}

fn ok<T>(value: T) -> EmitResult<T> {
    Ok(value)
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
    fn generate(&mut self) -> EmitResult<*const u8> {
        self.init();
        self.create_function()?;
        self.add_entry_bb();

        let block = self.ast.block.as_ref().unwrap();
        self.emit_stmt(block)?;

        unsafe {
            LLVMDisposeBuilder(self.builder);
        }

        self.verify()?;

        if self.ctxt.args.flag_emit_llvm {
            unsafe {
                LLVMDumpModule(self.module);
            }
        }

        fail()
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

    fn create_function(&mut self) -> EmitResult<()> {
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

        ok(())
    }

    fn add_entry_bb(&mut self) {
        unsafe {
            let bb = LLVMAppendBasicBlockInContext(
                self.context,
                self.function,
                b"entry\0".as_ptr() as *const _);

            LLVMPositionBuilderAtEnd(self.builder, bb);
        }
    }

    fn verify(&mut self) -> EmitResult<()> {
        unsafe {
            if LLVMVerifyFunction(self.function,
                                  LLVMVerifierFailureAction::LLVMPrintMessageAction) == 1 {
                println!("invalid llvm function!");
                fail()
            } else {
                ok(())
            }
        }
    }

    fn emit_stmt(&mut self, s: &'ast Stmt) -> EmitResult<()> {
        match *s {
            StmtExpr(ref stmt) => {
                self.emit_expr(&stmt.expr)?;

                Ok(())
            }

            StmtIf(_) => fail(),
            StmtLoop(_) => fail(),
            StmtWhile(_) => fail(),
            StmtFor(_) => fail(),
            StmtReturn(ref stmt) => self.emit_return(stmt),
            StmtBreak(_) => fail(),
            StmtContinue(_) => fail(),
            StmtBlock(_) => fail(),
            StmtVar(_) => fail(),
            StmtThrow(_) => fail(),
            StmtDefer(_) => fail(),
            StmtDo(_) => fail(),
            StmtSpawn(_) => fail(),
        }
    }

    fn emit_return(&mut self, s: &'ast StmtReturnType) -> EmitResult<()> {
        if let Some(ref expr) = s.expr {
            let value = self.emit_expr(expr)?;

            unsafe {
                LLVMBuildRet(self.builder, value);
            }

        } else {
            unsafe {
                LLVMBuildRetVoid(self.builder);
            }
        }

        ok(())
    }


    fn emit_expr(&mut self, e: &'ast Expr) -> EmitResult<*mut llvm::LLVMValue> {
        match *e {
            ExprLitChar(ref lit) => self.emit_lit_char(lit),
            ExprLitInt(ref lit) => self.emit_lit_int(lit),
            ExprLitFloat(ref lit) => self.emit_lit_float(lit),
            ExprLitBool(_) => fail(),
            ExprLitStr(_) => fail(),
            ExprLitStruct(_) => fail(),
            ExprUn(_) => fail(),
            ExprIdent(_) => fail(),
            ExprAssign(_) => fail(),
            ExprBin(_) => fail(),
            ExprCall(_) => fail(),
            ExprDelegation(_) => fail(),
            ExprField(_) => fail(),
            ExprSelf(_) => fail(),
            ExprSuper(_) => fail(),
            ExprNil(_) => fail(),
            ExprArray(_) => fail(),
            ExprConv(_) => fail(),
            ExprTry(_) => fail(),
            ExprLambda(_) => fail(),
        }
    }

    fn emit_lit_char(&mut self, e: &'ast ExprLitCharType) -> EmitResult<*mut llvm::LLVMValue> {
        unsafe {
            let ty = LLVMInt32TypeInContext(self.context);
            let value = LLVMConstInt(ty, e.value as u64, 0);
            ok(value)
        }
    }

    fn emit_lit_int(&mut self, e: &'ast ExprLitIntType) -> EmitResult<*mut llvm::LLVMValue> {
        unsafe {
            let ty = match e.suffix {
                IntSuffix::Byte => LLVMInt8TypeInContext(self.context),
                IntSuffix::Int => LLVMInt32TypeInContext(self.context),
                IntSuffix::Long => LLVMInt64TypeInContext(self.context),
            };

            let value = LLVMConstInt(ty, e.value, 0);
            ok(value)
        }
    }

    fn emit_lit_float(&mut self, e: &'ast ExprLitFloatType) -> EmitResult<*mut llvm::LLVMValue> {
        unsafe {
            let ty = match e.suffix {
                FloatSuffix::Float => LLVMFloatTypeInContext(self.context),
                FloatSuffix::Double => LLVMDoubleTypeInContext(self.context),
            };

            let value = LLVMConstReal(ty, e.value);
            ok(value)
        }
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
