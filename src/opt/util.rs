use std::ffi::CStr;
use std::ptr;

use llvm::core::*;
use llvm::orc::*;
use llvm::target::*;
use llvm::target_machine::*;
use llvm::target_machine::LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault;
use llvm::target_machine::LLVMCodeModel::LLVMCodeModelJITDefault;
use llvm::target_machine::LLVMRelocMode::LLVMRelocDefault;

pub struct LlvmJit {
    tm: *mut LLVMOpaqueTargetMachine,
    orc: *mut LLVMOrcOpaqueJITStack,
}

impl LlvmJit {
    pub fn new() -> LlvmJit {
        unsafe {
            LLVM_InitializeNativeTarget();
            LLVM_InitializeNativeAsmPrinter();

            let default_triple = LLVMGetDefaultTargetTriple();
            let mut error = ptr::null_mut();
            let mut target_ref = ptr::null_mut();

            if LLVMGetTargetFromTriple(default_triple, &mut target_ref, &mut error) != 0 {
                panic!("couldn't get target from triple: {:?}.", CStr::from_ptr(error));
            }

            if LLVMTargetHasJIT(target_ref) == 0 {
                panic!("target doesn't support JIT.");
            }

            let tm = LLVMCreateTargetMachine(target_ref,
                        default_triple,
                        ptr::null(),
                        ptr::null(),
                        LLVMCodeGenLevelDefault,
                        LLVMRelocDefault,
                        LLVMCodeModelJITDefault);

            assert!(!tm.is_null());
            LLVMDisposeMessage(default_triple);

            let orc = LLVMOrcCreateInstance(tm);

            LlvmJit {
                tm: tm,
                orc: orc,
            }
        }
    }
}