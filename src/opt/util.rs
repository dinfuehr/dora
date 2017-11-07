use llvm::target::*;

pub fn init() {
    unsafe {
        if LLVM_InitializeNativeTarget() != 0 {
            panic!("LLVM: InitializeNativeTarget failed.");
        }

        if LLVM_InitializeNativeAsmPrinter() != 0 {
            panic!("LLVM: LLVM_InitializeNativeAsmPrinter failed.");
        }
    }
}