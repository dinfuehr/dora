#![allow(dead_code)]
#![allow(non_camel_case_types)]
#![recursion_limit = "256"]

extern crate alloc;

#[cfg(target_os = "windows")]
extern crate windows_sys;

#[macro_use]
extern crate memoffset;

mod compiler;
mod cpu;
mod gc;
mod handle;
mod masm;
pub mod mem;
mod mirror;
mod os;
mod safepoint;
mod shape;
mod snapshot;
mod stack;
pub mod startup;
mod stdlib;
mod threads;
mod timer;
mod utils;
pub mod vm;

pub use compiler::aot::{
    AotBackend, AotCallRelocation, AotCodeKind, AotCodegenContext, AotCompilation, AotCompileArgs,
    AotCompileFn, AotCompileInputs, AotContextGuard, AotFunction, AotFunctionInfo, AotGcPoint,
    AotGlobalRelocation, AotInlinedFunction, AotKnownShape, AotKnownShapeKind, AotLocation,
    AotShape, AotStringId, AotStringRelocation, AotStringTable, CompilerInvocation,
    compile_boots_compiler_aot, compile_program_aot, compile_test_runner,
};
pub use compiler::dora_entry_trampoline;
pub use compiler::{AllocationSize, AnyReg};
pub use cpu::{
    CALLEE_SAVED_REGS, FREG_PARAMS, FREG_RESULT, FREG_TMP1, FReg, REG_PARAMS, REG_RESULT, REG_SP,
    REG_THREAD, REG_TMP1, REG_TMP2, Reg, STACK_FRAME_ALIGNMENT,
};
pub use dora_compiler::{
    AotAssemblyKind, AotEnumLayout, AotLayout, AotRecordLayout, CompilationData, FieldInstance,
    InstanceSize, MachineMode, ShapeVisitor, SpecializeSelf, get_bytecode, register_ty,
    write_assembly,
};
pub use dora_symbol::{demangle_name, mangle_name};
pub use gc::Address;
pub use gc::swiper::LARGE_OBJECT_SIZE;
pub use gc::tlab::MAX_TLAB_OBJECT_SIZE;
pub use handle::{Handle, create_handle, handle_scope};
pub use masm::{CondCode, Label, MacroAssembler, Mem, ScratchReg};
pub use mirror::{
    Header, Object, REMEMBERED_BIT_SHIFT, Ref, Str, UInt8Array, byte_array_from_buffer,
};
pub use shape::Shape;
pub use threads::{ThreadLocalData, current_thread};
pub use vm::VM;
pub use vm::{
    CollectorName, Compiler, FunctionInfoAot, InlinedFunctionAot, MemSize, ShapeKind, TargetArch,
    VmFlags, clear_vm, execute_on_main, parse_collector, parse_target_arch, set_vm,
};
