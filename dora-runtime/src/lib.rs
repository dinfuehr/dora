#![allow(dead_code)]
#![allow(non_camel_case_types)]
#![recursion_limit = "256"]

extern crate alloc;

#[cfg(target_os = "windows")]
extern crate windows_sys;

#[macro_use]
extern crate memoffset;

mod gc;
mod handle;
pub mod mem;
mod mirror;
mod os;
pub mod runtime;
mod safepoint;
mod shape;
mod snapshot;
mod stack;
pub mod startup;
mod stdlib;
mod threads;
mod timer;
mod utils;

pub use dora_compiler::cpu::{
    CALLEE_SAVED_REGS, FREG_PARAMS, FREG_RESULT, FREG_TMP1, FReg, REG_PARAMS, REG_RESULT, REG_SP,
    REG_THREAD, REG_TMP1, REG_TMP2, Reg, STACK_FRAME_ALIGNMENT,
};
pub use dora_compiler::{
    AllocationSize, AnyReg, AotAssemblyKind, AotBackend, AotCodeKind, AotCodegenContext,
    AotCompilation, AotCompileArgs, AotCompileFn, AotCompileInputs, AotContextGuard, AotEnumLayout,
    AotFunction, AotFunctionInfo, AotGcPoint, AotGlobalRelocationTarget, AotInlinedFunction,
    AotKnownShape, AotKnownShapeKind, AotLayout, AotLocation, AotRecordLayout, AotRelocation,
    AotRelocationTarget, AotShape, AotStringId, AotStringTable, CompilationData,
    CompilerInvocation, FieldInstance, GlobalLayoutEntry, InstanceSize, MachineMode, ShapeVisitor,
    SpecializeSelf, compile_boots_compiler_aot, compile_program_aot, compile_test_runner,
    dora_entry_trampoline, get_bytecode, register_ty, write_assembly,
};
pub use dora_compiler::{
    LARGE_OBJECT_SIZE, MAX_TLAB_OBJECT_SIZE, REMEMBERED_BIT_SHIFT, ThreadState,
};
pub use dora_symbol::{demangle_name, mangle_name};
pub use gc::Address;
pub use handle::{Handle, create_handle, handle_scope};
pub use mirror::{Header, Object, Ref, Str, UInt8Array, byte_array_from_buffer};
pub use runtime::Runtime;
pub use runtime::{
    CollectorName, Compiler, FunctionInfoAot, InlinedFunctionAot, MemSize, RuntimeFlags, ShapeKind,
    TargetArch, clear_runtime, execute_on_main, parse_collector, parse_target_arch, set_runtime,
};
pub use shape::Shape;
pub use threads::{ThreadLocalData, current_thread};
