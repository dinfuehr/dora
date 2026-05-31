#![allow(dead_code)]
#![allow(non_camel_case_types)]
#![recursion_limit = "256"]

extern crate alloc;

#[cfg(target_os = "windows")]
extern crate windows_sys;

#[macro_use]
extern crate memoffset;

pub mod aot;
mod boots;
pub mod compiler;
pub mod cpu;
pub mod gc;
mod handle;
pub mod masm;
pub mod mem;
pub mod mirror;
pub mod mode;
mod os;
mod safepoint;
mod shape;
pub mod size;
mod snapshot;
mod stack;
pub mod startup;
mod stdlib;
pub mod threads;
mod timer;
mod utils;
pub mod vm;

pub use aot::{AotAssemblyKind, write_assembly};
pub use boots::BootsAotBackend;
pub use compiler::aot::{
    AotBackend, AotCallRelocation, AotCodeKind, AotCodegenContext, AotCompilation, AotCompileArgs,
    AotCompileFn, AotCompileInputs, AotContextGuard, AotFunction, AotFunctionInfo, AotGcPoint,
    AotGlobalRelocation, AotInlinedFunction, AotKnownShape, AotKnownShapeKind, AotLocation,
    AotShape, AotStringId, AotStringRelocation, AotStringTable, CompilerInvocation,
    compile_boots_compiler_aot, compile_program_aot, compile_test_runner,
};
pub use compiler::dora_entry_trampoline;
pub use dora_symbol::{demangle_name, mangle_name};
use gc::Address;
use shape::{Shape, ShapeVisitor};
pub use vm::VM;
pub use vm::{
    CollectorName, Compiler, FunctionInfoAot, InlinedFunctionAot, MemSize, ShapeKind, TargetArch,
    VmFlags, clear_vm, execute_on_main, parse_collector, parse_target_arch, set_vm,
};
