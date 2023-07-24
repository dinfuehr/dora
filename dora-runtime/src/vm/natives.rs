use parking_lot::Mutex;

use std::collections::HashMap;

use crate::compiler::dora_entry_trampoline;
use crate::compiler::lazy_compilation_stub;
use crate::compiler::runtime_entry_trampoline::{self, NativeFct, NativeFctKind, NativeStubs};
use crate::gc::Address;
use crate::safepoint;
use crate::stdlib;
use crate::vm::VM;
use dora_bytecode::FunctionId;
use dora_bytecode::{BytecodeType, BytecodeTypeArray};

pub struct NativeMethods {
    // Trampolines for builtin functions without a proper FunctionId.
    // Generated on startup.
    lazy_compilation_stub: Option<Address>,
    dora_entry_trampoline: Option<Address>,
    trap_trampoline: Option<Address>,
    stack_overflow_trampoline: Option<Address>,
    safepoint_trampoline: Option<Address>,

    // Stores all trampolines generated at runtime for Dora-exposed functions
    // in the standard library.
    trampolines: Mutex<NativeStubs>,

    // Stores all native implementations for Dora-exposed functions.
    // Filled on startup.
    implementations: HashMap<FunctionId, Address>,
}

impl NativeMethods {
    pub fn new() -> NativeMethods {
        NativeMethods {
            lazy_compilation_stub: None,
            dora_entry_trampoline: None,
            trap_trampoline: None,
            stack_overflow_trampoline: None,
            safepoint_trampoline: None,

            trampolines: Mutex::new(NativeStubs::new()),
            implementations: HashMap::new(),
        }
    }

    pub fn lazy_compilation_stub(&self) -> Address {
        self.lazy_compilation_stub.expect("uninitialized field")
    }

    pub fn dora_entry_trampoline(&self) -> Address {
        self.dora_entry_trampoline.expect("uninitialized field")
    }

    pub fn trap_trampoline(&self) -> Address {
        self.trap_trampoline.expect("uninitialized field")
    }

    pub fn stack_overflow_trampoline(&self) -> Address {
        self.stack_overflow_trampoline.expect("uninitialized field")
    }

    pub fn safepoint_trampoline(&self) -> Address {
        self.safepoint_trampoline.expect("uninitialized field")
    }

    pub fn insert(&mut self, fct: FunctionId, address: Address) -> Option<Address> {
        self.implementations.insert(fct, address)
    }

    pub fn get(&self, fid: FunctionId) -> Option<Address> {
        self.implementations.get(&fid).copied()
    }

    pub fn lock_trampolines<F, R>(&self, fct: F) -> R
    where
        F: FnOnce(&mut NativeStubs) -> R,
    {
        let mut stdlib_trampolines = self.trampolines.lock();
        fct(&mut *stdlib_trampolines)
    }
}

pub fn setup_builtin_natives(vm: &mut VM) {
    vm.native_methods.dora_entry_trampoline =
        Some(dora_entry_trampoline::install(vm).instruction_start());

    let ifct = NativeFct {
        fctptr: Address::from_ptr(stdlib::trap as *const u8),
        args: BytecodeTypeArray::one(BytecodeType::Int32),
        return_type: BytecodeType::Unit,
        desc: NativeFctKind::TrapStub,
    };
    let code = runtime_entry_trampoline::generate(vm, ifct, false);
    vm.native_methods.trap_trampoline = Some(code.instruction_start());

    vm.native_methods.lazy_compilation_stub =
        Some(lazy_compilation_stub::generate(vm).instruction_start());

    let ifct = NativeFct {
        fctptr: Address::from_ptr(safepoint::stack_overflow as *const u8),
        args: BytecodeTypeArray::empty(),
        return_type: BytecodeType::Unit,
        desc: NativeFctKind::GuardCheckStub,
    };
    let code = runtime_entry_trampoline::generate(vm, ifct, false);
    vm.native_methods.stack_overflow_trampoline = Some(code.instruction_start());

    let ifct = NativeFct {
        fctptr: Address::from_ptr(safepoint::safepoint_slow as *const u8),
        args: BytecodeTypeArray::empty(),
        return_type: BytecodeType::Unit,
        desc: NativeFctKind::SafepointStub,
    };
    let code = runtime_entry_trampoline::generate(vm, ifct, false);
    vm.native_methods.safepoint_trampoline = Some(code.instruction_start());
}
