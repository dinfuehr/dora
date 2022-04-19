use crate::compiler::dora_entry_stub;
use crate::compiler::dora_exit_stubs::{self, NativeFct, NativeFctKind};
use crate::compiler::lazy_compile_stub;
use crate::gc::Address;
use crate::language::ty::SourceType;
use crate::safepoint;
use crate::stdlib;
use crate::vm::VM;

pub struct Stubs {
    compile_stub: Option<Address>,
    dora_entry_stub: Option<Address>,
    trap_stub: Option<Address>,
    stack_overflow_stub: Option<Address>,
    safepoint_stub: Option<Address>,
}

impl Stubs {
    pub fn new() -> Stubs {
        Stubs {
            compile_stub: None,
            dora_entry_stub: None,
            trap_stub: None,
            stack_overflow_stub: None,
            safepoint_stub: None,
        }
    }

    pub fn compile_stub(&self) -> Address {
        self.compile_stub.expect("uninitialized field")
    }

    pub fn dora_entry_stub(&self) -> Address {
        self.dora_entry_stub.expect("uninitialized field")
    }

    pub fn trap_stub(&self) -> Address {
        self.trap_stub.expect("uninitialized field")
    }

    pub fn stack_overflow_stub(&self) -> Address {
        self.stack_overflow_stub.expect("uninitialized field")
    }

    pub fn safepoint_stub(&self) -> Address {
        self.safepoint_stub.expect("uninitialized field")
    }
}

pub fn setup_stubs(vm: &mut VM) {
    vm.stubs.dora_entry_stub = Some(dora_entry_stub::install(vm).instruction_start());

    let ifct = NativeFct {
        fctptr: Address::from_ptr(stdlib::trap as *const u8),
        args: &[SourceType::Int32],
        return_type: SourceType::Unit,
        desc: NativeFctKind::TrapStub,
    };
    let code = dora_exit_stubs::generate(vm, ifct, false);
    vm.stubs.trap_stub = Some(code.instruction_start());

    vm.stubs.compile_stub = Some(lazy_compile_stub::generate(vm).instruction_start());

    let ifct = NativeFct {
        fctptr: Address::from_ptr(safepoint::stack_overflow as *const u8),
        args: &[],
        return_type: SourceType::Unit,
        desc: NativeFctKind::GuardCheckStub,
    };
    let code = dora_exit_stubs::generate(vm, ifct, false);
    vm.stubs.stack_overflow_stub = Some(code.instruction_start());

    let ifct = NativeFct {
        fctptr: Address::from_ptr(safepoint::safepoint_slow as *const u8),
        args: &[],
        return_type: SourceType::Unit,
        desc: NativeFctKind::SafepointStub,
    };
    let code = dora_exit_stubs::generate(vm, ifct, false);
    vm.stubs.safepoint_stub = Some(code.instruction_start());
}
