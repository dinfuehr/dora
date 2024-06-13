use parking_lot::{Condvar, Mutex};

use std::collections::HashMap;

use crate::gc::Address;
use crate::threads::{current_thread, parked_scope};
use crate::vm::{CodeId, VM};
use dora_bytecode::{BytecodeTypeArray, FunctionId};

#[derive(PartialEq, Debug)]
enum CompilationStatus {
    Compiled(CodeId),
    InProgress,
}

pub struct CompilationDatabase {
    inner: Mutex<HashMap<(FunctionId, BytecodeTypeArray), CompilationStatus>>,
    cv_notify: Condvar,
}

impl CompilationDatabase {
    pub fn new() -> CompilationDatabase {
        CompilationDatabase {
            inner: Mutex::new(HashMap::new()),
            cv_notify: Condvar::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.inner.lock().is_empty()
    }

    pub fn is_compiled(
        &self,
        vm: &VM,
        id: FunctionId,
        type_params: BytecodeTypeArray,
    ) -> Option<Address> {
        let inner = self.inner.lock();

        if let Some(status) = inner.get(&(id, type_params.clone())) {
            match status {
                CompilationStatus::Compiled(code_id) => {
                    let code = vm.code_objects.get(*code_id);
                    Some(code.instruction_start())
                }

                CompilationStatus::InProgress => None,
            }
        } else {
            None
        }
    }

    pub fn compilation_request(
        &self,
        vm: &VM,
        id: FunctionId,
        type_params: BytecodeTypeArray,
    ) -> Option<Address> {
        // We might block here if compilation is already in progress for this specific function.
        // In order to not block safepoints we park the thread right away.
        parked_scope(|| {
            let mut inner = self.inner.lock();

            loop {
                if let Some(status) = inner.get(&(id, type_params.clone())) {
                    match status {
                        CompilationStatus::Compiled(code_id) => {
                            let code = vm.code_objects.get(*code_id);
                            return Some(code.instruction_start());
                        }

                        CompilationStatus::InProgress => {
                            assert!(current_thread().is_parked());
                            self.cv_notify.wait(&mut inner);
                        }
                    }
                } else {
                    inner.insert((id, type_params), CompilationStatus::InProgress);
                    return None;
                }
            }
        })
    }

    pub fn finish_compilation(
        &self,
        id: FunctionId,
        type_params: BytecodeTypeArray,
        code_id: CodeId,
    ) {
        let mut inner = self.inner.lock();

        let old_value = inner.insert((id, type_params), CompilationStatus::Compiled(code_id));
        assert_eq!(old_value, Some(CompilationStatus::InProgress));

        self.cv_notify.notify_all();
    }

    pub fn compile_aot(&self, id: FunctionId, type_params: BytecodeTypeArray, code_id: CodeId) {
        let mut inner = self.inner.lock();

        assert!(inner
            .insert((id, type_params), CompilationStatus::Compiled(code_id))
            .is_none());
    }
}
