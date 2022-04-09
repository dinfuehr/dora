use parking_lot::{Condvar, Mutex};

use std::collections::HashMap;

use crate::gc::Address;
use crate::language::sem_analysis::FctDefinitionId;
use crate::language::ty::SourceTypeArray;
use crate::vm::{CodeId, VM};

#[derive(PartialEq, Debug)]
enum CompilationStatus {
    Compiled(CodeId),
    InProgress,
}

pub struct CompilationDatabase {
    inner: Mutex<HashMap<(FctDefinitionId, SourceTypeArray), CompilationStatus>>,
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
        id: FctDefinitionId,
        type_params: SourceTypeArray,
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
        id: FctDefinitionId,
        type_params: SourceTypeArray,
    ) -> Option<Address> {
        let mut inner = self.inner.lock();

        loop {
            if let Some(status) = inner.get(&(id, type_params.clone())) {
                match status {
                    CompilationStatus::Compiled(code_id) => {
                        let code = vm.code_objects.get(*code_id);
                        return Some(code.instruction_start());
                    }

                    CompilationStatus::InProgress => {
                        self.cv_notify.wait(&mut inner);
                    }
                }
            } else {
                inner.insert((id, type_params), CompilationStatus::InProgress);
                return None;
            }
        }
    }

    pub fn finish_compilation(
        &self,
        id: FctDefinitionId,
        type_params: SourceTypeArray,
        code_id: CodeId,
    ) {
        let mut inner = self.inner.lock();

        let old_value = inner.insert((id, type_params), CompilationStatus::Compiled(code_id));
        assert_eq!(old_value, Some(CompilationStatus::InProgress));

        self.cv_notify.notify_all();
    }
}
