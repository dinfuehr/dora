use crate::gc::Address;
use crate::handle::Handle;
use crate::object::Header;
use crate::threads::{current_thread, parked_scope, DoraThreadPtr};
use parking_lot::Mutex;
use std::collections::HashMap;
use std::sync::atomic::{AtomicI32, Ordering};

pub struct MutexMap {
    data: Mutex<HashMap<Address, HeadAndTail>>,
}

impl MutexMap {
    pub fn new() -> MutexMap {
        MutexMap {
            data: Mutex::new(HashMap::new()),
        }
    }

    pub fn wait(&self, mutex: Handle<ManagedMutex>, value: i32) {
        let thread = current_thread();
        let thread_ptr = DoraThreadPtr::new(thread);

        {
            let mut data = self.data.lock();
            let key = mutex.direct_ptr();

            if mutex.state.load(Ordering::SeqCst) != value {
                return;
            }

            let entry = data.get(&key).cloned();
            let (head, tail) = match entry {
                Some(entry) => {
                    let prev_thread = entry.tail.as_ref();
                    let mut prev_thread_data = prev_thread.blocking.lock();
                    let (blocking, next) = *prev_thread_data;
                    assert!(blocking && next.is_null());
                    *prev_thread_data = (true, thread_ptr);
                    (entry.head, entry.tail)
                }
                None => (thread_ptr, thread_ptr),
            };

            {
                let mut thread_data = thread.blocking.lock();
                let (blocking, next) = *thread_data;
                assert!(!blocking && next.is_null());
                *thread_data = (true, DoraThreadPtr::null());
            }

            data.insert(key, HeadAndTail { head, tail });
        }

        parked_scope(|| {
            let mut blocking_data = thread.blocking.lock();

            while blocking_data.0 {
                thread.cv_blocking.wait(&mut blocking_data);
            }
        });
    }

    pub fn notify(&self, mutex: Handle<ManagedMutex>) {
        let mut data = self.data.lock();
        let key = mutex.direct_ptr();

        let entry = data.get(&key).cloned();

        if let Some(entry) = entry {
            let wakeup_thread = entry.head.as_ref();

            let next = {
                let mut thread_data = wakeup_thread.blocking.lock();
                let (blocking, next) = *thread_data;
                assert!(blocking);
                *thread_data = (false, DoraThreadPtr::null());

                next
            };

            wakeup_thread.cv_blocking.notify_one();

            if next.is_null() {
                data.remove(&key);
            } else {
                data.insert(
                    key,
                    HeadAndTail {
                        head: next,
                        tail: entry.tail,
                    },
                );
            }
        }
    }
}

#[derive(Clone)]
struct HeadAndTail {
    head: DoraThreadPtr,
    tail: DoraThreadPtr,
}
#[repr(C)]
pub struct ManagedMutex {
    header: Header,
    state: AtomicI32,
}
