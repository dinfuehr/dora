use crate::gc::root::Slot;
use crate::gc::Address;
use crate::handle::Handle;
use crate::object::{Header, Ref};
use crate::threads::{current_thread, DoraThreadPtr};
use crate::vm::get_vm;
use parking_lot::Mutex;
use std::mem::MaybeUninit;
use std::sync::atomic::{AtomicI32, Ordering};

pub struct WaitLists {
    data: Mutex<ObjectHashMap<HeadAndTail>>,
}

impl WaitLists {
    pub fn new() -> WaitLists {
        WaitLists {
            data: Mutex::new(ObjectHashMap::new()),
        }
    }

    pub fn block(&self, mutex: Handle<ManagedMutex>, value: i32) {
        let thread = current_thread();
        let thread_ptr = DoraThreadPtr::new(thread);

        {
            let mut data = self.data.lock();
            let key = mutex.direct_ptr();

            let current_state = {
                let atomic_object = mutex.state;
                atomic_object.value.load(Ordering::SeqCst)
            };

            if current_state != value {
                return;
            }

            append_to_waitlist(thread_ptr, key, &mut *data);
        }

        thread.block();
    }

    pub fn enqueue(&self, condition: Handle<ManagedCondition>) {
        let thread = current_thread();
        let thread_ptr = DoraThreadPtr::new(thread);

        let mut data = self.data.lock();
        let key = condition.direct_ptr();

        {
            let atomic_object = condition.state;
            atomic_object.value.store(1, Ordering::SeqCst)
        }

        append_to_waitlist(thread_ptr, key, &mut *data);
    }

    pub fn wakeup(&self, address: Address) {
        let mut data = self.data.lock();
        let key = address;

        let entry = data.get(key).cloned();

        if let Some(entry) = entry {
            let wakeup_thread = entry.head.to_ref();
            let next = wakeup_thread.remove_from_waitlist();

            if next.is_null() {
                data.remove(key);
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

    pub fn wakeup_all(&self, address: Address) {
        let mut data = self.data.lock();
        let key = address;

        let entry = data.remove(key);

        if let Some(entry) = entry {
            let mut next = entry.head;

            while !next.is_null() {
                let wakeup_thread = next.to_ref();
                next = wakeup_thread.remove_from_waitlist();
            }
        }
    }

    pub fn visit_roots<F>(&self, fct: F)
    where
        F: FnMut(Slot),
    {
        let mut data = self.data.lock();
        data.visit_roots(fct);
    }
}

fn append_to_waitlist(
    thread_ptr: DoraThreadPtr,
    key: Address,
    data: &mut ObjectHashMap<HeadAndTail>,
) {
    let entry = data.get(key).cloned();
    let (head, tail) = match entry {
        Some(entry) => {
            let old_tail = entry.tail.to_ref();
            old_tail.set_waitlist_successor(thread_ptr);
            (entry.head, thread_ptr)
        }
        None => (thread_ptr, thread_ptr),
    };

    thread_ptr.to_ref().prepare_for_waitlist();
    let new_entry = HeadAndTail { head, tail };
    data.insert(key, new_entry);
}

#[derive(Clone)]
struct HeadAndTail {
    head: DoraThreadPtr,
    tail: DoraThreadPtr,
}
#[repr(C)]
pub struct ManagedMutex {
    header: Header,
    state: Ref<AtomicInt32>,
}

#[repr(C)]
pub struct ManagedCondition {
    header: Header,
    state: Ref<AtomicInt32>,
}

#[repr(C)]
pub struct AtomicInt32 {
    header: Header,
    value: AtomicI32,
}

const MIN_CAPACITY: usize = 8;
const EMPTY: usize = 0;
const DELETED: usize = 1;

pub struct ObjectHashMap<T> {
    data: Box<[HashMapEntry<T>]>,
    entries: usize,
    capacity: usize,
    gc_epoch: usize,
}

impl<T> ObjectHashMap<T> {
    fn new() -> ObjectHashMap<T> {
        ObjectHashMap {
            data: Box::new([]),
            entries: 0,
            capacity: 0,
            gc_epoch: 0,
        }
    }

    fn with_capacity(capacity: usize) -> ObjectHashMap<T> {
        let boxed_slice =
            unsafe { Box::<[HashMapEntry<T>]>::new_zeroed_slice(capacity).assume_init() };
        ObjectHashMap {
            data: boxed_slice,
            entries: 0,
            capacity: capacity,
            gc_epoch: get_vm().gc_epoch(),
        }
    }

    fn get(&mut self, key: Address) -> Option<&T> {
        if self.entries == 0 {
            return None;
        }

        self.maybe_rehash_on_get();
        let hash = key.to_usize();
        let mut idx = hash & (self.capacity - 1);

        loop {
            if self.is_live(idx) {
                if self.data[idx].key == key {
                    return Some(&self.data[idx].value);
                }
            } else if self.is_deleted(idx) {
                // There might be live entries after a deleted one.
            } else {
                debug_assert!(self.is_empty(idx));
                return None;
            }

            idx = (idx + 1) & (self.capacity - 1);
        }
    }

    fn insert(&mut self, key: Address, value: T) {
        self.maybe_rehash_on_insert();
        assert!(self.entries < self.capacity);

        let hash = key.to_usize();
        let mut idx = hash & (self.capacity - 1);

        loop {
            if self.is_live(idx) {
                if self.data[idx].key == key {
                    self.data[idx].value = value;
                    return;
                }
            } else {
                debug_assert!(self.is_empty(idx) || self.is_deleted(idx));
                self.data[idx].key = key;
                self.data[idx].value = value;
                self.entries += 1;
                return;
            }

            idx = (idx + 1) & (self.capacity - 1);
        }
    }

    fn remove(&mut self, key: Address) -> Option<T> {
        self.maybe_rehash_on_remove();

        let hash = key.to_usize();
        let mut idx = hash & (self.capacity - 1);

        loop {
            if self.is_live(idx) {
                if key == self.data[idx].key {
                    self.data[idx].key = DELETED.into();
                    let new_value: T = unsafe { MaybeUninit::uninit().assume_init() };
                    let value = std::mem::replace(&mut self.data[idx].value, new_value);
                    return Some(value);
                }
            } else if self.is_deleted(idx) {
                // There might be live entries after a deleted one.
            } else {
                assert!(self.is_empty(idx));
                return None;
            }

            idx = (idx + 1) & (self.capacity - 1);
        }
    }

    fn maybe_rehash_on_get(&mut self) {
        if self.invalidated_by_gc() {
            self.rehash(self.capacity);
        }
    }

    fn maybe_rehash_on_insert(&mut self) {
        if self.invalidated_by_gc() || self.overflow() {
            self.rehash(capacity_for_entries(self.entries + 1));
        }
    }

    fn maybe_rehash_on_remove(&mut self) {
        if self.invalidated_by_gc() || self.underflow() {
            self.rehash(capacity_for_entries(self.entries));
        }
    }

    fn overflow(&self) -> bool {
        self.entries + 1 > self.capacity - (self.capacity / 4)
    }

    fn underflow(&self) -> bool {
        self.entries < self.capacity / 4
    }

    fn invalidated_by_gc(&self) -> bool {
        self.gc_epoch != get_vm().gc_epoch()
    }

    fn rehash(&mut self, new_capacity: usize) {
        let mut new_map = ObjectHashMap::with_capacity(new_capacity);

        for idx in 0..self.data.len() {
            if self.is_live(idx) {
                let dummy: HashMapEntry<T> = unsafe { MaybeUninit::uninit().assume_init() };
                let entry = std::mem::replace(&mut self.data[idx], dummy);
                new_map.insert(entry.key, entry.value);
            }
        }

        self.data = std::mem::replace(&mut new_map.data, Box::new([]));
        self.entries = new_map.entries;
        self.capacity = new_map.capacity;
        self.gc_epoch = new_map.gc_epoch;
    }

    fn is_live(&self, idx: usize) -> bool {
        self.data[idx].key.to_usize() > DELETED
    }

    fn is_deleted(&self, idx: usize) -> bool {
        self.data[idx].key.to_usize() == DELETED
    }

    fn is_empty(&self, idx: usize) -> bool {
        self.data[idx].key.to_usize() == EMPTY
    }

    fn visit_roots<F>(&mut self, mut fct: F)
    where
        F: FnMut(Slot),
    {
        for idx in 0..self.data.len() {
            if self.is_live(idx) {
                let address = Address::from_ptr(&self.data[idx].key as *const _);
                fct(Slot::at(address));
            }
        }
    }
}

fn capacity_for_entries(entries: usize) -> usize {
    let mut capacity = MIN_CAPACITY;

    while entries > capacity - (capacity / 4) {
        capacity *= 2;
    }

    capacity
}

struct HashMapEntry<T> {
    key: Address,
    value: T,
}
