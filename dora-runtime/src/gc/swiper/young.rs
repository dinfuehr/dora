use parking_lot::Mutex;

use std::sync::atomic::{AtomicUsize, Ordering};

use crate::gc::{fill_region, fill_region_with, gen_aligned, Address, GenerationAllocator, Region};
use crate::mem;
use crate::os::{self, MemoryPermission};
use crate::vm::{get_vm, VM};

pub struct YoungGen {
    // bounds of eden & semi-spaces
    total: Region,

    semispaces: [Region; 2],
    from_index: AtomicUsize,

    // from/to-space
    semi: SemiSpace,

    protect: bool,

    protected: Mutex<YoungGenProtected>,
}

impl YoungGen {
    pub fn new(total: Region, young_size: usize, protect: bool) -> YoungGen {
        let semi = SemiSpace::new(total, young_size);
        let to_committed = semi.to_committed();
        let semi_size = young_size / 2;

        let semispaces = [semi.first.total(), semi.second.total()];

        let young = YoungGen {
            total,
            semispaces,
            from_index: AtomicUsize::new(0),
            semi,
            protect,
            protected: Mutex::new(YoungGenProtected {
                top: to_committed.start(),
                current_limit: to_committed.end(),
                from_index: 0,
                current_semi_size: semi_size,
                age_marker: to_committed.start(),
            }),
        };

        young.commit(semi_size);
        young.protect_from();

        young
    }

    pub(super) fn setup(&self, vm: &VM) {
        let to_committed = self.to_committed();
        fill_region(vm, to_committed.start(), to_committed.end());
    }

    fn commit(&self, semi_size: usize) {
        self.commit_semi_space(self.semispaces[0], 0, semi_size);
        self.commit_semi_space(self.semispaces[1], 0, semi_size);
    }

    pub fn from_committed(&self) -> Region {
        let size = self.current_semi_size();
        self.from_region().start().region_start(size)
    }

    fn from_index(&self) -> usize {
        self.from_index.load(Ordering::Relaxed)
    }

    fn swap_indices(&self) {
        let from_index = self.from_index();
        self.from_index
            .store((from_index + 1) % 2, Ordering::Relaxed);
    }

    fn from_region(&self) -> Region {
        self.semispaces[self.from_index()]
    }

    fn to_index(&self) -> usize {
        self.from_index() ^ 1
    }

    fn to_region(&self) -> Region {
        self.semispaces[self.to_index()]
    }

    fn current_semi_size(&self) -> usize {
        let protected = self.protected.lock();
        protected.current_semi_size
    }

    pub fn from_total(&self) -> Region {
        self.semi.from_total()
    }

    pub fn object_size(&self) -> usize {
        let protected = self.protected.lock();
        let start = self.to_total().start();
        protected.top.offset_from(start)
    }

    pub fn to_committed(&self) -> Region {
        let size = self.current_semi_size();
        self.to_region().start().region_start(size)
    }

    pub fn to_total(&self) -> Region {
        self.semi.to_total()
    }

    pub fn total(&self) -> Region {
        self.total.clone()
    }

    pub fn contains(&self, addr: Address) -> bool {
        self.total.contains(addr)
    }

    pub fn reset_after_full_gc(&self) {
        let from_committed = self.semi.to_committed();
        let to_committed = self.semi.to_committed();
        let vm = get_vm();
        fill_region(vm, from_committed.start(), from_committed.end());
        fill_region(vm, to_committed.start(), to_committed.end());

        let mut protected = self.protected.lock();
        protected.top = to_committed.start();
        protected.current_limit = to_committed.end();
        protected.age_marker = to_committed.start();
    }

    pub fn unprotect_from(&self) {
        if cfg!(debug_assertions) || self.protect {
            self.semi.unprotect_from();
        }
    }

    pub fn protect_from(&self) {
        if cfg!(debug_assertions) || self.protect {
            self.semi.protect_from();
        }
    }

    pub fn age_marker(&self) -> Address {
        let protected = self.protected.lock();
        protected.age_marker
    }

    pub fn swap_semi(&self) {
        self.semi.swap();
        self.swap_indices();

        assert_eq!(self.semi.from_total(), self.from_region());
        assert_eq!(self.semi.to_total(), self.to_region());

        let mut protected = self.protected.lock();
        let to_block = self.semi.to_block().committed();
        protected.top = to_block.start();
        protected.current_limit = to_block.end();
    }

    pub fn reset_after_minor_gc(&self, top: Address) {
        let vm = get_vm();
        let from_committed = self.semi.from_committed();
        fill_region(vm, from_committed.start(), from_committed.end());
        self.protect_from();

        let mut protected = self.protected.lock();
        protected.top = top;
        protected.age_marker = top;
        fill_region(vm, top, protected.current_limit);
    }

    pub fn bump_alloc(&self, size: usize) -> Option<Address> {
        let mut protected = self.protected.lock();
        assert_eq!(
            protected.current_limit,
            self.semi.to_block().committed().end()
        );
        if let Some(alloc) = protected.raw_alloc(size) {
            assert!(alloc.is_non_null());
            fill_region_with(get_vm(), protected.top, protected.current_limit, false);
            Some(alloc)
        } else {
            None
        }
    }

    pub fn resize_after_gc(&self, young_size: usize) {
        assert!(gen_aligned(young_size));
        let mut protected = self.protected.lock();
        let new_semi_size = young_size;
        let old_semi_size = protected.current_semi_size;
        self.semi.set_limit(young_size);

        self.commit_semi_space(self.semispaces[0], old_semi_size, new_semi_size);
        self.commit_semi_space(self.semispaces[1], old_semi_size, new_semi_size);

        let vm = get_vm();
        self.unprotect_from();
        let from_committed = self.semi.from_committed();
        fill_region(vm, from_committed.start(), from_committed.end());
        self.protect_from();

        protected.current_limit = self.semi.to_committed().end();
        protected.current_semi_size = young_size / 2;
        fill_region(vm, protected.top, protected.current_limit);
    }

    fn commit_semi_space(&self, block: Region, old_size: usize, new_size: usize) {
        assert!(mem::is_os_page_aligned(new_size));

        if old_size == new_size {
            return;
        }

        if old_size < new_size {
            let size = new_size - old_size;
            let start = block.start().offset(old_size);
            os::commit_at(start, size, MemoryPermission::ReadWrite);
        } else {
            assert!(new_size < old_size);
            let size = old_size - new_size;
            let start = block.start().offset(new_size);
            os::discard(start, size);
        }
    }

    pub fn committed_size(&self) -> usize {
        let protected = self.protected.lock();
        let semi_result = self.semi.committed_size();
        let new_result = protected.current_semi_size * 2;
        assert_eq!(semi_result, new_result);
        new_result
    }
}

struct SemiSpace {
    first: Block,
    second: Block,

    // decides whether first or second is the from-space (value 1=first or 2=second)
    from_index: AtomicUsize,
}

impl SemiSpace {
    fn new(total: Region, committed_semi_size: usize) -> SemiSpace {
        let total_semi_size = total.size() / 2;
        assert!(committed_semi_size <= total_semi_size);
        assert!(gen_aligned(committed_semi_size));

        let first = total.start.region_start(total_semi_size);
        let second = Region::new(first.end, total.end);
        assert!(first.size() == second.size());

        SemiSpace {
            first: Block::new(first.clone(), committed_semi_size / 2),
            second: Block::new(second, committed_semi_size / 2),

            from_index: AtomicUsize::new(1),
        }
    }

    fn from_block(&self) -> &Block {
        let from_index = self.from_index.load(Ordering::Relaxed);
        if from_index == 1 {
            &self.first
        } else {
            assert!(from_index == 2);
            &self.second
        }
    }

    fn to_block(&self) -> &Block {
        let from_index = self.from_index.load(Ordering::Relaxed);
        if from_index == 1 {
            &self.second
        } else {
            assert!(from_index == 2);
            &self.first
        }
    }

    fn from_total(&self) -> Region {
        self.from_block().total()
    }

    fn from_committed(&self) -> Region {
        self.from_block().committed()
    }

    fn to_total(&self) -> Region {
        self.to_block().total()
    }

    fn to_committed(&self) -> Region {
        self.to_block().committed()
    }

    // Make from-space writable.
    fn unprotect_from(&self) {
        // make memory writable again, so that we
        // can copy objects to the from-space.
        // Since this has some overhead, do it only in debug builds.
        let from_space = self.from_committed();

        os::protect(
            from_space.start,
            from_space.size(),
            MemoryPermission::ReadWrite,
        );
    }

    // Make from-space inaccessible.
    fn protect_from(&self) {
        // Make from-space unaccessible both from read/write.
        // Since this has some overhead, do it only in debug builds.
        let from_space = self.from_committed();
        os::protect(from_space.start, from_space.size(), MemoryPermission::None);
    }

    // Switch from- & to-semi-space.
    fn swap(&self) {
        self.swap_from_index();
    }

    fn swap_from_index(&self) {
        let from_index = self.from_index.load(Ordering::Relaxed);

        let updated_from_index = if from_index == 1 {
            2
        } else {
            assert!(from_index == 2);
            1
        };

        self.from_index.store(updated_from_index, Ordering::Relaxed);
    }

    fn set_limit(&self, size: usize) {
        self.from_block().set_committed(size / 2);
        self.to_block().set_committed(size / 2);
    }

    fn committed_size(&self) -> usize {
        self.from_block().committed_size() + self.to_block().committed_size()
    }
}

struct Block {
    start: Address,
    end: Address,
    committed: AtomicUsize,
}

impl Block {
    fn new(region: Region, committed_size: usize) -> Block {
        assert!(committed_size <= region.size());
        let committed = region.start.offset(committed_size);

        Block {
            start: region.start,
            end: region.end,
            committed: AtomicUsize::new(committed.to_usize()),
        }
    }

    fn total(&self) -> Region {
        Region::new(self.start, self.end)
    }

    fn committed(&self) -> Region {
        let committed = self.committed.load(Ordering::Relaxed);
        Region::new(self.start, committed.into())
    }

    fn committed_size(&self) -> usize {
        let committed = self.committed.load(Ordering::Relaxed);
        committed - self.start.to_usize()
    }

    fn set_committed(&self, new_size: usize) {
        assert!(mem::is_os_page_aligned(new_size));
        let new_committed = self.start.offset(new_size).to_usize();
        self.committed.store(new_committed, Ordering::Relaxed);
    }
}

impl GenerationAllocator for YoungGen {
    fn allocate(&self, size: usize) -> Option<Address> {
        let mut protected = self.protected.lock();
        protected.raw_alloc(size)
    }

    fn free(&self, _region: Region) {
        // No free list yet, so simply ignore this.
    }
}

struct YoungGenProtected {
    top: Address,
    current_limit: Address,

    from_index: usize,
    current_semi_size: usize,
    age_marker: Address,
}

impl YoungGenProtected {
    fn raw_alloc(&mut self, size: usize) -> Option<Address> {
        let next = self.top.offset(size);

        if next <= self.current_limit {
            let result = self.top;
            self.top = next;
            Some(result)
        } else {
            None
        }
    }
}
