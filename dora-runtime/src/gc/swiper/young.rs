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
    pub fn new(total: Region, semi_size: usize, protect: bool) -> YoungGen {
        let semi = SemiSpace::new(total, semi_size);
        let to_committed = semi.to_committed();

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
                current_committed_size: semi_size / 2,
                age_marker: to_committed.start(),
            }),
        };

        young.commit();
        young.protect_from();

        young
    }

    pub(super) fn setup(&self, vm: &VM) {
        let to_committed = self.to_committed();
        fill_region(vm, to_committed.start(), to_committed.end());
    }

    fn commit(&self) {
        self.semi.commit();
    }

    pub fn from_committed(&self) -> Region {
        self.semi.from_committed()
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
        self.semi.to_committed()
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

        let mut protected = self.protected.lock();
        protected.from_index = (protected.from_index + 1) % 2;

        let to_block = self.semi.to_block();
        protected.top = to_block.start;
        protected.current_limit = self.semi.to_block().committed().end();
    }

    pub fn reset_after_minor_gc(&self, top: Address) {
        let vm = get_vm();
        let from_committed = self.semi.from_committed();
        fill_region(vm, from_committed.start(), from_committed.end());
        self.semi.protect_from();

        let mut protected = self.protected.lock();
        protected.top = top;
        protected.age_marker = top;
        fill_region(vm, top, protected.current_limit);
    }

    pub fn bump_alloc(&self, size: usize) -> Address {
        let mut protected = self.protected.lock();
        assert_eq!(
            protected.current_limit,
            self.semi.to_block().committed().end()
        );
        if let Some(alloc) = protected.raw_alloc(size) {
            assert!(alloc.is_non_null());
            fill_region_with(get_vm(), protected.top, protected.current_limit, false);
            alloc
        } else {
            Address::null()
        }
    }

    pub fn resize_after_gc(&self, semi_size: usize) {
        assert!(gen_aligned(semi_size));
        self.semi.set_limit(semi_size);

        let vm = get_vm();
        self.unprotect_from();
        let from_committed = self.semi.from_committed();
        fill_region(vm, from_committed.start(), from_committed.end());
        self.protect_from();

        let mut protected = self.protected.lock();
        protected.current_limit = self.semi.to_committed().end();
        protected.current_committed_size = semi_size / 2;
        fill_region(vm, protected.top, protected.current_limit);
    }

    pub fn committed_size(&self) -> usize {
        let protected = self.protected.lock();
        let semi_result = self.semi.committed_size();
        let new_result = protected.current_committed_size * 2;
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

            from_index: AtomicUsize::new(2),
        }
    }

    fn commit(&self) {
        self.from_block().commit();
        self.to_block().commit();
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
        self.from_block().set_limit(size / 2);
        self.to_block().set_limit(size / 2);
    }

    fn committed_size(&self) -> usize {
        self.from_block().committed_size() + self.to_block().committed_size()
    }
}

struct Block {
    start: Address,
    end: Address,
    committed: AtomicUsize,
    limit: AtomicUsize,
}

impl Block {
    fn new(region: Region, committed_size: usize) -> Block {
        assert!(committed_size <= region.size());
        let committed = region.start.offset(committed_size);

        Block {
            start: region.start,
            end: region.end,
            committed: AtomicUsize::new(committed.to_usize()),
            limit: AtomicUsize::new(committed.to_usize()),
        }
    }

    fn commit(&self) {
        let size = self.committed_size();

        if size > 0 {
            os::commit_at(self.start, size, MemoryPermission::ReadWrite);
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

    fn set_limit(&self, new_size: usize) {
        assert!(mem::is_os_page_aligned(new_size));

        let old_committed = self.committed.load(Ordering::Relaxed);
        let new_committed = self.start.offset(new_size).to_usize();
        assert!(new_committed <= self.end.to_usize());

        if old_committed == new_committed {
            return;
        }

        let updated = self.committed.swap(new_committed, Ordering::Relaxed);
        assert!(updated == old_committed);
        self.limit.store(new_committed, Ordering::Relaxed);

        if old_committed < new_committed {
            let size = new_committed - old_committed;
            os::commit_at(old_committed.into(), size, MemoryPermission::ReadWrite);
        } else if old_committed > new_committed {
            let size = old_committed - new_committed;
            os::discard(new_committed.into(), size);
        }
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
    current_committed_size: usize,
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
