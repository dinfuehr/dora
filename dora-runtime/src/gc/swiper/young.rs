use parking_lot::Mutex;

use std::sync::atomic::{AtomicUsize, Ordering};

use crate::gc::bump::BumpAllocator;
use crate::gc::{fill_region, fill_region_with, gen_aligned, Address, GenerationAllocator, Region};
use crate::mem;
use crate::os::{self, MemoryPermission};
use crate::vm::{get_vm, VM};

pub struct YoungGen {
    // bounds of eden & semi-spaces
    total: Region,

    semispaces: [Region; 2],

    // from/to-space
    semi: SemiSpace,

    protected: Mutex<YoungGenProtected>,
}

impl YoungGen {
    pub fn new(total: Region, semi_size: usize, protect: bool) -> YoungGen {
        let semi = SemiSpace::new(total, semi_size, protect);
        let to_committed = semi.to_committed();

        let semispaces = [semi.first.total(), semi.second.total()];

        let young = YoungGen {
            total,
            semispaces,
            semi,
            protected: Mutex::new(YoungGenProtected {
                top: to_committed.start(),
                current_limit: to_committed.end(),
                from_index: 0,
                current_committed_size: semi_size / 2,
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

    pub fn from_active(&self) -> Region {
        self.semi.from_active()
    }

    #[allow(dead_code)]
    pub fn from_committed(&self) -> Region {
        self.semi.from_committed()
    }

    pub fn from_total(&self) -> Region {
        self.semi.from_total()
    }

    pub fn to_active(&self) -> Region {
        self.semi.to_active()
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

    pub fn clear(&self) {
        self.semi.clear_from();
        self.semi.clear_to();

        let alloc_region = self.semi.to_committed();
        self.semi.set_age_marker(alloc_region.start());

        let mut protected = self.protected.lock();
        protected.top = alloc_region.start();
        protected.current_limit = alloc_region.end();
    }

    pub fn active_size(&self) -> usize {
        self.semi.from_block().active().size()
    }

    pub fn unprotect_from(&self) {
        self.semi.unprotect_from();
    }

    pub fn protect_from(&self) {
        self.semi.protect_from();
    }

    pub fn swap_semi(&self) {
        self.semi.swap();

        let mut protected = self.protected.lock();
        protected.from_index = (protected.from_index + 1) % 2;

        let to_block = self.semi.to_block();
        protected.top = to_block.start;
        protected.current_limit = self.semi.to_block().committed().end();
    }

    pub fn minor_fail(&self, top: Address) {
        self.semi.to_block().set_top(top);
    }

    pub fn minor_success(&self, top: Address) {
        self.semi.clear_from();
        self.semi.protect_from();
        self.semi.to_block().set_top(top);
        self.semi.set_age_marker(top);
        let mut protected = self.protected.lock();
        protected.top = top;
    }

    pub fn should_be_promoted(&self, addr: Address) -> bool {
        debug_assert!(self.total.contains(addr));

        if addr < self.semi.total.start {
            return false;
        }

        self.semi.should_be_promoted(addr)
    }

    pub fn bump_alloc(&self, size: usize) -> Address {
        let mut protected = self.protected.lock();
        assert_eq!(
            protected.current_limit,
            self.semi.to_block().committed().end()
        );
        assert_eq!(protected.top, self.semi.to_block().alloc.top());
        if let Some(alloc) = protected.raw_alloc(size) {
            assert!(alloc.is_non_null());
            let bump_alloc = self.semi.bump_alloc(size);
            assert_eq!(bump_alloc, alloc);
            fill_region_with(get_vm(), protected.top, protected.current_limit, false);
            alloc
        } else {
            Address::null()
        }
    }

    pub fn set_limit(&self, semi_size: usize) {
        assert!(gen_aligned(semi_size));
        self.semi.set_limit(semi_size);

        let mut protected = self.protected.lock();
        protected.current_limit = self.semi.to_committed().end();
        protected.current_committed_size = semi_size / 2;
    }

    pub fn committed_size(&self) -> usize {
        self.semi.committed_size()
    }
}

struct SemiSpace {
    total: Region,

    first: Block,
    second: Block,

    // decides whether first or second is the from-space (value 1=first or 2=second)
    from_index: AtomicUsize,

    protect: bool,
    age_marker: AtomicUsize,
}

impl SemiSpace {
    fn new(total: Region, committed_semi_size: usize, protect: bool) -> SemiSpace {
        let total_semi_size = total.size() / 2;
        assert!(committed_semi_size <= total_semi_size);
        assert!(gen_aligned(committed_semi_size));

        let first = total.start.region_start(total_semi_size);
        let second = Region::new(first.end, total.end);
        assert!(first.size() == second.size());

        SemiSpace {
            total: total.clone(),

            first: Block::new(first.clone(), committed_semi_size / 2),
            second: Block::new(second, committed_semi_size / 2),

            from_index: AtomicUsize::new(2),
            protect,
            age_marker: AtomicUsize::new(first.start.to_usize()),
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

    fn from_active(&self) -> Region {
        self.from_block().active()
    }

    fn to_total(&self) -> Region {
        self.to_block().total()
    }

    fn to_committed(&self) -> Region {
        self.to_block().committed()
    }

    fn to_active(&self) -> Region {
        self.to_block().active()
    }

    // Make from-space writable.
    fn unprotect_from(&self) {
        // make memory writable again, so that we
        // can copy objects to the from-space.
        // Since this has some overhead, do it only in debug builds.
        if cfg!(debug_assertions) || self.protect {
            let from_space = self.from_committed();

            os::protect(
                from_space.start,
                from_space.size(),
                MemoryPermission::ReadWrite,
            );
        }
    }

    // Make from-space inaccessible.
    fn protect_from(&self) {
        // Make from-space unaccessible both from read/write.
        // Since this has some overhead, do it only in debug builds.
        if cfg!(debug_assertions) || self.protect {
            let from_space = self.from_committed();
            os::protect(from_space.start, from_space.size(), MemoryPermission::None);
        }
    }

    // Free all objects in from-semi-space.
    fn clear_from(&self) {
        self.from_block().reset_top();
    }

    // Free all objects in to-semi-space.
    fn clear_to(&self) {
        self.to_block().reset_top();
    }

    // Switch from- & to-semi-space.
    fn swap(&self) {
        self.swap_from_index();
    }

    fn set_age_marker(&self, top: Address) {
        self.age_marker.store(top.to_usize(), Ordering::Relaxed);
    }

    fn age_marker(&self) -> Address {
        self.age_marker.load(Ordering::Relaxed).into()
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

    fn should_be_promoted(&self, addr: Address) -> bool {
        let age_marker = self.age_marker();
        debug_assert!(self.from_active().contains(addr));
        debug_assert!(self.from_active().valid_top(age_marker));
        return addr < age_marker;
    }

    fn bump_alloc(&self, size: usize) -> Address {
        self.to_block().bump_alloc(size)
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
    alloc: BumpAllocator,
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
            alloc: BumpAllocator::new(region.start, committed),
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
        assert!(self.alloc.top().to_usize() <= new_committed);

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

        self.alloc.reset_limit(new_committed.into());
    }

    fn active(&self) -> Region {
        Region::new(self.start, self.alloc.top())
    }

    fn reset_top(&self) {
        let committed = self.committed.load(Ordering::Relaxed);
        self.alloc.reset(self.start, committed.into());
    }

    fn set_top(&self, addr: Address) {
        let committed = self.committed.load(Ordering::Relaxed);
        assert!(self.start <= addr && addr <= committed.into());
        self.alloc.reset(addr, committed.into())
    }

    fn bump_alloc(&self, size: usize) -> Address {
        self.alloc.bump_alloc(size)
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
