use std::sync::atomic::{AtomicUsize, Ordering};

use crate::gc::bump::BumpAllocator;
use crate::gc::{gen_aligned, Address, Region};
use crate::mem;
use crate::os::{self, MemoryPermission};

pub struct YoungGen {
    // bounds of eden & semi-spaces
    total: Region,

    // eden space
    eden: Eden,

    // from/to-space
    semi: SemiSpace,
}

impl YoungGen {
    pub fn new(total: Region, eden_size: usize, semi_size: usize, protect: bool) -> YoungGen {
        let eden_total_size = total.size() / 2;

        let eden_total = total.start.region_start(eden_total_size);
        let semi_total = Region::new(eden_total.end, total.end);

        let young = YoungGen {
            total,
            eden: Eden::new(eden_total, eden_size),
            semi: SemiSpace::new(semi_total, semi_size, protect),
        };

        young.commit();
        young.protect_from();

        young
    }

    fn commit(&self) {
        self.eden.commit();
        self.semi.commit();
    }

    pub fn eden_active(&self) -> Region {
        self.eden.active()
    }

    pub fn eden_committed(&self) -> Region {
        self.eden.committed()
    }

    pub fn eden_total(&self) -> Region {
        self.eden.total()
    }

    pub fn from_active(&self) -> Region {
        self.semi.from_active()
    }

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

    pub fn valid_top(&self, addr: Address) -> bool {
        self.total.valid_top(addr)
    }

    pub fn clear(&self) {
        self.eden.reset_top();
        self.semi.clear_from();
        self.semi.clear_to();

        let to_block = self.semi.to_block();
        self.semi.set_age_marker(to_block.start);
    }

    pub fn active_size(&self) -> usize {
        self.eden.active().size() + self.semi.from_block().active().size()
    }

    pub fn unprotect_from(&self) {
        self.semi.unprotect_from();
    }

    pub fn protect_from(&self) {
        self.semi.protect_from();
    }

    pub fn clear_eden(&self) {
        self.eden.reset_top();
    }

    pub fn swap_semi(&self) {
        self.semi.swap();
    }

    pub fn minor_fail(&self, top: Address) {
        self.semi.to_block().set_top(top);
    }

    pub fn minor_success(&self, top: Address) {
        self.eden.reset_top();
        self.semi.clear_from();
        self.semi.protect_from();
        self.semi.to_block().set_top(top);
        self.semi.set_age_marker(top);
    }

    pub fn should_be_promoted(&self, addr: Address) -> bool {
        debug_assert!(self.total.contains(addr));

        if addr < self.semi.total.start {
            return false;
        }

        self.semi.should_be_promoted(addr)
    }

    pub fn bump_alloc(&self, size: usize) -> Address {
        let eden_ptr = self.eden.bump_alloc(size);

        if eden_ptr.is_non_null() {
            return eden_ptr;
        }

        return self.semi.bump_alloc(size);
    }

    pub fn set_limit(&self, eden_size: usize, semi_size: usize) {
        assert!(gen_aligned(eden_size));
        assert!(gen_aligned(semi_size));

        self.eden.set_limit(eden_size);
        self.semi.set_limit(semi_size);
    }

    pub fn committed_size(&self) -> (usize, usize) {
        (self.eden.committed_size(), self.semi.committed_size())
    }
}

struct Eden {
    total: Region,
    block: Block,
    alloc: BumpAllocator,
}

impl Eden {
    fn new(total: Region, committed_size: usize) -> Eden {
        let limit = total.start.offset(committed_size);

        Eden {
            total: total.clone(),
            block: Block::new(total.clone(), committed_size),
            alloc: BumpAllocator::new(total.start, limit),
        }
    }

    fn commit(&self) {
        self.block.commit();
    }

    fn total(&self) -> Region {
        self.block.total()
    }

    fn committed(&self) -> Region {
        self.block.committed()
    }

    fn committed_size(&self) -> usize {
        self.block.committed_size()
    }

    fn active(&self) -> Region {
        self.block.active()
    }

    fn reset_top(&self) {
        self.block.reset_top();
    }

    fn bump_alloc(&self, size: usize) -> Address {
        self.block.bump_alloc(size)
    }

    fn set_limit(&self, size: usize) {
        self.block.set_limit(size);
    }
}

struct SemiSpace {
    total: Region,

    first: Block,
    second: Block,

    // decides whether first or second is the from-space (value 1=first or 2=second)
    from_index: AtomicUsize,

    protect: bool,
    alloc: BumpAllocator,
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

        let limit = first.start.offset(committed_semi_size);

        SemiSpace {
            total: total.clone(),

            first: Block::new(first.clone(), committed_semi_size / 2),
            second: Block::new(second, committed_semi_size / 2),

            from_index: AtomicUsize::new(2),
            protect,
            alloc: BumpAllocator::new(first.start, limit),
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
        assert!(mem::is_page_aligned(new_size));

        let old_committed = self.committed.load(Ordering::SeqCst);
        let new_committed = self.start.offset(new_size).to_usize();
        assert!(new_committed <= self.end.to_usize());
        assert!(self.alloc.top().to_usize() <= new_committed);

        if old_committed == new_committed {
            return;
        }

        let updated = self.committed.swap(new_committed, Ordering::SeqCst);
        assert!(updated == old_committed);
        self.limit.store(new_committed, Ordering::SeqCst);

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
