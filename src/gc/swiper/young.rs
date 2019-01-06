use std::cmp::max;
use std::sync::atomic::{AtomicUsize, Ordering};

use gc::bump::BumpAllocator;
use gc::{align_space, arena};
use gc::{Address, Region};
use os::{self, ProtType};

const SEMI_RATIO: usize = 3;

pub struct YoungGen {
    // bounds of eden & semi-spaces
    total: Region,

    // eden space
    eden: Eden,

    // from/to-space
    semi: SemiSpace,
}

impl YoungGen {
    pub fn new(total: Region, committed_size: usize, protect: bool) -> YoungGen {
        let semi_total_size = align_space(total.size() / SEMI_RATIO);
        let eden_total_size = total.size() - semi_total_size;

        let eden_total = total.start.region_start(eden_total_size);
        let semi_total = Region::new(eden_total.end, total.end);
        assert!(semi_total.size() == semi_total_size);

        let semi_committed = align_space(committed_size / SEMI_RATIO);
        let eden_committed = committed_size - semi_committed;

        let young = YoungGen {
            total: total,
            eden: Eden::new(eden_total, eden_committed),
            semi: SemiSpace::new(semi_total, semi_committed, protect),
        };

        young.commit();
        young.protect_to();

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
    }

    pub fn active_size(&self) -> usize {
        self.eden.active().size() + self.semi.from_block().active().size()
    }

    pub fn unprotect_to(&self) {
        self.semi.unprotect_to();
    }

    pub fn protect_to(&self) {
        self.semi.protect_to();
    }

    pub fn clear_eden(&self) {
        self.eden.reset_top();
    }

    pub fn swap_semi(&self, top: Address) {
        self.semi.swap(top);
    }

    pub fn swap_semi_and_keep_to_space(&self, top: Address) {
        self.semi.swap_and_keep_to_space(top);
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

    pub fn set_committed_size(&self, size: usize) {
        assert!(size <= self.total.size());
        let semi_target_size = align_space(size / SEMI_RATIO);

        // semi-space can't be smaller than current size of from-space though
        let from_active = self.semi.from_active().size();
        let semi_minimum_size = align_space(from_active * 2);

        let semi_committed = max(semi_minimum_size, semi_target_size);
        let eden_committed = size - semi_committed;

        self.semi.set_committed_size(semi_committed);
        self.eden.set_committed_size(eden_committed);
    }

    pub fn committed_size(&self) -> usize {
        self.semi.committed_size() + self.eden.committed_size()
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

    fn set_committed_size(&self, size: usize) {
        self.block.set_committed_size(size);
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
    fn new(total: Region, committed_size: usize, protect: bool) -> SemiSpace {
        let committed_semi_size = committed_size / 2;

        let semi_size = total.size() / 2;
        assert!(committed_semi_size <= semi_size);

        let first = total.start.region_start(semi_size);
        let second = Region::new(first.end, total.end);
        assert!(first.size() == second.size());

        let limit = total.start.offset(committed_semi_size);

        SemiSpace {
            total: total.clone(),

            first: Block::new(first.clone(), committed_semi_size),
            second: Block::new(second, committed_semi_size),

            from_index: AtomicUsize::new(1),
            protect: protect,
            alloc: BumpAllocator::new(total.start, limit),
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

    // Make to-space writable.
    fn unprotect_to(&self) {
        // make memory writable again, so that we
        // can copy objects to the to-space.
        // Since this has some overhead, do it only in debug builds.

        if cfg!(debug_assertions) || self.protect {
            let to_space = self.to_committed();

            os::mprotect(
                to_space.start.to_ptr::<u8>(),
                to_space.size(),
                ProtType::Writable,
            );
        }
    }

    // Make to-space inaccessible.
    fn protect_to(&self) {
        // Make from-space unaccessible both from read/write.
        // Since this has some overhead, do it only in debug builds.
        if cfg!(debug_assertions) || self.protect {
            let to_space = self.to_committed();
            os::mprotect(
                to_space.start.to_ptr::<u8>(),
                to_space.size(),
                ProtType::None,
            );
        }
    }

    // Free all objects in from-semi-space.
    fn clear_from(&self) {
        let from_space = self.from_block();

        self.age_marker
            .store(from_space.start.to_usize(), Ordering::Relaxed);
        from_space.reset_top();
    }

    // Free all objects in to-semi-space.
    fn clear_to(&self) {
        self.to_block().reset_top();
    }

    // Switch from- & to-semi-space. Mark to-space as empty.
    fn swap(&self, top: Address) {
        // current from-space is now empty
        self.from_block().reset_top();

        self.swap_and_keep_to_space(top);
    }

    // Switch from- & to-semi-space. Do not mark to-space as empty.
    fn swap_and_keep_to_space(&self, top: Address) {
        // swap from_index
        self.swap_from_index();

        // new from-space is already used up to `top`.
        self.from_block().set_top(top);
        self.age_marker.store(top.to_usize(), Ordering::Relaxed);
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
        debug_assert!(self.from_active().contains(addr));
        return addr.to_usize() < self.age_marker.load(Ordering::Relaxed);
    }

    fn bump_alloc(&self, size: usize) -> Address {
        self.from_block().bump_alloc(size)
    }

    fn set_committed_size(&self, size: usize) {
        self.from_block().set_committed_size(size / 2);
        self.to_block().set_committed_size(size / 2);
    }

    fn committed_size(&self) -> usize {
        self.from_block().committed_size() + self.to_block().committed_size()
    }
}

struct Block {
    start: Address,
    end: Address,
    committed: AtomicUsize,
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
            alloc: BumpAllocator::new(region.start, committed),
        }
    }

    fn commit(&self) {
        let size = self.committed_size();

        if size > 0 {
            arena::commit(self.start, size, false);
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

    fn set_committed_size(&self, new_size: usize) {
        let old_committed = self.committed.load(Ordering::Relaxed);
        let new_committed = self.start.offset(new_size).to_usize();
        assert!(new_committed <= self.end.to_usize());
        assert!(self.alloc.top().to_usize() <= new_committed);

        if old_committed == new_committed {
            return;
        }

        let updated =
            self.committed
                .compare_and_swap(old_committed, new_committed, Ordering::SeqCst);
        assert!(updated == old_committed);

        if old_committed < new_committed {
            let size = new_committed - old_committed;
            arena::commit(old_committed.into(), size, false);
        } else if old_committed > new_committed {
            let size = old_committed - new_committed;
            arena::forget(new_committed.into(), size);
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
