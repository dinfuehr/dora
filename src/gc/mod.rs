use std::cmp::{Ord, Ordering, PartialOrd};
use std::fmt;

use ctxt::SemContext;
use driver::cmd::{Args, CollectorName};
use gc::copy::CopyCollector;
use gc::swiper::Swiper;
use gc::space::{Space, SpaceConfig};
use gc::zero::ZeroCollector;

pub mod arena;
pub mod chunk;
pub mod copy;
pub mod root;
pub mod space;
pub mod swiper;
pub mod zero;

const INITIAL_SIZE: usize = 64 * 1024;
const LARGE_OBJECT_SIZE: usize = 64 * 1024;

const CHUNK_SIZE: usize = 8 * 1024;
const CODE_SPACE_LIMIT: usize = 128 * 1024;
const PERM_SPACE_LIMIT: usize = 64 * 1024;

pub struct Gc {
    collector: Box<Collector>,

    code_space: Space,
    perm_space: Space,
}

impl Gc {
    pub fn new(args: &Args) -> Gc {
        let code_config = SpaceConfig {
            executable: true,
            chunk: CHUNK_SIZE,
            limit: CODE_SPACE_LIMIT,
            align: 64,
        };

        let perm_config = SpaceConfig {
            executable: false,
            chunk: CHUNK_SIZE,
            limit: PERM_SPACE_LIMIT,
            align: 8,
        };

        let collector_name = args.flag_gc.unwrap_or(CollectorName::Swiper);

        let collector: Box<Collector> = match collector_name {
            CollectorName::Zero => box ZeroCollector::new(args),
            CollectorName::Copy => box CopyCollector::new(args),
            CollectorName::Swiper => box Swiper::new(args),
        };

        Gc {
            collector: collector,

            code_space: Space::new(code_config, "code"),
            perm_space: Space::new(perm_config, "perm"),
        }
    }

    pub fn needs_write_barrier(&self) -> bool {
        self.collector.needs_write_barrier()
    }

    pub fn card_table_offset(&self) -> usize {
        self.collector.card_table_offset()
    }

    pub fn alloc_code(&self, size: usize) -> *mut u8 {
        self.code_space.alloc(size)
    }

    pub fn alloc_perm(&self, size: usize) -> *mut u8 {
        self.perm_space.alloc(size)
    }

    pub fn alloc(&self, ctxt: &SemContext, size: usize, array_ref: bool) -> *const u8 {
        self.collector.alloc(ctxt, size, array_ref)
    }

    pub fn collect(&self, ctxt: &SemContext) {
        self.collector.collect(ctxt);
    }
}

trait Collector {
    // allocate object of given size
    fn alloc(&self, ctxt: &SemContext, size: usize, array_ref: bool) -> *const u8;

    fn collect(&self, ctxt: &SemContext);

    // decides whether to emit write barriers needed for
    // generational GC to write into card table
    fn needs_write_barrier(&self) -> bool {
        false
    }

    // only need if write barriers needed
    fn card_table_offset(&self) -> usize {
        0
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub struct Address(usize);

impl Address {
    #[inline(always)]
    pub fn from(val: usize) -> Address {
        Address(val)
    }

    #[inline(always)]
    pub fn offset_from(self, base: Address) -> usize {
        debug_assert!(self >= base);

        self.to_usize() - base.to_usize()
    }

    #[inline(always)]
    pub fn offset(self, offset: usize) -> Address {
        Address(self.0 + offset)
    }

    #[inline(always)]
    pub fn to_usize(self) -> usize {
        self.0
    }

    #[inline(always)]
    pub fn from_ptr<T>(ptr: *const T) -> Address {
        Address(ptr as usize)
    }

    #[inline(always)]
    pub fn to_ptr<T>(&self) -> *const T {
        self.0 as *const T
    }

    #[inline(always)]
    pub fn to_mut_ptr<T>(&self) -> *mut T {
        self.0 as *const T as *mut T
    }

    #[inline(always)]
    pub fn null() -> Address {
        Address(0)
    }

    #[inline(always)]
    pub fn is_null(self) -> bool {
        self.0 == 0
    }
}

impl fmt::Display for Address {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "0x{:x}", self.to_usize())
    }
}

impl PartialOrd for Address {
    fn partial_cmp(&self, other: &Address) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Address {
    fn cmp(&self, other: &Address) -> Ordering {
        self.to_usize().cmp(&other.to_usize())
    }
}

impl From<usize> for Address {
    fn from(val: usize) -> Address {
        Address(val)
    }
}
