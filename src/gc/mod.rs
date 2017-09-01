use std::sync::Mutex;

use ctxt::SemContext;
use driver::cmd::{Args, CollectorName};
use gc::copy::CopyCollector;
use gc::swiper::Swiper;
use gc::space::{Space, SpaceConfig};
use gc::zero::ZeroCollector;
use os;

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

    code_space: Mutex<Space>,
    perm_space: Mutex<Space>,
}

impl Gc {
    pub fn new(args: &Args) -> Gc {
        let code_config = SpaceConfig {
            prot: os::Executable,
            chunk_size: CHUNK_SIZE,
            limit: CODE_SPACE_LIMIT,
            align: 64,
        };

        let perm_config = SpaceConfig {
            prot: os::Writable,
            chunk_size: CHUNK_SIZE,
            limit: PERM_SPACE_LIMIT,
            align: 8,
        };

        let collector_name = args.flag_gc.unwrap_or(CollectorName::Copy);

        let collector: Box<Collector> = match collector_name {
            CollectorName::Zero => box ZeroCollector::new(args),
            CollectorName::Copy => box CopyCollector::new(args),
            CollectorName::Swiper => box Swiper::new(args),
        };

        Gc {
            collector: collector,

            code_space: Mutex::new(Space::new(code_config, "code")),
            perm_space: Mutex::new(Space::new(perm_config, "perm")),
        }
    }

    pub fn alloc_code(&self, size: usize) -> *mut u8 {
        self.code_space.lock().unwrap().alloc(size)
    }

    pub fn alloc_perm(&self, size: usize) -> *mut u8 {
        self.perm_space.lock().unwrap().alloc(size)
    }

    pub fn alloc(&self, ctxt: &SemContext, size: usize) -> *const u8 {
        self.collector.alloc(ctxt, size)
    }

    pub fn collect(&self, ctxt: &SemContext) {
        self.collector.collect(ctxt);
    }
}

trait Collector {
    fn alloc(&self, ctxt: &SemContext, size: usize) -> *const u8;
    fn collect(&self, ctxt: &SemContext);
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Address(usize);

impl Address {
    #[inline(always)]
    fn offset(self, offset: usize) -> Address {
        Address(self.0 + offset)
    }

    #[inline(always)]
    fn to_usize(self) -> usize {
        self.0
    }

    #[inline(always)]
    pub fn from_ptr<T>(ptr: *const T) -> Address {
        Address(ptr as usize)
    }

    #[inline(always)]
    pub fn null() -> Address {
        Address(0)
    }
}
