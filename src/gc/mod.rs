use std::sync::Mutex;

use ctxt::Context;
use driver::cmd::{Args, CollectorName};
use gc::copy::CopyCollector;
use gc::malloc::MallocCollector;
use gc::space::{Space, SpaceConfig};
use gc::zero::ZeroCollector;
use os;

pub mod arena;
pub mod chunk;
pub mod copy;
pub mod malloc;
pub mod root;
pub mod space;
pub mod zero;

const INITIAL_SIZE: usize = 64 * 1024;
const LARGE_OBJECT_SIZE: usize = 64 * 1024;

const CHUNK_SIZE: usize = 8 * 1024;
const CODE_SPACE_LIMIT: usize = 128 * 1024;
const PERM_SPACE_LIMIT: usize = 16 * 1024;

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

        let collector_name = args.flag_gc.unwrap_or(CollectorName::Malloc);

        let collector: Box<Collector> = match collector_name {
            CollectorName::Zero => box ZeroCollector::new(args),
            CollectorName::Copy => box CopyCollector::new(args),
            CollectorName::Malloc => box MallocCollector::new(),
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

    pub fn alloc(&self, ctxt: &Context, size: usize) -> *const u8 {
        self.collector.alloc(ctxt, size)
    }

    pub fn collect(&self, ctxt: &Context) {
        self.collector.collect(ctxt);
    }
}

trait Collector {
    fn alloc(&self, ctxt: &Context, size: usize) -> *const u8;
    fn collect(&self, ctxt: &Context);
}
