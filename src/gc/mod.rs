use ctxt::Context;
use driver::cmd::Args;
use gc::copy::{minor_collect, SemiSpace};
use gc::malloc::MallocSpace;
use gc::old::OldSpace;
use gc::root::get_rootset;
use gc::space::{Space, SpaceConfig};
use gc::zero::ZeroCollector;
use os;

pub mod arena;
pub mod chunk;
pub mod copy;
pub mod malloc;
pub mod old;
pub mod root;
pub mod space;
pub mod zero;

const INITIAL_SIZE: usize = 64 * 1024;
const LARGE_OBJECT_SIZE: usize = 64 * 1024;

const CHUNK_SIZE: usize = 8 * 1024;
const CODE_SPACE_LIMIT: usize = 128 * 1024;
const PERM_SPACE_LIMIT: usize = 16 * 1024;

pub struct Gc {
    malloc_space: MallocSpace,

    from_space: SemiSpace,
    to_space: SemiSpace,
    old_space: OldSpace,

    code_space: Space,
    perm_space: Space,

    zero_gc: Option<ZeroCollector>,

    pub stats: GcStats,
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

        let size = args.flag_heap_size.map(|x| *x).unwrap_or(INITIAL_SIZE) / 2;

        let zero_gc = if args.flag_gc_zero {
            Some(ZeroCollector::new(args))
        } else {
            None
        };

        Gc {
            stats: GcStats {
                collect_duration: 0,
                total_allocated: 0,
                collections: 0,
                allocations: 0,
            },

            malloc_space: MallocSpace::new(),

            from_space: SemiSpace::new(size),
            to_space: SemiSpace::new(size),
            old_space: OldSpace::new(),

            code_space: Space::new(code_config, "code"),
            perm_space: Space::new(perm_config, "perm"),

            zero_gc: zero_gc,
        }
    }

    pub fn alloc_code(&mut self, size: usize) -> *mut u8 {
        self.code_space.alloc(size)
    }

    pub fn alloc_perm(&mut self, size: usize) -> *mut u8 {
        self.perm_space.alloc(size)
    }

    pub fn alloc(&mut self, ctxt: &Context, size: usize) -> *const u8 {
        if ctxt.args.flag_gc_copy {
            self.alloc_copy(ctxt, size)
        } else if ctxt.args.flag_gc_zero {
            self.zero_gc.as_ref().unwrap().alloc(ctxt, size)
        } else {
            self.malloc_space.alloc(ctxt, &mut self.stats, size)
        }
    }

    fn alloc_copy(&mut self, ctxt: &Context, size: usize) -> *mut u8 {
        if ctxt.args.flag_gc_stress {
            let rootset = get_rootset(ctxt);
            minor_collect(ctxt, &mut self.from_space, &mut self.to_space, rootset);
        }

        let mut ptr = self.to_space.allocate(size);

        if ptr.is_null() {
            let rootset = get_rootset(ctxt);
            minor_collect(ctxt, &mut self.from_space, &mut self.to_space, rootset);

            ptr = self.to_space.allocate(size);
        }

        ptr as *mut u8
    }

    pub fn collect(&mut self, ctxt: &Context) {
        let rootset = get_rootset(ctxt);

        if ctxt.args.flag_gc_copy {
            minor_collect(ctxt, &mut self.from_space, &mut self.to_space, rootset);
        } else if ctxt.args.flag_gc_zero {
            self.zero_gc.as_ref().unwrap().collect(ctxt);
        } else {
            self.malloc_space.collect(ctxt, &mut self.stats, rootset);
        }
    }
}

pub struct GcStats {
    pub collect_duration: u64,
    pub total_allocated: u64,
    pub collections: u64,
    pub allocations: u64,
}
