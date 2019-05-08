use gc::{Address, K};

pub const SIZE_CLASSES: usize = 6;

pub const SIZE_CLASS_SMALLEST: SizeClass = SizeClass(0);
pub const SIZE_SMALLEST: usize = 16;

pub const SIZE_CLASS_TINY: SizeClass = SizeClass(1);
pub const SIZE_TINY: usize = 32;

pub const SIZE_CLASS_SMALL: SizeClass = SizeClass(2);
pub const SIZE_SMALL: usize = 128;

pub const SIZE_CLASS_MEDIUM: SizeClass = SizeClass(3);
pub const SIZE_MEDIUM: usize = 2 * K;

pub const SIZE_CLASS_LARGE: SizeClass = SizeClass(4);
pub const SIZE_LARGE: usize = 8 * K;

pub const SIZE_CLASS_HUGE: SizeClass = SizeClass(5);
pub const SIZE_HUGE: usize = 32 * K;

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct SizeClass(usize);

pub const SIZES: [usize; SIZE_CLASSES] = [
    SIZE_SMALLEST,
    SIZE_TINY,
    SIZE_SMALL,
    SIZE_MEDIUM,
    SIZE_LARGE,
    SIZE_HUGE,
];

impl SizeClass {
    fn next_up(size: usize) -> SizeClass {
        assert!(size >= SIZE_SMALLEST);

        if size <= SIZE_SMALLEST {
            SIZE_CLASS_SMALLEST
        } else if size <= SIZE_TINY {
            SIZE_CLASS_TINY
        } else if size <= SIZE_SMALL {
            SIZE_CLASS_SMALL
        } else if size <= SIZE_MEDIUM {
            SIZE_CLASS_MEDIUM
        } else if size <= SIZE_LARGE {
            SIZE_CLASS_LARGE
        } else {
            SIZE_CLASS_HUGE
        }
    }

    fn next_down(size: usize) -> SizeClass {
        assert!(size >= SIZE_SMALLEST);

        if size < SIZE_TINY {
            SIZE_CLASS_SMALLEST
        } else if size < SIZE_SMALL {
            SIZE_CLASS_TINY
        } else if size < SIZE_MEDIUM {
            SIZE_CLASS_SMALL
        } else if size < SIZE_LARGE {
            SIZE_CLASS_MEDIUM
        } else if size < SIZE_HUGE {
            SIZE_CLASS_LARGE
        } else {
            SIZE_CLASS_HUGE
        }
    }

    fn idx(self) -> usize {
        self.0
    }

    fn size(self) -> usize {
        SIZES[self.0]
    }
}

pub struct FreeList {
    classes: Vec<FreeListClass>,
}

impl FreeList {
    pub fn new() -> FreeList {
        let mut classes = Vec::with_capacity(SIZE_CLASSES);

        for _ in 0..SIZE_CLASSES {
            classes.push(FreeListClass::new());
        }

        FreeList { classes: classes }
    }

    pub fn add(&mut self, addr: Address, size: usize) {
        if size < SIZE_SMALLEST {
            return;
        }

        debug_assert!(size >= SIZE_SMALLEST);
        let szclass = SizeClass::next_down(size);
        self.classes[szclass.idx()].add(FreeSpace(addr));
    }

    pub fn alloc(&mut self, size: usize) -> FreeSpace {
        let szclass = SizeClass::next_up(size).idx();
        let last = SIZE_CLASS_HUGE.idx();

        for class in szclass..last {
            let result = self.classes[class].first();

            if result.is_non_null() {
                if result.size() < size {
                    let class = SizeClass(class);
                    println!(
                        "class={} class.size={} actual={} expected={}",
                        class.0,
                        class.size(),
                        result.size(),
                        size
                    );
                }
                assert!(result.size() >= size);
                return result;
            }
        }

        self.classes[SIZE_CLASS_HUGE.idx()].find(size)
    }
}

struct FreeListClass {
    spaces: Vec<FreeSpace>,
}

impl FreeListClass {
    fn new() -> FreeListClass {
        FreeListClass { spaces: Vec::new() }
    }

    fn add(&mut self, addr: FreeSpace) {
        self.spaces.push(addr);
    }

    fn first(&mut self) -> FreeSpace {
        if let Some(space) = self.spaces.pop() {
            space
        } else {
            FreeSpace::null()
        }
    }

    fn find(&mut self, minimum_size: usize) -> FreeSpace {
        let len = self.spaces.len();
        for idx in 0..len {
            let curr = self.spaces[idx];
            if curr.size() >= minimum_size {
                self.spaces.remove(idx);
                return curr;
            }
        }

        FreeSpace::null()
    }
}

#[derive(Copy, Clone)]
pub struct FreeSpace(Address);

impl FreeSpace {
    #[inline(always)]
    pub fn null() -> FreeSpace {
        FreeSpace(Address::null())
    }

    #[inline(always)]
    pub fn is_null(self) -> bool {
        self.addr().is_null()
    }

    #[inline(always)]
    pub fn is_non_null(self) -> bool {
        self.addr().is_non_null()
    }

    #[inline(always)]
    pub fn addr(self) -> Address {
        self.0
    }

    #[inline(always)]
    pub fn size(self) -> usize {
        let obj = self.addr().to_obj();
        obj.size() as usize
    }
}
