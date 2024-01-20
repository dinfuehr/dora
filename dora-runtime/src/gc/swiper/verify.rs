use parking_lot::MutexGuard;
use std::collections::HashSet;
use std::fmt;

use crate::gc::root::Slot;
use crate::gc::swiper::large::LargeSpace;
use crate::gc::swiper::old::{OldGen, OldGenProtected};
use crate::gc::swiper::young::YoungGen;
use crate::gc::swiper::BasePage;
use crate::gc::swiper::{LargePage, ReadOnlySpace, RegularPage, Swiper};
use crate::gc::{Address, Region};

use crate::vm::VM;

#[derive(Copy, Clone)]
pub enum VerifierPhase {
    PreMinor,
    PostMinor,
    PreFull,
    PostFull,
}

impl VerifierPhase {
    fn is_pre(self) -> bool {
        match self {
            VerifierPhase::PreMinor => true,
            VerifierPhase::PostMinor => false,
            VerifierPhase::PreFull => true,
            VerifierPhase::PostFull => false,
        }
    }

    fn is_post_minor(self) -> bool {
        match self {
            VerifierPhase::PostMinor => true,
            _ => false,
        }
    }

    fn is_post_full(self) -> bool {
        match self {
            VerifierPhase::PostFull => true,
            _ => false,
        }
    }

    #[allow(dead_code)]
    fn is_pre_full(self) -> bool {
        match self {
            VerifierPhase::PreFull => true,
            _ => false,
        }
    }
}

impl fmt::Display for VerifierPhase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let text = match self {
            VerifierPhase::PreMinor => "pre minor",
            VerifierPhase::PostMinor => "post minor",
            VerifierPhase::PreFull => "pre full",
            VerifierPhase::PostFull => "post full",
        };

        write!(f, "{}", text)
    }
}

pub struct Verifier<'a> {
    vm: &'a VM,
    swiper: &'a Swiper,
    young: &'a YoungGen,
    old: &'a OldGen,
    old_protected: MutexGuard<'a, OldGenProtected>,
    rootset: &'a [Slot],
    large: &'a LargeSpace,
    readonly_space: &'a ReadOnlySpace,
    minimum_remset: Vec<Address>,
    meta_space_start: Address,

    heap: Region,

    phase: VerifierPhase,
}

impl<'a> Verifier<'a> {
    pub fn new(
        vm: &'a VM,
        swiper: &'a Swiper,
        heap: Region,
        young: &'a YoungGen,
        old: &'a OldGen,
        rootset: &'a [Slot],
        large: &'a LargeSpace,
        readonly_space: &'a ReadOnlySpace,
        phase: VerifierPhase,
    ) -> Verifier<'a> {
        let old_protected = old.protected();

        Verifier {
            vm,
            swiper,
            young,
            old,
            old_protected,
            rootset,
            readonly_space,
            large,
            minimum_remset: Vec::new(),
            meta_space_start: vm.meta_space_start(),

            heap,

            phase,
        }
    }

    pub fn verify(&mut self) {
        self.verify_roots();
        self.verify_heap();
        self.verify_remembered_set();
    }

    fn verify_heap(&mut self) {
        let mut survivor_seen = true;

        for page in self.young.to_pages() {
            assert!(page.is_young());
            assert!(!page.is_readonly());
            assert!(!page.is_large());

            if survivor_seen {
                if !page.is_survivor() {
                    survivor_seen = false;
                }
            } else {
                assert!(!page.is_survivor());
            }

            self.verify_page(page);
        }

        for page in self.old_protected.pages() {
            assert!(!page.is_young());
            assert!(!page.is_readonly());
            assert!(!page.is_survivor());
            assert!(!page.is_large());

            self.verify_page(page);
        }

        for page in self.readonly_space.pages() {
            assert!(!page.is_young());
            assert!(page.is_readonly());
            assert!(!page.is_survivor());
            assert!(!page.is_large());

            self.verify_page(page);
        }

        self.large.iterate_pages(|page| {
            assert!(!page.is_young());
            assert!(page.is_large());

            self.verify_large_page(page);
        });
    }

    fn verify_roots(&self) {
        for root in self.rootset {
            self.verify_slot(*root, Address::null());
        }
    }

    fn verify_remembered_set(&self) {
        let remset = self.swiper.remset.read();

        if self.phase.is_post_full() {
            assert!(remset.is_empty());
        }

        let mut remset_as_set = HashSet::new();

        for &object in remset.iter() {
            assert!(!BasePage::from_address(object).is_young());
            remset_as_set.insert(object);
        }

        for object in &self.minimum_remset {
            assert!(remset_as_set.contains(object));
        }
    }

    fn verify_page(&mut self, page: RegularPage) {
        let region = page.object_area();
        let mut curr = region.start;
        assert!(region.end.is_page_aligned());
        assert!(!page.is_large());

        while curr < region.end {
            let object = curr.to_obj();
            let size = object.size(self.meta_space_start);
            let object_end = curr.offset(size);

            // Object is not supposed to cross page boundary.
            let page_for_object = RegularPage::from_address(curr);
            assert_eq!(page, page_for_object);
            assert!(object_end <= page.end());

            if !object.is_filler(self.vm) {
                self.verify_object(page.as_base_page(), curr);
            }

            curr = object_end;
        }

        assert!(curr == region.end, "object doesn't end at region end");
    }

    fn verify_large_page(&mut self, page: LargePage) {
        self.verify_object(page.as_base_page(), page.object_address());
    }

    fn verify_object(&mut self, page: BasePage, object_address: Address) {
        let object = object_address.to_obj();
        assert_eq!(object.header().is_marked(), page.is_readonly());

        if self.phase.is_post_full() {
            assert!(!object.header().is_remembered());
        } else if page.is_young() {
            assert!(object.header().is_remembered());
        }

        let mut object_has_young_ref = false;

        object.visit_reference_fields(self.vm.meta_space_start(), |child| {
            if self.verify_slot(child, object_address) {
                object_has_young_ref = true;
            }
        });

        if object_has_young_ref && !page.is_young() {
            assert!(object.header().is_remembered());
            self.minimum_remset.push(object_address);
        }
    }

    fn verify_slot(&self, slot: Slot, _container_obj: Address) -> bool {
        let referenced_object = slot.get();

        if referenced_object.is_null() {
            return false;
        }

        let page = BasePage::from_address(referenced_object);
        let is_young = page.is_young();

        let object = referenced_object.to_obj();

        // Verify that the address is the start of an object,
        // for this access its size.
        // To make sure this isn't optimized out by the compiler,
        // make sure that the size doesn't equal 1.
        assert!(
            object.size(self.meta_space_start) != 1,
            "object size shouldn't be 1"
        );

        is_young
    }
}
