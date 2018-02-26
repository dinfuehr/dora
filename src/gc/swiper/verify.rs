use gc::Address;
use gc::swiper::old::OldGen;
use gc::swiper::Region;
use gc::swiper::young::YoungGen;

use object::Obj;

pub struct Verifier<'a> {
    young: &'a YoungGen,
    old: &'a OldGen,

    old_region: Region,
    young_region: Region,
}

impl<'a> Verifier<'a> {
    pub fn new(young: &'a YoungGen, old: &'a OldGen) -> Verifier<'a> {
        Verifier {
            young: young,
            old: old,

            young_region: young.used_region(),
            old_region: old.used_region(),
        }
    }

    pub fn verify(&mut self) {
        self.verify_young();
        self.verify_old();
    }

    fn verify_young(&mut self) {
        let region = self.young_region.clone();
        self.verify_objects(region);
    }

    fn verify_old(&mut self) {
        let region = self.old_region.clone();
        self.verify_objects(region);
    }

    fn verify_objects(&mut self, region: Region) {
        let mut ptr = region.start;

        while ptr < region.end {
            let object = unsafe { &mut *ptr.to_mut_ptr::<Obj>() };

            object.visit_reference_fields(|child| {
                let child_ptr = child.get();
                self.verify_reference(child_ptr);
            });

            ptr = ptr.offset(object.size());
        }

        assert!(ptr == region.end, "object doesn't end at region end");
    }

    fn verify_reference(&mut self, obj: *mut Obj) {
        let addr = Address::from_ptr(obj);

        if obj.is_null() || self.old_region.contains(addr) || self.young_region.contains(addr) {
            return;
        }

        println!(
            "YNG: {:x}-{:x} active {:x}-{:x}",
            self.young.total.start.to_usize(),
            self.young.total.start.to_usize(),
            self.young_region.start.to_usize(),
            self.young_region.end.to_usize()
        );
        println!(
            "OLD: {:x}-{:x} active {:x}-{:x}",
            self.old.total.start.to_usize(),
            self.old.total.end.to_usize(),
            self.old_region.start.to_usize(),
            self.old_region.end.to_usize()
        );
        println!("found reference: {:x}", addr.to_usize());

        panic!("reference neither pointing into young or old generation.");
    }
}
