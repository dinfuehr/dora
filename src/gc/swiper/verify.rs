use gc::swiper::old::OldGen;
use gc::swiper::young::YoungGen;

pub struct Verifier<'a> {
    young: &'a YoungGen,
    old: &'a OldGen,
}

impl<'a> Verifier<'a> {
    pub fn new(young: &'a YoungGen, old: &'a OldGen) -> Verifier<'a> {
        Verifier {
            young: young,
            old: old,
        }
    }

    pub fn verify(&self) {
        // TODO
    }
}