use ctxt::SemContext;
use gc::root::Slot;
use gc::GcReason;

pub struct ParMinorCollector<'a, 'ast: 'a> {
    ctxt: &'a SemContext<'ast>,
    rootset: &'a [Slot],
    reason: GcReason,
}

impl<'a, 'ast> ParMinorCollector<'a, 'ast> {
    pub fn new(
        ctxt: &'a SemContext<'ast>,
        rootset: &'a [Slot],
        reason: GcReason,
    ) -> ParMinorCollector<'a, 'ast> {
        ParMinorCollector {
            ctxt: ctxt,
            rootset: rootset,
            reason: reason,
        }
    }

    pub fn collect(&mut self) {
        unimplemented!();
    }

    pub fn promotion_failed(&mut self) -> bool {
        false
    }
}