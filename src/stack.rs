use ctxt::{Context, FctId};

pub struct Stacktrace {
    elems: Vec<StackElem>
}

impl Stacktrace {
    pub fn new() -> Stacktrace {
        Stacktrace {
            elems: Vec::new()
        }
    }

    pub fn push_entry(&mut self, fct_id: FctId, lineno: u32) {
        self.elems.push(StackElem {
            fct_id: fct_id,
            lineno: lineno
        });
    }

    pub fn dump(&self, ctxt: &Context) {
        for elem in self.elems.iter().rev() {
            if elem.lineno == 0 {
                println!("fct {}:?", elem.fct_id.0);
            } else {
                println!("fct {}:{}", elem.fct_id.0, elem.lineno);
            }
        }
    }
}

struct StackElem {
    fct_id: FctId,
    lineno: u32
}
