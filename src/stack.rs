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
        for (ind, elem) in self.elems.iter().rev().enumerate() {
            let name = ctxt.fct_by_id(elem.fct_id, |fct| fct.full_name(ctxt));
            print!("  {}: {}:", ind, name);

            if elem.lineno == 0 {
                println!("?");
            } else {
                println!("{}", elem.lineno);
            }
        }
    }
}

struct StackElem {
    fct_id: FctId,
    lineno: u32
}
