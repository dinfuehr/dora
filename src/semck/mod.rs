use driver::ctxt::Context;

mod typeck;
mod nameck;

pub fn check(ctxt: &Context) {
    typeck::check(ctxt, ctxt.ast);
    nameck::check(ctxt, ctxt.ast);
}
