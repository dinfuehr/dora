use driver::ctxt::Context;

mod prelude;
mod typeck;
mod nameck;

pub fn check(ctxt: &Context) {
    prelude::init(ctxt);

    nameck::check(ctxt, ctxt.ast);
    typeck::check(ctxt, ctxt.ast);
}
