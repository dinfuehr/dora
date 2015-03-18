use ast::visit::Visitor;

struct ReturnsValue;

impl Visitor for ReturnsValue {
    type Returns = bool;
}
