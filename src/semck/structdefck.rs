use ctxt::{NodeMap, SemContext, StructFieldData, StructId};
use dora_parser::ast;
use dora_parser::ast::visit::{self, Visitor};
use dora_parser::error::msg::Msg;
use dora_parser::lexer::position::Position;
use semck;
use ty::BuiltinType;

pub fn check<'ast>(ctxt: &mut SemContext<'ast>, map_struct_defs: &NodeMap<StructId>) {
    let mut clsck = StructCheck {
        ctxt: ctxt,
        ast: ctxt.ast,
        struct_id: None,
        map_struct_defs: map_struct_defs,
    };

    clsck.check();
}

struct StructCheck<'x, 'ast: 'x> {
    ctxt: &'x mut SemContext<'ast>,
    ast: &'ast ast::Ast,
    map_struct_defs: &'x NodeMap<StructId>,

    struct_id: Option<StructId>,
}

impl<'x, 'ast> StructCheck<'x, 'ast> {
    fn check(&mut self) {
        self.visit_ast(self.ast);
    }
}

impl<'x, 'ast> Visitor<'ast> for StructCheck<'x, 'ast> {
    fn visit_struct(&mut self, s: &'ast ast::Struct) {
        self.struct_id = Some(*self.map_struct_defs.get(s.id).unwrap());

        visit::walk_struct(self, s);

        self.struct_id = None;
    }

    fn visit_struct_field(&mut self, f: &'ast ast::StructField) {
        let ty = semck::read_type(self.ctxt, &f.data_type).unwrap_or(BuiltinType::Unit);
        let id = self.struct_id.unwrap();

        let mut struc = self.ctxt.structs[id].borrow_mut();

        for field in &struc.fields {
            if field.name == f.name {
                let name = self.ctxt.interner.str(f.name).to_string();
                report(self.ctxt, f.pos, Msg::ShadowField(name));
                return;
            }
        }

        let field = StructFieldData {
            id: (struc.fields.len() as u32).into(),
            pos: f.pos,
            name: f.name,
            ty: ty,
            offset: 0,
        };

        struc.fields.push(field);
    }
}

fn report(ctxt: &SemContext, pos: Position, msg: Msg) {
    ctxt.diag.borrow_mut().report(pos, msg);
}

#[cfg(test)]
mod tests {
    use dora_parser::error::msg::Msg;
    use semck::tests::*;

    #[test]
    fn struct_field() {
        ok("struct Foo { a: int }");
        ok("struct Foo { a: int, b: int }");
        ok("struct Foo { a: int } struct Bar { a: int }");
        ok("struct Foo { a: int, bar: Bar } struct Bar { a: int }");
        err(
            "struct Bar { a: Unknown }",
            pos(1, 17),
            Msg::UnknownType("Unknown".into()),
        );
        err(
            "struct Foo { a: int, a: int }",
            pos(1, 22),
            Msg::ShadowField("a".into()),
        );
    }
}
