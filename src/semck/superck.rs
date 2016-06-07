use std::collections::{HashMap, HashSet};

use class::{Class, ClassId};
use ctxt::Context;
use error::msg::Msg;
use lexer::position::Position;
use object::Header;

pub fn check<'ast>(ctxt: &mut Context<'ast>) {
    cycle_detection(ctxt);
    if ctxt.diag.borrow().has_errors() { return; }

    determine_sizes(ctxt);
}

fn cycle_detection<'ast>(ctxt: &mut Context<'ast>) {
    for cls in &ctxt.classes {
        let mut map: HashSet<ClassId> = HashSet::new();
        map.insert(cls.id);

        let mut cur: &Class<'ast> = cls;

        loop {
            if let Some(parent) = cur.parent_class {
                if !map.insert(parent) {
                    let pos = cls.ast.map(|ast| ast.pos).unwrap_or(Position::new(1, 1));
                    ctxt.diag.borrow_mut().report(pos, Msg::CycleInHierarchy);

                    break;
                }

                cur = ctxt.cls_by_id(parent);

            } else {
                break;
            }
        }

    }
}

fn determine_sizes<'ast>(ctxt: &mut Context<'ast>) {
    let mut super_sizes: HashMap<ClassId, i32> = HashMap::new();

    for cls in &ctxt.classes {
        determine_recursive_size(ctxt, &mut super_sizes, cls.id);
    }

    for cls in &mut ctxt.classes {
        let super_size = *super_sizes.get(&cls.id).unwrap();
        cls.size += super_size;

        for field in &mut cls.fields {
            field.offset += super_size;
        }
    }
}

fn determine_recursive_size<'ast>(ctxt: &Context<'ast>,
                                  super_sizes: &mut HashMap<ClassId, i32>,
                                  id: ClassId) -> i32 {
    if let Some(&val) = super_sizes.get(&id) {
        return val;
    }

    let cls = ctxt.cls_by_id(id);

    let super_size = if let Some(parent_class) = cls.parent_class {
        let super_class = ctxt.cls_by_id(parent_class);
        super_class.size + determine_recursive_size(ctxt, super_sizes, parent_class)

    } else {
        Header::size()
    };

    super_sizes.insert(id, super_size);

    super_size
}

#[test]
fn test_super_size() {
    use semck::tests::ok_with_test;

    ok_with_test("open class A { var a: int; }
           open class B: A { var b1: int; var b2: int; }
           class C: B { var c: Str; }", |ctxt| {
        check_class(ctxt, "A", 4, None);
        check_field(ctxt, "A", "a", Header::size());
        check_class(ctxt, "B", 4*3, Some("A"));
        check_field(ctxt, "B", "b1", Header::size() + 4);
        check_class(ctxt, "C", 4*3+8, Some("B"));
        check_field(ctxt, "C", "c", Header::size() + 4 * 3);
    });
}

#[test]
fn test_cycle() {
    use semck::tests::{errors, pos};

    errors("open class A: B open class B: A", &[
        (pos(1, 6), Msg::CycleInHierarchy),
        (pos(1, 22), Msg::CycleInHierarchy)
    ]);
}

#[cfg(test)]
fn check_class<'ast>(ctxt: &Context<'ast>,
                     name: &'static str,
                     size: i32,
                     parent: Option<&'static str>) {
    let name = ctxt.interner.intern(name);
    let cls_id = ctxt.sym.borrow().get_class(name).unwrap();

    let parent_id = parent.map(|name| ctxt.interner.intern(name))
                          .map(|name| ctxt.sym.borrow().get_class(name).unwrap());

    let cls = ctxt.cls_by_id(cls_id);
    assert_eq!(parent_id, cls.parent_class);
    assert_eq!(Header::size() + size, cls.size);
}

#[cfg(test)]
fn check_field<'ast>(ctxt: &Context<'ast>,
                     cls_name: &'static str,
                     field_name: &'static str,
                     offset: i32) {
    let cls_name = ctxt.interner.intern(cls_name);
    let field_name = ctxt.interner.intern(field_name);
    let cls_id = ctxt.sym.borrow().get_class(cls_name).unwrap();
    let cls = ctxt.cls_by_id(cls_id);

    for field in &cls.fields {
        if field_name == field.name {
            assert_eq!(offset, field.offset);
            return;
        }
    }

    unreachable!();
}
