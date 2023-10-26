use crate::sema::AliasParent;
use crate::Sema;

pub fn check(sa: &Sema) {
    for (_id, alias) in sa.aliases.iter() {
        match alias.parent {
            AliasParent::None | AliasParent::Impl(..) => unimplemented!(),
            AliasParent::Trait(..) => unimplemented!(),
        }
    }
}
