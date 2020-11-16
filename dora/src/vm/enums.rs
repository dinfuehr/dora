use parking_lot::RwLock;

use std::collections::hash_map::HashMap;
use std::convert::TryInto;
use std::ops::Index;
use std::sync::Arc;

use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

use crate::mem;
use crate::object::Header;
use crate::semck::specialize::replace_type_param;
use crate::size::InstanceSize;
use crate::ty::{SourceType, TypeList, TypeListId};
use crate::utils::GrowableVec;
use crate::vm::{
    accessible_from, namespace_path, ClassDef, ClassDefId, ExtensionId, FctId, FieldDef, FileId,
    NamespaceId, TypeParam, VM,
};
use crate::vtable::VTableBox;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct EnumId(u32);

impl From<usize> for EnumId {
    fn from(data: usize) -> EnumId {
        EnumId(data.try_into().unwrap())
    }
}

impl Index<EnumId> for Vec<RwLock<EnumData>> {
    type Output = RwLock<EnumData>;

    fn index(&self, index: EnumId) -> &RwLock<EnumData> {
        &self[index.0 as usize]
    }
}

#[derive(Debug)]
pub struct EnumData {
    pub id: EnumId,
    pub file_id: FileId,
    pub namespace_id: NamespaceId,
    pub ast: Arc<ast::Enum>,
    pub pos: Position,
    pub name: Name,
    pub is_pub: bool,
    pub type_params: Vec<TypeParam>,
    pub variants: Vec<EnumVariant>,
    pub name_to_value: HashMap<Name, u32>,
    pub extensions: Vec<ExtensionId>,
    pub specializations: RwLock<HashMap<TypeList, EnumDefId>>,
    pub simple_enumeration: bool,
}

impl EnumData {
    pub fn type_param(&self, id: TypeListId) -> &TypeParam {
        &self.type_params[id.to_usize()]
    }

    pub fn name(&self, vm: &VM) -> String {
        namespace_path(vm, self.namespace_id, self.name)
    }

    pub fn name_with_params(&self, vm: &VM, type_list: &TypeList) -> String {
        let name = vm.interner.str(self.name);

        if type_list.len() > 0 {
            let type_list = type_list
                .iter()
                .map(|p| p.name_enum(vm, self))
                .collect::<Vec<_>>()
                .join(", ");

            format!("{}[{}]", name, type_list)
        } else {
            name.to_string()
        }
    }
}

#[derive(Debug)]
pub struct EnumVariant {
    pub id: usize,
    pub name: Name,
    pub types: Vec<SourceType>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct EnumDefId(u32);

impl From<usize> for EnumDefId {
    fn from(data: usize) -> EnumDefId {
        EnumDefId(data as u32)
    }
}

impl GrowableVec<RwLock<EnumDef>> {
    pub fn idx(&self, index: EnumDefId) -> Arc<RwLock<EnumDef>> {
        self.idx_usize(index.0 as usize)
    }
}

#[derive(Debug)]
pub struct EnumDef {
    pub id: EnumDefId,
    pub enum_id: EnumId,
    pub type_params: TypeList,
    pub layout: EnumLayout,
    pub variants: Vec<Option<ClassDefId>>,
}

impl EnumDef {
    pub fn ensure_class_for_variant(
        &mut self,
        vm: &VM,
        xenum: &EnumData,
        variant_id: usize,
    ) -> ClassDefId {
        if let Some(cls_def_id) = self.variants[variant_id] {
            return cls_def_id;
        }

        let variant = &xenum.variants[variant_id];
        let mut csize = Header::size() + 4;
        let mut fields = vec![FieldDef {
            offset: Header::size(),
            ty: SourceType::Int32,
        }];
        let mut ref_fields = Vec::new();

        for ty in &variant.types {
            let ty = replace_type_param(vm, ty.clone(), &self.type_params, None);
            assert!(ty.is_concrete_type(vm));

            if ty.is_unit() {
                continue;
            }

            let field_size = ty.size(vm);
            let field_align = ty.align(vm);

            let offset = mem::align_i32(csize, field_align);
            fields.push(FieldDef {
                offset,
                ty: ty.clone(),
            });

            csize = offset + field_size;

            if let Some(tuple_id) = ty.tuple_id() {
                let tuples = vm.tuples.lock();
                let tuple = tuples.get_tuple(tuple_id);

                for &ref_offset in tuple.references() {
                    ref_fields.push(offset + ref_offset);
                }
            } else if ty.reference_type() {
                ref_fields.push(offset);
            }
        }

        let instance_size = mem::align_i32(csize, mem::ptr_width());

        let mut class_defs = vm.class_defs.lock();
        let id: ClassDefId = class_defs.len().into();

        self.variants[variant_id] = Some(id);

        let class_def = Arc::new(RwLock::new(ClassDef {
            id,
            cls_id: None,
            type_params: TypeList::empty(),
            parent_id: None,
            size: InstanceSize::Fixed(instance_size),
            fields,
            ref_fields,
            vtable: RwLock::new(None),
        }));

        class_defs.push(class_def.clone());

        let class_def = class_def.read();

        let clsptr = &*class_def as *const ClassDef as *mut ClassDef;
        let vtable = VTableBox::new(clsptr, instance_size as usize, 0, &[]);
        *class_def.vtable.write() = Some(vtable);

        id
    }

    pub fn field_id(&mut self, xenum: &EnumData, variant_id: usize, element: u32) -> u32 {
        let variant = &xenum.variants[variant_id];
        let mut units = 0;

        for ty in &variant.types[0..element as usize] {
            if ty.is_unit() {
                units += 1;
            }
        }

        1 + element - units
    }
}

#[derive(Copy, Clone, Debug)]
pub enum EnumLayout {
    Int,
    Ptr,
    Tagged,
}

#[derive(Debug)]
pub struct EnumDefVariant {
    pub types: Vec<SourceType>,
}

pub fn find_methods_in_enum(
    vm: &VM,
    object_type: SourceType,
    name: Name,
    is_static: bool,
) -> Vec<(SourceType, FctId)> {
    let enum_id = object_type.enum_id().unwrap();
    let xenum = vm.enums[enum_id].read();

    for &extension_id in &xenum.extensions {
        let extension = vm.extensions[extension_id].read();

        let table = if is_static {
            &extension.static_names
        } else {
            &extension.instance_names
        };

        if let Some(&fct_id) = table.get(&name) {
            return vec![(object_type, fct_id)];
        }
    }

    Vec::new()
}

pub fn enum_accessible_from(vm: &VM, enum_id: EnumId, namespace_id: NamespaceId) -> bool {
    let xenum = vm.enums[enum_id].read();

    accessible_from(vm, xenum.namespace_id, xenum.is_pub, namespace_id)
}
