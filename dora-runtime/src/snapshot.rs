use std::collections::HashMap;
use std::fs::File;
use std::io::{BufWriter, Result as IoResult};
use std::sync::Arc;

use crate::ShapeKind;
use crate::gc::Address;
use crate::gc::root::iterate_strong_roots;
use crate::mirror::{Array, Ref, Str};
use crate::safepoint;
use crate::shape::Shape;
use crate::threads::DoraThread;
use crate::vm::{
    EnumLayout, VM, create_enum_instance, create_struct_instance, get_concrete_tuple_bty,
    specialize_ty,
};
use dora_bytecode::{
    BytecodeType, BytecodeTypeArray, ClassId, EnumId, display_ty, display_ty_array,
};
use fixedbitset::FixedBitSet;

mod json;

pub struct SnapshotGenerator<'a> {
    writer: BufWriter<File>,
    nodes: Vec<Node>,
    nodes_map: HashMap<Address, NodeId>,
    edges: Vec<Edge>,
    edge_buffer: Vec<Edge>,
    strings: Vec<String>,
    strings_map: HashMap<String, StringId>,
    shape_name_map: HashMap<NodeId, StringId>,
    value_map: HashMap<StringId, NodeId>,
    meta_space_start: Address,
    empty_string_id: StringId,
    shape_edge_name_id: StringId,
    shape_type_name_id: StringId,
    length_name_id: StringId,
    value_name_id: StringId,
    variant_name_id: StringId,
    actual_object_name_id: StringId,
    context_name_id: StringId,
    vm: &'a VM,
}

impl<'a> SnapshotGenerator<'a> {
    pub fn new(vm: &'a VM, file: File) -> IoResult<SnapshotGenerator<'a>> {
        let writer = BufWriter::new(file);
        let placeholder = StringId(0);

        Ok(SnapshotGenerator {
            writer,
            nodes: Vec::new(),
            nodes_map: HashMap::new(),
            edges: Vec::new(),
            edge_buffer: Vec::new(),
            strings: Vec::new(),
            strings_map: HashMap::new(),
            shape_name_map: HashMap::new(),
            meta_space_start: vm.meta_space_start(),
            shape_edge_name_id: placeholder, // Will be initialized later.
            shape_type_name_id: placeholder,
            empty_string_id: placeholder,
            length_name_id: placeholder,
            value_name_id: placeholder,
            variant_name_id: placeholder,
            actual_object_name_id: placeholder,
            context_name_id: placeholder,
            value_map: HashMap::new(),
            vm,
        })
    }

    pub fn generate(mut self, threads: &[Arc<DoraThread>]) -> IoResult<()> {
        self.initialize_strings();
        self.iterate_roots(threads);
        self.iterate_heap();
        self.verify_snapshot();
        self.serialize()
    }

    pub fn generate_in_safepoint(self) -> IoResult<()> {
        safepoint::stop_the_world(self.vm, |threads| self.generate(threads))
    }

    fn initialize_strings(&mut self) {
        assert!(self.strings.is_empty());
        self.empty_string_id = self.ensure_string("".to_string());
        self.shape_edge_name_id = self.ensure_string("shape".to_string());
        self.shape_type_name_id = self.ensure_string("Shape".to_string());
        self.length_name_id = self.ensure_string("length".to_string());
        self.value_name_id = self.ensure_string("value".to_string());
        self.variant_name_id = self.ensure_string("variant".to_string());
        self.actual_object_name_id = self.ensure_string("actual_object".to_string());
        self.context_name_id = self.ensure_string("context".to_string());
    }

    fn iterate_roots(&mut self, threads: &[Arc<DoraThread>]) {
        assert!(self.nodes.is_empty());
        self.nodes.push(Node {
            first_edge: EdgeId(0),
            edge_count: 1,
            self_size: 0,
            name: None,
            kind: NodeKind::Synthetic,
        });

        let root_node_id = NodeId(self.nodes.len());
        self.nodes.push(Node {
            first_edge: EdgeId(1),
            edge_count: 0,
            self_size: 0,
            name: None,
            kind: NodeKind::Synthetic,
        });

        self.add_edge(Edge {
            name_or_idx: 0,
            to_node_index: root_node_id,
            kind: EdgeKind::Element,
        });

        let mut edges = 0;

        iterate_strong_roots(self.vm, threads, |slot| {
            let object_address = slot.get();

            if object_address.is_non_null() {
                let object_node_id = self.ensure_node(object_address);

                self.add_edge(Edge {
                    name_or_idx: edges,
                    to_node_index: object_node_id,
                    kind: EdgeKind::Element,
                });

                edges += 1;
            }
        });

        self.node_mut(root_node_id).edge_count = edges;
        self.node_mut(root_node_id).name = Some(self.ensure_string("(GC roots)".into()));
    }

    fn iterate_heap(&mut self) {
        let swiper = self.vm.gc.collector().to_swiper();
        swiper.iterate_heap(self.vm, |address| self.process_object(address));
    }

    fn verify_snapshot(&mut self) {
        let mut edge_count = 0;
        let mut bitset = FixedBitSet::with_capacity(self.edges.len());

        for (_idx, node) in self.nodes.iter().enumerate() {
            edge_count += node.edge_count;

            for edge_idx in node.first_edge.0..node.first_edge.0 + node.edge_count {
                assert!(!bitset.contains(edge_idx));
                bitset.insert(edge_idx);
            }
        }

        assert_eq!(self.edges.len(), edge_count);
    }

    fn process_object(&mut self, address: Address) {
        let node_id = self.ensure_node(address);
        let object = address.to_obj();
        let size = object.size(self.meta_space_start);

        let shape = object.header().shape(self.meta_space_start);
        let shape_node_id = self.process_shape(shape);

        self.node_mut(node_id).self_size = size;

        let edge_start_idx = self.edge_buffer.len();

        self.edge_buffer.push(Edge {
            name_or_idx: self.shape_edge_name_id.0,
            to_node_index: shape_node_id,
            kind: EdgeKind::Property,
        });

        let mut is_string = false;

        match shape.kind() {
            ShapeKind::Class(cls_id, type_params) => {
                self.process_class_object(address, *cls_id, type_params, shape);
            }
            ShapeKind::Array(cls_id, type_params) => {
                self.process_array_object(address, *cls_id, type_params, shape);
            }

            ShapeKind::Enum(enum_id, type_params, variant_id) => {
                self.process_enum_object(address, *enum_id, type_params, *variant_id, shape);
            }

            ShapeKind::String => {
                is_string = true;
            }

            ShapeKind::Builtin => (),

            ShapeKind::Lambda(..) => {
                self.process_special_object(address, shape, self.context_name_id);
            }

            ShapeKind::TraitObject { .. } => {
                self.process_special_object(address, shape, self.actual_object_name_id);
            }
        }

        self.node_mut(node_id).first_edge = EdgeId(self.edges.len());
        self.node_mut(node_id).edge_count = self.edge_buffer.len() - edge_start_idx;
        self.edges.extend(self.edge_buffer.drain(edge_start_idx..));

        if is_string {
            let value: String = Str::cast(object).content_utf8().into();
            self.node_mut(node_id).name = Some(self.ensure_string(value));
            self.node_mut(node_id).kind = NodeKind::String;
        } else {
            let shape_name_id = self
                .shape_name_map
                .get(&shape_node_id)
                .cloned()
                .expect("missing shape name");
            self.node_mut(node_id).name = Some(shape_name_id);
        }
    }

    fn process_shape(&mut self, shape: &Shape) -> NodeId {
        if let Some(shape_node_id) = self.nodes_map.get(&shape.address()) {
            return *shape_node_id;
        }

        let shape_node_id = self.ensure_node(shape.address());
        let shape_name = match shape.kind() {
            ShapeKind::Class(cls_id, type_params) | ShapeKind::Array(cls_id, type_params) => {
                let class = self.vm.class(*cls_id);
                let class_name = class.name.clone();
                format!(
                    "{}{}",
                    class_name,
                    display_ty_array(&self.vm.program, type_params)
                )
            }
            ShapeKind::String => "String".into(),
            ShapeKind::Lambda(fct_id, type_params) => {
                let fct = self.vm.fct(*fct_id);
                let params = fct
                    .params
                    .iter()
                    .skip(1)
                    .map(|ty| {
                        let ty = specialize_ty(self.vm, None, ty.clone(), type_params);
                        display_ty(&self.vm.program, &ty)
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                let ret_ty = display_ty(&self.vm.program, &fct.return_type);
                format!("({}): {}", params, ret_ty)
            }
            ShapeKind::TraitObject {
                trait_ty,
                actual_object_ty,
            } => {
                format!(
                    "{} as {}",
                    display_ty(&self.vm.program, actual_object_ty),
                    display_ty(&self.vm.program, trait_ty)
                )
            }
            ShapeKind::Enum(enum_id, type_params, variant_idx) => {
                let enum_ = self.vm.program.enum_(*enum_id);
                let enum_name = enum_.name.clone();
                format!(
                    "{}{}::{}",
                    enum_name,
                    display_ty_array(&self.vm.program, type_params),
                    enum_.variants[*variant_idx as usize].name
                )
            }
            ShapeKind::Builtin => unreachable!(),
        };

        {
            let shape_instance_name_id = self.ensure_string(shape_name.clone());
            assert!(
                self.shape_name_map
                    .insert(shape_node_id, shape_instance_name_id)
                    .is_none()
            );
        }

        self.node_mut(shape_node_id).name = Some(self.shape_type_name_id);
        shape_node_id
    }

    fn process_class_object(
        &mut self,
        address: Address,
        cls_id: ClassId,
        type_params: &BytecodeTypeArray,
        shape: &Shape,
    ) {
        let class = self.vm.class(cls_id);

        for (field_idx, field) in class.fields.iter().enumerate() {
            let ty = specialize_ty(self.vm, None, field.ty.clone(), type_params);
            let field_offset = shape.fields[field_idx].offset;
            let field_addr = address.offset(field_offset as usize);

            let value_node_id = self.process_value(field_addr, &ty);

            if let Some(name) = field.name.clone() {
                let field_name_id = self.ensure_string(name);
                self.edge_buffer.push(Edge {
                    name_or_idx: field_name_id.0,
                    to_node_index: value_node_id,
                    kind: EdgeKind::Property,
                });
            } else {
                self.edge_buffer.push(Edge {
                    name_or_idx: field_idx,
                    to_node_index: value_node_id,
                    kind: EdgeKind::Element,
                });
            }
        }
    }

    fn process_enum_object(
        &mut self,
        address: Address,
        _enum_id: EnumId,
        _type_params: &BytecodeTypeArray,
        _variant_id: u32,
        shape: &Shape,
    ) {
        for (field_idx, field) in shape.fields.iter().enumerate() {
            let field_offset = shape.fields[field_idx].offset;
            let field_addr = address.offset(field_offset as usize);

            let value_node_id = self.process_value(field_addr, &field.ty);

            if field_idx == 0 {
                self.edge_buffer.push(Edge {
                    name_or_idx: self.variant_name_id.0,
                    to_node_index: value_node_id,
                    kind: EdgeKind::Property,
                });
            } else {
                self.edge_buffer.push(Edge {
                    name_or_idx: field_idx - 1,
                    to_node_index: value_node_id,
                    kind: EdgeKind::Element,
                });
            }
        }
    }

    fn process_special_object(&mut self, address: Address, shape: &Shape, name_id: StringId) {
        assert_eq!(shape.fields.len(), 1);

        let field = &shape.fields[0];
        let field_addr = address.offset(field.offset as usize);

        let value_node_id = self.process_value(field_addr, &field.ty);

        self.edge_buffer.push(Edge {
            name_or_idx: name_id.0,
            to_node_index: value_node_id,
            kind: EdgeKind::Property,
        });
    }

    fn process_value(&mut self, value_address: Address, ty: &BytecodeType) -> NodeId {
        match ty {
            BytecodeType::Unit => self.ensure_value("()".into()),
            BytecodeType::Bool => {
                let value = format!("{}", value_address.load::<bool>());
                self.ensure_value(value)
            }
            BytecodeType::Char => {
                let value = format!("{}", value_address.load::<char>());
                self.ensure_value(value)
            }
            BytecodeType::UInt8 => {
                let value = format!("{}", value_address.load::<u8>());
                self.ensure_value(value)
            }
            BytecodeType::Int32 => {
                let value = format!("{}", value_address.load::<i32>());
                self.ensure_value(value)
            }
            BytecodeType::Int64 => {
                let value = format!("{}", value_address.load::<i64>());
                self.ensure_value(value)
            }
            BytecodeType::Float32 => {
                let value = format!("{}", value_address.load::<f32>());
                self.ensure_value(value)
            }

            BytecodeType::Float64 => {
                let value = format!("{}", value_address.load::<f64>());
                self.ensure_value(value)
            }

            BytecodeType::Tuple(..) => self.process_tuple_value(value_address, ty),
            BytecodeType::Struct(..) => self.process_struct_value(value_address, ty),
            BytecodeType::Enum(..) => self.process_enum_value(value_address, ty),

            BytecodeType::Class(..)
            | BytecodeType::TraitObject(..)
            | BytecodeType::Lambda(..)
            | BytecodeType::Ptr => {
                let field_value = value_address.load::<Address>();

                if field_value.is_non_null() {
                    self.ensure_node(field_value)
                } else {
                    self.ensure_value("null".into())
                }
            }

            BytecodeType::Address => {
                let value = format!("{:?}", value_address.load::<Address>());
                self.ensure_value(value)
            }

            BytecodeType::This
            | BytecodeType::TypeParam(..)
            | BytecodeType::TypeAlias(..)
            | BytecodeType::Assoc { .. } => unreachable!(),
        }
    }

    fn process_tuple_value(&mut self, value_address: Address, ty: &BytecodeType) -> NodeId {
        let tuple = get_concrete_tuple_bty(self.vm, &ty);
        let subtypes = ty.tuple_subtypes();

        let edge_start_idx = self.edge_buffer.len();

        let node_id = NodeId(self.nodes.len());
        self.nodes.push(Node {
            kind: NodeKind::Synthetic,
            first_edge: EdgeId(0),
            edge_count: 0,
            self_size: 0,
            name: None,
        });

        for (idx, (offset, ty)) in tuple.offsets().iter().zip(subtypes.iter()).enumerate() {
            let element_address = value_address.offset(*offset as usize);
            let value_node_id = self.process_value(element_address, &ty);

            self.edge_buffer.push(Edge {
                name_or_idx: idx,
                to_node_index: value_node_id,
                kind: EdgeKind::Element,
            });
        }

        self.node_mut(node_id).first_edge = EdgeId(self.edges.len());
        self.node_mut(node_id).edge_count = self.edge_buffer.len() - edge_start_idx;
        self.edges.extend(self.edge_buffer.drain(edge_start_idx..));

        let params = subtypes
            .iter()
            .map(|ty| display_ty(&self.vm.program, &ty))
            .collect::<Vec<_>>()
            .join(", ");
        let name = format!("({})", params);
        self.node_mut(node_id).name = Some(self.ensure_string(name));

        node_id
    }

    fn process_struct_value(&mut self, value_address: Address, ty: &BytecodeType) -> NodeId {
        let (struct_id, type_params) = match ty {
            BytecodeType::Struct(struct_id, type_params) => (*struct_id, type_params),
            _ => unreachable!(),
        };

        let edge_start_idx = self.edge_buffer.len();

        let node_id = NodeId(self.nodes.len());
        self.nodes.push(Node {
            kind: NodeKind::Synthetic,
            first_edge: EdgeId(0),
            edge_count: 0,
            self_size: 0,
            name: None,
        });

        let struct_ = self.vm.struct_(struct_id);

        let sdef_id = create_struct_instance(self.vm, struct_id, type_params.clone());
        let sdef = self.vm.struct_instances.idx(sdef_id);

        for (idx, field) in sdef.fields.iter().enumerate() {
            let element_address = value_address.offset(field.offset as usize);
            let value_node_id = self.process_value(element_address, &field.ty);

            if let Some(field_name) = struct_.fields[idx].name.clone() {
                let field_name_id = self.ensure_string(field_name);

                self.edge_buffer.push(Edge {
                    name_or_idx: field_name_id.0,
                    to_node_index: value_node_id,
                    kind: EdgeKind::Property,
                });
            } else {
                self.edge_buffer.push(Edge {
                    name_or_idx: idx,
                    to_node_index: value_node_id,
                    kind: EdgeKind::Element,
                });
            }
        }

        self.node_mut(node_id).first_edge = EdgeId(self.edges.len());
        self.node_mut(node_id).edge_count = self.edge_buffer.len() - edge_start_idx;
        self.edges.extend(self.edge_buffer.drain(edge_start_idx..));

        let name = format!(
            "{}{}",
            struct_.name,
            display_ty_array(&self.vm.program, type_params)
        );
        self.node_mut(node_id).name = Some(self.ensure_string(name));

        node_id
    }

    fn process_enum_value(&mut self, value_address: Address, ty: &BytecodeType) -> NodeId {
        let (enum_id, type_params) = match ty {
            BytecodeType::Enum(enum_id, type_params) => (*enum_id, type_params),
            _ => unreachable!(),
        };

        let edge_start_idx = self.edge_buffer.len();

        let enum_ = self.vm.enum_(enum_id);
        let variant_name: &str;

        let edef_id = create_enum_instance(self.vm, enum_id, type_params.clone());
        let edef = self.vm.enum_instances.idx(edef_id);

        match edef.layout {
            EnumLayout::Int => {
                let variant_id = value_address.load::<i32>() as usize;
                variant_name = &enum_.variants[variant_id as usize].name;
            }
            EnumLayout::Ptr => {
                let address = value_address.load::<Address>();
                assert_eq!(enum_.variants.len(), 2);

                let variant0 = enum_.variants.first().unwrap();
                let variant1 = enum_.variants.last().unwrap();

                let (none_variant, some_variant) = if variant0.arguments.is_empty() {
                    (variant0, variant1)
                } else {
                    (variant1, variant0)
                };

                variant_name = if address.is_null() {
                    &none_variant.name
                } else {
                    &some_variant.name
                };

                if address.is_non_null() {
                    let value_node_id = self.ensure_node(address);
                    assert_eq!(some_variant.arguments.len(), 1);

                    self.edge_buffer.push(Edge {
                        name_or_idx: self.value_name_id.0,
                        to_node_index: value_node_id,
                        kind: EdgeKind::Property,
                    });
                }
            }
            EnumLayout::Tagged => {
                let address = value_address.load::<Address>();
                return self.ensure_node(address);
            }
        }

        let node_id = NodeId(self.nodes.len());
        self.nodes.push(Node {
            kind: NodeKind::Synthetic,
            first_edge: EdgeId(0),
            edge_count: 0,
            self_size: 0,
            name: None,
        });

        self.node_mut(node_id).first_edge = EdgeId(self.edges.len());
        self.node_mut(node_id).edge_count = self.edge_buffer.len() - edge_start_idx;
        self.edges.extend(self.edge_buffer.drain(edge_start_idx..));

        let name = format!(
            "{}{}::{}",
            enum_.name,
            display_ty_array(&self.vm.program, type_params),
            variant_name,
        );
        self.node_mut(node_id).name = Some(self.ensure_string(name));

        node_id
    }

    fn process_array_object(
        &mut self,
        address: Address,
        _cls_id: ClassId,
        type_params: &BytecodeTypeArray,
        shape: &Shape,
    ) {
        let array: Ref<Array<u8>> = address.into();
        let length = array.len();

        let element_size = shape.element_size();
        let mut element_addr = array.data_address();

        assert_eq!(type_params.len(), 1);
        let ty = type_params[0].clone();

        let length_id = self.ensure_value(format!("{}", length));

        self.edge_buffer.push(Edge {
            name_or_idx: self.length_name_id.0,
            to_node_index: length_id,
            kind: EdgeKind::Property,
        });

        for element_idx in 0..length {
            let element_node_id = self.process_value(element_addr, &ty);
            self.edge_buffer.push(Edge {
                name_or_idx: element_idx,
                to_node_index: element_node_id,
                kind: EdgeKind::Element,
            });
            element_addr = element_addr.offset(element_size);
        }
    }

    fn add_edge(&mut self, edge: Edge) -> EdgeId {
        let edge_id = EdgeId(self.edges.len());
        self.edges.push(edge);
        edge_id
    }

    fn ensure_node(&mut self, address: Address) -> NodeId {
        *self.nodes_map.entry(address).or_insert_with(|| {
            let id = NodeId(self.nodes.len());
            self.nodes.push(Node {
                kind: NodeKind::Object,
                first_edge: EdgeId(0),
                edge_count: 0,
                self_size: 0,
                name: None,
            });
            id
        })
    }

    fn ensure_string(&mut self, s: String) -> StringId {
        *self.strings_map.entry(s.clone()).or_insert_with(|| {
            let idx = self.strings.len();
            self.strings.push(s);
            StringId(idx)
        })
    }

    fn ensure_value(&mut self, value: String) -> NodeId {
        let value_string_id = self.ensure_string(value);

        if let Some(node_id) = self.value_map.get(&value_string_id) {
            return *node_id;
        }

        let node_id = NodeId(self.nodes.len());

        self.nodes.push(Node {
            kind: NodeKind::String,
            first_edge: EdgeId(0),
            edge_count: 0,
            self_size: 0,
            name: Some(value_string_id),
        });

        assert!(self.value_map.insert(value_string_id, node_id).is_none());

        node_id
    }

    fn node_mut(&mut self, id: NodeId) -> &mut Node {
        &mut self.nodes[id.0]
    }
}

#[derive(Clone, Copy, PartialEq, Hash, Eq)]
struct NodeId(usize);

struct Node {
    first_edge: EdgeId,
    edge_count: usize,
    self_size: usize,
    name: Option<StringId>,
    kind: NodeKind,
}

#[derive(Clone, Copy)]
enum NodeKind {
    Object,
    Synthetic,
    String,
}

#[derive(Clone, Copy)]
struct EdgeId(usize);

struct Edge {
    name_or_idx: usize,
    to_node_index: NodeId,
    kind: EdgeKind,
}

#[derive(Clone, Copy)]
enum EdgeKind {
    Element,
    Property,
}

#[derive(Clone, Copy, PartialEq, Hash, Eq)]
struct StringId(usize);
