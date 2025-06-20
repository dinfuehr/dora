use std::collections::HashMap;
use std::fs::File;
use std::io::{BufWriter, Result as IoResult};
use std::path::PathBuf;
use std::sync::Arc;

use crate::gc::root::iterate_strong_roots;
use crate::gc::Address;
use crate::mirror::{Array, Ref, Str};
use crate::safepoint;
use crate::shape::Shape;
use crate::threads::DoraThread;
use crate::vm::{specialize_ty, VM};
use crate::ShapeKind;
use dora_bytecode::{display_ty, display_ty_array, BytecodeType, BytecodeTypeArray, ClassId};
use fixedbitset::FixedBitSet;

mod json;

pub struct SnapshotGenerator<'a> {
    writer: BufWriter<File>,
    nodes: Vec<Node>,
    nodes_map: HashMap<Address, NodeId>,
    edges: Vec<Edge>,
    strings: Vec<String>,
    strings_map: HashMap<String, StringId>,
    shape_name_map: HashMap<NodeId, StringId>,
    value_map: HashMap<StringId, NodeId>,
    meta_space_start: Address,
    empty_string_id: StringId,
    shape_edge_name_id: StringId,
    shape_type_name_id: StringId,
    vm: &'a VM,
}

impl<'a> SnapshotGenerator<'a> {
    pub fn new(vm: &'a VM, path: PathBuf) -> IoResult<SnapshotGenerator<'a>> {
        let f = File::create(path)?;
        let writer = BufWriter::new(f);

        Ok(SnapshotGenerator {
            writer,
            nodes: Vec::new(),
            nodes_map: HashMap::new(),
            edges: Vec::new(),
            strings: Vec::new(),
            strings_map: HashMap::new(),
            shape_name_map: HashMap::new(),
            meta_space_start: vm.meta_space_start(),
            shape_edge_name_id: StringId(0), // Will be initialized later.
            shape_type_name_id: StringId(0),
            empty_string_id: StringId(0),
            value_map: HashMap::new(),
            vm,
        })
    }

    pub fn generate(mut self) -> IoResult<()> {
        safepoint::stop_the_world(self.vm, |threads| {
            self.initialize_strings();
            self.iterate_roots(threads);
            self.iterate_heap();
            self.verify_snapshot();
            self.serialize()
        })
    }

    fn initialize_strings(&mut self) {
        assert!(self.strings.is_empty());
        self.empty_string_id = self.ensure_string("".to_string());
        self.shape_edge_name_id = self.ensure_string("shape".to_string());
        self.shape_type_name_id = self.ensure_string("Shape".to_string());
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
        swiper.iterate_heap(self.vm, |address| self.process_object(self.vm, address));
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

    fn process_object(&mut self, vm: &VM, address: Address) {
        let node_id = self.ensure_node(address);
        let object = address.to_obj();
        let size = object.size(self.meta_space_start);

        let shape = object.header().shape(self.meta_space_start);
        let shape_node_id = self.process_shape(shape);

        self.node_mut(node_id).first_edge = EdgeId(self.edges.len());
        self.node_mut(node_id).self_size = size;

        let mut edge_count = 1;
        self.add_edge(Edge {
            name_or_idx: self.shape_edge_name_id.0,
            to_node_index: shape_node_id,
            kind: EdgeKind::Property,
        });

        edge_count += match shape.kind() {
            ShapeKind::Class(cls_id, type_params) => {
                self.process_class_object(address, *cls_id, type_params, shape)
            }
            ShapeKind::Array(cls_id, type_params) => {
                self.process_array_object(address, *cls_id, type_params, shape)
            }
            ShapeKind::String => 0,
            _ => 0,
        };

        self.node_mut(node_id).edge_count = edge_count;

        if std::ptr::eq(vm.known.string_shape(), shape) {
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
                let fct = &self.vm.program.functions[fct_id.0 as usize];
                let params = fct
                    .params
                    .iter()
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
                    "TraitObject: {} as {}",
                    display_ty(&self.vm.program, actual_object_ty),
                    display_ty(&self.vm.program, trait_ty)
                )
            }
            ShapeKind::Enum(enum_id, type_params) => {
                let enum_ = &self.vm.program.enums[enum_id.0 as usize];
                let enum_name = enum_.name.clone();
                format!(
                    "Enum: {}{}",
                    enum_name,
                    display_ty_array(&self.vm.program, type_params)
                )
            }
            ShapeKind::Builtin => unreachable!(),
        };

        {
            let shape_instance_name_id = self.ensure_string(shape_name.clone());
            assert!(self
                .shape_name_map
                .insert(shape_node_id, shape_instance_name_id)
                .is_none());
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
    ) -> usize {
        let mut edge_count = 0;
        let class = self.vm.class(cls_id);

        for (field_idx, field) in class.fields.iter().enumerate() {
            let ty = specialize_ty(self.vm, None, field.ty.clone(), type_params);
            let field_offset = shape.fields[field_idx].offset;
            let field_addr = address.offset(field_offset as usize);

            let value_node_id = self.process_value(field_addr, &ty);

            let field_name = if let Some(name) = field.name.as_ref() {
                name.clone()
            } else {
                format!("field_{}", field_idx)
            };

            let field_name_idx = self.ensure_string(field_name);
            self.add_edge(Edge {
                name_or_idx: field_name_idx.0,
                to_node_index: value_node_id,
                kind: EdgeKind::Property,
            });
            edge_count += 1;
        }

        edge_count
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

            BytecodeType::Tuple(..) => self.ensure_value("<tuple>".into()),
            BytecodeType::Struct(..) => self.ensure_value("<struct>".into()),
            BytecodeType::Enum(..) => self.ensure_value("<enum>".into()),

            BytecodeType::Class(..) | BytecodeType::TraitObject(..) | BytecodeType::Lambda(..) => {
                let field_value = value_address.load::<Address>();

                if field_value.is_non_null() {
                    self.ensure_node(field_value)
                } else {
                    self.ensure_value("null".into())
                }
            }

            BytecodeType::Ptr
            | BytecodeType::This
            | BytecodeType::TypeParam(..)
            | BytecodeType::TypeAlias(..)
            | BytecodeType::Assoc { .. }
            | BytecodeType::GenericAssoc { .. } => unreachable!(),
        }
    }

    fn process_array_object(
        &mut self,
        address: Address,
        _cls_id: ClassId,
        type_params: &BytecodeTypeArray,
        shape: &Shape,
    ) -> usize {
        let mut edge_count = 0;
        let array: Ref<Array<u8>> = address.into();
        let length = array.len();

        assert_eq!(shape.instance_size(), 0);
        let element_size = shape.element_size();
        let mut element_addr = array.data_address();

        assert_eq!(type_params.len(), 1);
        let ty = type_params[0].clone();

        for element_idx in 0..length {
            let element_node_id = self.process_value(element_addr, &ty);
            self.add_edge(Edge {
                name_or_idx: element_idx,
                to_node_index: element_node_id,
                kind: EdgeKind::Element,
            });
            edge_count += 1;
            element_addr = element_addr.offset(element_size);
        }

        edge_count
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
