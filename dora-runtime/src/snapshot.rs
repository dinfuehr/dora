use std::collections::HashMap;
use std::fs::File;
use std::io::{BufWriter, Result as IoResult};
use std::path::PathBuf;

use crate::gc::Address;
use crate::shape::Shape;
use crate::vm::{specialize_ty, VM};
use crate::ShapeKind;
use dora_bytecode::{display_ty, display_ty_array, BytecodeTypeArray, ClassId};

mod json;

pub struct SnapshotGenerator<'a> {
    writer: BufWriter<File>,
    nodes: Vec<Node>,
    nodes_map: HashMap<Address, NodeId>,
    edges: Vec<Edge>,
    strings: Vec<String>,
    strings_map: HashMap<String, usize>,
    meta_space_start: Address,
    shape_edge_name_idx: usize,
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
            meta_space_start: vm.meta_space_start(),
            shape_edge_name_idx: 0, // Will be initialized in generate()
            vm,
        })
    }

    pub fn generate(mut self) -> IoResult<()> {
        self.initialize_strings();
        self.iterate_heap();
        self.serialize()
    }

    fn initialize_strings(&mut self) {
        self.shape_edge_name_idx = self.ensure_string("shape".to_string());
    }

    fn iterate_heap(&mut self) {
        let swiper = self.vm.gc.collector().to_swiper();
        swiper.iterate_heap(self.vm, |address| self.process_object(self.vm, address));
    }

    fn process_object(&mut self, vm: &VM, address: Address) {
        let node_id = self.ensure_node(address);
        let object = address.to_obj();
        let size = object.size(self.meta_space_start);

        let shape = object.header().shape(self.meta_space_start);
        let shape_node_id = self.process_shape(shape);

        self.node_mut(node_id).self_size = size;

        // Add edge from object to shape
        self.edges.push(Edge {
            name_or_idx: self.shape_edge_name_idx,
            to_node_index: shape_node_id,
        });

        // Add edges for each reference field
        let mut edge_count = 1; // Start with 1 for the shape edge

        // Handle class fields
        if let ShapeKind::Class(cls_id, type_params) = shape.kind() {
            edge_count += self.process_class_object(vm, address, *cls_id, type_params, shape);
        }

        // Update edge count
        self.node_mut(node_id).edge_count = edge_count;
    }

    fn process_shape(&mut self, shape: &Shape) -> NodeId {
        if let Some(shape_node_id) = self.nodes_map.get(&shape.address()) {
            return *shape_node_id;
        }

        let shape_node_id = self.ensure_node(shape.address());
        let shape_name = match shape.kind() {
            ShapeKind::Class(cls_id, type_params) => {
                let class = self.vm.class(*cls_id);
                let class_name = class.name.clone();
                if type_params.is_empty() {
                    format!("Class: {}", class_name)
                } else {
                    format!(
                        "Class: {}[{}]",
                        class_name,
                        display_ty_array(&self.vm.program, type_params)
                    )
                }
            }
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
                if type_params.is_empty() {
                    format!("Enum: {}", enum_name)
                } else {
                    format!(
                        "Enum: {}[{}]",
                        enum_name,
                        display_ty_array(&self.vm.program, type_params)
                    )
                }
            }
            ShapeKind::Builtin => unreachable!(),
        };
        let shape_name_idx = self.ensure_string(shape_name);
        self.node_mut(shape_node_id).name = Some(shape_name_idx);
        shape_node_id
    }

    fn process_class_object(
        &mut self,
        vm: &VM,
        address: Address,
        cls_id: ClassId,
        type_params: &BytecodeTypeArray,
        shape: &Shape,
    ) -> usize {
        let mut edge_count = 0;
        let class = vm.class(cls_id);

        for (field_idx, field) in class.fields.iter().enumerate() {
            let ty = specialize_ty(vm, None, field.ty.clone(), type_params);
            if ty.is_reference_type() {
                let field_offset = shape.fields[field_idx].offset;
                let field_addr = address.offset(field_offset as usize);
                let field_value = unsafe { *(field_addr.to_ptr::<Address>()) };

                if !field_value.is_null() {
                    let field_node_id = self.ensure_node(field_value);
                    let field_name = match &field.name {
                        Some(name) => name.clone(),
                        None => format!("field_{}", field_idx),
                    };
                    let field_name_idx = self.ensure_string(field_name);
                    self.edges.push(Edge {
                        name_or_idx: field_name_idx,
                        to_node_index: field_node_id,
                    });
                    edge_count += 1;
                }
            }
        }

        edge_count
    }

    fn ensure_node(&mut self, address: Address) -> NodeId {
        *self.nodes_map.entry(address).or_insert_with(|| {
            let id = NodeId(self.nodes.len());
            self.nodes.push(Default::default());
            id
        })
    }

    fn ensure_string(&mut self, s: String) -> usize {
        *self.strings_map.entry(s.clone()).or_insert_with(|| {
            let idx = self.strings.len();
            self.strings.push(s);
            idx
        })
    }

    fn node_mut(&mut self, id: NodeId) -> &mut Node {
        &mut self.nodes[id.0]
    }
}

#[derive(Clone, Copy)]
struct NodeId(usize);

#[derive(Default)]
struct Node {
    edge_count: usize,
    self_size: usize,
    name: Option<usize>, // Index into strings array
}

struct Edge {
    name_or_idx: usize,
    to_node_index: NodeId,
}
