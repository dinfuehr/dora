use std::collections::HashMap;
use std::fs::File;
use std::io::{BufWriter, Result as IoResult, Write};

use crate::gc::Address;
use crate::vm::VM;

struct SnapshotGenerator<'a> {
    writer: &'a dyn Write,
    nodes: Vec<Node>,
    nodes_map: HashMap<Address, NodeId>,
    edges: Vec<Edge>,
    strings: Vec<String>,
    strings_map: HashMap<String, usize>,
    meta_space_start: Address,
}

impl<'a> SnapshotGenerator<'a> {
    fn generate(vm: &VM) -> IoResult<()> {
        let f = File::create("dora.heapsnapshot")?;
        let mut writer = BufWriter::new(f);

        let mut generator = SnapshotGenerator {
            writer: &mut writer,
            nodes: Vec::new(),
            nodes_map: HashMap::new(),
            edges: Vec::new(),
            strings: Vec::new(),
            strings_map: HashMap::new(),
            meta_space_start: vm.meta_space_start(),
        };
        generator.iterate_heap(vm);
        generator.dump()?;
        Ok(())
    }

    fn iterate_heap(&mut self, vm: &VM) {
        let swiper = vm.gc.collector().to_swiper();
        swiper.iterate_heap(vm, |address| {
            let node_id = self.ensure_node(address);

            let object = address.to_obj();
            let size = object.size(self.meta_space_start);
            self.node_mut(node_id).self_size = size;
        });
    }

    fn ensure_node(&mut self, address: Address) -> NodeId {
        *self.nodes_map.entry(address).or_insert_with(|| {
            let id = NodeId(self.nodes.len());
            self.nodes.push(Default::default());
            id
        })
    }

    fn node_mut(&mut self, id: NodeId) -> &mut Node {
        &mut self.nodes[id.0]
    }

    fn dump(&self) -> IoResult<()> {
        unimplemented!()
    }
}

#[derive(Clone, Copy)]
struct NodeId(usize);

#[derive(Default)]
struct Node {
    edge_count: usize,
    self_size: usize,
}

struct Edge {
    name_or_idx: usize,
    to_node_index: NodeId,
}
