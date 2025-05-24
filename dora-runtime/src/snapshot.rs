use std::collections::HashMap;
use std::fs::File;
use std::io::{BufWriter, Result as IoResult, Write};

use crate::vm::VM;

struct SnapshotGenerator<'a> {
    writer: &'a dyn Write,
    nodes: Vec<Node>,
    edges: Vec<Edge>,
    strings: Vec<String>,
    strings_map: HashMap<String, usize>,
}

impl<'a> SnapshotGenerator<'a> {
    fn generate(vm: &VM) -> IoResult<()> {
        let f = File::create("dora.heapsnapshot")?;
        let mut writer = BufWriter::new(f);

        let mut generator = SnapshotGenerator {
            writer: &mut writer,
            nodes: Vec::new(),
            edges: Vec::new(),
            strings: Vec::new(),
            strings_map: HashMap::new(),
        };
        generator.iterate_heap(vm);
        generator.dump()?;
        Ok(())
    }

    fn iterate_heap(&mut self, vm: &VM) {
        vm.gc
            .collector()
            .to_swiper()
            .iterate_heap(vm, |_address| unimplemented!());
    }

    fn dump(&self) -> IoResult<()> {
        unimplemented!()
    }
}

struct Node {
    edge_count: usize,
    self_size: usize,
}

struct Edge {
    name_or_idx: usize,
    to_nodex_index: usize,
}
