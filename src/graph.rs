// edge-labeled single-instruction graph
// edges are instructions, node states needs to be saved
// outside of graph
pub struct Graph<E> {
    nodes: Vec<NodeData>,
    edges: Vec<EdgeData<E>>,
}

impl<E> Graph<E> {
    pub fn new() -> Graph<E> {
        Graph {
            nodes: Vec::new(),
            edges: Vec::new()
        }
    }

    pub fn add_node(&mut self) -> NodeIndex {
        let idx = self.nodes.len();
        self.nodes.push(NodeData {
            successors: Vec::new(),
            predecessors: Vec::new()
        });

        idx
    }

    pub fn add_edge(&mut self, src: NodeIndex, dest: NodeIndex, data: E) {
        let idx = self.edges.len();
        self.edges.push(EdgeData::new(dest, data));

        {
            let src_node = &mut self.nodes[ src ];
            src_node.successors.push(idx);
        }

        let dest_node = &mut self.nodes[ dest ];
        dest_node.predecessors.push(idx);
    }
}

pub type NodeIndex = usize;

struct NodeData {
    successors: Vec<EdgeIndex>,
    predecessors: Vec<EdgeIndex>,
}

pub type EdgeIndex = usize;

struct EdgeData<T> {
    target: NodeIndex,

    data: T
}

impl<T> EdgeData<T> {
    fn new(target: NodeIndex, data: T) -> EdgeData<T> {
        EdgeData {
            target: target,
            data: data
        }
    }
}

#[test]
fn test_adding_nodes() {
    let mut g : Graph<()> = Graph::new();

    assert_eq!(0, g.add_node());
    assert_eq!(1, g.add_node());
}

#[test]
fn test_adding_edges() {
    let mut g = Graph::new();

    let n1 = g.add_node();
    let n2 = g.add_node();

    g.add_edge(n1, n2, 4711);
}

