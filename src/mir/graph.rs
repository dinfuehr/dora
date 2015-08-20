use std::convert::Into;

#[derive(Copy, Clone)]
pub struct NodeId(pub usize);

#[derive(Copy, Clone)]
pub struct EdgeId(pub usize);

pub struct Graph<N, E> {
    nodes: Vec<Node<N>>,
    edges: Vec<Edge<E>>
}

impl<N, E> Graph<N, E> {
    fn new() -> Graph<N, E> {
        Graph {
            nodes: Vec::new(),
            edges: Vec::new()
        }
    }

    fn add_node(&mut self, data: N) -> NodeId {
        self.nodes.push(Node::new(data));

        NodeId(self.nodes.len()-1)
    }

    fn add_edge(&mut self, src: NodeId, dest: NodeId, data: E) -> EdgeId {
        self.edges.push(Edge::new(src, dest, data));

        EdgeId(self.edges.len()-1)
    }

    fn edge(&self, id: EdgeId) -> &Edge<E> {
        &self.edges[id.0]
    }

    fn edge_data(&self, id: EdgeId) -> &E {
        &self.edges[id.0].data
    }

    fn edge_data_mut(&mut self, id: EdgeId) -> &mut E {
        &mut self.edges[id.0].data
    }

    fn node(&self, id: NodeId) -> &Node<N> {
        &self.nodes[id.0]
    }

    fn node_data(&self, id: NodeId) -> &N {
        &self.nodes[id.0].data
    }

    fn node_data_mut(&mut self, id: NodeId) -> &mut N {
        &mut self.nodes[id.0].data
    }
}

struct Node<N> {
    data: N
}

impl<N> Node<N> {
    fn new(data: N) -> Node<N> {
        Node {
            data: data
        }
    }
}

struct Edge<E> {
    data: E,
    src: NodeId,
    dest: NodeId
}

impl<E> Edge<E> {
    fn new(src: NodeId, dest: NodeId, data: E) -> Edge<E> {
        Edge {
            data: data,

            src: src,
            dest: dest
        }
    }
}
