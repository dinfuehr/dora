use std::convert::Into;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct NodeId(pub usize);

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct EdgeId(pub usize);

pub struct Graph<N, E> {
    nodes: Vec<Node<N>>,
    edges: Vec<Edge<E>>
}

impl<N, E> Graph<N, E> {
    pub fn new() -> Graph<N, E> {
        Graph {
            nodes: Vec::new(),
            edges: Vec::new()
        }
    }

    pub fn add_node(&mut self, data: N) -> NodeId {
        self.nodes.push(Node::new(data));

        NodeId(self.nodes.len()-1)
    }

    pub fn add_edge(&mut self, src: NodeId, dest: NodeId, data: E) -> EdgeId {
        self.edges.push(Edge::new(src, dest, data));

        let id = EdgeId(self.edges.len()-1);

        self.node_mut(src).add_outgoing(id);
        self.node_mut(dest).add_incoming(id);

        id
    }

    fn edge(&self, id: EdgeId) -> &Edge<E> {
        &self.edges[id.0]
    }

    fn edge_mut(&mut self, id: EdgeId) -> &mut Edge<E> {
        &mut self.edges[id.0]
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

    fn node_mut(&mut self, id: NodeId) -> &mut Node<N> {
        &mut self.nodes[id.0]
    }

    fn node_data(&self, id: NodeId) -> &N {
        &self.nodes[id.0].data
    }

    fn node_data_mut(&mut self, id: NodeId) -> &mut N {
        &mut self.nodes[id.0].data
    }
}

struct Node<N> {
    data: N,
    incoming: Vec<EdgeId>,
    outgoing: Vec<EdgeId>
}

impl<N> Node<N> {
    fn new(data: N) -> Node<N> {
        Node {
            data: data,
            incoming: Vec::new(),
            outgoing: Vec::new()
        }
    }

    fn add_incoming(&mut self, edge: EdgeId) {
        self.incoming.push(edge);
    }

    fn add_outgoing(&mut self, edge: EdgeId) {
        self.outgoing.push(edge);
    }

    pub fn incoming(&self) -> &[EdgeId] {
        &self.incoming
    }

    pub fn outgoing(&self) -> &[EdgeId] {
        &self.outgoing
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

#[test]
fn add_node() {
    let mut g: Graph<_, ()> = Graph::new();

    let id = g.add_node(1);
    let node = g.node(id);

    assert_eq!(1, node.data);
}

#[test]
fn add_edge() {
    let mut g = Graph::new();

    let n1 = g.add_node(());
    let n2 = g.add_node(());

    let e = g.add_edge(n1, n2, 3);
    let edge = g.edge(e);

    assert_eq!(n1, edge.src);
    assert_eq!(n2, edge.dest);
    assert_eq!(3, edge.data);

    assert_eq!([e], g.node(n1).outgoing());
    assert_eq!([e], g.node(n2).incoming());
}
