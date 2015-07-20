pub struct Graph {
    entry_node: Option<NodeId>,
    exit_node: Option<NodeId>,
    nodes: Vec<Node>,
}

impl Graph {
    fn new() -> Graph {
        Graph {
            entry_node: None,
            exit_node: None,
            nodes: Vec::new(),
        }
    }

    fn is_empty(&self) -> bool {
        self.entry_node.is_none() && self.exit_node.is_none()
    }

    // adds instruction to end of graph
    fn append_instr(&mut self, instr: Instr) {

    }

    // node for id
    fn node(&self, ind: NodeId) -> &Node {
        &self.nodes[ind.0]
    }

    // mutable node for id
    fn node_mut(&mut self, ind: NodeId) -> &mut Node {
        &mut self.nodes[ind.0]
    }

    // returns exit node
    fn exit(&self) -> &Node {
        self.node(self.exit_node.unwrap())
    }

    // returns exit node as mutable
    fn exit_mut(&mut self) -> &mut Node {
        &mut self.nodes[self.exit_node.unwrap().0]
    }

    // returns entry node
    fn entry(&self) -> &Node {
        &self.nodes[self.entry_node.unwrap().0]
    }

    // returns entry node as mutable
    fn entry_mut(&mut self) -> &mut Node {
        &mut self.nodes[self.entry_node.unwrap().0]
    }

    // appends node to end of graph
    fn append_node(&mut self, node: Node) {
        let id = self.add_node(node);

        if self.is_empty() {
            self.entry_node = Some(id);
            self.exit_node = Some(id);
        } else {
            self.exit_mut().add_successor(id);

            let exit_node_id = self.exit_node.unwrap();
            self.node_mut(id).add_predecessor(exit_node_id);

            self.exit_node = Some(id);
        }
    }

    // adds node to Graph and returns assigned NodeId
    fn add_node(&mut self, node: Node) -> NodeId {
        let ind = self.nodes.len();
        self.nodes.push(node);

        NodeId(ind)
    }
}

#[derive(Copy,Clone)]
struct NodeId(usize);

enum Node {
    // entry node has single successor
    NodeEntry(NodeEntryType),

    // exit node has multiple predecessors but no successor
    NodeExit(NodeExitType),

    // normal node has one successor and one predecessor
    NodeEdge(NodeEdgeType),

    // branch has one predecessor and two successors
    NodeBranch(NodeBranchType),

    // join has multiple predecessors and one successor
    NodeJoin(NodeJoinType),
}

struct NodeEntryType {
    succ: Option<NodeId>,
}

struct NodeExitType {
    pre: Vec<NodeId>
}

struct NodeEdgeType {
    pre: Option<NodeId>,
    instr: Instr,
    succ: Option<NodeId>,
}

struct NodeBranchType {
    pre: Option<NodeId>,
    succ_true: Option<NodeId>,
    succ_false: Option<NodeId>,
}

struct NodeJoinType {
    pre: Vec<NodeId>,
    succ: Option<NodeId>,
}

impl Node {
    fn add_predecessor(&mut self, node: NodeId) {
        match *self {
            Node::NodeEntry(_) => unreachable!(),

            Node::NodeEdge(ref mut val) => {
                assert!(val.pre.is_none());
                val.pre = Some(node);
            }

            Node::NodeBranch(ref mut val) => {
                assert!(val.pre.is_none());
                val.pre = Some(node);
            }

            Node::NodeJoin(ref mut val) => val.pre.push(node),

            Node::NodeExit(ref mut val) => val.pre.push(node)
        }
    }

    fn add_successor(&mut self, node: NodeId) {
        match *self {
            Node::NodeEntry(ref mut val) => {
                assert!(val.succ.is_none());
                val.succ = Some(node);
            }

            Node::NodeEdge(ref mut val) => {
                assert!(val.succ.is_none());
                val.succ = Some(node);
            }

            Node::NodeBranch(_) => unimplemented!(),

            Node::NodeJoin(ref mut val) => {
                assert!(val.succ.is_none());
                val.succ = Some(node);
            }

            Node::NodeExit(_) => unreachable!()
        }
    }
}

struct Instr;

#[test]
fn create_graph() {
    let graph = Graph::new();
    assert!(graph.is_empty());
}
