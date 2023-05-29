use std::io;
use std::sync::Arc;

use crate::{NodeId, Span, TokenKind};

pub type GreenNode = Arc<GreenNodeData>;
pub type GreenToken = Arc<GreenTokenData>;

#[derive(Clone, Debug)]
pub enum GreenElement {
    Node(GreenNode),
    Token(GreenToken),
}

impl From<GreenNode> for GreenElement {
    fn from(value: GreenNode) -> Self {
        GreenElement::Node(value)
    }
}

impl From<GreenToken> for GreenElement {
    fn from(value: GreenToken) -> Self {
        GreenElement::Token(value)
    }
}

impl GreenElement {
    pub fn kind(&self) -> TokenKind {
        match self {
            GreenElement::Node(ref node) => node.kind(),
            GreenElement::Token(ref token) => token.kind(),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            GreenElement::Node(ref node) => node.span(),
            GreenElement::Token(ref token) => token.span(),
        }
    }

    pub fn to_node(&self) -> Option<GreenNode> {
        match self {
            GreenElement::Node(ref node) => Some(node.clone()),
            GreenElement::Token(..) => None,
        }
    }

    pub fn to_token(&self) -> Option<GreenToken> {
        match self {
            GreenElement::Node(..) => None,
            GreenElement::Token(ref token) => Some(token.clone()),
        }
    }
}

#[derive(Clone, Debug)]
pub struct GreenNodeData {
    pub id: NodeId,
    pub kind: TokenKind,
    pub span: Span,
    pub children: Vec<GreenElement>,
}

impl GreenNodeData {
    pub fn new(
        id: NodeId,
        kind: TokenKind,
        span: Span,
        children: Vec<GreenElement>,
    ) -> GreenNodeData {
        GreenNodeData {
            id,
            kind,
            span,
            children,
        }
    }

    pub fn id(&self) -> NodeId {
        self.id
    }

    pub fn kind(&self) -> TokenKind {
        self.kind
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn children(&self) -> &[GreenElement] {
        &self.children
    }
}

#[derive(Clone, Debug)]
pub struct GreenTokenData {
    id: NodeId,
    kind: TokenKind,
    span: Span,
    value: String,
}

impl GreenTokenData {
    pub fn new(id: NodeId, kind: TokenKind, span: Span, value: String) -> GreenTokenData {
        GreenTokenData {
            id,
            kind,
            span,
            value,
        }
    }

    pub fn id(&self) -> NodeId {
        self.id
    }

    pub fn kind(&self) -> TokenKind {
        self.kind
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn value(&self) -> &str {
        self.value.as_str()
    }
}

pub struct GreenTreeBuilder {
    nodes: Vec<(usize, u32)>,
    next_id: usize,
    children: Vec<GreenElement>,
    offset: u32,
}

impl GreenTreeBuilder {
    pub fn new() -> GreenTreeBuilder {
        GreenTreeBuilder {
            nodes: Vec::new(),
            next_id: 0,
            children: Vec::new(),
            offset: 0,
        }
    }

    pub fn start_node(&mut self) {
        self.nodes.push((self.children.len(), self.offset));
    }

    pub fn create_marker(&mut self) -> Marker {
        Marker {
            children: self.children.len(),
            offset: self.offset,
        }
    }

    pub fn token(&mut self, kind: TokenKind, value: String) {
        assert!(kind < TokenKind::EOF);
        let id = self.new_node_id();
        let start = self.offset;
        let len = value.len().try_into().expect("overflow");
        self.offset += len;
        let span = Span::new(start, len);
        self.children
            .push(Arc::new(GreenTokenData::new(id, kind, span, value)).into());
    }

    pub fn finish_node(&mut self, kind: TokenKind) -> GreenNode {
        assert!(kind > TokenKind::EOF);
        let (children_start, start) = self.nodes.pop().expect("missing node start");
        self.finish_node_common(kind, children_start, start)
    }

    pub fn finish_node_starting_at(&mut self, kind: TokenKind, marker: Marker) -> GreenNode {
        assert!(kind > TokenKind::EOF);
        let children_start = marker.children;
        let start = marker.offset;
        self.finish_node_common(kind, children_start, start)
    }

    fn finish_node_common(
        &mut self,
        kind: TokenKind,
        children_start: usize,
        start: u32,
    ) -> GreenNode {
        let children = self.children.drain(children_start..).collect::<Vec<_>>();
        let id = self.new_node_id();
        let span = Span::new(start, self.offset - start);
        let node = Arc::new(GreenNodeData::new(id, kind, span, children));
        self.children.push(GreenElement::Node(node.clone()));
        node
    }

    pub fn abandon_node(&mut self) {
        self.nodes.pop().expect("missing node start");
    }

    pub fn create_tree(self) -> GreenNode {
        assert_eq!(self.children.len(), 1);
        let child = self.children.into_iter().next().expect("missing element");
        child.to_node().expect("node expected")
    }

    fn new_node_id(&mut self) -> NodeId {
        let value = self.next_id;
        self.next_id += 1;
        NodeId(usize::MAX - value)
    }
}

#[derive(Clone)]
pub struct Marker {
    children: usize,
    offset: u32,
}

pub fn dump(node: &GreenNode) {
    let mut stdout = io::stdout();
    dump_node(&mut stdout, node, 0);
}

fn dump_node(w: &mut dyn io::Write, node: &GreenNode, level: usize) {
    writeln!(
        w,
        "{}{:?} {}..{}",
        "  ".repeat(level),
        node.kind(),
        node.span().start(),
        node.span().end()
    )
    .expect("write! failed");

    for child in node.children() {
        match child {
            GreenElement::Node(ref node) => dump_node(w, node, level + 1),
            GreenElement::Token(ref token) => dump_token(w, token, level + 1),
        }
    }
}

fn dump_token(w: &mut dyn io::Write, token: &GreenToken, level: usize) {
    writeln!(
        w,
        "{}{:?} {}..{}",
        "  ".repeat(level),
        token.kind(),
        token.span().start(),
        token.span().end()
    )
    .expect("write! failed");
}
