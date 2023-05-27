use std::io;
use std::sync::Arc;

use crate::{NodeId, Span, TokenKind};

pub type SyntaxNode = Arc<SyntaxNodeData>;
pub type SyntaxToken = Arc<SyntaxTokenData>;

#[derive(Clone, Debug)]
pub enum SyntaxElement {
    Node(SyntaxNode),
    Token(SyntaxToken),
}

impl From<SyntaxNode> for SyntaxElement {
    fn from(value: SyntaxNode) -> Self {
        SyntaxElement::Node(value)
    }
}

impl From<SyntaxToken> for SyntaxElement {
    fn from(value: SyntaxToken) -> Self {
        SyntaxElement::Token(value)
    }
}

impl SyntaxElement {
    pub fn kind(&self) -> TokenKind {
        match self {
            SyntaxElement::Node(ref node) => node.kind(),
            SyntaxElement::Token(ref token) => token.kind(),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            SyntaxElement::Node(ref node) => node.span(),
            SyntaxElement::Token(ref token) => token.span(),
        }
    }

    pub fn to_node(&self) -> Option<SyntaxNode> {
        match self {
            SyntaxElement::Node(ref node) => Some(node.clone()),
            SyntaxElement::Token(..) => None,
        }
    }

    pub fn to_token(&self) -> Option<SyntaxToken> {
        match self {
            SyntaxElement::Node(..) => None,
            SyntaxElement::Token(ref token) => Some(token.clone()),
        }
    }
}

#[derive(Clone, Debug)]
pub struct SyntaxNodeData {
    pub id: NodeId,
    pub kind: TokenKind,
    pub span: Span,
    pub children: Vec<SyntaxElement>,
}

impl SyntaxNodeData {
    pub fn new(
        id: NodeId,
        kind: TokenKind,
        span: Span,
        children: Vec<SyntaxElement>,
    ) -> SyntaxNodeData {
        SyntaxNodeData {
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

    pub fn children(&self) -> &[SyntaxElement] {
        &self.children
    }
}

#[derive(Clone, Debug)]
pub struct SyntaxTokenData {
    id: NodeId,
    kind: TokenKind,
    span: Span,
    value: String,
}

impl SyntaxTokenData {
    pub fn new(id: NodeId, kind: TokenKind, span: Span, value: String) -> SyntaxTokenData {
        SyntaxTokenData {
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

pub struct SyntaxTreeBuilder {
    nodes: Vec<(usize, u32)>,
    next_id: usize,
    children: Vec<SyntaxElement>,
    offset: u32,
}

impl SyntaxTreeBuilder {
    pub fn new() -> SyntaxTreeBuilder {
        SyntaxTreeBuilder {
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
            .push(Arc::new(SyntaxTokenData::new(id, kind, span, value)).into());
    }

    pub fn finish_node(&mut self, kind: TokenKind) -> SyntaxNode {
        assert!(kind > TokenKind::EOF);
        let (children_start, start) = self.nodes.pop().expect("missing node start");
        self.finish_node_common(kind, children_start, start)
    }

    pub fn finish_node_starting_at(&mut self, kind: TokenKind, marker: Marker) -> SyntaxNode {
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
    ) -> SyntaxNode {
        let children = self.children.drain(children_start..).collect::<Vec<_>>();
        let id = self.new_node_id();
        let span = Span::new(start, self.offset - start);
        let node = Arc::new(SyntaxNodeData::new(id, kind, span, children));
        self.children.push(SyntaxElement::Node(node.clone()));
        node
    }

    pub fn abandon_node(&mut self) {
        self.nodes.pop().expect("missing node start");
    }

    pub fn create_tree(self) -> SyntaxNode {
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

pub fn dump(node: &SyntaxNode) {
    let mut stdout = io::stdout();
    dump_node(&mut stdout, node, 0);
}

fn dump_node(w: &mut dyn io::Write, node: &SyntaxNode, level: usize) {
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
            SyntaxElement::Node(ref node) => dump_node(w, node, level + 1),
            SyntaxElement::Token(ref token) => dump_token(w, token, level + 1),
        }
    }
}

fn dump_token(w: &mut dyn io::Write, token: &SyntaxToken, level: usize) {
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
