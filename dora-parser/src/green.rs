use std::io;
use std::sync::Arc;

use crate::{Span, TokenKind};

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

    pub fn len(&self) -> u32 {
        match self {
            GreenElement::Node(ref node) => node.len(),
            GreenElement::Token(ref token) => token.len(),
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
    pub kind: TokenKind,
    pub span: Span,
    pub len: u32,
    pub children: Vec<GreenElement>,
}

impl GreenNodeData {
    pub fn new(
        kind: TokenKind,
        span: Span,
        len: u32,
        children: Vec<GreenElement>,
    ) -> GreenNodeData {
        GreenNodeData {
            kind,
            span,
            len,
            children,
        }
    }

    pub fn kind(&self) -> TokenKind {
        self.kind
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn len(&self) -> u32 {
        assert_eq!(self.span.len(), self.len);
        self.len
    }

    pub fn children(&self) -> &[GreenElement] {
        &self.children
    }
}

#[derive(Clone, Debug)]
pub struct GreenTokenData {
    kind: TokenKind,
    span: Span,
    len: u32,
    value: String,
}

impl GreenTokenData {
    pub fn new(kind: TokenKind, span: Span, len: u32, value: String) -> GreenTokenData {
        GreenTokenData {
            kind,
            span,
            len,
            value,
        }
    }

    pub fn kind(&self) -> TokenKind {
        self.kind
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn len(&self) -> u32 {
        assert_eq!(self.span.len(), self.len);
        self.len
    }

    pub fn value(&self) -> &str {
        self.value.as_str()
    }
}

pub struct GreenTreeBuilder {
    nodes: Vec<(usize, u32)>,
    children: Vec<GreenElement>,
    offset: u32,
}

impl GreenTreeBuilder {
    pub fn new() -> GreenTreeBuilder {
        GreenTreeBuilder {
            nodes: Vec::new(),
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
        let start = self.offset;
        let len = value.len().try_into().expect("overflow");
        self.offset += len;
        let span = Span::new(start, len);
        self.children
            .push(Arc::new(GreenTokenData::new(kind, span, len, value)).into());
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
        let len = self.offset - start;
        let node = Arc::new(GreenNodeData::new(
            kind,
            Span::new(start, len),
            len,
            children,
        ));
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
}

#[derive(Clone)]
pub struct Marker {
    children: usize,
    offset: u32,
}

pub fn dump(node: &GreenNode) {
    let mut stdout = io::stdout();
    dump_node(&mut stdout, node, 0, 0);
}

fn dump_node(w: &mut dyn io::Write, node: &GreenNode, level: usize, mut offset: u32) {
    writeln!(
        w,
        "{}{:?} {}..{}",
        "  ".repeat(level),
        node.kind(),
        offset,
        offset + node.len()
    )
    .expect("write! failed");

    for child in node.children() {
        match child {
            GreenElement::Node(ref node) => dump_node(w, node, level + 1, offset),
            GreenElement::Token(ref token) => dump_token(w, token, level + 1, offset),
        }

        offset += child.len();
    }
}

fn dump_token(w: &mut dyn io::Write, token: &GreenToken, level: usize, offset: u32) {
    writeln!(
        w,
        "{}{:?} {}..{}",
        "  ".repeat(level),
        token.kind(),
        offset,
        offset + token.len()
    )
    .expect("write! failed");
}
