use std::sync::Arc;

use crate::{NodeId, Span, TokenKind};

pub type SyntaxNode = Arc<SyntaxNodeData>;
pub type SyntaxToken = Arc<SyntaxTokenData>;

pub enum SyntaxChild {
    Node(SyntaxNode),
    Token(SyntaxToken),
}

pub struct SyntaxNodeData {
    pub id: NodeId,
    pub kind: TokenKind,
    pub span: Span,
    pub children: Vec<SyntaxChild>,
}

impl SyntaxNodeData {
    pub fn new(
        id: NodeId,
        kind: TokenKind,
        span: Span,
        children: Vec<SyntaxChild>,
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

    pub fn children(&self) -> &[SyntaxChild] {
        &self.children
    }
}

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
    open_nodes: Vec<(usize, u32)>,
    next_id: usize,
    children: Vec<SyntaxChild>,
    offset: u32,
}

impl SyntaxTreeBuilder {
    pub fn new() -> SyntaxTreeBuilder {
        SyntaxTreeBuilder {
            open_nodes: Vec::new(),
            next_id: 0,
            children: Vec::new(),
            offset: 0,
        }
    }

    pub fn start_node(&mut self) {
        self.open_nodes.push((self.children.len(), self.offset));
    }

    pub fn token(&mut self, kind: TokenKind, value: String) {
        assert!(kind < TokenKind::EOF);
        let id = self.new_node_id();
        let start = self.offset;
        let len = value.len().try_into().expect("overflow");
        self.offset += len;
        let span = Span::new(start, len);
        self.children
            .push(SyntaxChild::Token(Arc::new(SyntaxTokenData::new(
                id, kind, span, value,
            ))));
    }

    pub fn finish_node(&mut self, kind: TokenKind) -> SyntaxNode {
        assert!(kind > TokenKind::EOF);
        let (children_start, start) = self.open_nodes.pop().expect("missing node start");
        let children = self.children.drain(children_start..).collect::<Vec<_>>();
        let id = self.new_node_id();
        let span = Span::new(start, self.offset - start);
        let node = Arc::new(SyntaxNodeData::new(id, kind, span, children));
        self.children.push(SyntaxChild::Node(node.clone()));
        node
    }

    pub fn create_tree(self) -> SyntaxNode {
        assert_eq!(self.children.len(), 1);
        let child = self.children.into_iter().next().expect("missing element");

        match child {
            SyntaxChild::Node(node) => node.clone(),
            SyntaxChild::Token(..) => unreachable!(),
        }
    }

    fn new_node_id(&mut self) -> NodeId {
        let value = self.next_id;
        self.next_id += 1;
        NodeId(usize::MAX - value)
    }
}
