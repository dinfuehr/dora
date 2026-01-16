use std::sync::Arc;

use smol_str::SmolStr;

use crate::TokenKind;

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub struct GreenId(pub(crate) u32);

impl GreenId {
    pub(crate) fn new(value: u32) -> GreenId {
        GreenId(value)
    }

    pub(crate) fn value(self) -> u32 {
        self.0
    }

    pub(crate) fn index(self) -> usize {
        self.0 as usize
    }
}

impl std::fmt::Display for GreenId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value())
    }
}

#[derive(Clone, Debug)]
pub struct GreenToken {
    pub kind: TokenKind,
    pub text: SmolStr,
}

#[derive(Clone, Debug)]
pub struct GreenNode {
    pub id: GreenId,
    pub syntax_kind: TokenKind,
    pub children: Vec<GreenElement>,
    pub text_length: u32,
}

#[derive(Clone)]
struct GreenElementIter {
    node: Arc<GreenNode>,
    index: usize,
}

impl GreenElementIter {
    fn new(node: Arc<GreenNode>) -> Self {
        GreenElementIter { node, index: 0 }
    }
}

impl Iterator for GreenElementIter {
    type Item = GreenElement;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.node.children.len() {
            let element = self.node.children[self.index].clone();
            self.index += 1;
            Some(element)
        } else {
            None
        }
    }
}

impl GreenNode {
    pub fn syntax_kind(&self) -> TokenKind {
        self.syntax_kind
    }

    pub fn children(&self) -> &[GreenElement] {
        &self.children
    }

    pub fn text_length(&self) -> u32 {
        self.text_length
    }

    pub fn to_string(&self) -> String {
        let mut result = String::with_capacity(self.text_length as usize);
        let mut worklist = Vec::new();
        worklist.push(GreenElementIter::new(Arc::new(self.clone())));

        while let Some(mut iter) = worklist.pop() {
            loop {
                let element = match iter.next() {
                    Some(element) => element,
                    None => break,
                };

                match element {
                    GreenElement::Token(token) => result.push_str(&token.text),
                    GreenElement::Node(node) => {
                        worklist.push(iter);
                        iter = GreenElementIter::new(node);
                    }
                }
            }
        }

        result
    }
}

#[derive(Clone, Debug)]
pub enum GreenElement {
    Token(Arc<GreenToken>),
    Node(Arc<GreenNode>),
}

impl GreenElement {
    pub fn syntax_kind(&self) -> TokenKind {
        match self {
            GreenElement::Token(token) => token.kind,
            GreenElement::Node(node) => node.syntax_kind(),
        }
    }

    pub fn is_token(&self) -> bool {
        matches!(self, GreenElement::Token(_))
    }

    pub fn is_node(&self) -> bool {
        matches!(self, GreenElement::Node(_))
    }

    pub fn is_trivia(&self) -> bool {
        matches!(self, GreenElement::Token(token) if token.kind.is_trivia())
    }

    pub fn to_token(&self) -> Option<&GreenToken> {
        match self {
            GreenElement::Token(token) => Some(token),
            _ => None,
        }
    }

    pub fn to_node(&self) -> Option<Arc<GreenNode>> {
        match self {
            GreenElement::Node(node) => Some(node.clone()),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::Parser;

    #[test]
    fn green_node_to_string_roundtrip() {
        let source = "fn foo() { let x = 1; }\n";
        let (file, errors) = Parser::from_string(source).parse();
        assert!(errors.is_empty());
        assert_eq!(source, file.root().green().to_string());
    }

    #[test]
    fn green_node_to_string_with_trivia() {
        let source = "fn foo() {\n  // comment\n  let x = 1 + 2;\n}\n";
        let (file, errors) = Parser::from_string(source).parse();
        assert!(errors.is_empty());
        assert_eq!(source, file.root().green().to_string());
    }
}
