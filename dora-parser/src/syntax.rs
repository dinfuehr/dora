use std::sync::Arc;

use crate::GreenElement;

pub struct SyntaxNode(Arc<SyntaxNodeData>);

pub struct SyntaxNodeData {
    _green: GreenElement,
}
