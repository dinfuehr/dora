use std::rc::Rc;
use std::sync::{Arc, OnceLock};

use dora_parser_derive::{AstEnum, AstUnion};

use crate::green::{GreenElement, GreenNode, GreenToken};
use crate::{Span, TokenKind};

pub mod json;
pub mod printer;

#[derive(Clone, Debug)]
pub struct File(Arc<FilePayload>);

#[derive(Clone, Debug)]
struct FilePayload {
    content: Arc<String>,
    root: Arc<GreenNode>,
}

impl File {
    pub(crate) fn new(content: Arc<String>, root: Arc<GreenNode>) -> File {
        File(Arc::new(FilePayload { content, root }))
    }

    fn payload(&self) -> &FilePayload {
        self.0.as_ref()
    }

    pub fn root(&self) -> SyntaxNode {
        let offset = TextOffset(0);
        SyntaxNode::new(self.payload().root.clone(), offset, None)
    }

    pub fn content(&self) -> &Arc<String> {
        &self.payload().content
    }

    pub fn syntax_at_offset(&self, offset: u32) -> Option<SyntaxNode> {
        find_innermost_node_at_offset(self.root(), offset)
    }

    pub fn syntax_by_ptr<T: SyntaxNodeBase>(&self, ptr: SyntaxNodePtr) -> T {
        let node = find_node_by_ptr(self.root(), ptr).expect("node not found for pointer");
        T::cast(node).expect("node of wrong kind")
    }

    pub fn token_at_offset(&self, offset: u32) -> Option<SyntaxToken> {
        find_token_at_offset(self.root(), offset)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, AstEnum)]
#[allow(unused)]
pub(crate) enum NodeKind {
    Alias,
    Alt,
    Argument,
    ArgumentList,
    AssignExpr,
    BinExpr,
    BlockExpr,
    BreakExpr,
    CallExpr,
    Class,
    Const,
    ContinueExpr,
    AsExpr,
    CtorField,
    CtorFieldList,
    CtorPattern,
    FieldExpr,
    ElementList,
    Enum,
    EnumVariant,
    ErrorElem,
    ExprStmt,
    Extern,
    FieldDecl,
    ForExpr,
    Function,
    Global,
    IdentPattern,
    IfExpr,
    Impl,
    IsExpr,
    LambdaExpr,
    LambdaType,
    Let,
    LitBoolExpr,
    LitCharExpr,
    LitFloatExpr,
    LitIntExpr,
    LitPatternBool,
    LitPatternChar,
    LitPatternInt,
    LitPatternFloat,
    LitPatternStr,
    LitStrExpr,
    MatchExpr,
    MatchArm,
    MethodCallExpr,
    Modifier,
    ModifierList,
    Module,
    PathExpr,
    PathSegment,
    Param,
    ParenExpr,
    PathData,
    QualifiedPathExpr,
    QualifiedPathType,
    RefType,
    PathType,
    Rest,
    ReturnExpr,
    Struct,
    TemplateExpr,
    Trait,
    TupleExpr,
    TuplePattern,
    TupleType,
    TypeArgumentList,
    TypeArgument,
    TypeBounds,
    TypeParam,
    TypeParamList,
    UnExpr,
    UnderscorePattern,
    Use,
    UseAs,
    UsePathSegment,
    UseGroup,
    UseName,
    UseTree,
    WhereClause,
    WhereClauseItem,
    WhileExpr,
}

pub trait SyntaxNodeBase: Sized {
    fn cast(node: SyntaxNode) -> Option<Self>;
    fn can_cast(kind: TokenKind) -> bool;

    fn span(&self) -> Span {
        self.syntax_node().span()
    }

    fn full_span(&self) -> Span {
        self.syntax_node().full_span()
    }

    fn text_length(&self) -> u32 {
        self.syntax_node().text_length()
    }

    fn to_string(&self) -> String {
        self.syntax_node().green().to_string()
    }

    fn children(&self) -> impl Iterator<Item = SyntaxNode> {
        self.syntax_node().children()
    }

    fn children_with_tokens(&self) -> SyntaxElementIter<'_> {
        self.syntax_node().children_with_tokens()
    }

    fn syntax_kind(&self) -> TokenKind {
        self.syntax_node().syntax_kind()
    }

    fn as_ptr(&self) -> SyntaxNodePtr {
        self.syntax_node().as_ptr()
    }

    fn syntax_node(&self) -> &SyntaxNode;

    fn parent(&self) -> Option<SyntaxNode> {
        self.syntax_node().parent()
    }

    fn offset(&self) -> TextOffset {
        self.syntax_node().offset()
    }

    fn unwrap(self) -> SyntaxNode;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TextOffset(u32);

impl TextOffset {
    pub fn value(&self) -> u32 {
        self.0
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SyntaxNodePtr {
    syntax_kind: TokenKind,
    span: Span,
}

impl SyntaxNodePtr {
    fn new(syntax_kind: TokenKind, span: Span) -> Self {
        SyntaxNodePtr { syntax_kind, span }
    }

    #[allow(unused)]
    pub(crate) fn syntax_kind(&self) -> TokenKind {
        self.syntax_kind
    }

    pub(crate) fn span(&self) -> Span {
        self.span
    }
}

#[derive(Clone, Debug)]
struct SyntaxNodeData {
    green: Arc<GreenNode>,
    offset: TextOffset,
    parent: Option<SyntaxNode>,
    non_trivia_span: OnceLock<Span>,
}

impl SyntaxNodeData {
    fn leading_trivia_length(&self) -> u32 {
        fn trivia_length(green_children: &[GreenElement]) -> (u32, bool) {
            let mut len = 0;

            for green_element in green_children {
                match green_element {
                    GreenElement::Token(token) => {
                        if token.kind.is_trivia() {
                            len += token.text.len() as u32;
                        } else {
                            return (len, true);
                        }
                    }
                    GreenElement::Node(node) => {
                        let (child_len, found_non_trivia) = trivia_length(node.children());
                        len += child_len;

                        if found_non_trivia {
                            return (len, true);
                        }
                    }
                }
            }

            (len, false)
        }

        let (len, _) = trivia_length(self.green.children());
        len
    }

    fn trailing_trivia_length(&self) -> u32 {
        fn trivia_length(green_children: &[GreenElement]) -> (u32, bool) {
            let mut len = 0;

            for green_element in green_children.iter().rev() {
                match green_element {
                    GreenElement::Token(token) => {
                        if token.kind.is_trivia() {
                            len += token.text.len() as u32;
                        } else {
                            return (len, true);
                        }
                    }
                    GreenElement::Node(node) => {
                        let (child_len, found_non_trivia) = trivia_length(node.children());
                        len += child_len;

                        if found_non_trivia {
                            return (len, true);
                        }
                    }
                }
            }

            (len, false)
        }

        let (len, _) = trivia_length(self.green.children());
        len
    }

    fn ensure_non_trivia_span(&self) -> Span {
        self.non_trivia_span
            .get_or_init(|| self.compute_non_trivia_span())
            .clone()
    }

    fn compute_non_trivia_span(&self) -> Span {
        let pre = self.leading_trivia_length();
        let post = self.trailing_trivia_length();

        let len = self.green.text_length().saturating_sub(pre + post);

        Span::new(self.offset.value() + pre, len)
    }
}

#[derive(Clone, Debug)]
pub struct SyntaxNode(Rc<SyntaxNodeData>);

impl SyntaxNode {
    pub fn new(green: Arc<GreenNode>, offset: TextOffset, parent: Option<SyntaxNode>) -> Self {
        SyntaxNode(Rc::new(SyntaxNodeData {
            green,
            offset,
            parent,
            non_trivia_span: OnceLock::new(),
        }))
    }

    pub fn offset(&self) -> TextOffset {
        self.0.offset.clone()
    }

    pub fn parent(&self) -> Option<SyntaxNode> {
        self.0.parent.clone()
    }

    pub fn span(&self) -> Span {
        self.non_trivia_span()
    }

    pub fn full_span(&self) -> Span {
        Span::new(self.offset().value(), self.text_length())
    }

    pub fn non_trivia_span(&self) -> Span {
        self.0.ensure_non_trivia_span()
    }

    pub fn as_ptr(&self) -> SyntaxNodePtr {
        SyntaxNodePtr::new(self.syntax_kind(), self.full_span())
    }

    pub fn children_with_tokens(&self) -> SyntaxElementIter<'_> {
        SyntaxElementIter::new(self.green().children(), self.offset(), Some(self.clone()))
    }

    pub fn children(&self) -> impl Iterator<Item = SyntaxNode> + '_ {
        self.children_with_tokens()
            .filter_map(|element| match element {
                SyntaxElement::Node(node) => Some(node),
                SyntaxElement::Token(_) => None,
            })
    }

    pub fn green(&self) -> &Arc<GreenNode> {
        &self.0.green
    }
}

impl SyntaxNodeBase for SyntaxNode {
    fn cast(node: SyntaxNode) -> Option<Self> {
        Some(node)
    }

    fn can_cast(_kind: TokenKind) -> bool {
        true
    }

    fn span(&self) -> Span {
        self.non_trivia_span()
    }

    fn full_span(&self) -> Span {
        self.full_span()
    }

    fn text_length(&self) -> u32 {
        self.green().text_length()
    }

    fn children(&self) -> impl Iterator<Item = SyntaxNode> {
        self.children()
    }

    fn children_with_tokens(&self) -> SyntaxElementIter<'_> {
        self.children_with_tokens()
    }

    fn syntax_kind(&self) -> TokenKind {
        self.green().syntax_kind()
    }

    fn as_ptr(&self) -> SyntaxNodePtr {
        self.as_ptr()
    }

    fn syntax_node(&self) -> &SyntaxNode {
        self
    }

    fn parent(&self) -> Option<SyntaxNode> {
        self.parent()
    }

    fn offset(&self) -> TextOffset {
        self.offset()
    }

    fn unwrap(self) -> SyntaxNode {
        self
    }
}

impl PartialEq for SyntaxNode {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0.green, &other.0.green)
    }
}

impl Eq for SyntaxNode {}

#[derive(Clone, Debug)]
struct SyntaxTokenData {
    green: Arc<GreenToken>,
    offset: TextOffset,
    parent: Option<SyntaxNode>,
}

#[derive(Clone, Debug)]
pub struct SyntaxToken(Rc<SyntaxTokenData>);

impl SyntaxToken {
    pub fn new(green: Arc<GreenToken>, offset: TextOffset, parent: Option<SyntaxNode>) -> Self {
        SyntaxToken(Rc::new(SyntaxTokenData {
            green,
            offset,
            parent,
        }))
    }

    pub fn green(&self) -> &GreenToken {
        self.0.green.as_ref()
    }

    pub fn syntax_kind(&self) -> TokenKind {
        self.0.green.kind
    }

    pub fn is_trivia(&self) -> bool {
        self.syntax_kind().is_trivia()
    }

    pub fn text(&self) -> &str {
        &self.0.green.text
    }

    pub fn text_length(&self) -> u32 {
        self.0.green.text.len() as u32
    }

    pub fn offset(&self) -> TextOffset {
        self.0.offset
    }

    pub fn parent(&self) -> Option<SyntaxNode> {
        self.0.parent.clone()
    }

    pub fn span(&self) -> Span {
        let start = self.offset().value();
        Span::new(start, self.text_length())
    }
}

impl PartialEq for SyntaxToken {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0.green, &other.0.green)
    }
}

impl Eq for SyntaxToken {}

pub enum SyntaxElement {
    Token(SyntaxToken),
    Node(SyntaxNode),
}

impl SyntaxElement {
    pub fn is_trivia(&self) -> bool {
        match self {
            SyntaxElement::Node(..) => false,
            SyntaxElement::Token(token) => token.syntax_kind().is_trivia(),
        }
    }

    pub fn syntax_kind(&self) -> TokenKind {
        match self {
            SyntaxElement::Node(node) => node.syntax_kind(),
            SyntaxElement::Token(token) => token.syntax_kind(),
        }
    }

    pub fn offset(&self) -> TextOffset {
        match self {
            SyntaxElement::Node(node) => node.offset(),
            SyntaxElement::Token(token) => token.offset(),
        }
    }

    pub fn parent(&self) -> Option<SyntaxNode> {
        match self {
            SyntaxElement::Node(node) => node.parent(),
            SyntaxElement::Token(token) => token.parent(),
        }
    }

    pub fn text_length(&self) -> u32 {
        match self {
            SyntaxElement::Node(node) => node.text_length(),
            SyntaxElement::Token(token) => token.text_length(),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            SyntaxElement::Node(node) => node.span(),
            SyntaxElement::Token(token) => token.span(),
        }
    }

    pub fn to_node(self) -> Option<SyntaxNode> {
        match self {
            SyntaxElement::Node(node) => Some(node),
            SyntaxElement::Token(..) => None,
        }
    }

    pub fn to_token(self) -> Option<SyntaxToken> {
        match self {
            SyntaxElement::Token(token) => Some(token),
            SyntaxElement::Node(..) => None,
        }
    }
}

pub fn walk_children<V: Visitor, N: SyntaxNodeBase>(v: &mut V, node: N) {
    for child in node.children() {
        visit_node(v, child);
    }
}

pub struct SyntaxElementIter<'a> {
    elements: &'a [GreenElement],
    index: usize,
    current_offset: u32,
    parent: Option<SyntaxNode>,
}

impl<'a> SyntaxElementIter<'a> {
    pub fn new(
        elements: &'a [GreenElement],
        start_offset: TextOffset,
        parent: Option<SyntaxNode>,
    ) -> Self {
        SyntaxElementIter {
            elements,
            index: 0,
            current_offset: start_offset.value(),
            parent,
        }
    }
}

impl<'a> SyntaxElementIter<'a> {
    pub fn peek(&self) -> Option<SyntaxElement> {
        self.current_element().and_then(|(e, _)| Some(e))
    }

    pub fn current_element(&self) -> Option<(SyntaxElement, u32)> {
        let element = self.elements.get(self.index)?;
        let offset = TextOffset(self.current_offset);

        match element {
            GreenElement::Token(green_token) => {
                let token = SyntaxToken::new(green_token.clone(), offset, self.parent.clone());
                let len = token.text().len().try_into().expect("overflow");
                Some((SyntaxElement::Token(token), len))
            }
            GreenElement::Node(node) => {
                let node = SyntaxNode::new(node.clone(), offset, self.parent.clone());
                let len = node.text_length();
                Some((SyntaxElement::Node(node), len))
            }
        }
    }

    pub fn peek_kind(&self) -> Option<TokenKind> {
        let element = self.elements.get(self.index)?;
        Some(element.syntax_kind())
    }

    pub fn peek_kind_ignore_trivia(&self) -> Option<TokenKind> {
        for element in &self.elements[self.index..] {
            if element.is_trivia() {
                continue;
            }

            return Some(element.syntax_kind());
        }

        None
    }
}

impl<'a> Iterator for SyntaxElementIter<'a> {
    type Item = SyntaxElement;

    fn next(&mut self) -> Option<Self::Item> {
        let (element, element_len) = self.current_element()?;
        self.index += 1;
        self.current_offset += element_len;
        Some(element)
    }
}

impl<'a> ExactSizeIterator for SyntaxElementIter<'a> {
    fn len(&self) -> usize {
        self.elements.len() - self.index
    }
}

impl AstAlias {
    pub fn modifier_list(&self) -> Option<AstModifierList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstModifierList::cast(n))
    }

    pub fn bounds(&self) -> Option<AstTypeBounds> {
        self.syntax_node()
            .children()
            .find_map(|n| AstTypeBounds::cast(n))
    }

    pub fn ty(&self) -> Option<AstType> {
        self.syntax_node().children().find_map(|n| AstType::cast(n))
    }

    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| t.syntax_kind() == TokenKind::IDENTIFIER)
    }

    pub fn type_param_list(&self) -> Option<AstTypeParamList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstTypeParamList::cast(n))
    }

    pub fn where_clause(&self) -> Option<AstWhereClause> {
        self.syntax_node()
            .children()
            .find_map(|n| AstWhereClause::cast(n))
    }
}

impl AstAlt {
    pub fn alts(&self) -> impl Iterator<Item = AstPattern> {
        self.syntax_node()
            .children()
            .filter_map(|n| AstPattern::cast(n))
    }
}

impl AstArgument {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| t.syntax_kind() == TokenKind::IDENTIFIER)
    }

    pub fn expr(&self) -> Option<AstExpr> {
        self.syntax_node().children().find_map(|n| AstExpr::cast(n))
    }
}

impl AstArgumentList {
    pub fn items(&self) -> impl Iterator<Item = AstArgument> {
        self.syntax_node()
            .children()
            .filter_map(|n| AstArgument::cast(n))
    }
}

impl AstBinExpr {
    pub fn lhs(&self) -> AstExpr {
        self.syntax_node()
            .children()
            .find_map(|n| AstExpr::cast(n))
            .unwrap()
    }

    pub fn rhs(&self) -> AstExpr {
        self.syntax_node()
            .children()
            .filter_map(|n| AstExpr::cast(n))
            .nth(1)
            .unwrap()
    }

    pub fn op(&self) -> BinOp {
        let tok = self
            .children_with_tokens()
            .filter_map(|elem| elem.to_token())
            .find(|tok| !tok.syntax_kind().is_trivia())
            .expect("binary operator missing token");

        match tok.syntax_kind() {
            TokenKind::OR_OR => BinOp::Or,
            TokenKind::AND_AND => BinOp::And,
            TokenKind::EQ_EQ => BinOp::Cmp(CmpOp::Eq),
            TokenKind::NOT_EQ => BinOp::Cmp(CmpOp::Ne),
            TokenKind::LT => BinOp::Cmp(CmpOp::Lt),
            TokenKind::LE => BinOp::Cmp(CmpOp::Le),
            TokenKind::GT => BinOp::Cmp(CmpOp::Gt),
            TokenKind::GE => BinOp::Cmp(CmpOp::Ge),
            TokenKind::EQ_EQ_EQ => BinOp::Cmp(CmpOp::Is),
            TokenKind::NOT_EQ_EQ => BinOp::Cmp(CmpOp::IsNot),
            TokenKind::OR => BinOp::BitOr,
            TokenKind::AND => BinOp::BitAnd,
            TokenKind::CARET => BinOp::BitXor,
            TokenKind::ADD => BinOp::Add,
            TokenKind::SUB => BinOp::Sub,
            TokenKind::MUL => BinOp::Mul,
            TokenKind::DIV => BinOp::Div,
            TokenKind::MODULO => BinOp::Mod,
            TokenKind::LT_LT => BinOp::ShiftL,
            TokenKind::GT_GT => BinOp::ArithShiftR,
            TokenKind::GT_GT_GT => BinOp::LogicalShiftR,
            _ => unreachable!("unexpected token for binary op"),
        }
    }
}

impl AstAssignExpr {
    pub fn lhs(&self) -> AstExpr {
        self.syntax_node()
            .children()
            .find_map(|n| AstExpr::cast(n))
            .unwrap()
    }

    pub fn rhs(&self) -> AstExpr {
        self.syntax_node()
            .children()
            .filter_map(|n| AstExpr::cast(n))
            .nth(1)
            .unwrap()
    }

    pub fn op(&self) -> AssignOp {
        let tok = self
            .children_with_tokens()
            .filter_map(|elem| elem.to_token())
            .find(|tok| !tok.syntax_kind().is_trivia())
            .expect("assign operator missing token");

        match tok.syntax_kind() {
            TokenKind::EQ => AssignOp::Assign,
            TokenKind::OR_EQ => AssignOp::BitOrAssign,
            TokenKind::AND_EQ => AssignOp::BitAndAssign,
            TokenKind::CARET_EQ => AssignOp::BitXorAssign,
            TokenKind::ADD_EQ => AssignOp::AddAssign,
            TokenKind::SUB_EQ => AssignOp::SubAssign,
            TokenKind::MUL_EQ => AssignOp::MulAssign,
            TokenKind::DIV_EQ => AssignOp::DivAssign,
            TokenKind::MOD_EQ => AssignOp::ModAssign,
            TokenKind::LT_LT_EQ => AssignOp::ShiftLAssign,
            TokenKind::GT_GT_EQ => AssignOp::ArithShiftRAssign,
            TokenKind::GT_GT_GT_EQ => AssignOp::LogicalShiftRAssign,
            _ => unreachable!("unexpected token for assign op"),
        }
    }
}

impl AstBlockExpr {
    pub fn stmts(&self) -> impl Iterator<Item = AstStmt> {
        self.syntax_node()
            .children()
            .filter_map(|n| AstStmt::cast(n))
    }

    pub fn stmts_without_tail(&self) -> impl Iterator<Item = AstStmt> {
        let mut iter = self.stmts().peekable();

        std::iter::from_fn(move || {
            let item = iter.next()?;
            if iter.peek().is_none() {
                // last element
                if item.is_expr_stmt() && item.clone().as_expr_stmt().semicolon().is_none() {
                    None
                } else {
                    Some(item)
                }
            } else {
                Some(item)
            }
        })
    }

    pub fn tail(&self) -> Option<AstStmt> {
        self.stmts().last().and_then(|s| {
            if let Some(stmt) = s.clone().to_expr_stmt() {
                if stmt.semicolon().is_none() {
                    Some(s)
                } else {
                    None
                }
            } else {
                None
            }
        })
    }
}

impl AstCallExpr {
    pub fn callee(&self) -> AstExpr {
        self.syntax_node()
            .children()
            .find_map(|n| AstExpr::cast(n))
            .unwrap()
    }

    pub fn object(&self) -> Option<AstExpr> {
        let callee_node = self.callee();

        match callee_node {
            AstExpr::FieldExpr(dot) => Some(dot.lhs()),
            _ => None,
        }
    }

    pub fn arg_list(&self) -> AstArgumentList {
        self.syntax_node()
            .children()
            .find_map(|n| AstArgumentList::cast(n))
            .unwrap()
    }
}

impl AstClass {
    pub fn modifier_list(&self) -> Option<AstModifierList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstModifierList::cast(n))
    }

    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| t.syntax_kind() == TokenKind::IDENTIFIER)
    }

    pub fn type_param_list(&self) -> Option<AstTypeParamList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstTypeParamList::cast(n))
    }

    pub fn where_clause(&self) -> Option<AstWhereClause> {
        self.syntax_node()
            .children()
            .find_map(|n| AstWhereClause::cast(n))
    }

    pub fn field_name_style(&self) -> FieldNameStyle {
        let found_brace = self
            .syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| t.syntax_kind() == TokenKind::L_BRACE)
            .is_some();

        if found_brace {
            FieldNameStyle::Named
        } else {
            FieldNameStyle::Positional
        }
    }

    pub fn fields(&self) -> impl Iterator<Item = AstFieldDecl> {
        self.syntax_node()
            .children()
            .filter_map(|n| AstFieldDecl::cast(n))
    }
}

impl AstConst {
    pub fn modifier_list(&self) -> Option<AstModifierList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstModifierList::cast(n))
    }

    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| t.syntax_kind() == TokenKind::IDENTIFIER)
    }

    pub fn data_type(&self) -> Option<AstType> {
        self.syntax_node().children().find_map(|n| AstType::cast(n))
    }

    pub fn expr(&self) -> Option<AstExpr> {
        self.syntax_node().children().find_map(|n| AstExpr::cast(n))
    }
}

impl AstAsExpr {
    pub fn object(&self) -> Option<AstExpr> {
        self.syntax_node().children().find_map(|n| AstExpr::cast(n))
    }

    pub fn data_type(&self) -> Option<AstType> {
        self.syntax_node().children().find_map(|n| AstType::cast(n))
    }
}

impl AstCtorField {
    pub fn ident(&self) -> Option<SyntaxToken> {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| t.syntax_kind() == TokenKind::IDENTIFIER)
    }

    pub fn pattern(&self) -> Option<AstPattern> {
        self.syntax_node()
            .children()
            .find_map(|n| AstPattern::cast(n))
    }
}

impl AstCtorFieldList {
    pub fn items(&self) -> impl Iterator<Item = AstCtorField> {
        self.syntax_node()
            .children()
            .filter_map(|n| AstCtorField::cast(n))
    }
}

impl AstCtorPattern {
    pub fn path(&self) -> AstPathData {
        self.syntax_node()
            .children()
            .find_map(|n| AstPathData::cast(n))
            .unwrap()
    }

    pub fn param_list(&self) -> Option<AstCtorFieldList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstCtorFieldList::cast(n))
    }
}

impl AstFieldExpr {
    pub fn lhs(&self) -> AstExpr {
        self.syntax_node()
            .children()
            .find_map(|n| AstExpr::cast(n))
            .unwrap()
    }

    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|n| n.to_token())
            .filter(|t| !t.syntax_kind().is_trivia())
            .nth(1)
    }

    pub fn dot_token(&self) -> SyntaxToken {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|n| n.to_token())
            .find(|x| x.syntax_kind() == TokenKind::DOT)
            .unwrap()
    }
}

#[derive(Clone, AstUnion)]
pub enum AstElement {
    Alias(AstAlias),
    Class(AstClass),
    Const(AstConst),
    Enum(AstEnum),
    #[ast_union_kind(ERROR_ELEM)]
    Error(SyntaxNode),
    Extern(AstExtern),
    Function(AstFunction),
    Global(AstGlobal),
    Impl(AstImpl),
    Module(AstModule),
    Struct(AstStruct),
    Trait(AstTrait),
    Use(AstUse),
}

impl AstElementList {
    pub fn items(&self) -> impl Iterator<Item = AstElement> {
        self.syntax_node()
            .children()
            .filter_map(|n| AstElement::cast(n))
    }

    pub fn items_len(&self) -> usize {
        self.items().count()
    }

    pub fn items_at(&self, index: usize) -> AstElement {
        self.items().nth(index).unwrap()
    }
}

impl AstEnum {
    pub fn modifier_list(&self) -> Option<AstModifierList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstModifierList::cast(n))
    }

    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| t.syntax_kind() == TokenKind::IDENTIFIER)
    }

    pub fn type_param_list(&self) -> Option<AstTypeParamList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstTypeParamList::cast(n))
    }

    pub fn where_clause(&self) -> Option<AstWhereClause> {
        self.syntax_node()
            .children()
            .find_map(|n| AstWhereClause::cast(n))
    }

    pub fn variants(&self) -> impl Iterator<Item = AstEnumVariant> {
        self.syntax_node()
            .children()
            .filter_map(|n| AstEnumVariant::cast(n))
    }

    pub fn variants_len(&self) -> usize {
        self.variants().count()
    }

    pub fn variants_at(&self, index: usize) -> AstEnumVariant {
        self.variants().nth(index).unwrap()
    }
}

impl AstEnumVariant {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| t.syntax_kind() == TokenKind::IDENTIFIER)
    }

    pub fn field_name_style(&self) -> FieldNameStyle {
        let found_brace = self
            .syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| t.syntax_kind() == TokenKind::L_BRACE)
            .is_some();

        if found_brace {
            FieldNameStyle::Named
        } else {
            FieldNameStyle::Positional
        }
    }

    pub fn fields(&self) -> impl Iterator<Item = AstFieldDecl> {
        self.syntax_node()
            .children()
            .filter_map(|n| AstFieldDecl::cast(n))
    }
}

#[derive(Clone, AstUnion)]
pub enum AstExpr {
    AssignExpr(AstAssignExpr),
    BinExpr(AstBinExpr),
    BlockExpr(AstBlockExpr),
    BreakExpr(AstBreakExpr),
    CallExpr(AstCallExpr),
    ContinueExpr(AstContinueExpr),
    AsExpr(AstAsExpr),
    FieldExpr(AstFieldExpr),
    #[ast_union_kind(ERROR_EXPR)]
    Error(SyntaxNode),
    ForExpr(AstForExpr),
    PathExpr(AstPathExpr),
    QualifiedPathExpr(AstQualifiedPathExpr),
    IfExpr(AstIfExpr),
    IsExpr(AstIsExpr),
    LambdaExpr(AstLambdaExpr),
    LitBoolExpr(AstLitBoolExpr),
    LitCharExpr(AstLitCharExpr),
    LitFloatExpr(AstLitFloatExpr),
    LitIntExpr(AstLitIntExpr),
    LitStrExpr(AstLitStrExpr),
    MatchExpr(AstMatchExpr),
    MethodCallExpr(AstMethodCallExpr),
    ParenExpr(AstParenExpr),
    ReturnExpr(AstReturnExpr),
    TemplateExpr(AstTemplateExpr),
    TupleExpr(AstTupleExpr),
    UnExpr(AstUnExpr),
    WhileExpr(AstWhileExpr),
}

impl AstExprStmt {
    pub fn expr(&self) -> AstExpr {
        self.syntax_node()
            .children()
            .find_map(|n| AstExpr::cast(n))
            .unwrap()
    }

    pub fn semicolon(&self) -> Option<SyntaxToken> {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| t.syntax_kind() == TokenKind::SEMICOLON)
    }
}

impl AstExtern {
    pub fn modifier_list(&self) -> Option<AstModifierList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstModifierList::cast(n))
    }

    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| t.syntax_kind() == TokenKind::IDENTIFIER)
    }
}

impl AstFieldDecl {
    pub fn modifier_list(&self) -> Option<AstModifierList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstModifierList::cast(n))
    }

    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| t.syntax_kind() == TokenKind::IDENTIFIER)
    }

    pub fn data_type(&self) -> Option<AstType> {
        self.syntax_node().children().find_map(|n| AstType::cast(n))
    }
}

#[derive(Copy, Clone, Debug)]
pub enum FieldNameStyle {
    Named,
    Positional,
}

impl FieldNameStyle {
    pub fn is_named(&self) -> bool {
        match self {
            FieldNameStyle::Named => true,
            _ => false,
        }
    }

    pub fn is_positional(&self) -> bool {
        match self {
            FieldNameStyle::Positional => true,
            _ => false,
        }
    }
}

impl AstForExpr {
    pub fn pattern(&self) -> AstPattern {
        self.syntax_node()
            .children()
            .find_map(|n| AstPattern::cast(n))
            .unwrap()
    }

    pub fn expr(&self) -> AstExpr {
        self.syntax_node()
            .children()
            .find_map(|n| AstExpr::cast(n))
            .unwrap()
    }

    pub fn block(&self) -> AstBlockExpr {
        self.syntax_node()
            .children()
            .find_map(|n| AstBlockExpr::cast(n))
            .unwrap()
    }
}

impl AstFunction {
    pub fn declaration_span(&self) -> Span {
        compute_declaration_span(self.syntax_node())
    }

    pub fn modifier_list(&self) -> Option<AstModifierList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstModifierList::cast(n))
    }

    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| t.syntax_kind() == TokenKind::IDENTIFIER)
    }

    pub fn type_param_list(&self) -> Option<AstTypeParamList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstTypeParamList::cast(n))
    }

    pub fn return_type(&self) -> Option<AstType> {
        self.syntax_node().children().find_map(|n| AstType::cast(n))
    }

    pub fn where_clause(&self) -> Option<AstWhereClause> {
        self.syntax_node()
            .children()
            .find_map(|n| AstWhereClause::cast(n))
    }

    pub fn params(&self) -> impl Iterator<Item = AstParam> {
        self.syntax_node()
            .children()
            .filter_map(|n| AstParam::cast(n))
    }

    pub fn params_len(&self) -> usize {
        self.params().count()
    }

    pub fn params_at(&self, index: usize) -> AstParam {
        self.params().nth(index).unwrap()
    }

    pub fn block(&self) -> Option<AstBlockExpr> {
        self.syntax_node()
            .children()
            .find_map(|n| AstBlockExpr::cast(n))
    }
}

#[derive(Clone, Debug)]
pub enum FunctionKind {
    Function,
    Lambda,
}

impl FunctionKind {
    pub fn is_lambda(&self) -> bool {
        match self {
            &FunctionKind::Lambda => true,
            &FunctionKind::Function => false,
        }
    }
}

impl AstGlobal {
    pub fn modifier_list(&self) -> Option<AstModifierList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstModifierList::cast(n))
    }

    pub fn mutable(&self) -> bool {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|n| n.syntax_kind() == TokenKind::MUT_KW)
            .is_some()
    }

    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| t.syntax_kind() == TokenKind::IDENTIFIER)
    }

    pub fn data_type(&self) -> Option<AstType> {
        self.syntax_node().children().find_map(|n| AstType::cast(n))
    }

    pub fn initial_value(&self) -> Option<AstExpr> {
        self.syntax_node().children().find_map(|n| AstExpr::cast(n))
    }
}

impl AstPathExpr {
    pub fn segments(&self) -> impl Iterator<Item = AstPathSegment> {
        self.syntax_node()
            .children()
            .filter_map(|n| AstPathSegment::cast(n))
    }

    /// Returns the path as a string with "::" separators (e.g., "foo::bar::baz").
    /// Useful for error messages.
    pub fn path_string(&self) -> String {
        self.segments()
            .map(|s| s.name().map(|t| t.text().to_string()).unwrap_or_default())
            .collect::<Vec<_>>()
            .join("::")
    }

    pub fn is_path(&self) -> bool {
        self.segments().count() > 1
    }

    pub fn last_separator(&self) -> Option<SyntaxToken> {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .filter(|t| t.syntax_kind() == TokenKind::COLON_COLON)
            .last()
    }
}

impl AstQualifiedPathExpr {
    /// The type being qualified (e.g., `T` in `[T as Trait]::Item`)
    pub fn ty(&self) -> AstType {
        self.syntax_node()
            .children()
            .find_map(|n| AstType::cast(n))
            .unwrap()
    }

    /// The trait type (e.g., `Trait` in `[T as Trait]::Item`)
    pub fn trait_ty(&self) -> AstType {
        self.syntax_node()
            .children()
            .filter_map(|n| AstType::cast(n))
            .nth(1)
            .unwrap()
    }

    /// The associated type name (e.g., `Item` in `[T as Trait]::Item`)
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| t.syntax_kind() == TokenKind::IDENTIFIER)
    }

    /// Additional path segments after the associated type (e.g., `::method` in `[T as Trait]::Item::method`)
    pub fn segments(&self) -> impl Iterator<Item = AstPathSegment> {
        self.syntax_node()
            .children()
            .filter_map(|n| AstPathSegment::cast(n))
    }
}

impl AstPathSegment {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| {
                matches!(
                    t.syntax_kind(),
                    TokenKind::IDENTIFIER
                        | TokenKind::SELF_KW
                        | TokenKind::UPCASE_SELF_KW
                        | TokenKind::PACKAGE_KW
                        | TokenKind::SUPER_KW
                )
            })
    }

    pub fn type_params(&self) -> impl Iterator<Item = AstTypeArgument> {
        self.syntax_node()
            .children()
            .filter_map(|n| AstTypeArgument::cast(n))
    }

    pub fn has_type_params(&self) -> bool {
        self.type_params().next().is_some()
    }

    pub fn type_params_span(&self) -> Option<Span> {
        let open = self
            .syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| t.syntax_kind() == TokenKind::L_BRACKET)?;
        let close = self
            .syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| t.syntax_kind() == TokenKind::R_BRACKET)?;
        Some(open.span().merge(close.span()))
    }
}

impl AstIdentPattern {
    pub fn mutable(&self) -> bool {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|n| n.syntax_kind() == TokenKind::MUT_KW)
            .is_some()
    }

    pub fn name(&self) -> SyntaxToken {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| t.syntax_kind() == TokenKind::IDENTIFIER)
            .unwrap()
    }
}

impl AstIfExpr {
    pub fn cond(&self) -> AstExpr {
        self.syntax_node()
            .children()
            .find_map(|n| AstExpr::cast(n))
            .unwrap()
    }

    pub fn then_block(&self) -> AstExpr {
        self.syntax_node()
            .children()
            .filter_map(|n| AstExpr::cast(n))
            .nth(1)
            .unwrap()
    }

    pub fn else_block(&self) -> Option<AstExpr> {
        self.syntax_node()
            .children()
            .filter_map(|n| AstExpr::cast(n))
            .nth(2)
    }
}

impl AstImpl {
    pub fn declaration_span(&self) -> Span {
        compute_declaration_span(self.syntax_node())
    }

    pub fn modifier_list(&self) -> Option<AstModifierList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstModifierList::cast(n))
    }

    pub fn trait_type(&self) -> Option<AstType> {
        let (first, second) = self.types();
        if second.is_some() { first } else { None }
    }

    pub fn extended_type(&self) -> Option<AstType> {
        let (first, second) = self.types();
        if second.is_some() { second } else { first }
    }

    fn types(&self) -> (Option<AstType>, Option<AstType>) {
        let mut types = self.syntax_node().children().filter_map(AstType::cast);
        let first = types.next();
        let second = types.next();

        (first, second)
    }

    pub fn element_list(&self) -> Option<AstElementList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstElementList::cast(n))
    }

    pub fn type_param_list(&self) -> Option<AstTypeParamList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstTypeParamList::cast(n))
    }

    pub fn where_clause(&self) -> Option<AstWhereClause> {
        self.syntax_node()
            .children()
            .find_map(|n| AstWhereClause::cast(n))
    }
}

impl AstIsExpr {
    pub fn value(&self) -> AstExpr {
        self.syntax_node()
            .children()
            .find_map(|n| AstExpr::cast(n))
            .unwrap()
    }

    pub fn pattern(&self) -> AstPattern {
        self.syntax_node()
            .children()
            .find_map(|n| AstPattern::cast(n))
            .unwrap()
    }
}

impl AstLambdaType {
    pub fn params(&self) -> impl Iterator<Item = AstType> {
        let mut types: Vec<AstType> = self
            .syntax_node()
            .children()
            .filter_map(|n| AstType::cast(n))
            .collect();
        types.pop();
        types.into_iter()
    }

    pub fn params_len(&self) -> usize {
        self.params().count()
    }

    pub fn params_at(&self, index: usize) -> AstType {
        self.params().nth(index).unwrap()
    }

    pub fn ret(&self) -> Option<AstType> {
        self.syntax_node()
            .children()
            .filter_map(|n| AstType::cast(n))
            .last()
    }
}

#[derive(Clone, AstUnion)]
pub enum AstCallable {
    Function(AstFunction),
    #[ast_union_kind(LAMBDA_EXPR)]
    LambdaExpr(AstLambdaExpr),
}

impl AstCallable {
    pub fn declaration_span(&self) -> Span {
        match self {
            AstCallable::Function(node) => node.declaration_span(),
            AstCallable::LambdaExpr(node) => node.declaration_span(),
        }
    }

    pub fn params(&self) -> Box<dyn Iterator<Item = AstParam> + '_> {
        match self {
            AstCallable::Function(node) => Box::new(node.params()),
            AstCallable::LambdaExpr(node) => Box::new(node.params()),
        }
    }

    pub fn params_len(&self) -> usize {
        match self {
            AstCallable::Function(node) => node.params_len(),
            AstCallable::LambdaExpr(node) => node.params_len(),
        }
    }

    pub fn return_type(&self) -> Option<AstType> {
        match self {
            AstCallable::Function(node) => node.return_type(),
            AstCallable::LambdaExpr(node) => node.return_type(),
        }
    }

    pub fn block(&self) -> Option<AstBlockExpr> {
        match self {
            AstCallable::Function(node) => node.block(),
            AstCallable::LambdaExpr(node) => node.block(),
        }
    }
}

impl AstLambdaExpr {
    pub fn declaration_span(&self) -> Span {
        self.span()
    }

    pub fn params(&self) -> impl Iterator<Item = AstParam> {
        self.syntax_node()
            .children()
            .filter_map(|n| AstParam::cast(n))
    }

    pub fn params_len(&self) -> usize {
        self.params().count()
    }

    pub fn params_at(&self, index: usize) -> AstParam {
        self.params().nth(index).unwrap()
    }

    pub fn return_type(&self) -> Option<AstType> {
        self.syntax_node().children().find_map(|n| AstType::cast(n))
    }

    pub fn block(&self) -> Option<AstBlockExpr> {
        self.syntax_node()
            .children()
            .find_map(|n| AstBlockExpr::cast(n))
    }
}

impl AstLet {
    pub fn pattern(&self) -> AstPattern {
        self.syntax_node()
            .children()
            .find_map(|n| AstPattern::cast(n))
            .unwrap()
    }

    pub fn expr(&self) -> Option<AstExpr> {
        self.syntax_node().children().find_map(|n| AstExpr::cast(n))
    }

    pub fn data_type(&self) -> Option<AstType> {
        self.syntax_node().children().find_map(|n| AstType::cast(n))
    }
}

impl AstLitBoolExpr {
    pub fn value(&self) -> bool {
        let t = self.token();

        match t.syntax_kind() {
            TokenKind::TRUE => true,
            TokenKind::FALSE => false,
            _ => unreachable!(),
        }
    }

    pub fn token(&self) -> SyntaxToken {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| !t.syntax_kind().is_trivia())
            .unwrap()
    }
}

impl AstLitCharExpr {
    pub fn token_as_string(&self) -> String {
        self.token().text().to_string()
    }

    pub fn token(&self) -> SyntaxToken {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| !t.syntax_kind().is_trivia())
            .unwrap()
    }
}

impl AstLitFloatExpr {
    pub fn token_as_string(&self) -> String {
        self.token().text().to_string()
    }

    pub fn token(&self) -> SyntaxToken {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| !t.syntax_kind().is_trivia())
            .unwrap()
    }
}

impl AstLitIntExpr {
    pub fn token_as_string(&self) -> String {
        self.token().text().to_string()
    }

    pub fn token(&self) -> SyntaxToken {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| !t.syntax_kind().is_trivia())
            .unwrap()
    }
}

impl AstLitPatternBool {
    pub fn expr(&self) -> AstExpr {
        self.syntax_node()
            .children()
            .find_map(|n| AstExpr::cast(n))
            .unwrap()
    }
}

impl AstLitPatternChar {
    pub fn expr(&self) -> AstExpr {
        self.syntax_node()
            .children()
            .find_map(|n| AstExpr::cast(n))
            .unwrap()
    }
}

impl AstLitPatternFloat {
    pub fn expr(&self) -> AstExpr {
        self.syntax_node()
            .children()
            .find_map(|n| AstExpr::cast(n))
            .unwrap()
    }
}

impl AstLitPatternInt {
    pub fn expr(&self) -> AstExpr {
        self.syntax_node()
            .children()
            .find_map(|n| AstExpr::cast(n))
            .unwrap()
    }
}

impl AstLitPatternStr {
    pub fn expr(&self) -> AstExpr {
        self.syntax_node()
            .children()
            .find_map(|n| AstExpr::cast(n))
            .unwrap()
    }
}

impl AstLitStrExpr {
    pub fn token_as_string(&self) -> String {
        self.token().text().to_string()
    }

    pub fn token(&self) -> SyntaxToken {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| !t.syntax_kind().is_trivia())
            .unwrap()
    }
}

impl AstMatchExpr {
    pub fn expr(&self) -> Option<AstExpr> {
        self.syntax_node().children().find_map(|n| AstExpr::cast(n))
    }

    pub fn arms(&self) -> impl Iterator<Item = AstMatchArm> {
        self.syntax_node()
            .children()
            .filter_map(|n| AstMatchArm::cast(n))
    }
}

impl AstMatchArm {
    pub fn pattern(&self) -> AstPattern {
        self.syntax_node()
            .children()
            .find_map(|n| AstPattern::cast(n))
            .unwrap()
    }

    pub fn cond(&self) -> Option<AstExpr> {
        let mut exprs = self
            .syntax_node()
            .children()
            .filter_map(|n| AstExpr::cast(n));
        let first = exprs.next()?;
        if exprs.next().is_some() {
            Some(first)
        } else {
            None
        }
    }

    pub fn value(&self) -> AstExpr {
        let mut exprs = self
            .syntax_node()
            .children()
            .filter_map(|n| AstExpr::cast(n));
        let first = exprs.next().unwrap();
        exprs.next().unwrap_or(first)
    }
}

impl AstMethodCallExpr {
    pub fn object(&self) -> AstExpr {
        self.syntax_node()
            .children()
            .find_map(|n| AstExpr::cast(n))
            .unwrap()
    }

    pub fn name(&self) -> SyntaxToken {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| t.syntax_kind() == TokenKind::IDENTIFIER)
            .unwrap()
    }

    pub fn field_span(&self) -> Span {
        self.object().span().merge(self.name().span())
    }

    pub fn type_argument_list(&self) -> Option<AstTypeArgumentList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstTypeArgumentList::cast(n))
    }

    pub fn arg_list(&self) -> AstArgumentList {
        self.syntax_node()
            .children()
            .find_map(|n| AstArgumentList::cast(n))
            .expect("missing argument list")
    }
}

impl AstModifier {
    pub fn first_token(&self) -> Option<SyntaxToken> {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| !t.syntax_kind().is_trivia())
    }

    pub fn ident(&self) -> Option<SyntaxToken> {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| t.syntax_kind() == TokenKind::IDENTIFIER)
    }
}

impl AstModifierList {
    pub fn items(&self) -> impl Iterator<Item = AstModifier> {
        self.syntax_node()
            .children()
            .filter_map(|n| AstModifier::cast(n))
    }
}

impl AstModule {
    pub fn modifier_list(&self) -> Option<AstModifierList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstModifierList::cast(n))
    }

    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| t.syntax_kind() == TokenKind::IDENTIFIER)
    }

    pub fn element_list(&self) -> Option<AstElementList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstElementList::cast(n))
    }
}

impl AstParam {
    pub fn pattern(&self) -> Option<AstPattern> {
        self.syntax_node()
            .children()
            .find_map(|n| AstPattern::cast(n))
    }

    pub fn data_type(&self) -> Option<AstType> {
        self.syntax_node().children().find_map(|n| AstType::cast(n))
    }

    pub fn variadic(&self) -> bool {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|n| n.to_token())
            .find(|t| t.syntax_kind() == TokenKind::DOT_DOT_DOT)
            .is_some()
    }
}

impl AstParenExpr {
    pub fn expr(&self) -> Option<AstExpr> {
        self.syntax_node().children().find_map(|n| AstExpr::cast(n))
    }
}

#[derive(Clone)]
pub enum TypePathSegment {
    UpcaseThis(SyntaxToken),
    Name(SyntaxToken),
    Error(SyntaxNode),
}

impl TypePathSegment {
    pub fn span(&self) -> Span {
        match self {
            TypePathSegment::UpcaseThis(token) => token.span(),
            TypePathSegment::Name(token) => token.span(),
            TypePathSegment::Error(node) => node.span(),
        }
    }

    pub fn is_upcase_this(&self) -> bool {
        matches!(self, TypePathSegment::UpcaseThis(_))
    }
}

impl AstPathData {
    pub fn segments(&self) -> impl Iterator<Item = TypePathSegment> {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| match e {
                SyntaxElement::Node(node) => {
                    if node.syntax_kind() == TokenKind::ERROR_PATH_SEGMENT {
                        Some(TypePathSegment::Error(node))
                    } else {
                        None
                    }
                }
                SyntaxElement::Token(token) => {
                    if token.syntax_kind() == TokenKind::IDENTIFIER {
                        Some(TypePathSegment::Name(token))
                    } else if token.syntax_kind() == TokenKind::UPCASE_SELF_KW {
                        Some(TypePathSegment::UpcaseThis(token))
                    } else {
                        None
                    }
                }
            })
    }
}

impl AstQualifiedPathType {
    pub fn ty(&self) -> AstType {
        self.syntax_node()
            .children()
            .find_map(|n| AstType::cast(n))
            .unwrap()
    }

    pub fn trait_ty(&self) -> AstType {
        self.syntax_node()
            .children()
            .filter_map(|n| AstType::cast(n))
            .nth(1)
            .unwrap()
    }

    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| t.syntax_kind() == TokenKind::IDENTIFIER)
    }
}

impl AstRefType {
    pub fn ty(&self) -> AstType {
        self.syntax_node()
            .children()
            .find_map(|n| AstType::cast(n))
            .unwrap()
    }
}

impl AstPathType {
    pub fn path(&self) -> AstPathData {
        self.syntax_node()
            .children()
            .find_map(|n| AstPathData::cast(n))
            .unwrap()
    }

    pub fn params(&self) -> impl Iterator<Item = AstTypeArgument> {
        self.syntax_node()
            .children()
            .filter_map(|n| AstTypeArgument::cast(n))
    }

    pub fn params_len(&self) -> usize {
        self.params().count()
    }

    pub fn params_at(&self, index: usize) -> AstTypeArgument {
        self.params().nth(index).unwrap()
    }
}

impl AstReturnExpr {
    pub fn expr(&self) -> Option<AstExpr> {
        self.syntax_node().children().find_map(|n| AstExpr::cast(n))
    }
}

impl AstStruct {
    pub fn modifier_list(&self) -> Option<AstModifierList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstModifierList::cast(n))
    }

    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| t.syntax_kind() == TokenKind::IDENTIFIER)
    }

    pub fn type_param_list(&self) -> Option<AstTypeParamList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstTypeParamList::cast(n))
    }

    pub fn where_clause(&self) -> Option<AstWhereClause> {
        self.syntax_node()
            .children()
            .find_map(|n| AstWhereClause::cast(n))
    }

    pub fn field_name_style(&self) -> FieldNameStyle {
        let found_brace = self
            .syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| t.syntax_kind() == TokenKind::L_BRACE)
            .is_some();

        if found_brace {
            FieldNameStyle::Named
        } else {
            FieldNameStyle::Positional
        }
    }

    pub fn fields(&self) -> impl Iterator<Item = AstFieldDecl> {
        self.syntax_node()
            .children()
            .filter_map(|n| AstFieldDecl::cast(n))
    }
}

impl AstTemplateExpr {
    pub fn parts(&self) -> impl Iterator<Item = AstExpr> {
        self.syntax_node()
            .children()
            .filter_map(|n| AstExpr::cast(n))
    }
}

impl AstTrait {
    pub fn modifier_list(&self) -> Option<AstModifierList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstModifierList::cast(n))
    }

    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| t.syntax_kind() == TokenKind::IDENTIFIER)
    }

    pub fn bounds(&self) -> Option<AstTypeBounds> {
        self.syntax_node()
            .children()
            .find_map(|n| AstTypeBounds::cast(n))
    }

    pub fn type_param_list(&self) -> Option<AstTypeParamList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstTypeParamList::cast(n))
    }

    pub fn where_clause(&self) -> Option<AstWhereClause> {
        self.syntax_node()
            .children()
            .find_map(|n| AstWhereClause::cast(n))
    }

    pub fn element_list(&self) -> Option<AstElementList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstElementList::cast(n))
    }
}

impl AstTupleExpr {
    pub fn values(&self) -> impl Iterator<Item = AstExpr> {
        self.syntax_node()
            .children()
            .filter_map(|n| AstExpr::cast(n))
    }
}

impl AstTuplePattern {
    pub fn params(&self) -> impl DoubleEndedIterator<Item = AstPattern> {
        self.syntax_node()
            .children()
            .filter_map(|n| AstPattern::cast(n))
            .collect::<Vec<_>>()
            .into_iter()
    }
}

impl AstTupleType {
    pub fn subtypes(&self) -> impl Iterator<Item = AstType> {
        self.syntax_node()
            .children()
            .filter_map(|n| AstType::cast(n))
    }
}

#[derive(Clone, AstUnion)]
pub enum AstPattern {
    Alt(AstAlt),
    CtorPattern(AstCtorPattern),
    #[ast_union_kind(ERROR_PATTERN)]
    Error(SyntaxNode),
    IdentPattern(AstIdentPattern),
    LitPatternBool(AstLitPatternBool),
    LitPatternChar(AstLitPatternChar),
    LitPatternInt(AstLitPatternInt),
    LitPatternFloat(AstLitPatternFloat),
    LitPatternStr(AstLitPatternStr),
    Rest(AstRest),
    TuplePattern(AstTuplePattern),
    UnderscorePattern(AstUnderscorePattern),
}

#[derive(Clone, AstUnion)]
pub enum AstStmt {
    #[ast_union_kind(ERROR_STMT)]
    Error(SyntaxNode),
    ExprStmt(AstExprStmt),
    Let(AstLet),
}

#[derive(Clone, AstUnion)]
pub enum AstType {
    #[ast_union_kind(ERROR_ELEM)]
    Error(SyntaxNode),
    LambdaType(AstLambdaType),
    QualifiedPathType(AstQualifiedPathType),
    RefType(AstRefType),
    PathType(AstPathType),
    TupleType(AstTupleType),
}

impl AstType {
    pub fn is_unit_type(&self) -> bool {
        match self {
            AstType::TupleType(value) => value.subtypes().next().is_none(),
            _ => false,
        }
    }
}

impl AstTypeArgumentList {
    pub fn items(&self) -> impl Iterator<Item = AstTypeArgument> {
        self.syntax_node()
            .children()
            .filter_map(|n| AstTypeArgument::cast(n))
    }

    pub fn items_len(&self) -> usize {
        self.items().count()
    }

    pub fn items_at(&self, index: usize) -> AstTypeArgument {
        self.items().nth(index).unwrap()
    }
}

impl AstTypeArgument {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| t.syntax_kind() == TokenKind::IDENTIFIER)
    }

    pub fn ty(&self) -> Option<AstType> {
        self.syntax_node().children().find_map(|n| AstType::cast(n))
    }
}

impl AstTypeBounds {
    pub fn items(&self) -> impl Iterator<Item = AstType> {
        self.syntax_node()
            .children()
            .filter_map(|n| AstType::cast(n))
    }

    pub fn items_len(&self) -> usize {
        self.items().count()
    }

    pub fn items_at(&self, index: usize) -> AstType {
        self.items().nth(index).unwrap()
    }
}

impl AstTypeParam {
    pub fn name(&self) -> Option<SyntaxToken> {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| t.syntax_kind() == TokenKind::IDENTIFIER)
    }

    pub fn bounds(&self) -> Option<AstTypeBounds> {
        self.syntax_node()
            .children()
            .find_map(|n| AstTypeBounds::cast(n))
    }
}

impl AstTypeParamList {
    pub fn items(&self) -> impl Iterator<Item = AstTypeParam> {
        self.syntax_node()
            .children()
            .filter_map(|n| AstTypeParam::cast(n))
    }

    pub fn items_len(&self) -> usize {
        self.items().count()
    }

    pub fn items_at(&self, index: usize) -> AstTypeParam {
        self.items().nth(index).unwrap()
    }
}

impl AstUnExpr {
    pub fn opnd(&self) -> AstExpr {
        self.syntax_node()
            .children()
            .find_map(|n| AstExpr::cast(n))
            .unwrap()
    }

    pub fn op(&self) -> UnOp {
        let tok = self
            .children_with_tokens()
            .filter_map(|elem| elem.to_token())
            .find(|tok| !tok.syntax_kind().is_trivia())
            .expect("unary operator missing token");

        match tok.syntax_kind() {
            TokenKind::SUB => UnOp::Neg,
            TokenKind::NOT => UnOp::Not,
            _ => unreachable!("unexpected token for unary op"),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum UnOp {
    Neg,
    Not,
}

impl UnOp {
    pub fn as_str(&self) -> &'static str {
        match *self {
            UnOp::Neg => "-",
            UnOp::Not => "!",
        }
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum CmpOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Is,
    IsNot,
}

impl CmpOp {
    pub fn as_str(&self) -> &'static str {
        match *self {
            CmpOp::Eq => "==",
            CmpOp::Ne => "!=",
            CmpOp::Lt => "<",
            CmpOp::Le => "<=",
            CmpOp::Gt => ">",
            CmpOp::Ge => ">=",
            CmpOp::Is => "===",
            CmpOp::IsNot => "!==",
        }
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Cmp(CmpOp),
    Or,
    And,
    BitOr,
    BitAnd,
    BitXor,
    ShiftL,
    ArithShiftR,
    LogicalShiftR,
}

impl BinOp {
    pub fn as_str(&self) -> &'static str {
        match *self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Mod => "%",
            BinOp::Cmp(op) => op.as_str(),
            BinOp::Or => "||",
            BinOp::And => "&&",
            BinOp::BitOr => "|",
            BinOp::BitAnd => "&",
            BinOp::BitXor => "^",
            BinOp::ShiftL => "<<",
            BinOp::ArithShiftR => ">>",
            BinOp::LogicalShiftR => ">>>",
        }
    }

    pub fn is_compare(&self) -> bool {
        match *self {
            BinOp::Cmp(cmp) if cmp != CmpOp::Is && cmp != CmpOp::IsNot => true,
            _ => false,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum AssignOp {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    BitOrAssign,
    BitAndAssign,
    BitXorAssign,
    ShiftLAssign,
    ArithShiftRAssign,
    LogicalShiftRAssign,
}

impl AssignOp {
    pub fn as_str(&self) -> &'static str {
        match *self {
            AssignOp::Assign => "=",
            AssignOp::AddAssign => "+=",
            AssignOp::SubAssign => "-=",
            AssignOp::MulAssign => "*=",
            AssignOp::DivAssign => "/=",
            AssignOp::ModAssign => "%=",
            AssignOp::BitOrAssign => "|=",
            AssignOp::BitAndAssign => "&=",
            AssignOp::BitXorAssign => "^=",
            AssignOp::ShiftLAssign => "<<=",
            AssignOp::ArithShiftRAssign => ">>=",
            AssignOp::LogicalShiftRAssign => ">>>=",
        }
    }

    pub fn to_bin_op(&self) -> Option<BinOp> {
        match *self {
            AssignOp::Assign => None,
            AssignOp::AddAssign => Some(BinOp::Add),
            AssignOp::SubAssign => Some(BinOp::Sub),
            AssignOp::MulAssign => Some(BinOp::Mul),
            AssignOp::DivAssign => Some(BinOp::Div),
            AssignOp::ModAssign => Some(BinOp::Mod),
            AssignOp::BitOrAssign => Some(BinOp::BitOr),
            AssignOp::BitAndAssign => Some(BinOp::BitAnd),
            AssignOp::BitXorAssign => Some(BinOp::BitXor),
            AssignOp::ShiftLAssign => Some(BinOp::ShiftL),
            AssignOp::ArithShiftRAssign => Some(BinOp::ArithShiftR),
            AssignOp::LogicalShiftRAssign => Some(BinOp::LogicalShiftR),
        }
    }
}

impl AstUse {
    pub fn modifier_list(&self) -> Option<AstModifierList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstModifierList::cast(n))
    }

    pub fn initial_atom(&self) -> Option<AstUsePathSegment> {
        self.syntax_node()
            .children()
            .find_map(|n| AstUsePathSegment::cast(n))
    }

    pub fn path(&self) -> AstUseTree {
        self.syntax_node()
            .children()
            .find_map(|n| AstUseTree::cast(n))
            .unwrap()
    }
}

impl AstUseAs {
    pub fn original_name(&self) -> SyntaxToken {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| t.syntax_kind() == TokenKind::IDENTIFIER)
            .unwrap()
    }

    pub fn target_name(&self) -> Option<SyntaxToken> {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .filter(|t| t.syntax_kind() == TokenKind::IDENTIFIER)
            .nth(1)
    }
}

impl AstUsePathSegment {
    pub fn kind(&self) -> TokenKind {
        self.children_with_tokens()
            .filter(|t| !t.is_trivia())
            .next()
            .expect("missing child")
            .syntax_kind()
    }

    pub fn to_name(&self) -> Option<SyntaxToken> {
        self.children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| t.syntax_kind() == TokenKind::IDENTIFIER)
    }
}

impl AstUseGroup {
    pub fn targets(&self) -> impl Iterator<Item = AstUseTree> {
        self.syntax_node()
            .children()
            .filter_map(|n| AstUseTree::cast(n))
    }
}

impl AstUseName {
    pub fn name(&self) -> SyntaxToken {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .find(|t| t.syntax_kind() == TokenKind::IDENTIFIER)
            .unwrap()
    }
}

impl AstUseTree {
    pub fn path(&self) -> impl Iterator<Item = SyntaxToken> {
        self.syntax_node()
            .children_with_tokens()
            .filter_map(|e| e.to_token())
            .filter(|t| t.syntax_kind() == TokenKind::IDENTIFIER)
    }

    pub fn target(&self) -> Option<AstUseTarget> {
        self.syntax_node()
            .children()
            .find_map(|n| AstUseTarget::cast(n))
    }
}

#[derive(Clone, AstUnion)]
pub enum AstUseTarget {
    UseName(AstUseName),
    UseAs(AstUseAs),
    UseGroup(AstUseGroup),
}

impl AstWhereClause {
    pub fn clauses(&self) -> impl Iterator<Item = AstWhereClauseItem> {
        self.syntax_node()
            .children()
            .filter_map(|n| AstWhereClauseItem::cast(n))
    }

    pub fn clauses_len(&self) -> usize {
        self.clauses().count()
    }

    pub fn clauses_at(&self, index: usize) -> AstWhereClauseItem {
        self.clauses().nth(index).unwrap()
    }
}

impl AstWhereClauseItem {
    pub fn ty(&self) -> Option<AstType> {
        self.syntax_node().children().find_map(|n| AstType::cast(n))
    }

    pub fn bounds(&self) -> impl Iterator<Item = AstType> {
        let mut types: Vec<_> = self
            .syntax_node()
            .children()
            .filter_map(|n| AstType::cast(n))
            .collect();
        if !types.is_empty() {
            types.remove(0);
        }
        types.into_iter()
    }

    pub fn bounds_len(&self) -> usize {
        self.bounds().count()
    }

    pub fn bounds_at(&self, index: usize) -> AstType {
        self.bounds().nth(index).unwrap()
    }
}

impl AstWhileExpr {
    pub fn cond(&self) -> AstExpr {
        self.syntax_node()
            .children()
            .find_map(|n| AstExpr::cast(n))
            .unwrap()
    }

    pub fn block(&self) -> AstBlockExpr {
        self.syntax_node()
            .children()
            .find_map(|n| AstBlockExpr::cast(n))
            .unwrap()
    }
}

fn find_innermost_node_at_offset(node: SyntaxNode, offset: u32) -> Option<SyntaxNode> {
    let span = node.span();
    if offset < span.start() || offset >= span.end() {
        return None;
    }
    for child in node.children() {
        if let Some(innermost) = find_innermost_node_at_offset(child, offset) {
            return Some(innermost);
        }
    }
    Some(node)
}

fn compute_declaration_span(node: &SyntaxNode) -> Span {
    let mut start = None;
    let mut end = None;

    for elem in node.children_with_tokens() {
        match elem {
            SyntaxElement::Node(node) if node.is_block_expr() => break,
            SyntaxElement::Node(node) => {
                let span = node.span();
                start.get_or_insert(span.start());
                end = Some(span.end());
            }
            SyntaxElement::Token(tok) => {
                if tok.syntax_kind().is_trivia() {
                    continue;
                }

                let span = tok.span();
                start.get_or_insert(span.start());
                end = Some(span.end());
            }
        }
    }

    let span = node.span();
    Span::new(start.unwrap_or(span.start()), end.unwrap_or(span.end()))
}

fn find_node_by_ptr(node: SyntaxNode, needle: SyntaxNodePtr) -> Option<SyntaxNode> {
    let needle_span = needle.span();
    let needle_start = needle_span.start();
    let needle_end = needle_span.end();
    let mut current = node;
    let mut offset = 0;

    'outer_loop: loop {
        if current.as_ptr() == needle {
            return Some(current);
        }

        for green_element in current.green().children() {
            if offset > needle_end {
                return None;
            }

            match green_element {
                GreenElement::Node(node) => {
                    let child_len = node.text_length();
                    let child_end = offset + child_len;

                    if child_end <= needle_start {
                        offset += child_len;
                        continue;
                    }

                    debug_assert!(needle_span.is_within(Span::new(offset, child_len)));
                    current = SyntaxNode::new(node.clone(), TextOffset(offset), Some(current));
                    continue 'outer_loop;
                }

                GreenElement::Token(token) => {
                    offset += token.text.len() as u32;
                }
            }
        }

        return None;
    }
}

fn find_token_at_offset(node: SyntaxNode, needle: u32) -> Option<SyntaxToken> {
    let tokens = find_tokens_at_offset(node, needle);

    match tokens.len() {
        0 => None,
        1 => tokens.into_iter().next(),
        _ => {
            let mut iter = tokens.into_iter();
            let left = iter.next().unwrap();
            let right = iter.next().unwrap();
            Some(pick_best_token(left, right))
        }
    }
}

fn pick_best_token(left: SyntaxToken, right: SyntaxToken) -> SyntaxToken {
    let left_is_ident = left.syntax_kind() == TokenKind::IDENTIFIER;
    let right_is_ident = right.syntax_kind() == TokenKind::IDENTIFIER;

    match (left_is_ident, right_is_ident) {
        (true, false) => left,
        (false, true) => right,
        _ => {
            // Neither or both are identifiers, pick first non-trivia
            if !left.is_trivia() { left } else { right }
        }
    }
}

fn find_tokens_at_offset(node: SyntaxNode, needle: u32) -> Vec<SyntaxToken> {
    // Worklist contains (slice of children, index, parent node)
    let mut worklist: Vec<(&[GreenElement], usize, SyntaxNode)> = Vec::with_capacity(4);
    worklist.push((node.green().children(), 0, node.clone()));

    let mut offset = 0;
    let mut tokens = Vec::new();

    'outer_loop: while let Some((children, mut index, parent)) = worklist.pop() {
        while index < children.len() {
            let element = &children[index];
            index += 1;

            match element {
                GreenElement::Node(child_green) => {
                    let child_len = child_green.text_length();
                    let child_end = offset + child_len;

                    if child_end < needle {
                        offset += child_len;
                    } else {
                        // Descend into this node
                        worklist.push((children, index, parent.clone()));
                        let child_node =
                            SyntaxNode::new(child_green.clone(), TextOffset(offset), Some(parent));
                        worklist.push((child_green.children(), 0, child_node));
                        continue 'outer_loop;
                    }
                }

                GreenElement::Token(green_token) => {
                    let token_len = green_token.text.len() as u32;
                    let token_end = offset + token_len;

                    // We haven't reached the needle yet.
                    if needle > token_end {
                        offset += token_len;
                        continue;
                    }

                    // We are past the needle and can stop.
                    if needle < offset {
                        break 'outer_loop;
                    }

                    debug_assert!(offset <= needle && needle <= token_end);

                    let token = SyntaxToken::new(
                        green_token.clone(),
                        TextOffset(offset),
                        Some(parent.clone()),
                    );

                    tokens.push(token);
                    offset += token_len;
                }
            }
        }
    }

    tokens
}

#[cfg(test)]
mod tests {
    use crate::Parser;
    use crate::ast::{SyntaxNode, SyntaxNodeBase};

    #[test]
    fn test_node_at_offset() {
        //             0         1         2
        //             012345678901234567890123
        let content = "fn main() { let x = 1; }";
        let parser = Parser::from_string(content);
        let (file, errors) = parser.parse();
        assert!(errors.is_empty());

        let node = file.syntax_at_offset(15);
        assert!(node.unwrap().is_let());

        let node = file.syntax_at_offset(0);
        assert!(node.unwrap().is_function());
    }

    #[test]
    fn test_token_at_offset() {
        use crate::TokenKind;

        //             0         1         2
        //             012345678901234567890123
        let content = "fn main() { let x = 1; }";
        let parser = Parser::from_string(content);
        let (file, errors) = parser.parse();
        assert!(errors.is_empty());

        // "fn" keyword at offset 0
        let token = file.token_at_offset(0).unwrap();
        assert_eq!(token.syntax_kind(), TokenKind::FN_KW);
        assert_eq!(token.text(), "fn");
        assert_eq!(token.offset().value(), 0);

        // "main" identifier at offset 3
        let token = file.token_at_offset(3).unwrap();
        assert_eq!(token.syntax_kind(), TokenKind::IDENTIFIER);
        assert_eq!(token.text(), "main");

        // "let" keyword at offset 12
        let token = file.token_at_offset(12).unwrap();
        assert_eq!(token.syntax_kind(), TokenKind::LET_KW);
        assert_eq!(token.text(), "let");

        // "x" identifier at offset 16
        let token = file.token_at_offset(16).unwrap();
        assert_eq!(token.syntax_kind(), TokenKind::IDENTIFIER);
        assert_eq!(token.text(), "x");

        // "1" literal at offset 20
        let token = file.token_at_offset(20).unwrap();
        assert_eq!(token.syntax_kind(), TokenKind::INT_LITERAL);
        assert_eq!(token.text(), "1");

        // Token at offset within "main" (offset 5)
        let token = file.token_at_offset(5).unwrap();
        assert_eq!(token.text(), "main");
    }

    #[test]
    fn test_token_at_offset_boundary() {
        use crate::TokenKind;

        //             0         1         2
        //             012345678901234567890123
        let content = "fn main() { let x = 1; }";
        let parser = Parser::from_string(content);
        let (file, errors) = parser.parse();
        assert!(errors.is_empty());

        // Offset 2 is between "fn" (0-2) and " " (2-3)
        // Should pick "fn" (first non-trivia)
        let token = file.token_at_offset(2).unwrap();
        assert_eq!(token.syntax_kind(), TokenKind::FN_KW);
        assert_eq!(token.text(), "fn");

        // Offset 3 is between " " (2-3) and "main" (3-7)
        // Should pick "main" (identifier wins over trivia)
        let token = file.token_at_offset(3).unwrap();
        assert_eq!(token.syntax_kind(), TokenKind::IDENTIFIER);
        assert_eq!(token.text(), "main");

        // Offset 7 is between "main" (3-7) and "(" (7-8)
        // Should pick "main" (identifier wins)
        let token = file.token_at_offset(7).unwrap();
        assert_eq!(token.syntax_kind(), TokenKind::IDENTIFIER);
        assert_eq!(token.text(), "main");

        // Offset 8 is between "(" (7-8) and ")" (8-9)
        // Neither is identifier or trivia, should pick first: "("
        let token = file.token_at_offset(8).unwrap();
        assert_eq!(token.syntax_kind(), TokenKind::L_PAREN);
        assert_eq!(token.text(), "(");

        // Offset 16 is between "x" (16-17) and " " (17-18)
        // Should pick "x" (identifier)
        let token = file.token_at_offset(16).unwrap();
        assert_eq!(token.syntax_kind(), TokenKind::IDENTIFIER);
        assert_eq!(token.text(), "x");
    }

    #[test]
    fn test_token_at_offset_field_access() {
        use crate::TokenKind;

        //             01234567890123456
        let content = "fn f() { foo.x; }";
        let parser = Parser::from_string(content);
        let (file, errors) = parser.parse();
        assert!(errors.is_empty());

        // Offset 12 is between "foo" (9-12) and "." (12-13)
        // Should pick "foo" (identifier wins over dot)
        let token = file.token_at_offset(12).unwrap();
        assert_eq!(token.syntax_kind(), TokenKind::IDENTIFIER);
        assert_eq!(token.text(), "foo");

        // Offset 13 is between "." (12-13) and "x" (13-14)
        // Should pick "x" (identifier wins over dot)
        let token = file.token_at_offset(13).unwrap();
        assert_eq!(token.syntax_kind(), TokenKind::IDENTIFIER);
        assert_eq!(token.text(), "x");
    }

    #[test]
    fn test_token_at_offset_parent_chain() {
        use crate::TokenKind;

        //              0         1         2
        //             012345678901234567890123
        let content = "fn main() { let x = 1; }";
        let parser = Parser::from_string(content);
        let (file, errors) = parser.parse();
        assert!(errors.is_empty());

        // Look up "x" identifier at offset 16
        let token = file.token_at_offset(16).unwrap();
        assert_eq!(token.syntax_kind(), TokenKind::IDENTIFIER);
        assert_eq!(token.text(), "x");

        // Traverse parent chain and collect kinds
        let mut kinds = Vec::new();
        let mut current = token.parent();
        while let Some(node) = current {
            kinds.push(node.syntax_kind());
            current = node.parent();
        }

        // Assert parent chain: IdentPattern -> Let -> BlockExpr -> Function -> ElementList
        assert_eq!(
            kinds,
            vec![
                TokenKind::IDENT_PATTERN,
                TokenKind::LET,
                TokenKind::BLOCK_EXPR,
                TokenKind::FUNCTION,
                TokenKind::ELEMENT_LIST,
            ]
        );
    }

    #[test]
    fn test_syntax_node_offset_and_parent() {
        //             0         1         2
        //             012345678901234567890123
        let content = "fn main() { let x = 1; }";
        let parser = Parser::from_string(content);
        let (file, errors) = parser.parse();
        assert!(errors.is_empty());

        // Get the root node
        let root = file.root();

        // Root should have offset 0 and no parent
        assert_eq!(root.offset().value(), 0);
        assert!(root.parent().is_none());

        // Get children of root
        let mut children = root.children();
        let first_child = children.next().unwrap();

        // First child should have a parent (the root)
        assert!(first_child.parent().is_some());
        let parent = first_child.parent().unwrap();
        assert_eq!(parent.as_ptr(), root.as_ptr());

        // First child (function) starts at offset 0: "fn main() { let x = 1; }"
        assert_eq!(first_child.offset().value(), 0);

        // Test with typed node (function) - use the first child we already have
        assert!(first_child.is_function());
        let function = first_child.as_function();

        // Function starts at offset 0
        assert_eq!(function.offset().value(), 0);

        // Get the function's block
        let block = function.block().unwrap();

        // Block should have the function as parent
        assert!(block.parent().is_some());
        let block_parent = block.parent().unwrap();
        assert!(block_parent.is_function());

        // Block starts at offset 9: " { let x = 1; }"
        assert_eq!(block.offset().value(), 9);

        // Get statements from the block to test deeper parent chain
        if block.stmts_without_tail().count() > 0 {
            let stmt = block.stmts_without_tail().next().unwrap();
            // Statement should have block as parent
            assert!(stmt.parent().is_some());
            let stmt_parent = stmt.parent().unwrap();
            assert_eq!(stmt_parent.as_ptr(), block.syntax_node().as_ptr());

            // Statement (let) starts at offset 11: " let x = 1;"
            assert_eq!(stmt.offset().value(), 11);
        }
    }

    #[test]
    fn test_parent_child_relationship() {
        let content = "fn foo() { if true { 1 } else { 2 } }";
        let parser = Parser::from_string(content);
        let (file, errors) = parser.parse();
        assert!(errors.is_empty());

        let root = file.root();

        // Navigate: root -> function -> block -> if
        let function = root.children().next().unwrap();
        assert!(function.is_function());
        assert_eq!(function.offset().value(), 0);

        let function_typed = function.clone().as_function();
        let block = function_typed.block().unwrap();

        // Verify parent chain
        assert!(block.parent().is_some());
        let block_parent = block.parent().unwrap();
        assert_eq!(block_parent.as_ptr(), function.as_ptr());

        assert_eq!(block.offset().value(), 8);

        if let Some(stmt) = block.tail() {
            assert!(stmt.parent().is_some());
            let stmt_parent = stmt.parent().unwrap();
            assert_eq!(stmt_parent.as_ptr(), block.syntax_node().as_ptr());
            let expr_stmt = stmt.as_expr_stmt();
            let expr = expr_stmt.expr();
            assert_eq!(expr.offset().value(), 11);
        }
    }

    #[test]
    fn test_syntax_node_ptr() {
        let content = "fn main() { let x = 1; }";
        let parser = Parser::from_string(content);
        let (file, errors) = parser.parse();
        assert!(errors.is_empty());

        let root = file.root();
        let function_node = root.children().next().unwrap();

        // Test as_ptr() creates a pointer
        let ptr1 = function_node.as_ptr();
        assert_eq!(ptr1.syntax_kind(), function_node.syntax_kind());
        assert_eq!(ptr1.span(), function_node.span());

        // Test that identical nodes produce equal pointers
        let ptr2 = function_node.as_ptr();
        assert_eq!(ptr1, ptr2);

        // Test that different nodes produce different pointers
        let function_typed = function_node.as_function();
        let block_node = function_typed.block().unwrap();
        let block_ptr = block_node.as_ptr();
        assert_ne!(ptr1, block_ptr);
        assert_ne!(ptr1.syntax_kind(), block_ptr.syntax_kind());
        assert_ne!(ptr1.span(), block_ptr.span());
    }

    #[test]
    fn test_node_by_ptr() {
        let content = "
            fn foo() {}
            struct Bar {
                a: Int,
                b: Int
            }
            fn baz() {}
        ";
        let parser = Parser::from_string(content);
        let (file, errors) = parser.parse();
        assert!(errors.is_empty());

        let root = file.root();
        let function_node = root.children().find(|n| n.is_struct()).unwrap();

        let function_ptr = function_node.as_ptr();
        let resolved_node = file.syntax_by_ptr::<SyntaxNode>(function_ptr);
        assert_eq!(resolved_node.span(), function_node.span());
        assert_eq!(resolved_node.syntax_kind(), function_node.syntax_kind());

        let field = function_node
            .children()
            .filter(|n| n.is_field_decl())
            .nth(1)
            .unwrap();
        let field_ptr = field.as_ptr();
        let resolved_field = file.syntax_by_ptr::<SyntaxNode>(field_ptr);
        assert_eq!(resolved_field.span(), field.span());
        assert_eq!(resolved_field.syntax_kind(), field.syntax_kind());
    }
}
