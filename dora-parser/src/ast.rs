use std::sync::{Arc, OnceLock};

use dora_parser_derive::{AstEnum, AstNode, AstUnion, extra_ast_node};
use id_arena::{Arena, Id};

use crate::{Span, TokenKind};

#[derive(Clone, Debug)]
pub struct GreenToken {
    pub kind: TokenKind,
    pub text: String,
}

#[derive(Clone, Debug)]
pub struct GreenNode {
    pub kind: TokenKind,
    pub children: Vec<GreenElement>,
}

#[derive(Clone, Debug)]
pub enum GreenElement {
    Token(GreenToken),
    Node(AstId),
}

impl GreenElement {
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

    pub fn to_node(&self) -> Option<AstId> {
        match self {
            GreenElement::Node(id) => Some(*id),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct File(Arc<FilePayload>);

#[derive(Clone, Debug)]
struct FilePayload {
    content: Arc<String>,
    nodes: Arena<Ast>,
    root_id: AstId,
}

impl File {
    pub(crate) fn new(content: Arc<String>, nodes: Arena<Ast>, root_id: AstId) -> File {
        File(Arc::new(FilePayload {
            content,
            nodes,
            root_id,
        }))
    }

    fn payload(&self) -> &FilePayload {
        self.0.as_ref()
    }

    pub(crate) fn node(&self, id: AstId) -> &Ast {
        &self.payload().nodes[id.0]
    }

    pub fn syntax_by_id<T: SyntaxNodeBase>(&self, id: SyntaxNodeId) -> T {
        // Note: parent is None here as we don't have context about the parent
        let node = SyntaxNode::new(self.clone(), id.id, TextOffset(id.offset), None);
        T::cast(node).expect("wrong type")
    }

    pub fn root(&self) -> SyntaxNode {
        let root_id = self.payload().root_id;
        let root_ast = self.node(root_id);
        let offset = TextOffset(root_ast.full_span().start());
        SyntaxNode::new(self.clone(), root_id, offset, None)
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
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub struct AstId(Id<Ast>);

impl AstId {
    pub(crate) fn new(value: Id<Ast>) -> AstId {
        AstId(value)
    }

    pub(crate) fn value(self) -> Id<Ast> {
        self.0
    }
}

impl std::fmt::Display for AstId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value().index())
    }
}

#[derive(Copy, Clone, Debug)]
pub struct SyntaxNodeId {
    id: AstId,
    offset: u32,
}

// We auto-generate the Ast enum from this NodeKind enum.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, AstEnum)]
#[allow(unused)]
pub(crate) enum NodeKind {
    Plain,
    Alias,
    Alt,
    ArgumentList,
    Bin,
    Block,
    Break,
    Call,
    Class,
    Const,
    Continue,
    Conv,
    CtorField,
    CtorFieldList,
    CtorPattern,
    DotExpr,
    ElementList,
    Enum,
    EnumVariant,
    ExprStmt,
    Extern,
    Field,
    For,
    Function,
    Global,
    IdentPattern,
    If,
    Impl,
    Is,
    Lambda,
    LambdaType,
    Let,
    LitBool,
    LitChar,
    LitFloat,
    LitInt,
    LitPattern,
    LitStr,
    Match,
    MatchArm,
    MethodCallExpr,
    Modifier,
    ModifierList,
    Module,
    Name,
    NameExpr,
    Param,
    Paren,
    Path,
    PathData,
    QualifiedPathType,
    RefType,
    RegularType,
    Rest,
    Return,
    Struct,
    Template,
    This,
    Trait,
    Tuple,
    TuplePattern,
    TupleType,
    TypeArgumentList,
    TypeArgument,
    TypeBounds,
    TypedExpr,
    TypeParam,
    TypeParamList,
    Un,
    UnderscorePattern,
    UpcaseThis,
    Use,
    UseAs,
    UseAtom,
    UseGroup,
    UsePath,
    WhereClause,
    WhereClauseItem,
    While,
}

// The Ast enum is auto-generated from NodeKind by the AstEnum proc macro.
impl Ast {
    pub fn is_blocklike(&self) -> bool {
        match self {
            &Ast::Block(_) => true,
            &Ast::If(_) => true,
            &Ast::Match(_) => true,
            &Ast::For(_) => true,
            &Ast::While(_) => true,
            _ => false,
        }
    }
}

pub trait SyntaxNodeBase: Sized {
    fn id(&self) -> AstId {
        self.syntax_node().id()
    }

    fn cast(node: SyntaxNode) -> Option<Self>;

    fn span(&self) -> Span {
        self.syntax_node().span()
    }

    fn full_span(&self) -> Span {
        self.syntax_node().full_span()
    }

    fn text_length(&self) -> u32 {
        self.syntax_node().text_length()
    }

    fn file(&self) -> &File {
        self.syntax_node().file()
    }

    fn children(&self) -> impl Iterator<Item = SyntaxNode> {
        self.syntax_node().children()
    }

    fn children_with_tokens(&self) -> GreenElementIter<'_> {
        self.syntax_node().children_with_tokens()
    }

    fn syntax_kind(&self) -> TokenKind {
        self.syntax_node().syntax_kind()
    }

    fn as_ptr(&self) -> SyntaxNodePtr {
        self.syntax_node().as_ptr()
    }

    fn as_syntax_node_id(&self) -> SyntaxNodeId {
        self.syntax_node().as_syntax_node_id()
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

    pub fn syntax_kind(&self) -> TokenKind {
        self.syntax_kind
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Clone, Debug)]
struct SyntaxNodeData {
    file: File,
    id: AstId,
    offset: TextOffset,
    parent: Option<SyntaxNode>,
    #[allow(unused)]
    non_trivia_span: OnceLock<Span>,
}

impl SyntaxNodeData {
    fn leading_trivia_length(&self) -> u32 {
        fn trivia_length(file: &File, green_children: &[GreenElement]) -> (u32, bool) {
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
                    GreenElement::Node(node_id) => {
                        let ast = file.node(*node_id);
                        let (child_len, found_non_trivia) =
                            trivia_length(file, ast.green_children());
                        len += child_len;

                        if found_non_trivia {
                            return (len, true);
                        }
                    }
                }
            }

            (len, false)
        }

        let ast = self.file.node(self.id);
        let (len, _) = trivia_length(&self.file, ast.green_children());
        len
    }

    fn trailing_trivia_length(&self) -> u32 {
        fn trivia_length(file: &File, green_children: &[GreenElement]) -> (u32, bool) {
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
                    GreenElement::Node(node_id) => {
                        let ast = file.node(*node_id);
                        let (child_len, found_non_trivia) =
                            trivia_length(file, ast.green_children());
                        len += child_len;

                        if found_non_trivia {
                            return (len, true);
                        }
                    }
                }
            }

            (len, false)
        }

        let ast = self.file.node(self.id);
        let (len, _) = trivia_length(&self.file, ast.green_children());
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

        let ast = self.file.node(self.id);
        let len = ast.text_length().saturating_sub(pre + post);

        Span::new(self.offset.value() + pre, len)
    }
}

#[derive(Clone, Debug)]
pub struct SyntaxNode(Arc<SyntaxNodeData>);

impl SyntaxNode {
    pub fn new(file: File, id: AstId, offset: TextOffset, parent: Option<SyntaxNode>) -> Self {
        SyntaxNode(Arc::new(SyntaxNodeData {
            file,
            id,
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

    pub fn as_syntax_node_id(&self) -> SyntaxNodeId {
        SyntaxNodeId {
            id: self.id(),
            offset: self.offset().value(),
        }
    }

    pub fn as_ptr(&self) -> SyntaxNodePtr {
        SyntaxNodePtr::new(self.syntax_kind(), self.full_span())
    }

    pub fn children_with_tokens(&self) -> GreenElementIter<'_> {
        GreenElementIter::new(
            self.file().clone(),
            self.ast().green_children(),
            self.offset(),
            Some(self.clone()),
        )
    }

    pub fn children(&self) -> impl Iterator<Item = SyntaxNode> + '_ {
        self.children_with_tokens()
            .filter_map(|element| match element {
                SyntaxElement::Node(node) => Some(node),
                SyntaxElement::Token(_) => None,
            })
    }

    fn ast(&self) -> &Ast {
        self.file().node(self.id())
    }
}

impl SyntaxNodeBase for SyntaxNode {
    fn id(&self) -> AstId {
        self.0.id
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Some(node)
    }

    fn span(&self) -> Span {
        self.non_trivia_span()
    }

    fn full_span(&self) -> Span {
        self.full_span()
    }

    fn text_length(&self) -> u32 {
        self.ast().text_length()
    }

    fn file(&self) -> &File {
        &self.0.file
    }

    fn children(&self) -> impl Iterator<Item = SyntaxNode> {
        self.children()
    }

    fn children_with_tokens(&self) -> GreenElementIter<'_> {
        self.children_with_tokens()
    }

    fn syntax_kind(&self) -> TokenKind {
        self.ast().syntax_kind()
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
        self.0.id == other.0.id && Arc::ptr_eq(&self.0.file.0, &other.0.file.0)
    }
}

impl Eq for SyntaxNode {}

#[derive(Clone, Debug)]
struct SyntaxTokenData {
    file: File,
    green: GreenToken,
    offset: TextOffset,
    parent: Option<SyntaxNode>,
}

#[derive(Clone, Debug)]
pub struct SyntaxToken(Arc<SyntaxTokenData>);

impl SyntaxToken {
    pub fn new(
        file: File,
        green: GreenToken,
        offset: TextOffset,
        parent: Option<SyntaxNode>,
    ) -> Self {
        SyntaxToken(Arc::new(SyntaxTokenData {
            file,
            green,
            offset,
            parent,
        }))
    }

    pub fn file(&self) -> &File {
        &self.0.file
    }

    pub fn green(&self) -> &GreenToken {
        &self.0.green
    }

    pub fn syntax_kind(&self) -> TokenKind {
        self.0.green.kind
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
        let end = start + self.text_length();
        Span::new(start, end)
    }
}

impl PartialEq for SyntaxToken {
    fn eq(&self, other: &Self) -> bool {
        self.0.offset == other.0.offset
            && self.0.green.kind == other.0.green.kind
            && Arc::ptr_eq(&self.0.file.0, &other.0.file.0)
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

pub struct AstIdIter<'a, T: SyntaxNodeBase> {
    file: File,
    ids: &'a [AstId],
    start: usize,
    end: usize,
    _phantom: std::marker::PhantomData<T>,
}

impl<'a, T: SyntaxNodeBase> AstIdIter<'a, T> {
    pub fn new(file: File, ids: &'a [AstId]) -> Self {
        AstIdIter {
            file,
            ids,
            start: 0,
            end: ids.len(),
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<'a, T: SyntaxNodeBase> Iterator for AstIdIter<'a, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.start >= self.end {
            return None;
        }

        let id = self.ids[self.start];
        self.start += 1;
        let child_ast = self.file.node(id);
        let offset = TextOffset(child_ast.full_span().start());
        // Note: We don't have parent information in AstIdIterator context
        let syntax_node = SyntaxNode::new(self.file.clone(), id, offset, None);
        Some(T::cast(syntax_node).expect("wrong type"))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.end - self.start;
        (len, Some(len))
    }
}

impl<'a, T: SyntaxNodeBase> DoubleEndedIterator for AstIdIter<'a, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.start >= self.end {
            return None;
        }

        self.end -= 1;
        let id = self.ids[self.end];
        let child_ast = self.file.node(id);
        let offset = TextOffset(child_ast.full_span().start());
        // Note: We don't have parent information in AstIdIterator context
        let syntax_node = SyntaxNode::new(self.file.clone(), id, offset, None);
        Some(T::cast(syntax_node).expect("wrong type"))
    }
}

impl<'a, T: SyntaxNodeBase> ExactSizeIterator for AstIdIter<'a, T> {
    fn len(&self) -> usize {
        self.end - self.start
    }
}

pub struct GreenElementIter<'a> {
    file: File,
    elements: &'a [GreenElement],
    index: usize,
    current_offset: u32,
    parent: Option<SyntaxNode>,
}

impl<'a> GreenElementIter<'a> {
    pub fn new(
        file: File,
        elements: &'a [GreenElement],
        start_offset: TextOffset,
        parent: Option<SyntaxNode>,
    ) -> Self {
        GreenElementIter {
            file,
            elements,
            index: 0,
            current_offset: start_offset.value(),
            parent,
        }
    }
}

impl<'a> Iterator for GreenElementIter<'a> {
    type Item = SyntaxElement;

    fn next(&mut self) -> Option<Self::Item> {
        let element = self.elements.get(self.index)?;
        let offset = TextOffset(self.current_offset);
        self.index += 1;

        let syntax_element = match element {
            GreenElement::Token(green_token) => {
                let token = SyntaxToken::new(
                    self.file.clone(),
                    green_token.clone(),
                    offset,
                    self.parent.clone(),
                );
                self.current_offset += green_token.text.len() as u32;
                SyntaxElement::Token(token)
            }
            GreenElement::Node(ast_id) => {
                let raw_node = self.file.node(*ast_id);
                let node = SyntaxNode::new(self.file.clone(), *ast_id, offset, self.parent.clone());
                self.current_offset += raw_node.text_length();
                SyntaxElement::Node(node)
            }
        };

        Some(syntax_element)
    }
}

impl<'a> ExactSizeIterator for GreenElementIter<'a> {
    fn len(&self) -> usize {
        self.elements.len() - self.index
    }
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Plain {
    pub kind: TokenKind,
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Alias {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(WhereClause)]
    pub pre_where_clause: Option<AstId>,
    #[ast_node_ref(TypeBounds)]
    pub bounds: Option<AstId>,
    #[ast_node_ref(Type)]
    pub ty: Option<AstId>,
    #[ast_node_ref(WhereClause)]
    pub post_where_clause: Option<AstId>,
}

impl AstAlias {
    pub fn modifier_list(&self) -> Option<AstModifierList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstModifierList::cast(n))
    }

    pub fn name(&self) -> Option<AstName> {
        self.syntax_node().children().find_map(|n| AstName::cast(n))
    }

    pub fn type_param_list(&self) -> Option<AstTypeParamList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstTypeParamList::cast(n))
    }
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Alt {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Pattern)]
    pub alts: Vec<AstId>,
}

extra_ast_node!(Argument, TokenKind::ARGUMENT);

impl AstArgument {
    pub fn name(&self) -> Option<AstName> {
        self.syntax_node().children().find_map(|n| AstName::cast(n))
    }

    pub fn expr(&self) -> Option<AstExpr> {
        self.syntax_node().children().find_map(|n| AstExpr::cast(n))
    }
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct ArgumentList {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Argument)]
    pub items: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Bin {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub op: BinOp,
    #[ast_node_ref(Expr)]
    pub lhs: AstId,
    #[ast_node_ref(Expr)]
    pub rhs: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Block {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Stmt)]
    pub stmts: Vec<AstId>,
    #[ast_node_ref(Expr)]
    pub expr: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Break {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Call {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Expr)]
    pub callee: AstId,
    #[ast_node_ref(ArgumentList)]
    pub arg_list: AstId,
}

impl AstCall {
    pub fn object(&self) -> Option<AstExpr> {
        let callee_node = self.callee();

        match callee_node {
            AstExpr::TypedExpr(type_expr) => {
                let node = type_expr.callee();
                if let Some(dot) = node.to_dot_expr() {
                    Some(dot.lhs())
                } else {
                    None
                }
            }

            AstExpr::DotExpr(dot) => Some(dot.lhs()),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Class {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Field)]
    pub fields: Vec<AstId>,
    #[ast_node_ref(TypeParamList)]
    pub type_param_list: Option<AstId>,
    pub field_name_style: FieldNameStyle,
}

impl AstClass {
    pub fn modifier_list(&self) -> Option<AstModifierList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstModifierList::cast(n))
    }

    pub fn name(&self) -> Option<AstName> {
        self.syntax_node().children().find_map(|n| AstName::cast(n))
    }

    pub fn where_clause(&self) -> Option<AstWhereClause> {
        self.syntax_node()
            .children()
            .find_map(|n| AstWhereClause::cast(n))
    }
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Const {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Expr)]
    pub expr: AstId,
}

impl AstConst {
    pub fn modifier_list(&self) -> Option<AstModifierList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstModifierList::cast(n))
    }

    pub fn name(&self) -> Option<AstName> {
        self.syntax_node().children().find_map(|n| AstName::cast(n))
    }

    pub fn data_type(&self) -> Option<AstType> {
        self.syntax_node().children().find_map(|n| AstType::cast(n))
    }
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Continue {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Conv {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Expr)]
    pub object: AstId,
}

impl AstConv {
    pub fn data_type(&self) -> Option<AstType> {
        self.syntax_node().children().find_map(|n| AstType::cast(n))
    }
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct CtorField {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Name)]
    pub ident: Option<AstId>,
    #[ast_node_ref(Pattern)]
    pub pattern: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct CtorFieldList {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(CtorField)]
    pub items: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct CtorPattern {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(PathData)]
    pub path: AstId,
    #[ast_node_ref(CtorFieldList)]
    pub param_list: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct DotExpr {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub op_span: Span,
    #[ast_node_ref(Expr)]
    pub lhs: AstId,
    #[ast_node_ref(Expr)]
    pub rhs: AstId,
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

#[derive(Clone, Debug, AstNode)]
pub(crate) struct ElementList {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Element)]
    pub items: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Enum {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(EnumVariant)]
    pub variants: Vec<AstId>,
}

impl AstEnum {
    pub fn modifier_list(&self) -> Option<AstModifierList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstModifierList::cast(n))
    }

    pub fn name(&self) -> Option<AstName> {
        self.syntax_node().children().find_map(|n| AstName::cast(n))
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

#[derive(Clone, Debug, AstNode)]
pub(crate) struct EnumVariant {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub field_name_style: FieldNameStyle,
    #[ast_node_ref(Field)]
    pub fields: Vec<AstId>,
}

impl AstEnumVariant {
    pub fn name(&self) -> Option<AstName> {
        self.syntax_node().children().find_map(|n| AstName::cast(n))
    }
}

#[derive(Clone, AstUnion)]
pub enum AstExpr {
    Bin(AstBin),
    Block(AstBlock),
    Break(AstBreak),
    Call(AstCall),
    Continue(AstContinue),
    Conv(AstConv),
    DotExpr(AstDotExpr),
    #[ast_union_kind(ERROR_EXPR)]
    Error(SyntaxNode),
    For(AstFor),
    NameExpr(AstNameExpr),
    If(AstIf),
    Is(AstIs),
    Lambda(AstLambda),
    LitBool(AstLitBool),
    LitChar(AstLitChar),
    LitFloat(AstLitFloat),
    LitInt(AstLitInt),
    LitStr(AstLitStr),
    Match(AstMatch),
    MethodCallExpr(AstMethodCallExpr),
    Paren(AstParen),
    Path(AstPath),
    Return(AstReturn),
    Template(AstTemplate),
    This(AstThis),
    Tuple(AstTuple),
    TypedExpr(AstTypedExpr),
    Un(AstUn),
    While(AstWhile),
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct ExprStmt {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Expr)]
    pub expr: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Extern {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Name)]
    pub identifier: Option<AstId>,
}

impl AstExtern {
    pub fn modifier_list(&self) -> Option<AstModifierList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstModifierList::cast(n))
    }

    pub fn name(&self) -> Option<AstName> {
        self.syntax_node().children().find_map(|n| AstName::cast(n))
    }
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Field {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
}

impl AstField {
    pub fn modifier_list(&self) -> Option<AstModifierList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstModifierList::cast(n))
    }

    pub fn name(&self) -> Option<AstName> {
        self.syntax_node().children().find_map(|n| AstName::cast(n))
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

#[derive(Clone, Debug, AstNode)]
pub(crate) struct For {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Pattern)]
    pub pattern: AstId,
    #[ast_node_ref(Expr)]
    pub expr: AstId,
    #[ast_node_ref(Block)]
    pub block: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Function {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub kind: FunctionKind,
    pub declaration_span: Span,
    #[ast_node_ref(Param)]
    pub params: Vec<AstId>,
    #[ast_node_ref(Block)]
    pub block: Option<AstId>,
}

impl AstFunction {
    pub fn modifier_list(&self) -> Option<AstModifierList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstModifierList::cast(n))
    }

    pub fn name(&self) -> Option<AstName> {
        self.syntax_node().children().find_map(|n| AstName::cast(n))
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

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Global {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub mutable: bool,
    #[ast_node_ref(Expr)]
    pub initial_value: Option<AstId>,
}

impl AstGlobal {
    pub fn modifier_list(&self) -> Option<AstModifierList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstModifierList::cast(n))
    }

    pub fn name(&self) -> Option<AstName> {
        self.syntax_node().children().find_map(|n| AstName::cast(n))
    }

    pub fn data_type(&self) -> Option<AstType> {
        self.syntax_node().children().find_map(|n| AstType::cast(n))
    }
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Name {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub name: String,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct NameExpr {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub name: String,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct IdentPattern {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub mutable: bool,
    #[ast_node_ref(Name)]
    pub name: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct If {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Expr)]
    pub cond: AstId,
    #[ast_node_ref(Expr)]
    pub then_block: AstId,
    #[ast_node_ref(Expr)]
    pub else_block: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Impl {
    pub declaration_span: Span,
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Type)]
    pub trait_type: Option<AstId>,
    #[ast_node_ref(Type)]
    pub extended_type: Option<AstId>,
    #[ast_node_ref(ElementList)]
    pub element_list: Option<AstId>,
}

impl AstImpl {
    pub fn modifier_list(&self) -> Option<AstModifierList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstModifierList::cast(n))
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

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Is {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Expr)]
    pub value: AstId,
    #[ast_node_ref(Pattern)]
    pub pattern: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Lambda {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Function)]
    pub fct: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct LambdaType {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Type)]
    pub params: Vec<AstId>,
    #[ast_node_ref(Type)]
    pub ret: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Let {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Pattern)]
    pub pattern: AstId,
    #[ast_node_ref(Expr)]
    pub expr: Option<AstId>,
}

impl AstLet {
    pub fn data_type(&self) -> Option<AstType> {
        self.syntax_node().children().find_map(|n| AstType::cast(n))
    }
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct LitBool {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub value: bool,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct LitChar {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub value: String,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct LitFloat {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub value: String,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct LitInt {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub value: String,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct LitPattern {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub kind: PatternLitKind,
    #[ast_node_ref(Expr)]
    pub expr: AstId,
}

#[derive(Clone, Debug)]
pub enum PatternLitKind {
    Bool,
    Char,
    Int,
    String,
    Float,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct LitStr {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub value: String,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Match {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Expr)]
    pub expr: AstId,
    #[ast_node_ref(MatchArm)]
    pub arms: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct MatchArm {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Pattern)]
    pub pattern: AstId,
    #[ast_node_ref(Expr)]
    pub cond: Option<AstId>,
    #[ast_node_ref(Expr)]
    pub value: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct MethodCallExpr {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Expr)]
    pub object: AstId,
    #[ast_node_ref(Name)]
    pub name: AstId,
    #[ast_node_ref(TypeArgumentList)]
    pub type_argument_list: Option<AstId>,
    #[ast_node_ref(ArgumentList)]
    pub arg_list: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Modifier {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub kind: TokenKind,
    #[ast_node_ref(Name)]
    pub ident: Option<AstId>,
}

// remove in next step
#[derive(Clone, Debug, AstNode)]
pub(crate) struct ModifierList {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Modifier)]
    pub items: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Module {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(ElementList)]
    pub element_list: Option<AstId>,
}

impl AstModule {
    pub fn modifier_list(&self) -> Option<AstModifierList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstModifierList::cast(n))
    }

    pub fn name(&self) -> Option<AstName> {
        self.syntax_node().children().find_map(|n| AstName::cast(n))
    }
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Param {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Pattern)]
    pub pattern: AstId,
    pub variadic: bool,
}

impl AstParam {
    pub fn data_type(&self) -> Option<AstType> {
        self.syntax_node().children().find_map(|n| AstType::cast(n))
    }
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Paren {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Expr)]
    pub expr: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Path {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    pub op_span: Span,

    #[ast_node_ref(Expr)]
    pub lhs: AstId,
    #[ast_node_ref(Expr)]
    pub rhs: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct PathData {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(PathSegment)]
    pub segments: Vec<AstId>,
}

#[derive(Clone, AstUnion)]
pub enum AstPathSegment {
    UpcaseThis(AstUpcaseThis),
    Name(AstName),
    #[ast_union_kind(ERROR_PATH_SEGMENT)]
    Error(SyntaxNode),
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct QualifiedPathType {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Type)]
    pub ty: AstId,
    #[ast_node_ref(Type)]
    pub trait_ty: AstId,
}

impl AstQualifiedPathType {
    pub fn name(&self) -> Option<AstName> {
        self.syntax_node().children().find_map(|n| AstName::cast(n))
    }
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct RefType {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
}

impl AstRefType {
    pub fn ty(&self) -> Option<AstType> {
        self.syntax_node().children().find_map(|n| AstType::cast(n))
    }
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct RegularType {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(PathData)]
    pub path: AstId,
    #[ast_node_ref(TypeArgument)]
    pub params: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Rest {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Return {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Expr)]
    pub expr: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Struct {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Field)]
    pub fields: Vec<AstId>,
    pub field_style: FieldNameStyle,
}

impl AstStruct {
    pub fn modifier_list(&self) -> Option<AstModifierList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstModifierList::cast(n))
    }

    pub fn name(&self) -> Option<AstName> {
        self.syntax_node().children().find_map(|n| AstName::cast(n))
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

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Template {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Expr)]
    pub parts: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct This {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Trait {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(TypeBounds)]
    pub bounds: Option<AstId>,
    #[ast_node_ref(ElementList)]
    pub element_list: Option<AstId>,
}

impl AstTrait {
    pub fn modifier_list(&self) -> Option<AstModifierList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstModifierList::cast(n))
    }

    pub fn name(&self) -> Option<AstName> {
        self.syntax_node().children().find_map(|n| AstName::cast(n))
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

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Tuple {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Expr)]
    pub values: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct TuplePattern {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Pattern)]
    pub params: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct TupleType {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Type)]
    pub subtypes: Vec<AstId>,
}

#[derive(Clone, AstUnion)]
pub enum AstPattern {
    Alt(AstAlt),
    CtorPattern(AstCtorPattern),
    #[ast_union_kind(ERROR_PATTERN)]
    Error(SyntaxNode),
    IdentPattern(AstIdentPattern),
    LitPattern(AstLitPattern),
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
    RegularType(AstRegularType),
    TupleType(AstTupleType),
}

impl AstType {
    pub fn is_unit_type(&self) -> bool {
        match self {
            AstType::TupleType(value) => value.subtypes_len() == 0,
            _ => false,
        }
    }
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct TypeArgumentList {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(TypeArgument)]
    pub items: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct TypeArgument {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
}

impl AstTypeArgument {
    pub fn name(&self) -> Option<AstName> {
        self.syntax_node().children().find_map(|n| AstName::cast(n))
    }

    pub fn ty(&self) -> Option<AstType> {
        self.syntax_node().children().find_map(|n| AstType::cast(n))
    }
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct TypeBounds {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Type)]
    pub items: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct TypedExpr {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    pub op_span: Span,

    #[ast_node_ref(Expr)]
    pub callee: AstId,
    #[ast_node_ref(Type)]
    pub args: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct TypeParam {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(TypeBounds)]
    pub bounds: Option<AstId>,
}

impl AstTypeParam {
    pub fn name(&self) -> Option<AstName> {
        self.syntax_node().children().find_map(|n| AstName::cast(n))
    }
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct TypeParamList {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(TypeParam)]
    pub items: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Un {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub op: UnOp,
    #[ast_node_ref(Expr)]
    pub opnd: AstId,
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
    Assign,
    AddAssign,
    Add,
    Sub,
    SubAssign,
    Mul,
    MulAssign,
    Div,
    DivAssign,
    Mod,
    ModAssign,
    Cmp(CmpOp),
    Or,
    And,
    BitOr,
    BitOrAssign,
    BitAnd,
    BitAndAssign,
    BitXor,
    BitXorAssign,
    ShiftL,
    ShiftLAssign,
    ArithShiftR,
    ArithShiftRAssign,
    LogicalShiftR,
    LogicalShiftRAssign,
}

impl BinOp {
    pub fn as_str(&self) -> &'static str {
        match *self {
            BinOp::Assign => "=",
            BinOp::Add => "+",
            BinOp::AddAssign => "+=",
            BinOp::Sub => "-",
            BinOp::SubAssign => "-=",
            BinOp::Mul => "*",
            BinOp::MulAssign => "*=",
            BinOp::Div => "/",
            BinOp::DivAssign => "/=",
            BinOp::Mod => "%",
            BinOp::ModAssign => "%=",
            BinOp::Cmp(op) => op.as_str(),
            BinOp::Or => "||",
            BinOp::And => "&&",
            BinOp::BitOr => "|",
            BinOp::BitOrAssign => "|=",
            BinOp::BitAnd => "&",
            BinOp::BitAndAssign => "&=",
            BinOp::BitXor => "^",
            BinOp::BitXorAssign => "^=",
            BinOp::ShiftL => "<<",
            BinOp::ShiftLAssign => "<<=",
            BinOp::ArithShiftR => ">>",
            BinOp::ArithShiftRAssign => ">>=",
            BinOp::LogicalShiftR => ">>>",
            BinOp::LogicalShiftRAssign => ">>>=",
        }
    }

    pub fn is_any_assign(&self) -> bool {
        match *self {
            BinOp::Assign
            | BinOp::AddAssign
            | BinOp::SubAssign
            | BinOp::MulAssign
            | BinOp::ModAssign
            | BinOp::DivAssign
            | BinOp::BitOrAssign
            | BinOp::BitAndAssign
            | BinOp::BitXorAssign
            | BinOp::ShiftLAssign
            | BinOp::ArithShiftRAssign
            | BinOp::LogicalShiftRAssign => true,
            _ => false,
        }
    }

    pub fn is_compare(&self) -> bool {
        match *self {
            BinOp::Cmp(cmp) if cmp != CmpOp::Is && cmp != CmpOp::IsNot => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct UnderscorePattern {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct UpcaseThis {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct Use {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(UsePath)]
    pub path: AstId,
}

impl AstUse {
    pub fn modifier_list(&self) -> Option<AstModifierList> {
        self.syntax_node()
            .children()
            .find_map(|n| AstModifierList::cast(n))
    }
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct UseAs {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(UseAtom)]
    pub original_name: AstId,
}

impl AstUseAs {
    pub fn target_name(&self) -> Option<AstName> {
        self.syntax_node().children().find_map(|n| AstName::cast(n))
    }
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct UseAtom {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
}

impl AstUseAtom {
    pub fn kind(&self) -> TokenKind {
        self.children_with_tokens()
            .filter(|t| !t.is_trivia())
            .next()
            .expect("missing child")
            .syntax_kind()
    }

    pub fn to_name(&self) -> Option<AstName> {
        self.children().find_map(|n| AstName::cast(n))
    }
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct UseGroup {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(UsePath)]
    pub targets: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct UsePath {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(UseAtom)]
    pub path: Vec<AstId>,
    #[ast_node_ref(UseTarget)]
    pub target: AstId,
}

#[derive(Clone, AstUnion)]
pub enum AstUseTarget {
    Error(SyntaxNode),
    UseAs(AstUseAs),
    UseAtom(AstUseAtom),
    UseGroup(AstUseGroup),
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct WhereClause {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(WhereClauseItem)]
    pub clauses: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct WhereClauseItem {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Type)]
    pub bounds: Vec<AstId>,
}

impl AstWhereClauseItem {
    pub fn ty(&self) -> Option<AstType> {
        self.syntax_node().children().find_map(|n| AstType::cast(n))
    }
}

#[derive(Clone, Debug, AstNode)]
pub(crate) struct While {
    pub full_span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Expr)]
    pub cond: AstId,
    #[ast_node_ref(Block)]
    pub block: AstId,
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

fn find_node_by_ptr(node: SyntaxNode, needle: SyntaxNodePtr) -> Option<SyntaxNode> {
    let file = node.file().clone();
    let needle_span = needle.span();
    let needle_start = needle_span.start();
    let needle_end = needle_span.end();
    let mut current = node;
    let mut offset = 0;

    'outer_loop: loop {
        if current.as_ptr() == needle {
            return Some(current);
        }

        for green_element in current.ast().green_children() {
            if offset > needle_end {
                return None;
            }

            match green_element {
                GreenElement::Node(node_id) => {
                    let node_id = *node_id;
                    let node = file.node(node_id);
                    let child_len = node.text_length();
                    let child_end = offset + child_len;

                    if child_end <= needle_start {
                        offset += child_len;
                        continue;
                    }

                    debug_assert!(needle_span.is_within(Span::new(offset, child_len)));
                    current =
                        SyntaxNode::new(file.clone(), node_id, TextOffset(offset), Some(current));
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

#[cfg(test)]
mod tests {
    use crate::Parser;
    use crate::ast::{SyntaxNode, SyntaxNodeBase};

    #[test]
    fn test_node_at_offset() {
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
    fn test_syntax_node_offset_and_parent() {
        //                0         1         2
        //                012345678901234567890123
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
        assert_eq!(parent.id(), root.id());

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

        // Block starts at offset 10: "{ let x = 1; }"
        assert_eq!(block.offset().value(), 10);

        // Get statements from the block to test deeper parent chain
        if block.stmts_len() > 0 {
            let stmt = block.stmts_at(0);
            // Statement should have block as parent
            assert!(stmt.parent().is_some());
            let stmt_parent = stmt.parent().unwrap();
            assert_eq!(stmt_parent.id(), block.syntax_node().id());

            // Statement (let) starts at offset 12: "let x = 1;"
            assert_eq!(stmt.offset().value(), 12);
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
        assert_eq!(block_parent.id(), function.id());

        assert_eq!(block.offset().value(), 9);

        if let Some(expr) = block.expr() {
            assert!(expr.parent().is_some());
            let expr_parent = expr.parent().unwrap();
            assert_eq!(expr_parent.id(), block.syntax_node().id());
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
            .filter(|n| n.is_field())
            .nth(1)
            .unwrap();
        let field_ptr = field.as_ptr();
        let resolved_field = file.syntax_by_ptr::<SyntaxNode>(field_ptr);
        assert_eq!(resolved_field.span(), field.span());
        assert_eq!(resolved_field.syntax_kind(), field.syntax_kind());
    }
}
