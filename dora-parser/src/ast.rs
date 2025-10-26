use std::sync::Arc;

use dora_parser_derive::{AstEnum, AstNode};
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
    pub fn new(content: Arc<String>, nodes: Arena<Ast>, root_id: AstId) -> File {
        File(Arc::new(FilePayload {
            content,
            nodes,
            root_id,
        }))
    }

    fn payload(&self) -> &FilePayload {
        self.0.as_ref()
    }

    pub fn node(&self, id: AstId) -> &Ast {
        &self.payload().nodes[id]
    }

    #[allow(unused)]
    pub fn node2(&self, id: AstId) -> SyntaxNode {
        let node_ast = self.node(id);
        let offset = TextOffset(node_ast.span().start());
        // Note: parent is None here as we don't have context about the parent
        SyntaxNode::new(self.clone(), id, offset, None)
    }

    pub fn root(&self) -> SyntaxNode {
        let root_id = self.root_id();
        let root_ast = self.node(root_id);
        let offset = TextOffset(root_ast.span().start());
        SyntaxNode::new(self.clone(), root_id, offset, None)
    }

    pub fn root_id(&self) -> AstId {
        self.payload().root_id
    }

    pub fn content(&self) -> &Arc<String> {
        &self.payload().content
    }

    pub fn node_at_offset(&self, offset: u32) -> Option<SyntaxNode> {
        find_innermost_node_at_offset(self.root(), offset)
    }

    pub fn node_by_ptr(&self, ptr: SyntaxNodePtr) -> SyntaxNode {
        find_node_by_ptr(self.root(), ptr).expect("node not found for pointer")
    }

    pub fn raw_root(&self) -> &Root {
        self.node(self.payload().root_id)
            .to_root()
            .expect("file expected")
    }

    #[cfg(test)]
    pub fn fct0(&self) -> &Function {
        self.node(self.raw_root().elements[0]).as_function()
    }

    #[cfg(test)]
    pub fn fct(&self, index: usize) -> &Function {
        self.node(self.raw_root().elements[index]).as_function()
    }

    #[cfg(test)]
    pub fn cls0(&self) -> &Class {
        self.node(self.raw_root().elements[0]).as_class()
    }

    #[cfg(test)]
    pub fn cls(&self, index: usize) -> &Class {
        self.node(self.raw_root().elements[index]).as_class()
    }

    #[cfg(test)]
    pub fn struct0(&self) -> &Struct {
        self.node(self.raw_root().elements[0]).as_struct()
    }

    #[cfg(test)]
    pub fn enum0(&self) -> &Enum {
        self.node(self.raw_root().elements[0]).as_enum()
    }

    #[cfg(test)]
    pub fn module0(&self) -> &Module {
        self.node(self.raw_root().elements[0]).as_module()
    }

    #[cfg(test)]
    pub fn trait_(&self, index: usize) -> &Trait {
        self.node(self.raw_root().elements[index]).as_trait()
    }

    #[cfg(test)]
    pub fn trait0(&self) -> &Trait {
        self.node(self.raw_root().elements[0]).as_trait()
    }

    #[cfg(test)]
    pub fn impl0(&self) -> &Impl {
        self.node(self.raw_root().elements[0]).as_impl()
    }

    #[cfg(test)]
    pub fn global0(&self) -> &Global {
        self.node(self.raw_root().elements[0]).as_global()
    }

    #[cfg(test)]
    pub fn const0(&self) -> &Const {
        self.node(self.raw_root().elements[0]).as_const()
    }
}

pub type AstId = Id<Ast>;

// We auto-generate the Ast enum from this NodeKind enum.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, AstEnum)]
pub enum NodeKind {
    Alias,
    Alt,
    Argument,
    Bin,
    Block,
    Break,
    Call,
    Class,
    Const,
    Continue,
    Conv,
    CtorField,
    CtorPattern,
    DotExpr,
    Enum,
    EnumVariant,
    Error,
    ExprStmt,
    Extern,
    Field,
    For,
    Function,
    Global,
    Ident,
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
    Modifier,
    ModifierList,
    Module,
    Param,
    Paren,
    Path,
    PathData,
    QualifiedPathType,
    RefType,
    RegularType,
    Rest,
    Return,
    Root,
    Struct,
    Template,
    This,
    Trait,
    Tuple,
    TuplePattern,
    TupleType,
    Type,
    TypeArgument,
    TypeBounds,
    TypedExpr,
    TypeParam,
    TypeParamList,
    Un,
    UnderscorePattern,
    UpcaseThis,
    Use,
    UseAtom,
    UseGroup,
    UsePath,
    UseTargetName,
    WhereClause,
    WhereClauseItem,
    While,
}

// The Ast enum is auto-generated from NodeKind by the AstEnum proc macro.
impl Ast {
    #[cfg(test)]
    pub fn is_unit(&self) -> bool {
        match self {
            &Ast::TupleType(ref val) if val.subtypes.len() == 0 => true,
            _ => false,
        }
    }

    pub fn is_un_op(&self, op: UnOp) -> bool {
        match *self {
            Ast::Un(ref e) if e.op == op => true,
            _ => false,
        }
    }

    pub fn to_bin_and(&self) -> Option<&Bin> {
        self.to_bin().filter(|e| e.op == BinOp::And)
    }

    pub fn is_lit_true(&self) -> bool {
        match *self {
            Ast::LitBool(ref lit) if lit.value => true,
            _ => false,
        }
    }

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
    type RawType;

    fn id(&self) -> AstId;
    fn cast(node: SyntaxNode) -> Option<Self>;
    fn raw_node(&self) -> &Self::RawType;
    fn span(&self) -> Span;
    fn text_length(&self) -> u32;
    fn file(&self) -> &File;
    fn node_children(&self) -> impl Iterator<Item = SyntaxNode>;
    fn node_kind(&self) -> NodeKind;
    fn syntax_kind(&self) -> TokenKind;
    fn as_ptr(&self) -> SyntaxNodePtr;
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
        }))
    }

    pub fn offset(&self) -> TextOffset {
        self.0.offset.clone()
    }

    pub fn parent(&self) -> Option<SyntaxNode> {
        self.0.parent.clone()
    }

    pub fn as_ptr(&self) -> SyntaxNodePtr {
        SyntaxNodePtr::new(self.syntax_kind(), self.span())
    }

    pub fn children(&self) -> GreenElementIterator<'_> {
        GreenElementIterator::new(
            self.file().clone(),
            self.raw_node().green_children(),
            self.offset(),
            Some(self.clone()),
        )
    }

    pub fn node_children(&self) -> impl Iterator<Item = SyntaxNode> + '_ {
        self.children().filter_map(|element| match element {
            SyntaxElement::Node(node) => Some(node),
            SyntaxElement::Token(_) => None,
        })
    }
}

impl SyntaxNodeBase for SyntaxNode {
    type RawType = Ast;

    fn id(&self) -> AstId {
        self.0.id
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Some(node)
    }

    fn raw_node(&self) -> &Ast {
        self.0.file.node(self.0.id)
    }

    fn span(&self) -> Span {
        self.raw_node().span()
    }

    fn text_length(&self) -> u32 {
        self.raw_node().text_length()
    }

    fn file(&self) -> &File {
        &self.0.file
    }

    fn node_children(&self) -> impl Iterator<Item = SyntaxNode> {
        self.node_children()
    }

    fn node_kind(&self) -> NodeKind {
        self.raw_node().kind()
    }

    fn syntax_kind(&self) -> TokenKind {
        self.raw_node().syntax_kind()
    }

    fn as_ptr(&self) -> SyntaxNodePtr {
        self.as_ptr()
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
}

pub fn walk_children<V: Visitor, N: SyntaxNodeBase>(v: &mut V, node: N) {
    for child in node.node_children() {
        visit_node(v, child);
    }
}

pub struct AstIdIterator<'a, T: SyntaxNodeBase> {
    file: File,
    ids: &'a [AstId],
    index: usize,
    _phantom: std::marker::PhantomData<T>,
}

impl<'a, T: SyntaxNodeBase> AstIdIterator<'a, T> {
    pub fn new(file: File, ids: &'a [AstId]) -> Self {
        AstIdIterator {
            file,
            ids,
            index: 0,
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<'a, T: SyntaxNodeBase> Iterator for AstIdIterator<'a, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.ids.len() {
            let id = self.ids[self.index];
            self.index += 1;
            let child_ast = self.file.node(id);
            let offset = TextOffset(child_ast.span().start());
            // Note: We don't have parent information in AstIdIterator context
            let syntax_node = SyntaxNode::new(self.file.clone(), id, offset, None);
            Some(T::cast(syntax_node).expect("wrong type"))
        } else {
            None
        }
    }
}

impl<'a, T: SyntaxNodeBase> ExactSizeIterator for AstIdIterator<'a, T> {
    fn len(&self) -> usize {
        self.ids.len() - self.index
    }
}

pub struct GreenElementIterator<'a> {
    file: File,
    elements: &'a [GreenElement],
    index: usize,
    current_offset: u32,
    parent: Option<SyntaxNode>,
}

impl<'a> GreenElementIterator<'a> {
    pub fn new(
        file: File,
        elements: &'a [GreenElement],
        start_offset: TextOffset,
        parent: Option<SyntaxNode>,
    ) -> Self {
        GreenElementIterator {
            file,
            elements,
            index: 0,
            current_offset: start_offset.value(),
            parent,
        }
    }
}

impl<'a> Iterator for GreenElementIterator<'a> {
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

impl<'a> ExactSizeIterator for GreenElementIterator<'a> {
    fn len(&self) -> usize {
        self.elements.len() - self.index
    }
}

#[derive(Clone, Debug, AstNode)]
pub struct Root {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    pub elements: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Alias {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(ModifierList)]
    pub modifiers: Option<AstId>,
    #[ast_node_ref(Ident)]
    pub name: Option<AstId>,
    #[ast_node_ref(TypeParamList)]
    pub type_params: Option<AstId>,
    #[ast_node_ref(WhereClause)]
    pub pre_where_clause: Option<AstId>,
    #[ast_node_ref(TypeBounds)]
    pub bounds: AstId,
    #[ast_node_ref(Type)]
    pub ty: Option<AstId>,
    #[ast_node_ref(WhereClause)]
    pub post_where_clause: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Alt {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub alts: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Argument {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    #[ast_node_ref(Ident)]
    pub name: Option<AstId>,
    pub expr: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct Bin {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub op: BinOp,
    pub lhs: AstId,
    pub rhs: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct Block {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub stmts: Vec<AstId>,
    pub expr: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Break {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
}

#[derive(Clone, Debug, AstNode)]
pub struct Call {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub callee: AstId,
    pub args: Vec<AstId>,
}

impl Call {
    pub fn object(&self, file: &File) -> Option<AstId> {
        let callee_node = file.node(self.callee);
        if let Some(type_param) = callee_node.to_typed_expr() {
            let node = file.node(type_param.callee);
            if let Some(dot) = node.to_dot_expr() {
                Some(dot.lhs)
            } else {
                None
            }
        } else if let Some(dot) = callee_node.to_dot_expr() {
            Some(dot.lhs)
        } else {
            None
        }
    }

    pub fn object_or_callee<'a>(&self, file: &'a File) -> AstId {
        self.object(file).unwrap_or(self.callee)
    }
}

#[derive(Clone, Debug, AstNode)]
pub struct Class {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    #[ast_node_ref(ModifierList)]
    pub modifiers: Option<AstId>,
    #[ast_node_ref(Ident)]
    pub name: Option<AstId>,

    #[ast_node_ref(Field)]
    pub fields: Vec<AstId>,
    #[ast_node_ref(TypeParamList)]
    pub type_params: Option<AstId>,
    #[ast_node_ref(WhereClause)]
    pub where_clause: Option<AstId>,
    pub field_name_style: FieldNameStyle,
}

#[derive(Clone, Debug, AstNode)]
pub struct Const {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    #[ast_node_ref(ModifierList)]
    pub modifiers: Option<AstId>,
    #[ast_node_ref(Ident)]
    pub name: Option<AstId>,
    #[ast_node_ref(Type)]
    pub data_type: AstId,
    pub expr: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct Continue {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
}

#[derive(Clone, Debug, AstNode)]
pub struct Conv {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub object: AstId,
    pub data_type: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct CtorField {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    pub ident: Option<AstId>,
    pub pattern: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct CtorPattern {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    pub path: AstId,
    pub params: Option<Vec<AstId>>,
}

#[derive(Clone, Debug, AstNode)]
pub struct DotExpr {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    pub op_span: Span,

    pub lhs: AstId,
    pub rhs: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct Enum {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    #[ast_node_ref(ModifierList)]
    pub modifiers: Option<AstId>,
    #[ast_node_ref(Ident)]
    pub name: Option<AstId>,
    #[ast_node_ref(TypeParamList)]
    pub type_params: Option<AstId>,
    #[ast_node_ref(EnumVariant)]
    pub variants: Vec<AstId>,
    #[ast_node_ref(WhereClause)]
    pub where_clause: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct EnumVariant {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    pub name: Option<AstId>,
    pub field_name_style: FieldNameStyle,
    pub fields: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Error {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
}

#[derive(Clone, Debug, AstNode)]
pub struct ExprStmt {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub expr: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct Extern {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    #[ast_node_ref(ModifierList)]
    pub modifiers: Option<AstId>,
    #[ast_node_ref(Ident)]
    pub name: Option<AstId>,
    #[ast_node_ref(Ident)]
    pub identifier: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Field {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    #[ast_node_ref(ModifierList)]
    pub modifiers: Option<AstId>,
    #[ast_node_ref(Ident)]
    pub name: Option<AstId>,
    #[ast_node_ref(Type)]
    pub data_type: AstId,
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
pub struct For {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub pattern: AstId,
    pub expr: AstId,
    pub block: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct Function {
    pub declaration_span: Span,
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    #[ast_node_ref(ModifierList)]
    pub modifiers: Option<AstId>,
    pub kind: FunctionKind,

    #[ast_node_ref(Ident)]
    pub name: Option<AstId>,
    #[ast_node_ref(TypeParamList)]
    pub type_params: Option<AstId>,
    #[ast_node_ref(Param)]
    pub params: Vec<AstId>,
    #[ast_node_ref(Type)]
    pub return_type: Option<AstId>,
    #[ast_node_ref(WhereClause)]
    pub where_clause: Option<AstId>,
    pub block: Option<AstId>,
}

impl Function {
    pub fn block(&self) -> AstId {
        self.block.unwrap()
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
pub struct Global {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    #[ast_node_ref(ModifierList)]
    pub modifiers: Option<AstId>,
    #[ast_node_ref(Ident)]
    pub name: Option<AstId>,
    pub mutable: bool,
    #[ast_node_ref(Type)]
    pub data_type: AstId,
    pub initial_value: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Ident {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    pub name: String,
}

#[derive(Clone, Debug, AstNode)]
pub struct IdentPattern {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    pub mutable: bool,
    pub name: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct If {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub cond: AstId,
    pub then_block: AstId,
    pub else_block: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Impl {
    pub declaration_span: Span,
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(ModifierList)]
    pub modifiers: Option<AstId>,
    #[ast_node_ref(TypeParamList)]
    pub type_params: Option<AstId>,
    pub trait_type: Option<AstId>,
    #[ast_node_ref(Type)]
    pub extended_type: AstId,
    #[ast_node_ref(WhereClause)]
    pub where_clause: Option<AstId>,

    pub methods: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Is {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub value: AstId,
    pub pattern: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct Lambda {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    pub fct_id: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct LambdaType {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Type)]
    pub params: Vec<AstId>,
    #[ast_node_ref(Type)]
    pub ret: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Let {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub pattern: AstId,

    pub data_type: Option<AstId>,
    pub expr: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct LitBool {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub value: bool,
}

#[derive(Clone, Debug, AstNode)]
pub struct LitChar {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    pub value: String,
}

#[derive(Clone, Debug, AstNode)]
pub struct LitFloat {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub value: String,
}

#[derive(Clone, Debug, AstNode)]
pub struct LitInt {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub value: String,
}

#[derive(Clone, Debug, AstNode)]
pub struct LitPattern {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    pub kind: PatternLitKind,
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
pub struct LitStr {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub value: String,
}

#[derive(Clone, Debug, AstNode)]
pub struct Match {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub expr: AstId,
    pub arms: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct MatchArm {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub pattern: AstId,
    pub cond: Option<AstId>,
    pub value: AstId,
}

#[derive(Clone, Debug)]
pub struct PatternError {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
}

#[derive(Clone, Debug, AstNode)]
pub struct Modifier {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    pub kind: TokenKind,
    pub ident: Option<AstId>,
}

impl Modifier {
    pub fn is_pub(&self) -> bool {
        self.kind == TokenKind::PUB_KW
    }

    pub fn is_static(&self) -> bool {
        self.kind == TokenKind::STATIC_KW
    }

    pub fn is_at(&self) -> bool {
        self.kind == TokenKind::AT
    }
}

// remove in next step
#[derive(Clone, Debug, AstNode)]
pub struct ModifierList {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    pub modifiers: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Module {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    #[ast_node_ref(ModifierList)]
    pub modifiers: Option<AstId>,
    #[ast_node_ref(Ident)]
    pub name: Option<AstId>,
    pub elements: Option<Vec<AstId>>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Param {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    pub pattern: AstId,
    #[ast_node_ref(Type)]
    pub data_type: AstId,
    pub variadic: bool,
}

#[derive(Clone, Debug, AstNode)]
pub struct Paren {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    pub expr: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct Path {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    pub op_span: Span,

    pub lhs: AstId,
    pub rhs: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct PathData {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    pub segments: Vec<AstId>,
}

#[derive(Clone, Debug)]
pub struct PathSegmentSelf {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
}

#[derive(Clone, Debug)]
pub struct PathSegmentIdent {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    pub name: AstId,
}

#[derive(Clone, Debug)]
pub struct PatternParam {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    pub mutable: bool,
    pub name: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct QualifiedPathType {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub ty: AstId,
    pub trait_ty: AstId,
    pub name: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct RefType {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub ty: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct RegularType {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub path: AstId,
    pub params: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Rest {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
}

#[derive(Clone, Debug, AstNode)]
pub struct Return {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub expr: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Struct {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    #[ast_node_ref(ModifierList)]
    pub modifiers: Option<AstId>,
    #[ast_node_ref(Ident)]
    pub name: Option<AstId>,
    #[ast_node_ref(Field)]
    pub fields: Vec<AstId>,
    #[ast_node_ref(TypeParamList)]
    pub type_params: Option<AstId>,
    pub where_clause: Option<AstId>,
    pub field_style: FieldNameStyle,
}

#[derive(Clone, Debug, AstNode)]
pub struct Template {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub parts: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct This {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
}

#[derive(Clone, Debug, AstNode)]
pub struct Trait {
    #[ast_node_ref(Ident)]
    pub name: Option<AstId>,
    #[ast_node_ref(ModifierList)]
    pub modifiers: Option<AstId>,
    #[ast_node_ref(TypeParamList)]
    pub type_params: Option<AstId>,
    #[ast_node_ref(TypeBounds)]
    pub bounds: AstId,
    #[ast_node_ref(WhereClause)]
    pub where_clause: Option<AstId>,
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    pub methods: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Tuple {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub values: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct TuplePattern {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    pub params: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct TupleType {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Type)]
    pub subtypes: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Type {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub inner: AstId,
}

impl AstType {
    pub fn is_unit_type(&self) -> bool {
        if let Some(tuple_type) = AstTupleType::cast(self.inner()) {
            tuple_type.subtypes_len() == 0
        } else {
            false
        }
    }
}

#[derive(Clone, Debug, AstNode)]
pub struct TypeArgument {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Ident)]
    pub name: Option<AstId>,
    #[ast_node_ref(Type)]
    pub ty: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct TypeBounds {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    #[ast_node_ref(Type)]
    pub items: Vec<AstId>,
}

#[derive(Clone, Debug)]
pub struct TypeGenericType {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub path: AstId,
    pub params: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct TypedExpr {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    pub op_span: Span,

    pub callee: AstId,
    pub args: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct TypeParam {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    pub name: Option<AstId>,
    #[ast_node_ref(TypeBounds)]
    pub bounds: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct TypeParamList {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    #[ast_node_ref(TypeParam)]
    pub params: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Un {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub op: UnOp,
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
pub struct UnderscorePattern {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
}

#[derive(Clone, Debug, AstNode)]
pub struct UpcaseThis {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
}

#[derive(Clone, Debug, AstNode)]
pub struct Use {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    #[ast_node_ref(ModifierList)]
    pub modifiers: Option<AstId>,
    #[ast_node_ref(UsePath)]
    pub path: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct UseAtom {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    pub value: UsePathComponentValue,
}

#[derive(Clone, Debug, AstNode)]
pub struct UseGroup {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    pub targets: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct UsePath {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    #[ast_node_ref(UseAtom)]
    pub path: Vec<AstId>,
    pub target: UsePathDescriptor,
}

#[derive(Clone, Debug)]
pub enum UsePathDescriptor {
    Default,
    As(AstId),
    Group(AstId),
    Error,
}

#[derive(Clone, Debug)]
pub enum UsePathComponentValue {
    This,
    Super,
    Package,
    Name(AstId),
    Error,
}

#[derive(Clone, Debug, AstNode)]
pub struct UseTargetName {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    pub name: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct WhereClause {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    pub clauses: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct WhereClauseItem {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,
    pub ty: AstId,
    pub bounds: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct While {
    pub span: Span,
    pub green_elements: Vec<GreenElement>,
    pub text_length: u32,

    pub cond: AstId,
    pub block: AstId,
}

fn find_innermost_node_at_offset(node: SyntaxNode, offset: u32) -> Option<SyntaxNode> {
    let span = node.span();
    if offset < span.start() || offset >= span.end() {
        return None;
    }
    for child in node.node_children() {
        if let Some(innermost) = find_innermost_node_at_offset(child, offset) {
            return Some(innermost);
        }
    }
    Some(node)
}

fn find_node_by_ptr(node: SyntaxNode, ptr: SyntaxNodePtr) -> Option<SyntaxNode> {
    if node.as_ptr() == ptr {
        return Some(node);
    }

    let target_span = ptr.span();

    for child in node.node_children() {
        let child_span = child.span();

        if child_span.end() <= target_span.start() {
            continue;
        }

        if child_span.start() >= target_span.end() {
            return None;
        }

        if let Some(found) = find_node_by_ptr(child, ptr) {
            return Some(found);
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::SyntaxNodeBase;
    use crate::Parser;

    #[test]
    fn test_node_at_offset() {
        let content = "fn main() { let x = 1; }";
        let parser = Parser::from_string(content);
        let (file, errors) = parser.parse();
        assert!(errors.is_empty());

        let node = file.node_at_offset(15);
        assert!(node.unwrap().is_let());

        let node = file.node_at_offset(0);
        assert!(node.unwrap().is_function());
    }

    #[test]
    fn test_green_children() {
        let content = "fn main() { let x = 1; }";
        let parser = Parser::from_string(content);
        let (file, errors) = parser.parse();
        assert!(errors.is_empty());

        let root = file.node(file.root_id());
        let green_children = root.green_children();

        // The root should have at least one green element (the function)
        assert!(!green_children.is_empty());

        // Test that we can access green_children on different node types
        let root_node = file.raw_root();
        let function_id = root_node.elements[0];
        let function_ast = file.node(function_id);
        let function_green_children = function_ast.green_children();
        assert!(!function_green_children.is_empty());
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
        let mut children = root.node_children();
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
        let block = function.block().unwrap().as_block();

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
        let function = root.node_children().next().unwrap();
        assert!(function.is_function());
        assert_eq!(function.offset().value(), 0);

        let function_typed = function.clone().as_function();
        let block = function_typed.block().unwrap();
        assert!(block.is_block());

        // Verify parent chain
        assert!(block.parent().is_some());
        let block_parent = block.parent().unwrap();
        assert_eq!(block_parent.id(), function.id());

        assert_eq!(block.offset().value(), 9);

        let block_typed = block.clone().as_block();
        if let Some(expr) = block_typed.expr() {
            // The if expression should have the block as parent
            assert!(expr.parent().is_some());
            let expr_parent = expr.parent().unwrap();
            assert_eq!(expr_parent.id(), block_typed.syntax_node().id());
            assert_eq!(expr.offset().value(), 11);
        }
    }

    #[test]
    fn test_cast_method() {
        use super::{AstBlock, AstFunction};

        let content = "fn main() { let x = 1; }";
        let parser = Parser::from_string(content);
        let (file, errors) = parser.parse();
        assert!(errors.is_empty());

        let root = file.root();
        let function_node = root.node_children().next().unwrap();

        // Test successful cast - SyntaxNode is a Function
        let function_cast = AstFunction::cast(function_node.clone());
        assert!(function_cast.is_some());
        let function = function_cast.unwrap();
        assert_eq!(function.id(), function_node.id());

        // Test failed cast - SyntaxNode is not a Block
        let block_cast = AstBlock::cast(function_node.clone());
        assert!(block_cast.is_none());

        // Test casting a Block node successfully
        let function_typed = function_node.as_function();
        let block_node = function_typed.block().unwrap();
        let block_cast = AstBlock::cast(block_node.clone());
        assert!(block_cast.is_some());
        assert_eq!(block_cast.unwrap().id(), block_node.id());
    }

    #[test]
    fn test_syntax_node_ptr() {
        let content = "fn main() { let x = 1; }";
        let parser = Parser::from_string(content);
        let (file, errors) = parser.parse();
        assert!(errors.is_empty());

        let root = file.root();
        let function_node = root.node_children().next().unwrap();

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
        let function_node = root.node_children().find(|n| n.is_struct()).unwrap();

        let function_ptr = function_node.as_ptr();
        let resolved_node = file.node_by_ptr(function_ptr);
        assert_eq!(resolved_node.span(), function_node.span());
        assert_eq!(resolved_node.syntax_kind(), function_node.syntax_kind());

        let field = function_node
            .node_children()
            .filter(|n| n.is_field())
            .nth(1)
            .unwrap();
        let field_ptr = field.as_ptr();
        let resolved_field = file.node_by_ptr(field_ptr);
        assert_eq!(resolved_field.span(), field.span());
        assert_eq!(resolved_field.syntax_kind(), field.syntax_kind());
    }
}
