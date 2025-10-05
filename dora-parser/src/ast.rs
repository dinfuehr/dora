use std::sync::Arc;

use dora_parser_derive::{AstEnum, AstNode};
use id_arena::{Arena, Id};

use crate::{Span, TokenKind};

pub mod dump;

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
    pub fn node2(&self, id: AstId) -> AstNode {
        AstNode {
            file: self.clone(),
            id,
        }
    }

    pub fn root(&self) -> AstNode {
        AstNode::new(self.clone(), self.root_id())
    }

    pub fn root_id(&self) -> AstId {
        self.payload().root_id
    }

    pub fn content(&self) -> &Arc<String> {
        &self.payload().content
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

#[derive(Clone, Debug, AstEnum)]
pub enum Ast {
    Alias(Alias),
    Alt(Alt),
    Argument(Argument),
    Bin(Bin),
    Block(Block),
    Break(Break),
    Call(Call),
    Class(Class),
    Const(Const),
    Continue(Continue),
    Conv(Conv),
    CtorField(CtorField),
    CtorPattern(CtorPattern),
    Dot(Dot),
    Enum(Enum),
    Error(Error),
    ExprStmt(ExprStmt),
    Extern(Extern),
    Field(Field),
    For(For),
    Function(Function),
    Global(Global),
    Ident(Ident),
    IdentPattern(IdentPattern),
    If(If),
    Impl(Impl),
    Is(Is),
    Lambda(Lambda),
    LambdaType(LambdaType),
    Let(Let),
    LitBool(LitBool),
    LitChar(LitChar),
    LitFloat(LitFloat),
    LitInt(LitInt),
    LitPattern(LitPattern),
    LitStr(LitStr),
    Match(Match),
    MatchArm(MatchArm),
    Modifier(Modifier),
    ModifierList(ModifierList),
    Module(Module),
    Param(Param),
    Paren(Paren),
    Path(Path),
    PathData(PathData),
    QualifiedPathType(QualifiedPathType),
    RegularType(RegularType),
    Rest(Rest),
    Return(Return),
    Root(Root),
    Struct(Struct),
    Template(Template),
    This(This),
    Trait(Trait),
    Tuple(Tuple),
    TuplePattern(TuplePattern),
    TupleType(TupleType),
    TypeArgument(TypeArgument),
    TypedExpr(TypedExpr),
    TypeParamList(TypeParamList),
    TypeParam(TypeParam),
    Un(Un),
    Underscore(Underscore),
    UpcaseThis(UpcaseThis),
    Use(Use),
    UseGroup(UseGroup),
    UsePath(UsePath),
    UseTargetName(UseTargetName),
    WhereClause(WhereClause),
    WhereClauseItem(WhereClauseItem),
    While(While),
}

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

pub trait AstNodeBase: Sized {
    fn new(file: File, id: AstId) -> Self;
    fn id(&self) -> AstId;
    fn raw_node(&self) -> &Ast;
    fn span(&self) -> Span;
    fn file(&self) -> &File;
    fn children(&self) -> impl Iterator<Item = AstNode>;
}

#[derive(Clone, Debug)]
pub struct AstNode {
    file: File,
    id: AstId,
}

impl AstNodeBase for AstNode {
    fn new(file: File, id: AstId) -> Self {
        AstNode { file, id }
    }

    fn id(&self) -> AstId {
        self.id
    }

    fn raw_node(&self) -> &Ast {
        self.file.node(self.id)
    }

    fn span(&self) -> Span {
        self.raw_node().span()
    }

    fn file(&self) -> &File {
        &self.file
    }

    fn children(&self) -> impl Iterator<Item = AstNode> {
        let children_vec = self.raw_node().children();
        let file = self.file.clone();
        children_vec
            .into_iter()
            .map(move |id| AstNode::new(file.clone(), id))
    }
}

impl PartialEq for AstNode {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && Arc::ptr_eq(&self.file.0, &other.file.0)
    }
}

impl Eq for AstNode {}

pub fn walk_children<V: Visitor, N: AstNodeBase>(v: &mut V, node: N) {
    for child in node.children() {
        visit_node(v, child);
    }
}

pub struct AstIdIterator<'a, T: AstNodeBase> {
    file: File,
    ids: &'a [AstId],
    index: usize,
    _phantom: std::marker::PhantomData<T>,
}

impl<'a, T: AstNodeBase> AstIdIterator<'a, T> {
    pub fn new(file: File, ids: &'a [AstId]) -> Self {
        AstIdIterator {
            file,
            ids,
            index: 0,
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<'a, T: AstNodeBase> Iterator for AstIdIterator<'a, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.ids.len() {
            let id = self.ids[self.index];
            self.index += 1;
            Some(T::new(self.file.clone(), id))
        } else {
            None
        }
    }
}

impl<'a, T: AstNodeBase> ExactSizeIterator for AstIdIterator<'a, T> {
    fn len(&self) -> usize {
        self.ids.len() - self.index
    }
}

#[derive(Clone, Debug, AstNode)]
pub struct Root {
    pub span: Span,
    pub elements: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Alias {
    pub span: Span,

    pub modifiers: Option<AstId>,
    pub name: Option<AstId>,
    pub type_params: Option<AstId>,
    pub pre_where_clause: Option<AstId>,
    pub bounds: Vec<AstId>,
    pub ty: Option<AstId>,
    pub post_where_clause: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Alt {
    pub span: Span,

    pub alts: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Argument {
    pub span: Span,
    pub name: Option<AstId>,
    pub expr: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct Bin {
    pub span: Span,

    pub op: BinOp,
    pub lhs: AstId,
    pub rhs: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct Block {
    pub span: Span,

    pub stmts: Vec<AstId>,
    pub expr: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Break {
    pub span: Span,
}

#[derive(Clone, Debug, AstNode)]
pub struct Call {
    pub span: Span,

    pub callee: AstId,
    pub args: Vec<AstId>,
}

impl Call {
    pub fn object(&self, file: &File) -> Option<AstId> {
        let callee_node = file.node(self.callee);
        if let Some(type_param) = callee_node.to_typed_expr() {
            let node = file.node(type_param.callee);
            if let Some(dot) = node.to_dot() {
                Some(dot.lhs)
            } else {
                None
            }
        } else if let Some(dot) = callee_node.to_dot() {
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
    pub modifiers: Option<AstId>,
    pub name: Option<AstId>,

    pub fields: Vec<AstId>,
    pub type_params: Option<AstId>,
    pub where_clause: Option<AstId>,
    pub field_name_style: FieldNameStyle,
}

#[derive(Clone, Debug, AstNode)]
pub struct Const {
    pub span: Span,
    pub modifiers: Option<AstId>,
    pub name: Option<AstId>,
    pub data_type: AstId,
    pub expr: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct Continue {
    pub span: Span,
}

#[derive(Clone, Debug, AstNode)]
pub struct Conv {
    pub span: Span,

    pub object: AstId,
    pub data_type: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct CtorField {
    pub span: Span,
    pub ident: Option<AstId>,
    pub pattern: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct CtorPattern {
    pub span: Span,
    pub path: AstId,
    pub params: Option<Vec<AstId>>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Dot {
    pub span: Span,
    pub op_span: Span,

    pub lhs: AstId,
    pub rhs: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct Enum {
    pub span: Span,
    pub modifiers: Option<AstId>,
    pub name: Option<AstId>,
    pub type_params: Option<AstId>,
    pub variants: Vec<EnumVariant>,
    pub where_clause: Option<AstId>,
}

#[derive(Clone, Debug)]
pub struct EnumVariant {
    pub span: Span,
    pub name: Option<AstId>,
    pub field_name_style: FieldNameStyle,
    pub fields: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Error {
    pub span: Span,
}

#[derive(Clone, Debug, AstNode)]
pub struct ExprStmt {
    pub span: Span,

    pub expr: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct Extern {
    pub span: Span,
    pub modifiers: Option<AstId>,
    pub name: Option<AstId>,
    pub identifier: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Field {
    pub span: Span,
    pub modifiers: Option<AstId>,
    pub name: Option<AstId>,
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

    pub pattern: AstId,
    pub expr: AstId,
    pub block: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct Function {
    pub declaration_span: Span,
    pub span: Span,
    pub modifiers: Option<AstId>,
    pub kind: FunctionKind,

    #[ast_node_ref(Ident)]
    pub name: Option<AstId>,
    pub type_params: Option<AstId>,
    pub params: Vec<AstId>,
    pub return_type: Option<AstId>,
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
    pub modifiers: Option<AstId>,
    pub name: Option<AstId>,
    pub mutable: bool,
    pub data_type: AstId,
    pub initial_value: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Ident {
    pub span: Span,
    pub name: String,
}

#[derive(Clone, Debug, AstNode)]
pub struct IdentPattern {
    pub span: Span,
    pub mutable: bool,
    pub name: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct If {
    pub span: Span,

    pub cond: AstId,
    pub then_block: AstId,
    pub else_block: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Impl {
    pub declaration_span: Span,
    pub span: Span,

    pub modifiers: Option<AstId>,
    pub type_params: Option<AstId>,
    pub trait_type: Option<AstId>,
    pub extended_type: AstId,
    pub where_clause: Option<AstId>,

    pub methods: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Is {
    pub span: Span,

    pub value: AstId,
    pub pattern: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct Lambda {
    pub span: Span,
    pub fct_id: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct LambdaType {
    pub span: Span,

    pub params: Vec<AstId>,
    pub ret: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Let {
    pub span: Span,

    pub pattern: AstId,

    pub data_type: Option<AstId>,
    pub expr: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct LitBool {
    pub span: Span,

    pub value: bool,
}

#[derive(Clone, Debug, AstNode)]
pub struct LitChar {
    pub span: Span,
    pub value: String,
}

#[derive(Clone, Debug, AstNode)]
pub struct LitFloat {
    pub span: Span,

    pub value: String,
}

#[derive(Clone, Debug, AstNode)]
pub struct LitInt {
    pub span: Span,

    pub value: String,
}

#[derive(Clone, Debug, AstNode)]
pub struct LitPattern {
    pub span: Span,
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

    pub value: String,
}

#[derive(Clone, Debug, AstNode)]
pub struct Match {
    pub span: Span,

    pub expr: AstId,
    pub arms: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct MatchArm {
    pub span: Span,

    pub pattern: AstId,
    pub cond: Option<AstId>,
    pub value: AstId,
}

#[derive(Clone, Debug)]
pub struct PatternError {
    pub span: Span,
}

#[derive(Clone, Debug, AstNode)]
pub struct Modifier {
    pub span: Span,
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
    pub modifiers: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Module {
    pub span: Span,
    pub modifiers: Option<AstId>,
    pub name: Option<AstId>,
    pub elements: Option<Vec<AstId>>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Param {
    pub span: Span,
    pub pattern: AstId,
    pub data_type: AstId,
    pub variadic: bool,
}

#[derive(Clone, Debug, AstNode)]
pub struct Paren {
    pub span: Span,
    pub expr: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct Path {
    pub span: Span,
    pub op_span: Span,

    pub lhs: AstId,
    pub rhs: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct PathData {
    pub span: Span,
    pub segments: Vec<AstId>,
}

#[derive(Clone, Debug)]
pub struct PathSegmentSelf {
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct PathSegmentIdent {
    pub span: Span,
    pub name: AstId,
}

#[derive(Clone, Debug)]
pub struct PatternParam {
    pub span: Span,
    pub mutable: bool,
    pub name: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct QualifiedPathType {
    pub span: Span,

    pub ty: AstId,
    pub trait_ty: AstId,
    pub name: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct RegularType {
    pub span: Span,

    pub path: AstId,
    pub params: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Rest {
    pub span: Span,
}

#[derive(Clone, Debug, AstNode)]
pub struct Return {
    pub span: Span,

    pub expr: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Struct {
    pub span: Span,
    pub modifiers: Option<AstId>,
    pub name: Option<AstId>,
    pub fields: Vec<AstId>,
    pub type_params: Option<AstId>,
    pub where_clause: Option<AstId>,
    pub field_style: FieldNameStyle,
}

#[derive(Clone, Debug, AstNode)]
pub struct Template {
    pub span: Span,

    pub parts: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct This {
    pub span: Span,
}

#[derive(Clone, Debug, AstNode)]
pub struct Trait {
    pub name: Option<AstId>,
    pub modifiers: Option<AstId>,
    pub type_params: Option<AstId>,
    pub bounds: Vec<AstId>,
    pub where_clause: Option<AstId>,
    pub span: Span,
    pub methods: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Tuple {
    pub span: Span,

    pub values: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct TuplePattern {
    pub span: Span,
    pub params: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct TupleType {
    pub span: Span,

    pub subtypes: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct TypeArgument {
    pub span: Span,

    #[ast_node_ref(Ident)]
    pub name: Option<AstId>,
    pub ty: AstId,
}

#[derive(Clone, Debug)]
pub struct TypeGenericType {
    pub span: Span,

    pub path: AstId,
    pub params: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct TypedExpr {
    pub span: Span,
    pub op_span: Span,

    pub callee: AstId,
    pub args: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct TypeParam {
    pub span: Span,
    pub name: Option<AstId>,
    pub bounds: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct TypeParamList {
    pub span: Span,
    pub params: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Un {
    pub span: Span,

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
pub struct Underscore {
    pub span: Span,
}

#[derive(Clone, Debug, AstNode)]
pub struct UpcaseThis {
    pub span: Span,
}

#[derive(Clone, Debug, AstNode)]
pub struct Use {
    pub span: Span,
    pub modifiers: Option<AstId>,
    pub path: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct UseGroup {
    pub span: Span,
    pub targets: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct UsePath {
    pub span: Span,
    pub path: Vec<UseAtom>,
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
pub struct UseAtom {
    pub span: Span,
    pub value: UsePathComponentValue,
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
    pub name: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct WhereClause {
    pub span: Span,
    pub clauses: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct WhereClauseItem {
    pub span: Span,
    pub ty: AstId,
    pub bounds: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct While {
    pub span: Span,

    pub cond: AstId,
    pub block: AstId,
}
