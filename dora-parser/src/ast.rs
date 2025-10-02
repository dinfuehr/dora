use std::sync::Arc;

use dora_parser_derive::{AstEnum, AstNode};
use id_arena::{Arena, Id};

use crate::{Span, TokenKind};

pub mod dump;
pub mod visit;

#[derive(Clone, Debug)]
pub struct File {
    pub content: Arc<String>,
    pub ast_nodes: Arena<Ast>,
    pub elements: Vec<AstId>,
}

impl File {
    pub fn node(&self, id: AstId) -> &Ast {
        &self.ast_nodes[id]
    }

    #[cfg(test)]
    pub fn fct0(&self) -> &Function {
        self.node(self.elements[0]).to_function().unwrap()
    }

    #[cfg(test)]
    pub fn fct(&self, index: usize) -> &Function {
        self.node(self.elements[index]).to_function().unwrap()
    }

    #[cfg(test)]
    pub fn cls0(&self) -> &Class {
        self.node(self.elements[0]).to_class().unwrap()
    }

    #[cfg(test)]
    pub fn cls(&self, index: usize) -> &Class {
        self.node(self.elements[index]).to_class().unwrap()
    }

    #[cfg(test)]
    pub fn struct0(&self) -> &Struct {
        self.node(self.elements[0]).to_struct().unwrap()
    }

    #[cfg(test)]
    pub fn enum0(&self) -> &Enum {
        self.node(self.elements[0]).to_enum().unwrap()
    }

    #[cfg(test)]
    pub fn module0(&self) -> &Module {
        self.node(self.elements[0]).to_module().unwrap()
    }

    #[cfg(test)]
    pub fn trait_(&self, index: usize) -> &Trait {
        self.node(self.elements[index]).to_trait().unwrap()
    }

    #[cfg(test)]
    pub fn trait0(&self) -> &Trait {
        self.node(self.elements[0]).to_trait().unwrap()
    }

    #[cfg(test)]
    pub fn impl0(&self) -> &Impl {
        self.node(self.elements[0]).to_impl().unwrap()
    }

    #[cfg(test)]
    pub fn global0(&self) -> &Global {
        self.node(self.elements[0]).to_global().unwrap()
    }

    #[cfg(test)]
    pub fn const0(&self) -> &Const {
        self.node(self.elements[0]).to_const().unwrap()
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
    Extern(ExternPackage),
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
    MatchArm(Arm),
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
    pub fn create_regular(span: Span, path: AstId, params: Vec<AstId>) -> Ast {
        Ast::RegularType(RegularType { span, path, params })
    }

    pub fn create_qualified_path(
        span: Span,
        ty: AstId,
        trait_ty: AstId,
        name: Option<AstId>,
    ) -> Ast {
        Ast::QualifiedPathType(QualifiedPathType {
            span,
            ty,
            trait_ty,
            name,
        })
    }

    pub fn create_fct(span: Span, params: Vec<AstId>, ret: Option<AstId>) -> Ast {
        Ast::LambdaType(LambdaType { span, params, ret })
    }

    pub fn create_tuple_type(span: Span, subtypes: Vec<AstId>) -> Ast {
        Ast::TupleType(TupleType { span, subtypes })
    }

    #[cfg(test)]
    pub fn is_unit(&self) -> bool {
        match self {
            &Ast::TupleType(ref val) if val.subtypes.len() == 0 => true,
            _ => false,
        }
    }

    pub fn create_let_stmt(
        span: Span,
        pattern: AstId,
        data_type: Option<AstId>,
        expr: Option<AstId>,
    ) -> Ast {
        Ast::Let(Let {
            span,

            pattern,
            data_type,
            expr,
        })
    }

    pub fn create_expr_stmt(span: Span, expr: AstId) -> Ast {
        Ast::ExprStmt(ExprStmt { span, expr })
    }

    pub fn create_block(span: Span, stmts: Vec<AstId>, expr: Option<AstId>) -> Ast {
        Ast::Block(Block { span, stmts, expr })
    }

    pub fn create_if(span: Span, cond: AstId, then_block: AstId, else_block: Option<AstId>) -> Ast {
        Ast::If(If {
            span,
            cond,
            then_block,
            else_block,
        })
    }

    pub fn create_match(span: Span, expr: AstId, arms: Vec<AstId>) -> Ast {
        Ast::Match(Match { span, expr, arms })
    }

    pub fn create_for(span: Span, pattern: AstId, expr: AstId, block: AstId) -> Ast {
        Ast::For(For {
            span,

            pattern,
            expr,
            block,
        })
    }

    pub fn create_while(span: Span, cond: AstId, block: AstId) -> Ast {
        Ast::While(While { span, cond, block })
    }

    pub fn create_return(span: Span, expr: Option<AstId>) -> Ast {
        Ast::Return(Return { span, expr })
    }

    pub fn create_break(span: Span) -> Ast {
        Ast::Break(Break { span })
    }

    pub fn create_continue(span: Span) -> Ast {
        Ast::Continue(Continue { span })
    }

    pub fn create_un(span: Span, op: UnOp, opnd: AstId) -> Ast {
        Ast::Un(Un { span, op, opnd })
    }

    pub fn create_bin(span: Span, op: BinOp, lhs: AstId, rhs: AstId) -> Ast {
        Ast::Bin(Bin { span, op, lhs, rhs })
    }

    pub fn create_conv(span: Span, object: AstId, data_type: AstId) -> Ast {
        Ast::Conv(Conv {
            span,

            object,
            data_type,
        })
    }

    pub fn create_is(span: Span, object: AstId, pattern: AstId) -> Ast {
        Ast::Is(Is {
            span,

            value: object,
            pattern,
        })
    }

    pub fn create_lit_char(span: Span, full_value: String) -> Ast {
        Ast::LitChar(LitChar {
            span,
            value: full_value,
        })
    }

    pub fn create_lit_int(span: Span, value: String) -> Ast {
        Ast::LitInt(LitInt { span, value })
    }

    pub fn create_lit_float(span: Span, value: String) -> Ast {
        Ast::LitFloat(LitFloat { span, value })
    }

    pub fn create_lit_str(span: Span, value: String) -> Ast {
        Ast::LitStr(LitStr { span, value })
    }

    pub fn create_template(span: Span, parts: Vec<AstId>) -> Ast {
        Ast::Template(Template { span, parts })
    }

    pub fn create_lit_bool(span: Span, value: bool) -> Ast {
        Ast::LitBool(LitBool { span, value })
    }

    pub fn create_this(span: Span) -> Ast {
        Ast::This(This { span })
    }

    pub fn create_ident(span: Span, name: String) -> Ast {
        Ast::Ident(Ident { span, name })
    }

    pub fn create_paren(span: Span, expr: AstId) -> Ast {
        Ast::Paren(Paren { span, expr })
    }

    pub fn create_call(span: Span, callee: AstId, args: Vec<AstId>) -> Ast {
        Ast::Call(Call { span, callee, args })
    }

    pub fn create_type_param(span: Span, op_span: Span, callee: AstId, args: Vec<AstId>) -> Ast {
        Ast::TypedExpr(TypedExpr {
            span,
            op_span,

            callee,
            args,
        })
    }

    pub fn create_path(span: Span, op_span: Span, lhs: AstId, rhs: AstId) -> Ast {
        Ast::Path(Path {
            span,
            op_span,
            lhs,
            rhs,
        })
    }

    pub fn create_dot(span: Span, op_span: Span, lhs: AstId, rhs: AstId) -> Ast {
        Ast::Dot(Dot {
            span,
            op_span,

            lhs,
            rhs,
        })
    }

    pub fn create_lambda(span: Span, fct_id: AstId) -> Ast {
        Ast::Lambda(Lambda { span, fct_id })
    }

    pub fn create_tuple(span: Span, values: Vec<AstId>) -> Ast {
        Ast::Tuple(Tuple { span, values })
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
pub struct Module {
    pub span: Span,
    pub modifiers: Option<AstId>,
    pub name: Option<AstId>,
    pub elements: Option<Vec<AstId>>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Use {
    pub span: Span,
    pub modifiers: Option<AstId>,
    pub path: AstId,
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

#[derive(Clone, Debug, AstNode)]
pub struct UseGroup {
    pub span: Span,
    pub targets: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct UseTargetName {
    pub span: Span,
    pub name: Option<AstId>,
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
pub struct Const {
    pub span: Span,
    pub modifiers: Option<AstId>,
    pub name: Option<AstId>,
    pub data_type: AstId,
    pub expr: AstId,
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
pub struct Struct {
    pub span: Span,
    pub modifiers: Option<AstId>,
    pub name: Option<AstId>,
    pub fields: Vec<AstId>,
    pub type_params: Option<AstId>,
    pub where_clause: Option<AstId>,
    pub field_style: FieldNameStyle,
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
pub struct TupleType {
    pub span: Span,

    pub subtypes: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct LambdaType {
    pub span: Span,

    pub params: Vec<AstId>,
    pub ret: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct RegularType {
    pub span: Span,

    pub path: AstId,
    pub params: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct TypeArgument {
    pub span: Span,

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
pub struct QualifiedPathType {
    pub span: Span,

    pub ty: AstId,
    pub trait_ty: AstId,
    pub name: Option<AstId>,
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
pub struct ExternPackage {
    pub span: Span,
    pub modifiers: Option<AstId>,
    pub name: Option<AstId>,
    pub identifier: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct TypeParamList {
    pub span: Span,
    pub params: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct TypeParam {
    pub span: Span,
    pub name: Option<AstId>,
    pub bounds: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Field {
    pub span: Span,
    pub modifiers: Option<AstId>,
    pub name: Option<AstId>,
    pub data_type: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct Error {
    pub span: Span,
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
pub struct Function {
    pub declaration_span: Span,
    pub span: Span,
    pub modifiers: Option<AstId>,
    pub kind: FunctionKind,

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

// remove in next step
#[derive(Clone, Debug, AstNode)]
pub struct ModifierList {
    pub span: Span,
    pub modifiers: Vec<AstId>,
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

#[derive(Clone, Debug, AstNode)]
pub struct Param {
    pub span: Span,
    pub pattern: AstId,
    pub data_type: AstId,
    pub variadic: bool,
}

#[derive(Clone, Debug, AstNode)]
pub struct Let {
    pub span: Span,

    pub pattern: AstId,

    pub data_type: Option<AstId>,
    pub expr: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct For {
    pub span: Span,

    pub pattern: AstId,
    pub expr: AstId,
    pub block: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct While {
    pub span: Span,

    pub cond: AstId,
    pub block: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct ExprStmt {
    pub span: Span,

    pub expr: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct Return {
    pub span: Span,

    pub expr: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Break {
    pub span: Span,
}

#[derive(Clone, Debug, AstNode)]
pub struct Continue {
    pub span: Span,
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
pub struct Lambda {
    pub span: Span,
    pub fct_id: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct If {
    pub span: Span,

    pub cond: AstId,
    pub then_block: AstId,
    pub else_block: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Tuple {
    pub span: Span,

    pub values: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Conv {
    pub span: Span,

    pub object: AstId,
    pub data_type: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct Is {
    pub span: Span,

    pub value: AstId,
    pub pattern: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct Un {
    pub span: Span,

    pub op: UnOp,
    pub opnd: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct Bin {
    pub span: Span,

    pub op: BinOp,
    pub lhs: AstId,
    pub rhs: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct LitChar {
    pub span: Span,
    pub value: String,
}

#[derive(Clone, Debug, AstNode)]
pub struct LitInt {
    pub span: Span,

    pub value: String,
}

#[derive(Clone, Debug, AstNode)]
pub struct LitFloat {
    pub span: Span,

    pub value: String,
}

#[derive(Clone, Debug, AstNode)]
pub struct LitStr {
    pub span: Span,

    pub value: String,
}

#[derive(Clone, Debug, AstNode)]
pub struct Template {
    pub span: Span,

    pub parts: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct LitBool {
    pub span: Span,

    pub value: bool,
}

#[derive(Clone, Debug, AstNode)]
pub struct Block {
    pub span: Span,

    pub stmts: Vec<AstId>,
    pub expr: Option<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct This {
    pub span: Span,
}

#[derive(Clone, Debug, AstNode)]
pub struct UpcaseThis {
    pub span: Span,
}

#[derive(Clone, Debug, AstNode)]
pub struct Ident {
    pub span: Span,
    pub name: String,
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
pub struct Argument {
    pub span: Span,
    pub name: Option<AstId>,
    pub expr: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct Paren {
    pub span: Span,
    pub expr: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct Match {
    pub span: Span,

    pub expr: AstId,
    pub arms: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Arm {
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
pub struct Alt {
    pub span: Span,

    pub alts: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Underscore {
    pub span: Span,
}

#[derive(Clone, Debug, AstNode)]
pub struct Rest {
    pub span: Span,
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
pub struct IdentPattern {
    pub span: Span,
    pub mutable: bool,
    pub name: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct TuplePattern {
    pub span: Span,
    pub params: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct CtorPattern {
    pub span: Span,
    pub path: AstId,
    pub params: Option<Vec<AstId>>,
}

#[derive(Clone, Debug, AstNode)]
pub struct CtorField {
    pub span: Span,
    pub ident: Option<AstId>,
    pub pattern: AstId,
}

#[derive(Clone, Debug)]
pub struct PatternParam {
    pub span: Span,
    pub mutable: bool,
    pub name: Option<AstId>,
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

#[derive(Clone, Debug, AstNode)]
pub struct TypedExpr {
    pub span: Span,
    pub op_span: Span,

    pub callee: AstId,
    pub args: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Path {
    pub span: Span,
    pub op_span: Span,

    pub lhs: AstId,
    pub rhs: AstId,
}

#[derive(Clone, Debug, AstNode)]
pub struct Dot {
    pub span: Span,
    pub op_span: Span,

    pub lhs: AstId,
    pub rhs: AstId,
}
