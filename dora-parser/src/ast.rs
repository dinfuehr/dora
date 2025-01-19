use std::fmt;
use std::slice::Iter;
use std::sync::Arc;

use crate::green::{GreenNode, GreenToken};
use crate::token::TokenKind::*;
use crate::{Span, TokenKind};

pub mod dump;
pub mod visit;

#[derive(Clone, Debug)]
pub struct File {
    pub green: GreenNode,
    pub elements: Vec<Elem>,
}

impl File {
    #[cfg(test)]
    pub fn fct0(&self) -> &Function {
        self.elements[0].to_function().unwrap()
    }

    #[cfg(test)]
    pub fn fct(&self, index: usize) -> &Function {
        self.elements[index].to_function().unwrap()
    }

    #[cfg(test)]
    pub fn cls0(&self) -> &Class {
        self.elements[0].to_class().unwrap()
    }

    #[cfg(test)]
    pub fn cls(&self, index: usize) -> &Class {
        self.elements[index].to_class().unwrap()
    }

    #[cfg(test)]
    pub fn struct0(&self) -> &Struct {
        self.elements[0].to_struct().unwrap()
    }

    #[cfg(test)]
    pub fn enum0(&self) -> &Enum {
        self.elements[0].to_enum().unwrap()
    }

    #[cfg(test)]
    pub fn module0(&self) -> &Module {
        self.elements[0].to_module().unwrap()
    }

    #[cfg(test)]
    pub fn trait_(&self, index: usize) -> &Trait {
        self.elements[index].to_trait().unwrap()
    }

    #[cfg(test)]
    pub fn trait0(&self) -> &Trait {
        self.elements[0].to_trait().unwrap()
    }

    #[cfg(test)]
    pub fn impl0(&self) -> &Impl {
        self.elements[0].to_impl().unwrap()
    }

    #[cfg(test)]
    pub fn global0(&self) -> &Global {
        self.elements[0].to_global().unwrap()
    }

    #[cfg(test)]
    pub fn const0(&self) -> &Const {
        self.elements[0].to_const().unwrap()
    }
}

#[derive(PartialEq, Eq, Copy, Clone, Debug, Hash)]
pub struct NodeId(pub usize);

impl fmt::Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#{}", self.0)
    }
}

pub type Elem = Arc<ElemData>;

#[derive(Clone, Debug)]
pub enum ElemData {
    Function(Arc<Function>),
    Class(Arc<Class>),
    Struct(Arc<Struct>),
    Trait(Arc<Trait>),
    Impl(Arc<Impl>),
    Global(Arc<Global>),
    Const(Arc<Const>),
    Enum(Arc<Enum>),
    Module(Arc<Module>),
    Use(Arc<Use>),
    Extern(Arc<ExternPackage>),
    Alias(Arc<Alias>),
    Error { id: NodeId, span: Span },
}

impl ElemData {
    pub fn span(&self) -> Span {
        match self {
            ElemData::Function(ref node) => node.span,
            ElemData::Class(ref node) => node.span,
            ElemData::Struct(ref node) => node.span,
            ElemData::Trait(ref node) => node.span,
            ElemData::Impl(ref node) => node.span,
            ElemData::Global(ref node) => node.span,
            ElemData::Const(ref node) => node.span,
            ElemData::Enum(ref node) => node.span,
            ElemData::Module(ref node) => node.span,
            ElemData::Use(ref node) => node.span,
            ElemData::Extern(ref node) => node.span,
            ElemData::Alias(ref node) => node.span,
            ElemData::Error { span, .. } => span.clone(),
        }
    }

    pub fn to_function(&self) -> Option<&Function> {
        match self {
            &ElemData::Function(ref fct) => Some(fct),
            _ => None,
        }
    }

    pub fn to_class(&self) -> Option<&Class> {
        match self {
            &ElemData::Class(ref class) => Some(class),
            _ => None,
        }
    }

    pub fn to_enum(&self) -> Option<&Enum> {
        match self {
            &ElemData::Enum(ref enum_) => Some(enum_),
            _ => None,
        }
    }

    pub fn to_module(&self) -> Option<&Module> {
        match self {
            &ElemData::Module(ref module) => Some(module),
            _ => None,
        }
    }

    pub fn to_struct(&self) -> Option<&Struct> {
        match self {
            &ElemData::Struct(ref struc) => Some(struc),
            _ => None,
        }
    }

    pub fn to_trait(&self) -> Option<&Trait> {
        match self {
            &ElemData::Trait(ref trait_) => Some(trait_),
            _ => None,
        }
    }

    pub fn to_impl(&self) -> Option<&Impl> {
        match self {
            &ElemData::Impl(ref impl_) => Some(impl_),
            _ => None,
        }
    }

    pub fn to_global(&self) -> Option<&Global> {
        match self {
            &ElemData::Global(ref global) => Some(global),
            _ => None,
        }
    }

    pub fn to_const(&self) -> Option<&Const> {
        match self {
            &ElemData::Const(ref konst) => Some(konst),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct IdentData {
    pub span: Span,
    pub name_as_string: String,
}

pub type Ident = Arc<IdentData>;

#[derive(Clone, Debug)]
pub struct Global {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub modifiers: Option<ModifierList>,
    pub name: Option<Ident>,
    pub mutable: bool,
    pub data_type: Type,
    pub initial_value: Option<Expr>,
}

#[derive(Clone, Debug)]
pub struct Module {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub modifiers: Option<ModifierList>,
    pub name: Option<Ident>,
    pub elements: Option<Vec<Elem>>,
}

#[derive(Clone, Debug)]
pub struct Use {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub modifiers: Option<ModifierList>,
    pub path: Arc<UsePath>,
}

#[derive(Clone, Debug)]
pub struct UsePath {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub path: Vec<UseAtom>,
    pub target: UsePathDescriptor,
}

#[derive(Clone, Debug)]
pub enum UsePathDescriptor {
    Default,
    As(UseTargetName),
    Group(Arc<UseGroup>),
    Error,
}

#[derive(Clone, Debug)]
pub struct UseGroup {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub targets: Vec<Arc<UsePath>>,
}

#[derive(Clone, Debug)]
pub struct UseTargetName {
    pub green: GreenNode,
    pub span: Span,
    pub name: Option<Ident>,
}

#[derive(Clone, Debug)]
pub struct UseAtom {
    pub green: GreenNode,
    pub span: Span,
    pub value: UsePathComponentValue,
}

#[derive(Clone, Debug)]
pub enum UsePathComponentValue {
    This,
    Super,
    Package,
    Name(Ident),
    Error,
}

#[derive(Clone, Debug)]
pub struct Const {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub modifiers: Option<ModifierList>,
    pub name: Option<Ident>,
    pub data_type: Type,
    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub struct Enum {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub modifiers: Option<ModifierList>,
    pub name: Option<Ident>,
    pub type_params: Option<TypeParams>,
    pub variants: Vec<EnumVariant>,
    pub where_bounds: Option<WhereBounds>,
}

#[derive(Clone, Debug)]
pub struct EnumVariant {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub name: Option<Ident>,
    pub field_name_style: FieldNameStyle,
    pub fields: Vec<Arc<Field>>,
}

#[derive(Clone, Debug)]
pub struct Struct {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub modifiers: Option<ModifierList>,
    pub name: Option<Ident>,
    pub fields: Vec<Arc<Field>>,
    pub type_params: Option<TypeParams>,
    pub where_bounds: Option<WhereBounds>,
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

pub type WhereBounds = Arc<WhereBoundsData>;

#[derive(Clone, Debug)]
pub struct WhereBoundsData {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub clauses: Vec<WhereClause>,
}

pub type WhereClause = Arc<WhereBoundData>;

#[derive(Clone, Debug)]
pub struct WhereBoundData {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub ty: Type,
    pub bounds: Vec<Type>,
}

pub type Type = Arc<TypeData>;

#[derive(Clone, Debug)]
pub enum TypeData {
    Regular(TypeRegularType),
    Tuple(TypeTupleType),
    Lambda(TypeLambdaType),
    Error { id: NodeId, span: Span },
}

#[derive(Clone, Debug)]
pub struct TypeTupleType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub subtypes: Vec<Type>,
}

#[derive(Clone, Debug)]
pub struct TypeLambdaType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub params: Vec<Type>,
    pub ret: Option<Type>,
}

#[derive(Clone, Debug)]
pub struct TypeRegularType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub path: Path,
    pub params: Vec<Arc<TypeArgument>>,
}

#[derive(Clone, Debug)]
pub struct TypeArgument {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub name: Option<Ident>,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub struct TypePathType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub path: Path,
}

#[derive(Clone, Debug)]
pub struct TypeGenericType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub path: Type,
    pub params: Vec<Type>,
}

impl TypeRegularType {
    #[cfg(test)]
    pub fn name(&self) -> &str {
        assert_eq!(self.path.segments.len(), 1);
        self.path
            .segments
            .last()
            .expect("missing segment")
            .as_name_str()
    }
}

impl TypeData {
    pub fn create_regular(
        id: NodeId,
        span: Span,
        green: GreenNode,
        path: Path,
        params: Vec<Arc<TypeArgument>>,
    ) -> TypeData {
        TypeData::Regular(TypeRegularType {
            id,
            span,
            green,
            path,
            params,
        })
    }

    pub fn create_fct(
        id: NodeId,
        span: Span,
        green: GreenNode,
        params: Vec<Type>,
        ret: Option<Type>,
    ) -> TypeData {
        TypeData::Lambda(TypeLambdaType {
            id,
            span,
            green,
            params,
            ret,
        })
    }

    pub fn create_tuple(id: NodeId, span: Span, green: GreenNode, subtypes: Vec<Type>) -> TypeData {
        TypeData::Tuple(TypeTupleType {
            id,
            span,
            green,
            subtypes,
        })
    }

    pub fn to_regular(&self) -> Option<&TypeRegularType> {
        match *self {
            TypeData::Regular(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_tuple(&self) -> Option<&TypeTupleType> {
        match *self {
            TypeData::Tuple(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_fct(&self) -> Option<&TypeLambdaType> {
        match *self {
            TypeData::Lambda(ref val) => Some(val),
            _ => None,
        }
    }

    #[cfg(test)]
    pub fn is_unit(&self) -> bool {
        match self {
            &TypeData::Tuple(ref val) if val.subtypes.len() == 0 => true,
            _ => false,
        }
    }

    #[cfg(test)]
    pub fn to_string(&self) -> String {
        match *self {
            TypeData::Regular(ref val) => val.name().to_string(),

            TypeData::Tuple(ref val) => {
                let types: Vec<String> = val.subtypes.iter().map(|t| t.to_string()).collect();

                format!("({})", types.join(", "))
            }

            TypeData::Lambda(ref val) => {
                let types: Vec<String> = val.params.iter().map(|t| t.to_string()).collect();

                if let Some(ref ret) = val.ret {
                    let ret = ret.to_string();
                    format!("({}) -> {}", types.join(", "), ret)
                } else {
                    format!("({}) -> ()", types.join(", "))
                }
            }

            TypeData::Error { .. } => "error type".into(),
        }
    }

    pub fn span(&self) -> Span {
        match *self {
            TypeData::Regular(ref val) => val.span,
            TypeData::Tuple(ref val) => val.span,
            TypeData::Lambda(ref val) => val.span,
            TypeData::Error { span, .. } => span,
        }
    }

    pub fn id(&self) -> NodeId {
        match *self {
            TypeData::Regular(ref val) => val.id,
            TypeData::Tuple(ref val) => val.id,
            TypeData::Lambda(ref val) => val.id,
            TypeData::Error { id, .. } => id,
        }
    }

    #[cfg(test)]
    pub fn name(&self) -> &str {
        match *self {
            TypeData::Regular(ref regular) => regular.name(),
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Impl {
    pub id: NodeId,
    pub declaration_span: Span,
    pub span: Span,
    pub green: GreenNode,

    pub modifiers: Option<ModifierList>,
    pub type_params: Option<TypeParams>,
    pub trait_type: Option<Type>,
    pub extended_type: Type,
    pub where_bounds: Option<WhereBounds>,

    pub methods: Vec<Elem>,
}

#[derive(Clone, Debug)]
pub struct Trait {
    pub id: NodeId,
    pub name: Option<Ident>,
    pub green: GreenNode,
    pub modifiers: Option<ModifierList>,
    pub type_params: Option<TypeParams>,
    pub bounds: Vec<Type>,
    pub where_bounds: Option<WhereBounds>,
    pub span: Span,
    pub methods: Vec<Elem>,
}

#[derive(Clone, Debug)]
pub struct Alias {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub modifiers: Option<ModifierList>,
    pub name: Option<Ident>,
    pub type_params: Option<TypeParams>,
    pub pre_where_bounds: Option<WhereBounds>,
    pub bounds: Vec<Type>,
    pub ty: Option<Type>,
    pub post_where_bounds: Option<WhereBounds>,
}

#[derive(Clone, Debug)]
pub struct Class {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub modifiers: Option<ModifierList>,
    pub name: Option<Ident>,

    pub fields: Vec<Arc<Field>>,
    pub type_params: Option<TypeParams>,
    pub where_bounds: Option<WhereBounds>,
    pub field_name_style: FieldNameStyle,
}

#[derive(Clone, Debug)]
pub struct ExternPackage {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub modifiers: Option<ModifierList>,
    pub name: Option<Ident>,
    pub identifier: Option<Ident>,
}

#[derive(Clone, Debug)]
pub struct TypeParams {
    pub span: Span,
    pub params: Vec<TypeParam>,
}

#[derive(Clone, Debug)]
pub struct TypeParam {
    pub span: Span,
    pub name: Option<Ident>,
    pub bounds: Vec<Type>,
}

#[derive(Clone, Debug)]
pub struct Field {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub modifiers: Option<ModifierList>,
    pub name: Option<Ident>,
    pub data_type: Type,
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

#[derive(Clone, Debug)]
pub struct Function {
    pub id: NodeId,
    pub declaration_span: Span,
    pub span: Span,
    pub green: GreenNode,
    pub modifiers: Option<ModifierList>,
    pub kind: FunctionKind,

    pub name: Option<Ident>,
    pub type_params: Option<TypeParams>,
    pub params: Vec<Arc<Param>>,
    pub return_type: Option<Type>,
    pub where_bounds: Option<WhereBounds>,
    pub block: Option<Expr>,
}

impl Function {
    pub fn block(&self) -> &Expr {
        self.block.as_ref().unwrap()
    }
}

// remove in next step
#[derive(Clone, Debug)]
pub struct ModifierList {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub modifiers: Vec<Modifier>,
}

impl ModifierList {
    pub fn iter(&self) -> Iter<Modifier> {
        self.modifiers.iter()
    }
}

#[derive(Clone, Debug)]
pub struct Modifier {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
}

impl Modifier {
    pub fn pub_token(&self) -> Option<GreenToken> {
        find_token(&self.green, PUB_KW)
    }

    pub fn static_token(&self) -> Option<GreenToken> {
        find_token(&self.green, STATIC_KW)
    }

    pub fn at_token(&self) -> Option<GreenToken> {
        find_token(&self.green, AT)
    }

    pub fn ident_token(&self) -> Option<GreenToken> {
        find_token(&self.green, IDENTIFIER)
    }
}

#[derive(Clone, Debug)]
pub struct Param {
    pub id: NodeId,
    pub span: Span,
    pub name: Option<Ident>,
    pub mutable: bool,
    pub data_type: Type,
    pub variadic: bool,
}

pub type Stmt = Arc<StmtData>;

#[derive(Clone, Debug)]
pub enum StmtData {
    Let(StmtLetType),
    Expr(StmtExprType),
}

impl StmtData {
    pub fn create_let(
        id: NodeId,
        span: Span,
        pattern: Arc<Pattern>,
        data_type: Option<Type>,
        expr: Option<Expr>,
    ) -> StmtData {
        StmtData::Let(StmtLetType {
            id,
            span,

            pattern,
            data_type,
            expr,
        })
    }

    pub fn create_expr(id: NodeId, span: Span, expr: Expr) -> StmtData {
        StmtData::Expr(StmtExprType { id, span, expr })
    }

    pub fn span(&self) -> Span {
        match *self {
            StmtData::Let(ref stmt) => stmt.span,
            StmtData::Expr(ref stmt) => stmt.span,
        }
    }

    pub fn to_let(&self) -> Option<&StmtLetType> {
        match *self {
            StmtData::Let(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_let(&self) -> bool {
        match *self {
            StmtData::Let(_) => true,
            _ => false,
        }
    }

    pub fn to_expr(&self) -> Option<&StmtExprType> {
        match *self {
            StmtData::Expr(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_expr(&self) -> bool {
        match *self {
            StmtData::Expr(_) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct StmtLetType {
    pub id: NodeId,
    pub span: Span,

    pub pattern: Arc<Pattern>,

    pub data_type: Option<Type>,
    pub expr: Option<Expr>,
}

#[derive(Clone, Debug)]
pub struct ExprForType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub pattern: Arc<Pattern>,
    pub expr: Expr,
    pub block: Expr,
}

#[derive(Clone, Debug)]
pub struct ExprWhileType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub cond: Expr,
    pub block: Expr,
}

#[derive(Clone, Debug)]
pub struct StmtExprType {
    pub id: NodeId,
    pub span: Span,

    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub struct ExprReturnType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub expr: Option<Expr>,
}

#[derive(Clone, Debug)]
pub struct ExprBreakType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
}

#[derive(Clone, Debug)]
pub struct ExprContinueType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
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

pub type Expr = Arc<ExprData>;

#[derive(Clone, Debug)]
pub enum ExprData {
    Un(ExprUnType),
    Bin(ExprBinType),
    LitChar(ExprLitCharType),
    LitInt(ExprLitIntType),
    LitFloat(ExprLitFloatType),
    LitStr(ExprLitStrType),
    Template(ExprTemplateType),
    LitBool(ExprLitBoolType),
    Ident(ExprIdentType),
    Call(ExprCallType),
    TypeParam(ExprTypeParamType),
    Path(ExprPathType),
    Dot(ExprDotType),
    This(ExprSelfType),
    Conv(ExprConvType),
    Is(ExprIsType),
    Lambda(Arc<Function>),
    Block(ExprBlockType),
    If(ExprIfType),
    For(ExprForType),
    While(ExprWhileType),
    Tuple(ExprTupleType),
    Paren(ExprParenType),
    Match(ExprMatchType),
    Break(ExprBreakType),
    Continue(ExprContinueType),
    Return(ExprReturnType),
    Error { id: NodeId, span: Span },
}

impl ExprData {
    pub fn create_block(
        id: NodeId,
        span: Span,
        green: GreenNode,
        stmts: Vec<Stmt>,
        expr: Option<Expr>,
    ) -> ExprData {
        ExprData::Block(ExprBlockType {
            id,
            span,
            green,

            stmts,
            expr,
        })
    }

    pub fn create_if(
        id: NodeId,
        span: Span,
        green: GreenNode,
        cond: Expr,
        then_block: Expr,
        else_block: Option<Expr>,
    ) -> ExprData {
        ExprData::If(ExprIfType {
            id,
            span,
            green,
            cond,
            then_block,
            else_block,
        })
    }

    pub fn create_match(
        id: NodeId,
        span: Span,
        green: GreenNode,
        expr: Expr,
        arms: Vec<Arc<MatchArmType>>,
    ) -> ExprData {
        ExprData::Match(ExprMatchType {
            id,
            span,
            green,
            expr,
            arms,
        })
    }

    pub fn create_for(
        id: NodeId,
        span: Span,
        green: GreenNode,
        pattern: Arc<Pattern>,
        expr: Expr,
        block: Expr,
    ) -> ExprData {
        ExprData::For(ExprForType {
            id,
            span,
            green,

            pattern,
            expr,
            block,
        })
    }

    pub fn create_while(
        id: NodeId,
        span: Span,
        green: GreenNode,
        cond: Expr,
        block: Expr,
    ) -> ExprData {
        ExprData::While(ExprWhileType {
            id,
            span,
            green,

            cond,
            block,
        })
    }

    pub fn create_return(id: NodeId, span: Span, green: GreenNode, expr: Option<Expr>) -> ExprData {
        ExprData::Return(ExprReturnType {
            id,
            span,
            green,
            expr,
        })
    }

    pub fn create_break(id: NodeId, span: Span, green: GreenNode) -> ExprData {
        ExprData::Break(ExprBreakType { id, span, green })
    }

    pub fn create_continue(id: NodeId, span: Span, green: GreenNode) -> ExprData {
        ExprData::Continue(ExprContinueType { id, span, green })
    }

    pub fn create_un(id: NodeId, span: Span, green: GreenNode, op: UnOp, opnd: Expr) -> ExprData {
        ExprData::Un(ExprUnType {
            id,
            span,
            green,
            op,
            opnd,
        })
    }

    pub fn create_bin(id: NodeId, span: Span, op: BinOp, lhs: Expr, rhs: Expr) -> ExprData {
        ExprData::Bin(ExprBinType {
            id,
            span,

            op,
            lhs,
            rhs,
        })
    }

    pub fn create_conv(id: NodeId, span: Span, object: Expr, data_type: Type) -> ExprData {
        ExprData::Conv(ExprConvType {
            id,
            span,

            object,
            data_type,
        })
    }

    pub fn create_is(id: NodeId, span: Span, object: Expr, pattern: Arc<Pattern>) -> ExprData {
        ExprData::Is(ExprIsType {
            id,
            span,

            value: object,
            pattern,
        })
    }

    pub fn create_lit_char(
        id: NodeId,
        span: Span,
        green: GreenNode,
        full_value: String,
    ) -> ExprData {
        ExprData::LitChar(ExprLitCharType {
            id,
            span,
            green,
            value: full_value,
        })
    }

    pub fn create_lit_int(id: NodeId, span: Span, green: GreenNode, value: String) -> ExprData {
        ExprData::LitInt(ExprLitIntType {
            id,
            span,
            green,
            value,
        })
    }

    pub fn create_lit_float(id: NodeId, span: Span, green: GreenNode, value: String) -> ExprData {
        ExprData::LitFloat(ExprLitFloatType {
            id,
            span,
            green,
            value,
        })
    }

    pub fn create_lit_str(id: NodeId, span: Span, green: GreenNode, value: String) -> ExprData {
        ExprData::LitStr(ExprLitStrType {
            id,
            span,
            green,
            value,
        })
    }

    pub fn create_template(id: NodeId, span: Span, green: GreenNode, parts: Vec<Expr>) -> ExprData {
        ExprData::Template(ExprTemplateType {
            id,
            span,
            green,
            parts,
        })
    }

    pub fn create_lit_bool(id: NodeId, span: Span, value: bool) -> ExprData {
        ExprData::LitBool(ExprLitBoolType { id, span, value })
    }

    pub fn create_this(id: NodeId, span: Span, green: GreenNode) -> ExprData {
        ExprData::This(ExprSelfType { id, span, green })
    }

    pub fn create_ident(id: NodeId, span: Span, green: GreenNode, name: String) -> ExprData {
        ExprData::Ident(ExprIdentType {
            id,
            span,
            green,
            name,
        })
    }

    pub fn create_paren(id: NodeId, span: Span, green: GreenNode, expr: Expr) -> ExprData {
        ExprData::Paren(ExprParenType {
            id,
            span,
            green,
            expr,
        })
    }

    pub fn create_call(id: NodeId, span: Span, callee: Expr, args: Vec<Arc<Argument>>) -> ExprData {
        ExprData::Call(ExprCallType {
            id,
            span,

            callee,
            args,
        })
    }

    pub fn create_type_param(
        id: NodeId,
        span: Span,
        op_span: Span,
        callee: Expr,
        args: Vec<Type>,
    ) -> ExprData {
        ExprData::TypeParam(ExprTypeParamType {
            id,
            span,
            op_span,

            callee,
            args,
        })
    }

    pub fn create_path(id: NodeId, span: Span, op_span: Span, lhs: Expr, rhs: Expr) -> ExprData {
        ExprData::Path(ExprPathType {
            id,
            span,
            op_span,
            lhs,
            rhs,
        })
    }

    pub fn create_dot(id: NodeId, span: Span, op_span: Span, lhs: Expr, rhs: Expr) -> ExprData {
        ExprData::Dot(ExprDotType {
            id,
            span,
            op_span,

            lhs,
            rhs,
        })
    }

    pub fn create_lambda(fct: Arc<Function>) -> ExprData {
        ExprData::Lambda(fct)
    }

    pub fn create_tuple(id: NodeId, span: Span, green: GreenNode, values: Vec<Expr>) -> ExprData {
        ExprData::Tuple(ExprTupleType {
            id,
            span,
            green,
            values,
        })
    }

    pub fn to_for(&self) -> Option<&ExprForType> {
        match *self {
            ExprData::For(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_for(&self) -> bool {
        match *self {
            ExprData::For(_) => true,
            _ => false,
        }
    }

    pub fn to_while(&self) -> Option<&ExprWhileType> {
        match *self {
            ExprData::While(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_while(&self) -> bool {
        match *self {
            ExprData::While(_) => true,
            _ => false,
        }
    }

    pub fn to_un(&self) -> Option<&ExprUnType> {
        match *self {
            ExprData::Un(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_un(&self) -> bool {
        match *self {
            ExprData::Un(_) => true,
            _ => false,
        }
    }

    pub fn is_un_op(&self, op: UnOp) -> bool {
        match *self {
            ExprData::Un(ref e) if e.op == op => true,
            _ => false,
        }
    }

    pub fn to_bin(&self) -> Option<&ExprBinType> {
        match *self {
            ExprData::Bin(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_bin(&self) -> bool {
        match *self {
            ExprData::Bin(_) => true,
            _ => false,
        }
    }

    pub fn is_is(&self) -> bool {
        match *self {
            ExprData::Is(_) => true,
            _ => false,
        }
    }

    pub fn to_is(&self) -> Option<&ExprIsType> {
        match *self {
            ExprData::Is(ref e) => Some(e),
            _ => None,
        }
    }

    pub fn to_paren(&self) -> Option<&ExprParenType> {
        match *self {
            ExprData::Paren(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_paren(&self) -> bool {
        match *self {
            ExprData::Paren(_) => true,
            _ => false,
        }
    }

    pub fn to_ident(&self) -> Option<&ExprIdentType> {
        match *self {
            ExprData::Ident(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_ident(&self) -> bool {
        match *self {
            ExprData::Ident(_) => true,
            _ => false,
        }
    }

    pub fn to_call(&self) -> Option<&ExprCallType> {
        match *self {
            ExprData::Call(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_call(&self) -> bool {
        match *self {
            ExprData::Call(_) => true,
            _ => false,
        }
    }

    pub fn to_path(&self) -> Option<&ExprPathType> {
        match *self {
            ExprData::Path(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_path(&self) -> bool {
        match *self {
            ExprData::Path(_) => true,
            _ => false,
        }
    }

    pub fn to_type_param(&self) -> Option<&ExprTypeParamType> {
        match *self {
            ExprData::TypeParam(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_type_param(&self) -> bool {
        match *self {
            ExprData::TypeParam(_) => true,
            _ => false,
        }
    }

    pub fn to_lit_char(&self) -> Option<&ExprLitCharType> {
        match *self {
            ExprData::LitChar(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_lit_char(&self) -> bool {
        match *self {
            ExprData::LitChar(_) => true,
            _ => false,
        }
    }

    pub fn to_lit_int(&self) -> Option<&ExprLitIntType> {
        match *self {
            ExprData::LitInt(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_lit_int(&self) -> bool {
        match *self {
            ExprData::LitInt(_) => true,
            _ => false,
        }
    }

    pub fn to_template(&self) -> Option<&ExprTemplateType> {
        match *self {
            ExprData::Template(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_template(&self) -> bool {
        match *self {
            ExprData::Template(_) => true,
            _ => false,
        }
    }

    pub fn to_lit_float(&self) -> Option<&ExprLitFloatType> {
        match *self {
            ExprData::LitFloat(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_lit_float(&self) -> bool {
        match *self {
            ExprData::LitFloat(_) => true,
            _ => false,
        }
    }

    pub fn to_lit_str(&self) -> Option<&ExprLitStrType> {
        match *self {
            ExprData::LitStr(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_lit_str(&self) -> bool {
        match *self {
            ExprData::LitStr(_) => true,
            _ => false,
        }
    }

    pub fn to_lit_bool(&self) -> Option<&ExprLitBoolType> {
        match *self {
            ExprData::LitBool(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_lit_bool(&self) -> bool {
        match *self {
            ExprData::LitBool(_) => true,
            _ => false,
        }
    }

    pub fn is_lit_true(&self) -> bool {
        match *self {
            ExprData::LitBool(ref lit) if lit.value => true,
            _ => false,
        }
    }

    pub fn to_dot(&self) -> Option<&ExprDotType> {
        match *self {
            ExprData::Dot(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_dot(&self) -> bool {
        match *self {
            ExprData::Dot(_) => true,
            _ => false,
        }
    }

    pub fn is_this(&self) -> bool {
        match *self {
            ExprData::This(_) => true,
            _ => false,
        }
    }

    pub fn to_conv(&self) -> Option<&ExprConvType> {
        match *self {
            ExprData::Conv(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_conv(&self) -> bool {
        match *self {
            ExprData::Conv(_) => true,
            _ => false,
        }
    }

    pub fn to_lambda(&self) -> Option<Arc<Function>> {
        match *self {
            ExprData::Lambda(ref val) => Some(val.clone()),
            _ => None,
        }
    }

    pub fn is_lambda(&self) -> bool {
        match self {
            &ExprData::Lambda(_) => true,
            _ => false,
        }
    }

    pub fn to_tuple(&self) -> Option<&ExprTupleType> {
        match *self {
            ExprData::Tuple(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_tuple(&self) -> bool {
        match *self {
            ExprData::Tuple(_) => true,
            _ => false,
        }
    }

    pub fn to_block(&self) -> Option<&ExprBlockType> {
        match *self {
            ExprData::Block(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_block(&self) -> bool {
        match self {
            &ExprData::Block(..)
            | &ExprData::If(..)
            | &ExprData::Match(..)
            | &ExprData::While(..)
            | &ExprData::For(..) => true,

            _ => false,
        }
    }

    pub fn to_if(&self) -> Option<&ExprIfType> {
        match *self {
            ExprData::If(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_if(&self) -> bool {
        match *self {
            ExprData::If(_) => true,
            _ => false,
        }
    }

    pub fn to_break(&self) -> Option<&ExprBreakType> {
        match *self {
            ExprData::Break(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_break(&self) -> bool {
        match *self {
            ExprData::Break(_) => true,
            _ => false,
        }
    }

    pub fn to_continue(&self) -> Option<&ExprContinueType> {
        match *self {
            ExprData::Continue(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_continue(&self) -> bool {
        match *self {
            ExprData::Continue(_) => true,
            _ => false,
        }
    }

    pub fn to_return(&self) -> Option<&ExprReturnType> {
        match *self {
            ExprData::Return(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_return(&self) -> bool {
        match *self {
            ExprData::Return(_) => true,
            _ => false,
        }
    }

    pub fn is_blocklike(&self) -> bool {
        match self {
            &ExprData::Block(_) => true,
            &ExprData::If(_) => true,
            &ExprData::Match(_) => true,
            &ExprData::For(_) => true,
            &ExprData::While(_) => true,
            _ => false,
        }
    }

    pub fn span(&self) -> Span {
        match *self {
            ExprData::Un(ref val) => val.span,
            ExprData::Bin(ref val) => val.span,
            ExprData::LitChar(ref val) => val.span,
            ExprData::LitInt(ref val) => val.span,
            ExprData::LitFloat(ref val) => val.span,
            ExprData::LitStr(ref val) => val.span,
            ExprData::Template(ref val) => val.span,
            ExprData::LitBool(ref val) => val.span,
            ExprData::Ident(ref val) => val.span,
            ExprData::Call(ref val) => val.span,
            ExprData::TypeParam(ref val) => val.span,
            ExprData::Path(ref val) => val.span,
            ExprData::Dot(ref val) => val.span,
            ExprData::This(ref val) => val.span,
            ExprData::Conv(ref val) => val.span,
            ExprData::Is(ref val) => val.span,
            ExprData::Lambda(ref val) => val.span,
            ExprData::Block(ref val) => val.span,
            ExprData::If(ref val) => val.span,
            ExprData::Tuple(ref val) => val.span,
            ExprData::Paren(ref val) => val.span,
            ExprData::Match(ref val) => val.span,
            ExprData::For(ref val) => val.span,
            ExprData::While(ref val) => val.span,
            ExprData::Break(ref val) => val.span,
            ExprData::Continue(ref val) => val.span,
            ExprData::Return(ref val) => val.span,
            ExprData::Error { span, .. } => span,
        }
    }

    pub fn id(&self) -> NodeId {
        match *self {
            ExprData::Un(ref val) => val.id,
            ExprData::Bin(ref val) => val.id,
            ExprData::LitChar(ref val) => val.id,
            ExprData::LitInt(ref val) => val.id,
            ExprData::LitFloat(ref val) => val.id,
            ExprData::LitStr(ref val) => val.id,
            ExprData::Template(ref val) => val.id,
            ExprData::LitBool(ref val) => val.id,
            ExprData::Ident(ref val) => val.id,
            ExprData::Call(ref val) => val.id,
            ExprData::TypeParam(ref val) => val.id,
            ExprData::Path(ref val) => val.id,
            ExprData::Dot(ref val) => val.id,
            ExprData::This(ref val) => val.id,
            ExprData::Conv(ref val) => val.id,
            ExprData::Is(ref val) => val.id,
            ExprData::Lambda(ref val) => val.id,
            ExprData::Block(ref val) => val.id,
            ExprData::If(ref val) => val.id,
            ExprData::Tuple(ref val) => val.id,
            ExprData::Paren(ref val) => val.id,
            ExprData::Match(ref val) => val.id,
            ExprData::For(ref val) => val.id,
            ExprData::While(ref val) => val.id,
            ExprData::Break(ref val) => val.id,
            ExprData::Continue(ref val) => val.id,
            ExprData::Return(ref val) => val.id,
            ExprData::Error { id, .. } => id,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExprIfType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub cond: Expr,
    pub then_block: Expr,
    pub else_block: Option<Expr>,
}

#[derive(Clone, Debug)]
pub struct ExprTupleType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub values: Vec<Expr>,
}

#[derive(Clone, Debug)]
pub struct ExprConvType {
    pub id: NodeId,
    pub span: Span,

    pub object: Expr,
    pub data_type: Type,
}

#[derive(Clone, Debug)]
pub struct ExprIsType {
    pub id: NodeId,
    pub span: Span,

    pub value: Expr,
    pub pattern: Arc<Pattern>,
}

#[derive(Clone, Debug)]
pub struct ExprUnType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub op: UnOp,
    pub opnd: Expr,
}

#[derive(Clone, Debug)]
pub struct ExprBinType {
    pub id: NodeId,
    pub span: Span,

    pub op: BinOp,
    pub lhs: Expr,
    pub rhs: Expr,
}

#[derive(Clone, Debug)]
pub struct ExprLitCharType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub value: String,
}

#[derive(Clone, Debug)]
pub struct ExprLitIntType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub value: String,
}

#[derive(Clone, Debug)]
pub struct ExprLitFloatType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub value: String,
}

#[derive(Clone, Debug)]
pub struct ExprLitStrType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub value: String,
}

#[derive(Clone, Debug)]
pub struct ExprTemplateType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub parts: Vec<Expr>,
}

#[derive(Clone, Debug)]
pub struct ExprLitBoolType {
    pub id: NodeId,
    pub span: Span,

    pub value: bool,
}

#[derive(Clone, Debug)]
pub struct ExprBlockType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub stmts: Vec<Stmt>,
    pub expr: Option<Expr>,
}

#[derive(Clone, Debug)]
pub struct ExprSelfType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
}

#[derive(Clone, Debug)]
pub struct ExprIdentType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub name: String,
}

#[derive(Clone, Debug)]
pub struct ExprCallType {
    pub id: NodeId,
    pub span: Span,

    pub callee: Expr,
    pub args: Vec<Arc<Argument>>,
}

impl ExprCallType {
    pub fn object(&self) -> Option<&ExprData> {
        if let Some(type_param) = self.callee.to_type_param() {
            if let Some(dot) = type_param.callee.to_dot() {
                Some(&dot.lhs)
            } else {
                None
            }
        } else if let Some(dot) = self.callee.to_dot() {
            Some(&dot.lhs)
        } else {
            None
        }
    }

    pub fn object_or_callee(&self) -> &ExprData {
        self.object().unwrap_or(&self.callee)
    }

    pub fn callee(&self) -> &ExprData {
        &self.callee
    }
}

#[derive(Clone, Debug)]
pub struct Argument {
    pub id: NodeId,
    pub span: Span,
    pub name: Option<Ident>,
    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub struct ExprParenType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,
    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub struct ExprMatchType {
    pub id: NodeId,
    pub span: Span,
    pub green: GreenNode,

    pub expr: Expr,
    pub arms: Vec<Arc<MatchArmType>>,
}

#[derive(Clone, Debug)]
pub struct MatchArmType {
    pub id: NodeId,
    pub span: Span,

    pub pattern: Arc<Pattern>,
    pub cond: Option<Expr>,
    pub value: Expr,
}

#[derive(Clone, Debug)]
pub struct PatternError {
    pub id: NodeId,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct PatternAlt {
    pub id: NodeId,
    pub span: Span,

    pub alts: Vec<Arc<Pattern>>,
}

impl PatternAlt {
    pub fn first_alt(&self) -> Option<&Arc<Pattern>> {
        self.alts.get(0)
    }

    pub fn is_rest(&self) -> bool {
        self.alts.len() == 1 && self.alts[0].is_rest()
    }

    pub fn is_underscore(&self) -> bool {
        self.alts.len() == 1 && self.alts[0].is_underscore()
    }

    pub fn is_ident(&self) -> bool {
        self.alts.len() == 1 && self.alts[0].is_ident()
    }

    pub fn to_ident(&self) -> Option<&PatternIdent> {
        assert!(self.alts.len() == 1);
        self.alts[0].to_ident()
    }

    pub fn is_lit_bool(&self) -> bool {
        self.alts.len() == 1 && self.alts[0].is_lit_bool()
    }

    pub fn is_lit_char(&self) -> bool {
        self.alts.len() == 1 && self.alts[0].is_lit_char()
    }

    pub fn is_lit_int(&self) -> bool {
        self.alts.len() == 1 && self.alts[0].is_lit_int()
    }

    pub fn is_lit_float(&self) -> bool {
        self.alts.len() == 1 && self.alts[0].is_lit_float()
    }

    pub fn is_lit_string(&self) -> bool {
        self.alts.len() == 1 && self.alts[0].is_lit_string()
    }
}

#[derive(Clone, Debug)]
pub enum Pattern {
    Underscore(PatternUnderscore),
    LitBool(PatternLit),
    LitChar(PatternLit),
    LitString(PatternLit),
    LitInt(PatternLit),
    LitFloat(PatternLit),
    Tuple(PatternTuple),
    Ident(PatternIdent),
    ClassOrStructOrEnum(PatternClassOrStructOrEnum),
    Rest(PatternRest),
    Alt(PatternAlt),
    Error(PatternError),
}

impl Pattern {
    pub fn id(&self) -> NodeId {
        match self {
            Pattern::Underscore(p) => p.id,
            Pattern::LitBool(p) => p.id,
            Pattern::LitChar(p) => p.id,
            Pattern::LitString(p) => p.id,
            Pattern::LitInt(p) => p.id,
            Pattern::LitFloat(p) => p.id,
            Pattern::Tuple(p) => p.id,
            Pattern::Ident(p) => p.id,
            Pattern::ClassOrStructOrEnum(p) => p.id,
            Pattern::Rest(p) => p.id,
            Pattern::Alt(p) => p.id,
            Pattern::Error(p) => p.id,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Pattern::Underscore(p) => p.span,
            Pattern::LitBool(p) => p.span,
            Pattern::LitChar(p) => p.span,
            Pattern::LitString(p) => p.span,
            Pattern::LitInt(p) => p.span,
            Pattern::LitFloat(p) => p.span,
            Pattern::Tuple(p) => p.span,
            Pattern::Ident(p) => p.span,
            Pattern::ClassOrStructOrEnum(p) => p.span,
            Pattern::Rest(p) => p.span,
            Pattern::Alt(p) => p.span,
            Pattern::Error(p) => p.span,
        }
    }

    pub fn is_underscore(&self) -> bool {
        match self {
            Pattern::Underscore(..) => true,
            _ => false,
        }
    }

    pub fn is_rest(&self) -> bool {
        match self {
            Pattern::Rest(..) => true,
            _ => false,
        }
    }

    pub fn is_lit_bool(&self) -> bool {
        match self {
            Pattern::LitBool(..) => true,
            _ => false,
        }
    }

    pub fn is_lit_char(&self) -> bool {
        match self {
            Pattern::LitChar(..) => true,
            _ => false,
        }
    }

    pub fn is_lit_int(&self) -> bool {
        match self {
            Pattern::LitInt(..) => true,
            _ => false,
        }
    }

    pub fn is_lit_float(&self) -> bool {
        match self {
            Pattern::LitFloat(..) => true,
            _ => false,
        }
    }

    pub fn is_lit_string(&self) -> bool {
        match self {
            Pattern::LitString(..) => true,
            _ => false,
        }
    }

    pub fn is_ident(&self) -> bool {
        match self {
            Pattern::Ident(..) => true,
            _ => false,
        }
    }

    pub fn to_ident(&self) -> Option<&PatternIdent> {
        match self {
            Pattern::Ident(ref p) => Some(p),
            _ => None,
        }
    }

    pub fn is_tuple(&self) -> bool {
        match self {
            Pattern::Tuple(..) => true,
            _ => false,
        }
    }

    pub fn to_tuple(&self) -> Option<&PatternTuple> {
        match self {
            Pattern::Tuple(ref p) => Some(p),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct PatternUnderscore {
    pub id: NodeId,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct PatternRest {
    pub id: NodeId,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct PatternLit {
    pub id: NodeId,
    pub span: Span,
    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub struct PatternIdent {
    pub id: NodeId,
    pub span: Span,
    pub mutable: bool,
    pub name: Ident,
}

#[derive(Clone, Debug)]
pub struct PatternTuple {
    pub id: NodeId,
    pub span: Span,
    pub params: Vec<Arc<Pattern>>,
}

#[derive(Clone, Debug)]
pub struct PatternClassOrStructOrEnum {
    pub id: NodeId,
    pub span: Span,
    pub path: Path,
    pub params: Option<Vec<Arc<PatternField>>>,
}

#[derive(Clone, Debug)]
pub struct PatternField {
    pub id: NodeId,
    pub span: Span,
    pub ident: Option<Ident>,
    pub pattern: Arc<Pattern>,
}

#[derive(Clone, Debug)]
pub struct PatternParam {
    pub id: NodeId,
    pub span: Span,
    pub mutable: bool,
    pub name: Option<Ident>,
}

pub type Path = Arc<PathData>;

#[derive(Clone, Debug)]
pub struct PathData {
    pub id: NodeId,
    pub span: Span,
    pub segments: Vec<PathSegment>,
}

pub type PathSegment = Arc<PathSegmentData>;

#[derive(Clone, Debug)]
pub enum PathSegmentData {
    Self_(PathSegmentSelf),
    Ident(PathSegmentIdent),
    Error { id: NodeId, span: Span },
}

impl PathSegmentData {
    pub fn id(&self) -> NodeId {
        match self {
            PathSegmentData::Self_(ref node) => node.id,
            PathSegmentData::Ident(ref node) => node.id,
            PathSegmentData::Error { id, .. } => id.clone(),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            PathSegmentData::Self_(ref node) => node.span,
            PathSegmentData::Ident(ref node) => node.span,
            PathSegmentData::Error { span, .. } => span.clone(),
        }
    }

    pub fn to_ident(&self) -> Option<&PathSegmentIdent> {
        match self {
            PathSegmentData::Ident(ref node) => Some(node),
            _ => None,
        }
    }

    pub fn is_self(&self) -> bool {
        match self {
            PathSegmentData::Self_(..) => true,
            _ => false,
        }
    }
}

impl PathSegmentData {
    #[cfg(test)]
    pub fn as_name_str(&self) -> &str {
        match self {
            PathSegmentData::Ident(ref ident) => &ident.name.name_as_string,
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct PathSegmentSelf {
    pub id: NodeId,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct PathSegmentIdent {
    pub id: NodeId,
    pub span: Span,
    pub name: Ident,
}

#[derive(Clone, Debug)]
pub struct ExprTypeParamType {
    pub id: NodeId,
    pub span: Span,
    pub op_span: Span,

    pub callee: Expr,
    pub args: Vec<Type>,
}

#[derive(Clone, Debug)]
pub struct ExprPathType {
    pub id: NodeId,
    pub span: Span,
    pub op_span: Span,

    pub lhs: Expr,
    pub rhs: Expr,
}

#[derive(Clone, Debug)]
pub struct ExprDotType {
    pub id: NodeId,
    pub span: Span,
    pub op_span: Span,

    pub lhs: Expr,
    pub rhs: Expr,
}

fn find_token(node: &GreenNode, token: TokenKind) -> Option<GreenToken> {
    node.children
        .iter()
        .find(|c| c.kind() == token)
        .map(|t| t.to_token().expect("should be token"))
}
