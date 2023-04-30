use std::fmt;
use std::slice::Iter;
use std::sync::Arc;

#[cfg(test)]
use crate::interner::Interner;

use crate::interner::Name;
use crate::{FloatSuffix, IntBase, IntSuffix, Span};

pub mod dump;
pub mod visit;

#[derive(Clone, Debug)]
pub struct File {
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
    pub fn alias0(&self) -> &Alias {
        self.elements[0].to_alias().unwrap()
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

#[derive(Clone, Debug)]
pub enum Elem {
    Function(Arc<Function>),
    Class(Arc<Class>),
    Struct(Arc<Struct>),
    Trait(Arc<Trait>),
    Impl(Arc<Impl>),
    Global(Arc<Global>),
    Const(Arc<Const>),
    Enum(Arc<Enum>),
    Alias(Arc<Alias>),
    Module(Arc<Module>),
    Use(Arc<Use>),
    Extern(Arc<ExternPackage>),
}

impl Elem {
    pub fn id(&self) -> NodeId {
        match self {
            &Elem::Function(ref f) => f.id,
            &Elem::Class(ref c) => c.id,
            &Elem::Struct(ref s) => s.id,
            &Elem::Trait(ref t) => t.id,
            &Elem::Impl(ref i) => i.id,
            &Elem::Global(ref g) => g.id,
            &Elem::Const(ref c) => c.id,
            &Elem::Enum(ref e) => e.id,
            &Elem::Alias(ref e) => e.id,
            &Elem::Module(ref e) => e.id,
            &Elem::Use(ref e) => e.id,
            &Elem::Extern(ref e) => e.id,
        }
    }

    pub fn to_function(&self) -> Option<&Function> {
        match self {
            &Elem::Function(ref fct) => Some(fct),
            _ => None,
        }
    }

    pub fn to_class(&self) -> Option<&Class> {
        match self {
            &Elem::Class(ref class) => Some(class),
            _ => None,
        }
    }

    pub fn to_enum(&self) -> Option<&Enum> {
        match self {
            &Elem::Enum(ref enum_) => Some(enum_),
            _ => None,
        }
    }

    pub fn to_alias(&self) -> Option<&Alias> {
        match self {
            &Elem::Alias(ref alias) => Some(alias),
            _ => None,
        }
    }

    pub fn to_module(&self) -> Option<&Module> {
        match self {
            &Elem::Module(ref module) => Some(module),
            _ => None,
        }
    }

    pub fn to_struct(&self) -> Option<&Struct> {
        match self {
            &Elem::Struct(ref struc) => Some(struc),
            _ => None,
        }
    }

    pub fn to_trait(&self) -> Option<&Trait> {
        match self {
            &Elem::Trait(ref trait_) => Some(trait_),
            _ => None,
        }
    }

    pub fn to_impl(&self) -> Option<&Impl> {
        match self {
            &Elem::Impl(ref impl_) => Some(impl_),
            _ => None,
        }
    }

    pub fn to_global(&self) -> Option<&Global> {
        match self {
            &Elem::Global(ref global) => Some(global),
            _ => None,
        }
    }

    pub fn to_const(&self) -> Option<&Const> {
        match self {
            &Elem::Const(ref konst) => Some(konst),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct IdentData {
    pub span: Span,
    pub name: Name,
}

pub type Ident = Arc<IdentData>;

#[derive(Clone, Debug)]
pub struct Global {
    pub id: NodeId,
    pub span: Span,
    pub name: Option<Ident>,
    pub mutable: bool,
    pub data_type: Type,
    pub initial_value: Option<Expr>,
    pub visibility: Visibility,
}

#[derive(Clone, Debug)]
pub struct Module {
    pub id: NodeId,
    pub span: Span,
    pub name: Option<Ident>,
    pub elements: Option<Vec<Elem>>,
    pub visibility: Visibility,
}

#[derive(Clone, Debug)]
pub struct Use {
    pub id: NodeId,
    pub span: Span,
    pub common_path: Vec<UsePathComponent>,
    pub target: UseTargetDescriptor,
}

#[derive(Clone, Debug)]
pub enum UseTargetDescriptor {
    Default,
    As(UseTargetName),
    Group(UseTargetGroup),
}

#[derive(Clone, Debug)]
pub struct UseTargetGroup {
    pub span: Span,
    pub targets: Vec<Arc<Use>>,
}

#[derive(Clone, Debug)]
pub struct UseTargetName {
    pub span: Span,
    pub name: Option<Ident>,
}

#[derive(Clone, Debug)]
pub struct UsePathComponent {
    pub span: Span,
    pub value: UsePathComponentValue,
}

#[derive(Clone, Debug)]
pub enum UsePathComponentValue {
    This,
    Super,
    Package,
    Name(Ident),
}

#[derive(Clone, Debug)]
pub struct Const {
    pub id: NodeId,
    pub span: Span,
    pub name: Option<Ident>,
    pub data_type: Type,
    pub expr: Expr,
    pub visibility: Visibility,
}

#[derive(Clone, Debug)]
pub struct Enum {
    pub id: NodeId,
    pub span: Span,
    pub name: Option<Ident>,
    pub type_params: Option<Vec<TypeParam>>,
    pub variants: Vec<EnumVariant>,
    pub visibility: Visibility,
}

#[derive(Clone, Debug)]
pub struct EnumVariant {
    pub id: NodeId,
    pub span: Span,
    pub name: Option<Ident>,
    pub types: Option<Vec<Type>>,
}

#[derive(Clone, Debug)]
pub struct Alias {
    pub id: NodeId,
    pub span: Span,
    pub name: Option<Ident>,
    pub ty: Type,
    pub visibility: Visibility,
}

#[derive(Clone, Debug)]
pub struct Struct {
    pub id: NodeId,
    pub span: Span,
    pub name: Option<Ident>,
    pub fields: Vec<StructField>,
    pub visibility: Visibility,
    pub internal: bool,
    pub type_params: Option<Vec<TypeParam>>,
}

#[derive(Clone, Debug)]
pub struct StructField {
    pub id: NodeId,
    pub span: Span,
    pub name: Option<Ident>,
    pub data_type: Type,
    pub visibility: Visibility,
}

pub type Type = Arc<TypeData>;

#[derive(Clone, Debug)]
pub enum TypeData {
    This(TypeSelfType),
    Basic(TypeBasicType),
    Tuple(TypeTupleType),
    Lambda(TypeLambdaType),
    Error { id: NodeId, span: Span },
}

#[derive(Clone, Debug)]
pub struct TypeSelfType {
    pub id: NodeId,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct TypeTupleType {
    pub id: NodeId,
    pub span: Span,

    pub subtypes: Vec<Type>,
}

#[derive(Clone, Debug)]
pub struct TypeLambdaType {
    pub id: NodeId,
    pub span: Span,

    pub params: Vec<Type>,
    pub ret: Option<Type>,
}

#[derive(Clone, Debug)]
pub struct TypeBasicType {
    pub id: NodeId,
    pub span: Span,

    pub path: Path,
    pub params: Vec<Type>,
}

impl TypeBasicType {
    #[cfg(test)]
    pub fn name(&self) -> Name {
        assert_eq!(self.path.names.len(), 1);
        self.path.names.last().cloned().unwrap().name
    }
}

impl TypeData {
    pub fn create_self(id: NodeId, span: Span) -> TypeData {
        TypeData::This(TypeSelfType { id, span })
    }

    pub fn create_basic(id: NodeId, span: Span, path: Path, params: Vec<Type>) -> TypeData {
        TypeData::Basic(TypeBasicType {
            id,
            span,
            path,
            params,
        })
    }

    pub fn create_fct(id: NodeId, span: Span, params: Vec<Type>, ret: Option<Type>) -> TypeData {
        TypeData::Lambda(TypeLambdaType {
            id,
            span,
            params,
            ret,
        })
    }

    pub fn create_tuple(id: NodeId, span: Span, subtypes: Vec<Type>) -> TypeData {
        TypeData::Tuple(TypeTupleType { id, span, subtypes })
    }

    pub fn to_basic(&self) -> Option<&TypeBasicType> {
        match *self {
            TypeData::Basic(ref val) => Some(val),
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
    pub fn to_string(&self, interner: &Interner) -> String {
        match *self {
            TypeData::This(_) => "Self".into(),
            TypeData::Basic(ref val) => format!("{}", *interner.str(val.name())),

            TypeData::Tuple(ref val) => {
                let types: Vec<String> =
                    val.subtypes.iter().map(|t| t.to_string(interner)).collect();

                format!("({})", types.join(", "))
            }

            TypeData::Lambda(ref val) => {
                let types: Vec<String> = val.params.iter().map(|t| t.to_string(interner)).collect();

                if let Some(ref ret) = val.ret {
                    let ret = ret.to_string(interner);
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
            TypeData::This(ref val) => val.span,
            TypeData::Basic(ref val) => val.span,
            TypeData::Tuple(ref val) => val.span,
            TypeData::Lambda(ref val) => val.span,
            TypeData::Error { span, .. } => span,
        }
    }

    pub fn id(&self) -> NodeId {
        match *self {
            TypeData::This(ref val) => val.id,
            TypeData::Basic(ref val) => val.id,
            TypeData::Tuple(ref val) => val.id,
            TypeData::Lambda(ref val) => val.id,
            TypeData::Error { id, .. } => id,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Impl {
    pub id: NodeId,
    pub span: Span,

    pub type_params: Option<Vec<TypeParam>>,
    pub trait_type: Option<Type>,
    pub extended_type: Type,
    pub methods: Vec<Arc<Function>>,
}

#[derive(Clone, Debug)]
pub struct Trait {
    pub id: NodeId,
    pub name: Option<Ident>,
    pub type_params: Option<Vec<TypeParam>>,
    pub span: Span,
    pub methods: Vec<Arc<Function>>,
    pub visibility: Visibility,
}

#[derive(Clone, Debug)]
pub struct Class {
    pub id: NodeId,
    pub span: Span,
    pub name: Option<Ident>,
    pub internal: bool,
    pub visibility: Visibility,

    pub fields: Vec<Field>,
    pub type_params: Option<Vec<TypeParam>>,
}

#[derive(Clone, Debug)]
pub struct ExternPackage {
    pub id: NodeId,
    pub span: Span,
    pub name: Option<Ident>,
    pub identifier: Option<Ident>,
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
    pub name: Option<Ident>,
    pub data_type: Type,
    pub primary_ctor: bool,
    pub expr: Option<Expr>,
    pub mutable: bool,
    pub visibility: Visibility,
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
    pub kind: FunctionKind,
    pub span: Span,
    pub name: Option<Ident>,
    pub is_optimize_immediately: bool,
    pub visibility: Visibility,
    pub is_static: bool,
    pub is_test: bool,
    pub internal: bool,
    pub is_constructor: bool,

    pub params: Vec<Param>,

    pub return_type: Option<Type>,
    pub block: Option<Expr>,
    pub type_params: Option<Vec<TypeParam>>,
}

impl Function {
    pub fn block(&self) -> &Expr {
        self.block.as_ref().unwrap()
    }
}

// remove in next step
#[derive(Clone, Debug)]
pub struct Modifiers(Vec<ModifierElement>);

// remove in next step
impl Modifiers {
    pub fn new() -> Modifiers {
        Modifiers(Vec::new())
    }

    pub fn contains(&self, modifier: Annotation) -> bool {
        self.0.iter().find(|el| el.value == modifier).is_some()
    }

    pub fn add(&mut self, modifier: Annotation, span: Span) {
        self.0.push(ModifierElement {
            value: modifier,
            span,
        });
    }

    pub fn iter(&self) -> Iter<ModifierElement> {
        self.0.iter()
    }
}

// remove in next step
#[derive(Clone, Debug)]
pub struct ModifierElement {
    pub value: Annotation,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct AnnotationUsages(Vec<AnnotationUsage>);

impl AnnotationUsages {
    pub fn new() -> AnnotationUsages {
        AnnotationUsages(Vec::new())
    }

    pub fn contains(&self, name: Name) -> bool {
        self.0.iter().find(|el| el.name == name).is_some()
    }

    pub fn add(&mut self, annotation_usage: AnnotationUsage) {
        self.0.push(annotation_usage);
    }

    pub fn iter(&self) -> Iter<AnnotationUsage> {
        self.0.iter()
    }
}

#[derive(Clone, Debug)]
pub struct AnnotationUsage {
    pub name: Name,
    pub span: Span,
    pub type_args: Vec<TypeData>,
    pub term_args: Vec<Expr>,
}

// rename to InternalAnnotation in next step
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Annotation {
    Internal,
    Pub,
    Static,
    Test,
    OptimizeImmediately,
    Error,
}

impl Annotation {
    pub fn name(&self) -> &'static str {
        match *self {
            Annotation::Internal => "internal",
            Annotation::Pub => "pub",
            Annotation::Static => "static",
            Annotation::Test => "test",
            Annotation::OptimizeImmediately => "optimizeImmediately",
            Annotation::Error => "<error>",
        }
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
    While(StmtWhileType),
    Expr(StmtExprType),
    Break(StmtBreakType),
    Continue(StmtContinueType),
    Return(StmtReturnType),
    For(StmtForType),
}

impl StmtData {
    pub fn create_let(
        id: NodeId,
        span: Span,
        pattern: Box<LetPattern>,
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

    pub fn create_for(
        id: NodeId,
        span: Span,
        pattern: Box<LetPattern>,
        expr: Expr,
        block: Stmt,
    ) -> StmtData {
        StmtData::For(StmtForType {
            id,
            span,

            pattern,
            expr,
            block,
        })
    }

    pub fn create_while(id: NodeId, span: Span, cond: Expr, block: Stmt) -> StmtData {
        StmtData::While(StmtWhileType {
            id,
            span,

            cond,
            block,
        })
    }

    pub fn create_expr(id: NodeId, span: Span, expr: Expr) -> StmtData {
        StmtData::Expr(StmtExprType { id, span, expr })
    }

    pub fn create_break(id: NodeId, span: Span) -> StmtData {
        StmtData::Break(StmtBreakType { id, span })
    }

    pub fn create_continue(id: NodeId, span: Span) -> StmtData {
        StmtData::Continue(StmtContinueType { id, span })
    }

    pub fn create_return(id: NodeId, span: Span, expr: Option<Expr>) -> StmtData {
        StmtData::Return(StmtReturnType { id, span, expr })
    }

    pub fn id(&self) -> NodeId {
        match *self {
            StmtData::Let(ref stmt) => stmt.id,
            StmtData::While(ref stmt) => stmt.id,
            StmtData::For(ref stmt) => stmt.id,
            StmtData::Expr(ref stmt) => stmt.id,
            StmtData::Break(ref stmt) => stmt.id,
            StmtData::Continue(ref stmt) => stmt.id,
            StmtData::Return(ref stmt) => stmt.id,
        }
    }

    pub fn span(&self) -> Span {
        match *self {
            StmtData::Let(ref stmt) => stmt.span,
            StmtData::While(ref stmt) => stmt.span,
            StmtData::For(ref stmt) => stmt.span,
            StmtData::Expr(ref stmt) => stmt.span,
            StmtData::Break(ref stmt) => stmt.span,
            StmtData::Continue(ref stmt) => stmt.span,
            StmtData::Return(ref stmt) => stmt.span,
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

    pub fn to_while(&self) -> Option<&StmtWhileType> {
        match *self {
            StmtData::While(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_while(&self) -> bool {
        match *self {
            StmtData::While(_) => true,
            _ => false,
        }
    }

    pub fn to_for(&self) -> Option<&StmtForType> {
        match *self {
            StmtData::For(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_for(&self) -> bool {
        match *self {
            StmtData::For(_) => true,
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

    pub fn to_return(&self) -> Option<&StmtReturnType> {
        match *self {
            StmtData::Return(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_return(&self) -> bool {
        match *self {
            StmtData::Return(_) => true,
            _ => false,
        }
    }

    pub fn to_break(&self) -> Option<&StmtBreakType> {
        match *self {
            StmtData::Break(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_break(&self) -> bool {
        match *self {
            StmtData::Break(_) => true,
            _ => false,
        }
    }

    pub fn to_continue(&self) -> Option<&StmtContinueType> {
        match *self {
            StmtData::Continue(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_continue(&self) -> bool {
        match *self {
            StmtData::Continue(_) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct StmtLetType {
    pub id: NodeId,
    pub span: Span,

    pub pattern: Box<LetPattern>,

    pub data_type: Option<Type>,
    pub expr: Option<Expr>,
}

#[derive(Clone, Debug)]
pub enum LetPattern {
    Ident(LetIdentType),
    Tuple(LetTupleType),
    Underscore(LetUnderscoreType),
}

impl LetPattern {
    pub fn is_ident(&self) -> bool {
        match self {
            LetPattern::Ident(_) => true,
            _ => false,
        }
    }

    pub fn is_tuple(&self) -> bool {
        match self {
            LetPattern::Tuple(_) => true,
            _ => false,
        }
    }

    pub fn is_underscore(&self) -> bool {
        match self {
            LetPattern::Underscore(_) => true,
            _ => false,
        }
    }

    pub fn to_name(&self) -> Option<Name> {
        match self {
            LetPattern::Ident(ref ident) => ident.name.as_ref().map(|i| i.name),
            _ => None,
        }
    }

    pub fn to_ident(&self) -> Option<&LetIdentType> {
        match self {
            LetPattern::Ident(ref ident) => Some(ident),
            _ => None,
        }
    }

    pub fn to_tuple(&self) -> Option<&LetTupleType> {
        match self {
            LetPattern::Tuple(ref tuple) => Some(tuple),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct LetUnderscoreType {
    pub id: NodeId,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct LetIdentType {
    pub id: NodeId,
    pub span: Span,
    pub mutable: bool,
    pub name: Option<Ident>,
}

#[derive(Clone, Debug)]
pub struct LetTupleType {
    pub id: NodeId,
    pub span: Span,
    pub parts: Vec<Box<LetPattern>>,
}

#[derive(Clone, Debug)]
pub struct StmtForType {
    pub id: NodeId,
    pub span: Span,

    pub pattern: Box<LetPattern>,
    pub expr: Expr,
    pub block: Stmt,
}

#[derive(Clone, Debug)]
pub struct StmtWhileType {
    pub id: NodeId,
    pub span: Span,

    pub cond: Expr,
    pub block: Stmt,
}

#[derive(Clone, Debug)]
pub struct StmtExprType {
    pub id: NodeId,
    pub span: Span,

    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub struct StmtReturnType {
    pub id: NodeId,
    pub span: Span,

    pub expr: Option<Expr>,
}

#[derive(Clone, Debug)]
pub struct StmtBreakType {
    pub id: NodeId,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct StmtContinueType {
    pub id: NodeId,
    pub span: Span,
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum UnOp {
    Plus,
    Neg,
    Not,
}

impl UnOp {
    pub fn as_str(&self) -> &'static str {
        match *self {
            UnOp::Plus => "+",
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
            BinOp::Assign => "=",
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

    pub fn is_any_assign(&self) -> bool {
        match *self {
            BinOp::Assign => true,
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
    Lambda(Arc<Function>),
    Block(ExprBlockType),
    If(ExprIfType),
    Tuple(ExprTupleType),
    Paren(ExprParenType),
    Match(ExprMatchType),
    Error { id: NodeId, span: Span },
}

impl ExprData {
    pub fn create_block(id: NodeId, span: Span, stmts: Vec<Stmt>, expr: Option<Expr>) -> ExprData {
        ExprData::Block(ExprBlockType {
            id,
            span,

            stmts,
            expr,
        })
    }

    pub fn create_if(
        id: NodeId,
        span: Span,
        cond: Expr,
        then_block: Expr,
        else_block: Option<Expr>,
    ) -> ExprData {
        ExprData::If(ExprIfType {
            id,
            span,

            cond,
            then_block,
            else_block,
        })
    }

    pub fn create_match(id: NodeId, span: Span, expr: Expr, cases: Vec<MatchCaseType>) -> ExprData {
        ExprData::Match(ExprMatchType {
            id,
            span,
            expr,
            cases,
        })
    }

    pub fn create_un(id: NodeId, span: Span, op: UnOp, opnd: Expr) -> ExprData {
        ExprData::Un(ExprUnType { id, span, op, opnd })
    }

    pub fn create_bin(id: NodeId, span: Span, op: BinOp, lhs: Expr, rhs: Expr) -> ExprData {
        ExprData::Bin(ExprBinType {
            id,
            span,

            op,
            initializer: false,
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

    pub fn create_lit_char(id: NodeId, span: Span, value: char) -> ExprData {
        ExprData::LitChar(ExprLitCharType { id, span, value })
    }

    pub fn create_lit_int(
        id: NodeId,
        span: Span,
        value: u64,
        base: IntBase,
        suffix: IntSuffix,
    ) -> ExprData {
        ExprData::LitInt(ExprLitIntType {
            id,
            span,

            value,
            base,
            suffix,
        })
    }

    pub fn create_lit_float(id: NodeId, span: Span, value: f64, suffix: FloatSuffix) -> ExprData {
        ExprData::LitFloat(ExprLitFloatType {
            id,
            span,
            value,
            suffix,
        })
    }

    pub fn create_lit_str(id: NodeId, span: Span, value: String) -> ExprData {
        ExprData::LitStr(ExprLitStrType { id, span, value })
    }

    pub fn create_template(id: NodeId, span: Span, parts: Vec<Expr>) -> ExprData {
        ExprData::Template(ExprTemplateType { id, span, parts })
    }

    pub fn create_lit_bool(id: NodeId, span: Span, value: bool) -> ExprData {
        ExprData::LitBool(ExprLitBoolType { id, span, value })
    }

    pub fn create_this(id: NodeId, span: Span) -> ExprData {
        ExprData::This(ExprSelfType { id, span })
    }

    pub fn create_ident(id: NodeId, span: Span, name: Name) -> ExprData {
        ExprData::Ident(ExprIdentType { id, span, name })
    }

    pub fn create_paren(id: NodeId, span: Span, expr: Expr) -> ExprData {
        ExprData::Paren(ExprParenType { id, span, expr })
    }

    pub fn create_call(id: NodeId, span: Span, callee: Expr, args: Vec<Expr>) -> ExprData {
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

    pub fn create_tuple(id: NodeId, span: Span, values: Vec<Expr>) -> ExprData {
        ExprData::Tuple(ExprTupleType { id, span, values })
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
            &ExprData::Block(_) => true,
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

    pub fn needs_semicolon(&self) -> bool {
        match self {
            &ExprData::Block(_) => false,
            &ExprData::If(_) => false,
            &ExprData::Match(_) => false,
            _ => true,
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
            ExprData::Lambda(ref val) => val.span,
            ExprData::Block(ref val) => val.span,
            ExprData::If(ref val) => val.span,
            ExprData::Tuple(ref val) => val.span,
            ExprData::Paren(ref val) => val.span,
            ExprData::Match(ref val) => val.span,
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
            ExprData::Lambda(ref val) => val.id,
            ExprData::Block(ref val) => val.id,
            ExprData::If(ref val) => val.id,
            ExprData::Tuple(ref val) => val.id,
            ExprData::Paren(ref val) => val.id,
            ExprData::Match(ref val) => val.id,
            ExprData::Error { id, .. } => id,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExprIfType {
    pub id: NodeId,
    pub span: Span,

    pub cond: Expr,
    pub then_block: Expr,
    pub else_block: Option<Expr>,
}

#[derive(Clone, Debug)]
pub struct ExprTupleType {
    pub id: NodeId,
    pub span: Span,

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
pub struct ExprUnType {
    pub id: NodeId,
    pub span: Span,

    pub op: UnOp,
    pub opnd: Expr,
}

#[derive(Clone, Debug)]
pub struct ExprBinType {
    pub id: NodeId,
    pub span: Span,

    pub op: BinOp,
    pub initializer: bool,
    pub lhs: Expr,
    pub rhs: Expr,
}

#[derive(Clone, Debug)]
pub struct ExprLitCharType {
    pub id: NodeId,
    pub span: Span,

    pub value: char,
}

#[derive(Clone, Debug)]
pub struct ExprLitIntType {
    pub id: NodeId,
    pub span: Span,

    pub value: u64,
    pub base: IntBase,
    pub suffix: IntSuffix,
}

#[derive(Clone, Debug)]
pub struct ExprLitFloatType {
    pub id: NodeId,
    pub span: Span,

    pub value: f64,
    pub suffix: FloatSuffix,
}

#[derive(Clone, Debug)]
pub struct ExprLitStrType {
    pub id: NodeId,
    pub span: Span,

    pub value: String,
}

#[derive(Clone, Debug)]
pub struct ExprTemplateType {
    pub id: NodeId,
    pub span: Span,

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

    pub stmts: Vec<Stmt>,
    pub expr: Option<Expr>,
}

#[derive(Clone, Debug)]
pub struct ExprSelfType {
    pub id: NodeId,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct ExprIdentType {
    pub id: NodeId,
    pub span: Span,
    pub name: Name,
}

#[derive(Clone, Debug)]
pub struct ExprCallType {
    pub id: NodeId,
    pub span: Span,

    pub callee: Expr,
    pub args: Vec<Expr>,
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
}

#[derive(Clone, Debug)]
pub struct ExprParenType {
    pub id: NodeId,
    pub span: Span,

    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub struct ExprMatchType {
    pub id: NodeId,
    pub span: Span,

    pub expr: Expr,
    pub cases: Vec<MatchCaseType>,
}

#[derive(Clone, Debug)]
pub struct MatchCaseType {
    pub id: NodeId,
    pub span: Span,

    pub patterns: Vec<MatchPattern>,
    pub value: Expr,
}

#[derive(Clone, Debug)]
pub struct MatchPattern {
    pub id: NodeId,
    pub span: Span,
    pub data: MatchPatternData,
}

#[derive(Clone, Debug)]
pub enum MatchPatternData {
    Underscore,
    Ident(MatchPatternIdent),
}

#[derive(Clone, Debug)]
pub struct MatchPatternIdent {
    pub path: Path,
    pub params: Option<Vec<MatchPatternParam>>,
}

#[derive(Clone, Debug)]
pub struct MatchPatternParam {
    pub id: NodeId,
    pub span: Span,
    pub name: Option<Ident>,
    pub mutable: bool,
}

pub type Path = Arc<PathData>;

#[derive(Clone, Debug)]
pub struct PathData {
    pub id: NodeId,
    pub span: Span,
    pub names: Vec<Ident>,
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

#[derive(Copy, Clone, Debug)]
pub enum Visibility {
    Public,
    Default,
}

impl Visibility {
    pub fn from_modifiers(modifiers: &Modifiers) -> Visibility {
        if modifiers.contains(Annotation::Pub) {
            Visibility::Public
        } else {
            Visibility::Default
        }
    }
    pub fn is_public(self) -> bool {
        match self {
            Visibility::Public => true,
            Visibility::Default => false,
        }
    }

    pub fn is_default(self) -> bool {
        match self {
            Visibility::Public => false,
            Visibility::Default => true,
        }
    }
}
