use std::fmt;
use std::slice::Iter;
use std::sync::Arc;

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

#[derive(PartialEq, Eq, Copy, Clone, Debug, Hash)]
pub struct NodeId(pub usize);

impl fmt::Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#{}", self.0)
    }
}

pub type AstId = Id<Ast>;

#[derive(Clone, Debug)]
pub enum Ast {
    Function(Function),
    Class(Class),
    Struct(Struct),
    WhereClause(WhereClause),
    WhereClauseItem(WhereClauseItem),
    Field(Field),
    Trait(Trait),
    Impl(Impl),
    Global(Global),
    Const(Const),
    Enum(Enum),
    Module(Module),
    Use(Use),
    UsePath(UsePath),
    UseGroup(UseGroup),
    UseTargetName(UseTargetName),
    Extern(ExternPackage),
    Alias(Alias),
    Argument(Argument),
    Param(Param),
    RegularType(TypeRegularType),
    TupleType(TypeTupleType),
    LambdaType(TypeLambdaType),
    QualifiedPathType(TypeQualifiedPathType),
    LetStmt(StmtLetType),
    ExprStmt(StmtExprType),
    Un(ExprUnType),
    Bin(ExprBinType),
    LitChar(ExprLitCharType),
    LitInt(ExprLitIntType),
    LitFloat(ExprLitFloatType),
    LitStr(ExprLitStrType),
    Template(ExprTemplateType),
    LitBool(ExprLitBoolType),
    Ident(Ident),
    Call(ExprCallType),
    TypeParam(ExprTypeParamType),
    Path(ExprPathType),
    PathData(PathData),
    Dot(ExprDotType),
    This(ExprSelfType),
    UpcaseThis(UpcaseThis),
    Conv(ExprConvType),
    Is(ExprIsType),
    Lambda(ExprLambdaType),
    Block(ExprBlockType),
    If(ExprIfType),
    For(ExprForType),
    While(ExprWhileType),
    Tuple(ExprTupleType),
    Paren(ExprParenType),
    Match(ExprMatchType),
    MatchArm(MatchArmType),
    Break(ExprBreakType),
    Continue(ExprContinueType),
    Return(ExprReturnType),
    TypeArgument(TypeArgument),
    Underscore(PatternUnderscore),
    LitPattern(PatternLit),
    IdentPattern(PatternIdent),
    TuplePattern(PatternTuple),
    ConstructorPattern(PatternConstructor),
    ConstructorField(PatternField),
    Rest(PatternRest),
    Alt(PatternAlt),
    Error(Error),
}

impl Ast {
    pub fn id(&self) -> NodeId {
        match self {
            Ast::Function(ref node) => node.id,
            Ast::Class(ref node) => node.id,
            Ast::Struct(ref node) => node.id,
            Ast::WhereClause(ref node) => node.id,
            Ast::WhereClauseItem(ref node) => node.id,
            Ast::Field(ref node) => node.id,
            Ast::Trait(ref node) => node.id,
            Ast::Impl(ref node) => node.id,
            Ast::Global(ref node) => node.id,
            Ast::Const(ref node) => node.id,
            Ast::Enum(ref node) => node.id,
            Ast::Module(ref node) => node.id,
            Ast::Use(ref node) => node.id,
            Ast::UsePath(ref node) => node.id,
            Ast::UseGroup(ref node) => node.id,
            Ast::UseTargetName(ref node) => node.id,
            Ast::Extern(ref node) => node.id,
            Ast::Alias(ref node) => node.id,
            Ast::Argument(ref node) => node.id,
            Ast::Param(ref node) => node.id,
            Ast::RegularType(ref node) => node.id,
            Ast::TupleType(ref node) => node.id,
            Ast::LambdaType(ref node) => node.id,
            Ast::QualifiedPathType(ref node) => node.id,
            Ast::LetStmt(ref node) => node.id,
            Ast::ExprStmt(ref node) => node.id,
            Ast::Un(ref val) => val.id,
            Ast::Bin(ref val) => val.id,
            Ast::LitChar(ref val) => val.id,
            Ast::LitInt(ref val) => val.id,
            Ast::LitFloat(ref val) => val.id,
            Ast::LitStr(ref val) => val.id,
            Ast::Template(ref val) => val.id,
            Ast::LitBool(ref val) => val.id,
            Ast::Ident(ref val) => val.id,
            Ast::Call(ref val) => val.id,
            Ast::TypeParam(ref val) => val.id,
            Ast::Path(ref val) => val.id,
            Ast::PathData(ref node) => node.id,
            Ast::Dot(ref val) => val.id,
            Ast::This(ref val) => val.id,
            Ast::UpcaseThis(ref val) => val.id,
            Ast::Conv(ref val) => val.id,
            Ast::Is(ref val) => val.id,
            Ast::Lambda(ref val) => val.id,
            Ast::Block(ref val) => val.id,
            Ast::If(ref val) => val.id,
            Ast::Tuple(ref val) => val.id,
            Ast::Paren(ref val) => val.id,
            Ast::Match(ref val) => val.id,
            Ast::MatchArm(ref node) => node.id,
            Ast::For(ref val) => val.id,
            Ast::While(ref val) => val.id,
            Ast::Break(ref val) => val.id,
            Ast::Continue(ref val) => val.id,
            Ast::Return(ref val) => val.id,
            Ast::TypeArgument(ref node) => node.id,
            Ast::Underscore(ref node) => node.id,
            Ast::LitPattern(ref node) => node.id,
            Ast::IdentPattern(ref node) => node.id,
            Ast::TuplePattern(ref node) => node.id,
            Ast::ConstructorPattern(ref node) => node.id,
            Ast::ConstructorField(ref node) => node.id,
            Ast::Rest(ref node) => node.id,
            Ast::Alt(ref node) => node.id,
            Ast::Error(ref node) => node.id,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Ast::Function(ref node) => node.span,
            Ast::Class(ref node) => node.span,
            Ast::Struct(ref node) => node.span,
            Ast::WhereClause(ref node) => node.span,
            Ast::WhereClauseItem(ref node) => node.span,
            Ast::Field(ref node) => node.span,
            Ast::Trait(ref node) => node.span,
            Ast::Impl(ref node) => node.span,
            Ast::Global(ref node) => node.span,
            Ast::Const(ref node) => node.span,
            Ast::Enum(ref node) => node.span,
            Ast::Module(ref node) => node.span,
            Ast::Use(ref node) => node.span,
            Ast::UsePath(ref node) => node.span,
            Ast::UseGroup(ref node) => node.span,
            Ast::UseTargetName(ref node) => node.span,
            Ast::Extern(ref node) => node.span,
            Ast::Alias(ref node) => node.span,
            Ast::Argument(ref node) => node.span,
            Ast::Param(ref node) => node.span,
            Ast::RegularType(ref node) => node.span,
            Ast::TupleType(ref node) => node.span,
            Ast::LambdaType(ref node) => node.span,
            Ast::QualifiedPathType(ref node) => node.span,
            Ast::LetStmt(ref node) => node.span,
            Ast::ExprStmt(ref node) => node.span,
            Ast::Un(ref val) => val.span,
            Ast::Bin(ref val) => val.span,
            Ast::LitChar(ref val) => val.span,
            Ast::LitInt(ref val) => val.span,
            Ast::LitFloat(ref val) => val.span,
            Ast::LitStr(ref val) => val.span,
            Ast::Template(ref val) => val.span,
            Ast::LitBool(ref val) => val.span,
            Ast::Ident(ref val) => val.span,
            Ast::Call(ref val) => val.span,
            Ast::TypeParam(ref val) => val.span,
            Ast::Path(ref val) => val.span,
            Ast::PathData(ref node) => node.span,
            Ast::Dot(ref val) => val.span,
            Ast::This(ref val) => val.span,
            Ast::UpcaseThis(ref val) => val.span,
            Ast::Conv(ref val) => val.span,
            Ast::Is(ref val) => val.span,
            Ast::Lambda(ref val) => val.span,
            Ast::Block(ref val) => val.span,
            Ast::If(ref val) => val.span,
            Ast::Tuple(ref val) => val.span,
            Ast::Paren(ref val) => val.span,
            Ast::Match(ref val) => val.span,
            Ast::MatchArm(ref node) => node.span,
            Ast::For(ref val) => val.span,
            Ast::While(ref val) => val.span,
            Ast::Break(ref val) => val.span,
            Ast::Continue(ref val) => val.span,
            Ast::Return(ref val) => val.span,
            Ast::TypeArgument(ref node) => node.span,
            Ast::Underscore(ref node) => node.span,
            Ast::LitPattern(ref node) => node.span,
            Ast::IdentPattern(ref node) => node.span,
            Ast::TuplePattern(ref node) => node.span,
            Ast::ConstructorPattern(ref node) => node.span,
            Ast::ConstructorField(ref node) => node.span,
            Ast::Rest(ref node) => node.span,
            Ast::Alt(ref node) => node.span,
            Ast::Error(ref node) => node.span,
        }
    }

    pub fn to_function(&self) -> Option<&Function> {
        match self {
            &Ast::Function(ref fct) => Some(fct),
            _ => None,
        }
    }

    pub fn to_where_clause(&self) -> Option<&WhereClause> {
        match self {
            &Ast::WhereClause(ref node) => Some(node),
            _ => None,
        }
    }

    pub fn to_where_clause_item(&self) -> Option<&WhereClauseItem> {
        match self {
            &Ast::WhereClauseItem(ref node) => Some(node),
            _ => None,
        }
    }

    pub fn to_match_arm(&self) -> Option<&MatchArmType> {
        match self {
            &Ast::MatchArm(ref node) => Some(node),
            _ => None,
        }
    }

    pub fn to_type_argument(&self) -> Option<&TypeArgument> {
        match self {
            &Ast::TypeArgument(ref node) => Some(node),
            _ => None,
        }
    }

    pub fn to_use_path(&self) -> Option<&UsePath> {
        match self {
            &Ast::UsePath(ref node) => Some(node),
            _ => None,
        }
    }

    pub fn to_use_group(&self) -> Option<&UseGroup> {
        match self {
            &Ast::UseGroup(ref node) => Some(node),
            _ => None,
        }
    }

    pub fn to_use_target_name(&self) -> Option<&UseTargetName> {
        match self {
            &Ast::UseTargetName(ref node) => Some(node),
            _ => None,
        }
    }

    pub fn to_class(&self) -> Option<&Class> {
        match self {
            &Ast::Class(ref class) => Some(class),
            _ => None,
        }
    }

    pub fn to_field(&self) -> Option<&Field> {
        match self {
            &Ast::Field(ref node) => Some(node),
            _ => None,
        }
    }

    pub fn to_enum(&self) -> Option<&Enum> {
        match self {
            &Ast::Enum(ref enum_) => Some(enum_),
            _ => None,
        }
    }

    pub fn to_module(&self) -> Option<&Module> {
        match self {
            &Ast::Module(ref module) => Some(module),
            _ => None,
        }
    }

    pub fn to_struct(&self) -> Option<&Struct> {
        match self {
            &Ast::Struct(ref struc) => Some(struc),
            _ => None,
        }
    }

    pub fn to_trait(&self) -> Option<&Trait> {
        match self {
            &Ast::Trait(ref trait_) => Some(trait_),
            _ => None,
        }
    }

    pub fn to_impl(&self) -> Option<&Impl> {
        match self {
            &Ast::Impl(ref impl_) => Some(impl_),
            _ => None,
        }
    }

    pub fn to_global(&self) -> Option<&Global> {
        match self {
            &Ast::Global(ref global) => Some(global),
            _ => None,
        }
    }

    pub fn to_const(&self) -> Option<&Const> {
        match self {
            &Ast::Const(ref konst) => Some(konst),
            _ => None,
        }
    }

    pub fn is_argument(&self) -> bool {
        match self {
            &Ast::Argument(..) => true,
            _ => false,
        }
    }

    pub fn to_argument(&self) -> Option<&Argument> {
        match self {
            &Ast::Argument(ref node) => Some(node),
            _ => None,
        }
    }

    pub fn to_param(&self) -> Option<&Param> {
        match self {
            &Ast::Param(ref node) => Some(node),
            _ => None,
        }
    }

    pub fn is_upcase_this(&self) -> bool {
        match self {
            &Ast::UpcaseThis(..) => true,
            _ => false,
        }
    }

    pub fn to_underscore(&self) -> Option<&PatternUnderscore> {
        match self {
            &Ast::Underscore(ref node) => Some(node),
            _ => None,
        }
    }

    pub fn is_underscore(&self) -> bool {
        match self {
            &Ast::Underscore(..) => true,
            _ => false,
        }
    }

    pub fn to_lit_pattern(&self) -> Option<&PatternLit> {
        match self {
            &Ast::LitPattern(ref node) => Some(node),
            _ => None,
        }
    }

    pub fn is_lit_pattern(&self) -> bool {
        match self {
            &Ast::LitPattern(..) => true,
            _ => false,
        }
    }

    pub fn to_ident_pattern(&self) -> Option<&PatternIdent> {
        match self {
            &Ast::IdentPattern(ref node) => Some(node),
            _ => None,
        }
    }

    pub fn is_ident_pattern(&self) -> bool {
        match self {
            &Ast::IdentPattern(..) => true,
            _ => false,
        }
    }

    pub fn to_tuple_pattern(&self) -> Option<&PatternTuple> {
        match self {
            &Ast::TuplePattern(ref node) => Some(node),
            _ => None,
        }
    }

    pub fn is_tuple_pattern(&self) -> bool {
        match self {
            &Ast::TuplePattern(..) => true,
            _ => false,
        }
    }

    pub fn to_constructor_pattern(&self) -> Option<&PatternConstructor> {
        match self {
            &Ast::ConstructorPattern(ref node) => Some(node),
            _ => None,
        }
    }

    pub fn is_constructor_pattern(&self) -> bool {
        match self {
            &Ast::ConstructorPattern(..) => true,
            _ => false,
        }
    }

    pub fn to_constructor_field(&self) -> Option<&PatternField> {
        match self {
            &Ast::ConstructorField(ref node) => Some(node),
            _ => None,
        }
    }

    pub fn is_constructor_field(&self) -> bool {
        match self {
            &Ast::ConstructorField(..) => true,
            _ => false,
        }
    }

    pub fn to_rest(&self) -> Option<&PatternRest> {
        match self {
            &Ast::Rest(ref node) => Some(node),
            _ => None,
        }
    }

    pub fn is_rest(&self) -> bool {
        match self {
            &Ast::Rest(..) => true,
            _ => false,
        }
    }

    pub fn to_alt(&self) -> Option<&PatternAlt> {
        match self {
            &Ast::Alt(ref node) => Some(node),
            _ => None,
        }
    }

    pub fn is_alt(&self) -> bool {
        match self {
            &Ast::Alt(..) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Global {
    pub id: NodeId,
    pub span: Span,
    pub modifiers: Option<ModifierList>,
    pub name: Option<AstId>,
    pub mutable: bool,
    pub data_type: AstId,
    pub initial_value: Option<AstId>,
}

#[derive(Clone, Debug)]
pub struct Module {
    pub id: NodeId,
    pub span: Span,
    pub modifiers: Option<ModifierList>,
    pub name: Option<AstId>,
    pub elements: Option<Vec<AstId>>,
}

#[derive(Clone, Debug)]
pub struct Use {
    pub id: NodeId,
    pub span: Span,
    pub modifiers: Option<ModifierList>,
    pub path: AstId,
}

#[derive(Clone, Debug)]
pub struct UsePath {
    pub id: NodeId,
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
pub struct UseGroup {
    pub id: NodeId,
    pub span: Span,
    pub targets: Vec<AstId>,
}

#[derive(Clone, Debug)]
pub struct UseTargetName {
    pub id: NodeId,
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

#[derive(Clone, Debug)]
pub struct Const {
    pub id: NodeId,
    pub span: Span,
    pub modifiers: Option<ModifierList>,
    pub name: Option<AstId>,
    pub data_type: AstId,
    pub expr: AstId,
}

#[derive(Clone, Debug)]
pub struct Enum {
    pub id: NodeId,
    pub span: Span,
    pub modifiers: Option<ModifierList>,
    pub name: Option<AstId>,
    pub type_params: Option<TypeParams>,
    pub variants: Vec<EnumVariant>,
    pub where_clause: Option<AstId>,
}

#[derive(Clone, Debug)]
pub struct EnumVariant {
    pub id: NodeId,
    pub span: Span,
    pub name: Option<AstId>,
    pub field_name_style: FieldNameStyle,
    pub fields: Vec<AstId>,
}

#[derive(Clone, Debug)]
pub struct Struct {
    pub id: NodeId,
    pub span: Span,
    pub modifiers: Option<ModifierList>,
    pub name: Option<AstId>,
    pub fields: Vec<AstId>,
    pub type_params: Option<TypeParams>,
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

#[derive(Clone, Debug)]
pub struct WhereClause {
    pub id: NodeId,
    pub span: Span,
    pub clauses: Vec<AstId>,
}

#[derive(Clone, Debug)]
pub struct WhereClauseItem {
    pub id: NodeId,
    pub span: Span,
    pub ty: AstId,
    pub bounds: Vec<AstId>,
}

#[derive(Clone, Debug)]
pub struct TypeTupleType {
    pub id: NodeId,
    pub span: Span,

    pub subtypes: Vec<AstId>,
}

#[derive(Clone, Debug)]
pub struct TypeLambdaType {
    pub id: NodeId,
    pub span: Span,

    pub params: Vec<AstId>,
    pub ret: Option<AstId>,
}

#[derive(Clone, Debug)]
pub struct TypeRegularType {
    pub id: NodeId,
    pub span: Span,

    pub path: AstId,
    pub params: Vec<AstId>,
}

#[derive(Clone, Debug)]
pub struct TypeArgument {
    pub id: NodeId,
    pub span: Span,

    pub name: Option<AstId>,
    pub ty: AstId,
}

#[derive(Clone, Debug)]
pub struct TypeGenericType {
    pub id: NodeId,
    pub span: Span,

    pub path: AstId,
    pub params: Vec<AstId>,
}

#[derive(Clone, Debug)]
pub struct TypeQualifiedPathType {
    pub id: NodeId,
    pub span: Span,

    pub ty: AstId,
    pub trait_ty: AstId,
    pub name: Option<AstId>,
}

impl Ast {
    pub fn create_regular(id: NodeId, span: Span, path: AstId, params: Vec<AstId>) -> Ast {
        Ast::RegularType(TypeRegularType {
            id,
            span,
            path,
            params,
        })
    }

    pub fn create_qualified_path(
        id: NodeId,
        span: Span,
        ty: AstId,
        trait_ty: AstId,
        name: Option<AstId>,
    ) -> Ast {
        Ast::QualifiedPathType(TypeQualifiedPathType {
            id,
            span,
            ty,
            trait_ty,
            name,
        })
    }

    pub fn create_fct(id: NodeId, span: Span, params: Vec<AstId>, ret: Option<AstId>) -> Ast {
        Ast::LambdaType(TypeLambdaType {
            id,
            span,
            params,
            ret,
        })
    }

    pub fn create_tuple_type(id: NodeId, span: Span, subtypes: Vec<AstId>) -> Ast {
        Ast::TupleType(TypeTupleType { id, span, subtypes })
    }

    pub fn to_regular(&self) -> Option<&TypeRegularType> {
        match *self {
            Ast::RegularType(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_tuple_type(&self) -> Option<&TypeTupleType> {
        match *self {
            Ast::TupleType(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_fct(&self) -> Option<&TypeLambdaType> {
        match *self {
            Ast::LambdaType(ref val) => Some(val),
            _ => None,
        }
    }

    #[cfg(test)]
    pub fn is_unit(&self) -> bool {
        match self {
            &Ast::TupleType(ref val) if val.subtypes.len() == 0 => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Impl {
    pub id: NodeId,
    pub declaration_span: Span,
    pub span: Span,

    pub modifiers: Option<ModifierList>,
    pub type_params: Option<TypeParams>,
    pub trait_type: Option<AstId>,
    pub extended_type: AstId,
    pub where_clause: Option<AstId>,

    pub methods: Vec<AstId>,
}

#[derive(Clone, Debug)]
pub struct Trait {
    pub id: NodeId,
    pub name: Option<AstId>,
    pub modifiers: Option<ModifierList>,
    pub type_params: Option<TypeParams>,
    pub bounds: Vec<AstId>,
    pub where_clause: Option<AstId>,
    pub span: Span,
    pub methods: Vec<AstId>,
}

#[derive(Clone, Debug)]
pub struct Alias {
    pub id: NodeId,
    pub span: Span,

    pub modifiers: Option<ModifierList>,
    pub name: Option<AstId>,
    pub type_params: Option<TypeParams>,
    pub pre_where_clause: Option<AstId>,
    pub bounds: Vec<AstId>,
    pub ty: Option<AstId>,
    pub post_where_clause: Option<AstId>,
}

#[derive(Clone, Debug)]
pub struct Class {
    pub id: NodeId,
    pub span: Span,
    pub modifiers: Option<ModifierList>,
    pub name: Option<AstId>,

    pub fields: Vec<AstId>,
    pub type_params: Option<TypeParams>,
    pub where_clause: Option<AstId>,
    pub field_name_style: FieldNameStyle,
}

#[derive(Clone, Debug)]
pub struct ExternPackage {
    pub id: NodeId,
    pub span: Span,
    pub modifiers: Option<ModifierList>,
    pub name: Option<AstId>,
    pub identifier: Option<AstId>,
}

#[derive(Clone, Debug)]
pub struct TypeParams {
    pub span: Span,
    pub params: Vec<TypeParam>,
}

#[derive(Clone, Debug)]
pub struct TypeParam {
    pub span: Span,
    pub name: Option<AstId>,
    pub bounds: Vec<AstId>,
}

#[derive(Clone, Debug)]
pub struct Field {
    pub id: NodeId,
    pub span: Span,
    pub modifiers: Option<ModifierList>,
    pub name: Option<AstId>,
    pub data_type: AstId,
}

#[derive(Clone, Debug)]
pub struct Error {
    pub id: NodeId,
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

#[derive(Clone, Debug)]
pub struct Function {
    pub id: NodeId,
    pub declaration_span: Span,
    pub span: Span,
    pub modifiers: Option<ModifierList>,
    pub kind: FunctionKind,

    pub name: Option<AstId>,
    pub type_params: Option<TypeParams>,
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
#[derive(Clone, Debug)]
pub struct ModifierList {
    pub id: NodeId,
    pub span: Span,
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

#[derive(Clone, Debug)]
pub struct Param {
    pub id: NodeId,
    pub span: Span,
    pub pattern: AstId,
    pub data_type: AstId,
    pub variadic: bool,
}

impl Ast {
    pub fn create_let_stmt(
        id: NodeId,
        span: Span,
        pattern: AstId,
        data_type: Option<AstId>,
        expr: Option<AstId>,
    ) -> Ast {
        Ast::LetStmt(StmtLetType {
            id,
            span,

            pattern,
            data_type,
            expr,
        })
    }

    pub fn create_expr_stmt(id: NodeId, span: Span, expr: AstId) -> Ast {
        Ast::ExprStmt(StmtExprType { id, span, expr })
    }

    pub fn to_let(&self) -> Option<&StmtLetType> {
        match *self {
            Ast::LetStmt(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_let(&self) -> bool {
        match *self {
            Ast::LetStmt(_) => true,
            _ => false,
        }
    }

    pub fn to_expr(&self) -> Option<&StmtExprType> {
        match *self {
            Ast::ExprStmt(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_expr(&self) -> bool {
        match *self {
            Ast::ExprStmt(_) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct StmtLetType {
    pub id: NodeId,
    pub span: Span,

    pub pattern: AstId,

    pub data_type: Option<AstId>,
    pub expr: Option<AstId>,
}

#[derive(Clone, Debug)]
pub struct ExprForType {
    pub id: NodeId,
    pub span: Span,

    pub pattern: AstId,
    pub expr: AstId,
    pub block: AstId,
}

#[derive(Clone, Debug)]
pub struct ExprWhileType {
    pub id: NodeId,
    pub span: Span,

    pub cond: AstId,
    pub block: AstId,
}

#[derive(Clone, Debug)]
pub struct StmtExprType {
    pub id: NodeId,
    pub span: Span,

    pub expr: AstId,
}

#[derive(Clone, Debug)]
pub struct ExprReturnType {
    pub id: NodeId,
    pub span: Span,

    pub expr: Option<AstId>,
}

#[derive(Clone, Debug)]
pub struct ExprBreakType {
    pub id: NodeId,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct ExprContinueType {
    pub id: NodeId,
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

impl Ast {
    pub fn create_block(id: NodeId, span: Span, stmts: Vec<AstId>, expr: Option<AstId>) -> Ast {
        Ast::Block(ExprBlockType {
            id,
            span,

            stmts,
            expr,
        })
    }

    pub fn create_if(
        id: NodeId,
        span: Span,
        cond: AstId,
        then_block: AstId,
        else_block: Option<AstId>,
    ) -> Ast {
        Ast::If(ExprIfType {
            id,
            span,
            cond,
            then_block,
            else_block,
        })
    }

    pub fn create_match(id: NodeId, span: Span, expr: AstId, arms: Vec<AstId>) -> Ast {
        Ast::Match(ExprMatchType {
            id,
            span,
            expr,
            arms,
        })
    }

    pub fn create_for(id: NodeId, span: Span, pattern: AstId, expr: AstId, block: AstId) -> Ast {
        Ast::For(ExprForType {
            id,
            span,

            pattern,
            expr,
            block,
        })
    }

    pub fn create_while(id: NodeId, span: Span, cond: AstId, block: AstId) -> Ast {
        Ast::While(ExprWhileType {
            id,
            span,

            cond,
            block,
        })
    }

    pub fn create_return(id: NodeId, span: Span, expr: Option<AstId>) -> Ast {
        Ast::Return(ExprReturnType { id, span, expr })
    }

    pub fn create_break(id: NodeId, span: Span) -> Ast {
        Ast::Break(ExprBreakType { id, span })
    }

    pub fn create_continue(id: NodeId, span: Span) -> Ast {
        Ast::Continue(ExprContinueType { id, span })
    }

    pub fn create_un(id: NodeId, span: Span, op: UnOp, opnd: AstId) -> Ast {
        Ast::Un(ExprUnType { id, span, op, opnd })
    }

    pub fn create_bin(id: NodeId, span: Span, op: BinOp, lhs: AstId, rhs: AstId) -> Ast {
        Ast::Bin(ExprBinType {
            id,
            span,

            op,
            lhs,
            rhs,
        })
    }

    pub fn create_conv(id: NodeId, span: Span, object: AstId, data_type: AstId) -> Ast {
        Ast::Conv(ExprConvType {
            id,
            span,

            object,
            data_type,
        })
    }

    pub fn create_is(id: NodeId, span: Span, object: AstId, pattern: AstId) -> Ast {
        Ast::Is(ExprIsType {
            id,
            span,

            value: object,
            pattern,
        })
    }

    pub fn create_lit_char(id: NodeId, span: Span, full_value: String) -> Ast {
        Ast::LitChar(ExprLitCharType {
            id,
            span,
            value: full_value,
        })
    }

    pub fn create_lit_int(id: NodeId, span: Span, value: String) -> Ast {
        Ast::LitInt(ExprLitIntType { id, span, value })
    }

    pub fn create_lit_float(id: NodeId, span: Span, value: String) -> Ast {
        Ast::LitFloat(ExprLitFloatType { id, span, value })
    }

    pub fn create_lit_str(id: NodeId, span: Span, value: String) -> Ast {
        Ast::LitStr(ExprLitStrType { id, span, value })
    }

    pub fn create_template(id: NodeId, span: Span, parts: Vec<AstId>) -> Ast {
        Ast::Template(ExprTemplateType { id, span, parts })
    }

    pub fn create_lit_bool(id: NodeId, span: Span, value: bool) -> Ast {
        Ast::LitBool(ExprLitBoolType { id, span, value })
    }

    pub fn create_this(id: NodeId, span: Span) -> Ast {
        Ast::This(ExprSelfType { id, span })
    }

    pub fn create_ident(id: NodeId, span: Span, name: String) -> Ast {
        Ast::Ident(Ident { id, span, name })
    }

    pub fn create_paren(id: NodeId, span: Span, expr: AstId) -> Ast {
        Ast::Paren(ExprParenType { id, span, expr })
    }

    pub fn create_call(id: NodeId, span: Span, callee: AstId, args: Vec<AstId>) -> Ast {
        Ast::Call(ExprCallType {
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
        callee: AstId,
        args: Vec<AstId>,
    ) -> Ast {
        Ast::TypeParam(ExprTypeParamType {
            id,
            span,
            op_span,

            callee,
            args,
        })
    }

    pub fn create_path(id: NodeId, span: Span, op_span: Span, lhs: AstId, rhs: AstId) -> Ast {
        Ast::Path(ExprPathType {
            id,
            span,
            op_span,
            lhs,
            rhs,
        })
    }

    pub fn create_dot(id: NodeId, span: Span, op_span: Span, lhs: AstId, rhs: AstId) -> Ast {
        Ast::Dot(ExprDotType {
            id,
            span,
            op_span,

            lhs,
            rhs,
        })
    }

    pub fn create_lambda(id: NodeId, span: Span, fct_id: AstId) -> Ast {
        Ast::Lambda(ExprLambdaType { id, span, fct_id })
    }

    pub fn create_tuple(id: NodeId, span: Span, values: Vec<AstId>) -> Ast {
        Ast::Tuple(ExprTupleType { id, span, values })
    }

    pub fn to_for(&self) -> Option<&ExprForType> {
        match *self {
            Ast::For(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_for(&self) -> bool {
        match *self {
            Ast::For(_) => true,
            _ => false,
        }
    }

    pub fn to_while(&self) -> Option<&ExprWhileType> {
        match *self {
            Ast::While(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_while(&self) -> bool {
        match *self {
            Ast::While(_) => true,
            _ => false,
        }
    }

    pub fn to_un(&self) -> Option<&ExprUnType> {
        match *self {
            Ast::Un(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_un(&self) -> bool {
        match *self {
            Ast::Un(_) => true,
            _ => false,
        }
    }

    pub fn is_un_op(&self, op: UnOp) -> bool {
        match *self {
            Ast::Un(ref e) if e.op == op => true,
            _ => false,
        }
    }

    pub fn to_bin(&self) -> Option<&ExprBinType> {
        match *self {
            Ast::Bin(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_bin_and(&self) -> Option<&ExprBinType> {
        self.to_bin().filter(|e| e.op == BinOp::And)
    }

    pub fn is_bin(&self) -> bool {
        match *self {
            Ast::Bin(_) => true,
            _ => false,
        }
    }

    pub fn is_is(&self) -> bool {
        match *self {
            Ast::Is(_) => true,
            _ => false,
        }
    }

    pub fn to_is(&self) -> Option<&ExprIsType> {
        match *self {
            Ast::Is(ref e) => Some(e),
            _ => None,
        }
    }

    pub fn to_paren(&self) -> Option<&ExprParenType> {
        match *self {
            Ast::Paren(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_paren(&self) -> bool {
        match *self {
            Ast::Paren(_) => true,
            _ => false,
        }
    }

    pub fn to_ident(&self) -> Option<&Ident> {
        match *self {
            Ast::Ident(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_ident(&self) -> bool {
        match *self {
            Ast::Ident(_) => true,
            _ => false,
        }
    }

    pub fn to_call(&self) -> Option<&ExprCallType> {
        match *self {
            Ast::Call(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_call(&self) -> bool {
        match *self {
            Ast::Call(_) => true,
            _ => false,
        }
    }

    pub fn to_path(&self) -> Option<&ExprPathType> {
        match *self {
            Ast::Path(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_path_data(&self) -> Option<&PathData> {
        match *self {
            Ast::PathData(ref node) => Some(node),
            _ => None,
        }
    }

    pub fn is_path(&self) -> bool {
        match *self {
            Ast::Path(_) => true,
            _ => false,
        }
    }

    pub fn to_type_param(&self) -> Option<&ExprTypeParamType> {
        match *self {
            Ast::TypeParam(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_type_param(&self) -> bool {
        match *self {
            Ast::TypeParam(_) => true,
            _ => false,
        }
    }

    pub fn to_lit_char(&self) -> Option<&ExprLitCharType> {
        match *self {
            Ast::LitChar(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_lit_char(&self) -> bool {
        match *self {
            Ast::LitChar(_) => true,
            _ => false,
        }
    }

    pub fn to_lit_int(&self) -> Option<&ExprLitIntType> {
        match *self {
            Ast::LitInt(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_lit_int(&self) -> bool {
        match *self {
            Ast::LitInt(_) => true,
            _ => false,
        }
    }

    pub fn to_template(&self) -> Option<&ExprTemplateType> {
        match *self {
            Ast::Template(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_template(&self) -> bool {
        match *self {
            Ast::Template(_) => true,
            _ => false,
        }
    }

    pub fn to_lit_float(&self) -> Option<&ExprLitFloatType> {
        match *self {
            Ast::LitFloat(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_lit_float(&self) -> bool {
        match *self {
            Ast::LitFloat(_) => true,
            _ => false,
        }
    }

    pub fn to_lit_str(&self) -> Option<&ExprLitStrType> {
        match *self {
            Ast::LitStr(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_lit_str(&self) -> bool {
        match *self {
            Ast::LitStr(_) => true,
            _ => false,
        }
    }

    pub fn to_lit_bool(&self) -> Option<&ExprLitBoolType> {
        match *self {
            Ast::LitBool(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_lit_bool(&self) -> bool {
        match *self {
            Ast::LitBool(_) => true,
            _ => false,
        }
    }

    pub fn is_lit_true(&self) -> bool {
        match *self {
            Ast::LitBool(ref lit) if lit.value => true,
            _ => false,
        }
    }

    pub fn to_dot(&self) -> Option<&ExprDotType> {
        match *self {
            Ast::Dot(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_dot(&self) -> bool {
        match *self {
            Ast::Dot(_) => true,
            _ => false,
        }
    }

    pub fn is_this(&self) -> bool {
        match *self {
            Ast::This(_) => true,
            _ => false,
        }
    }

    pub fn to_conv(&self) -> Option<&ExprConvType> {
        match *self {
            Ast::Conv(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_conv(&self) -> bool {
        match *self {
            Ast::Conv(_) => true,
            _ => false,
        }
    }

    pub fn to_lambda(&self) -> Option<&ExprLambdaType> {
        match *self {
            Ast::Lambda(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_lambda(&self) -> bool {
        match self {
            &Ast::Lambda { .. } => true,
            _ => false,
        }
    }

    pub fn to_tuple(&self) -> Option<&ExprTupleType> {
        match *self {
            Ast::Tuple(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_tuple(&self) -> bool {
        match *self {
            Ast::Tuple(_) => true,
            _ => false,
        }
    }

    pub fn to_block(&self) -> Option<&ExprBlockType> {
        match *self {
            Ast::Block(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_block(&self) -> bool {
        match self {
            &Ast::Block(..) | &Ast::If(..) | &Ast::Match(..) | &Ast::While(..) | &Ast::For(..) => {
                true
            }

            _ => false,
        }
    }

    pub fn to_if(&self) -> Option<&ExprIfType> {
        match *self {
            Ast::If(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_if(&self) -> bool {
        match *self {
            Ast::If(_) => true,
            _ => false,
        }
    }

    pub fn to_break(&self) -> Option<&ExprBreakType> {
        match *self {
            Ast::Break(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_break(&self) -> bool {
        match *self {
            Ast::Break(_) => true,
            _ => false,
        }
    }

    pub fn to_continue(&self) -> Option<&ExprContinueType> {
        match *self {
            Ast::Continue(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_continue(&self) -> bool {
        match *self {
            Ast::Continue(_) => true,
            _ => false,
        }
    }

    pub fn to_return(&self) -> Option<&ExprReturnType> {
        match *self {
            Ast::Return(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_return(&self) -> bool {
        match *self {
            Ast::Return(_) => true,
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

#[derive(Clone, Debug)]
pub struct ExprLambdaType {
    pub id: NodeId,
    pub span: Span,
    pub fct_id: AstId,
}

#[derive(Clone, Debug)]
pub struct ExprIfType {
    pub id: NodeId,
    pub span: Span,

    pub cond: AstId,
    pub then_block: AstId,
    pub else_block: Option<AstId>,
}

#[derive(Clone, Debug)]
pub struct ExprTupleType {
    pub id: NodeId,
    pub span: Span,

    pub values: Vec<AstId>,
}

#[derive(Clone, Debug)]
pub struct ExprConvType {
    pub id: NodeId,
    pub span: Span,

    pub object: AstId,
    pub data_type: AstId,
}

#[derive(Clone, Debug)]
pub struct ExprIsType {
    pub id: NodeId,
    pub span: Span,

    pub value: AstId,
    pub pattern: AstId,
}

#[derive(Clone, Debug)]
pub struct ExprUnType {
    pub id: NodeId,
    pub span: Span,

    pub op: UnOp,
    pub opnd: AstId,
}

#[derive(Clone, Debug)]
pub struct ExprBinType {
    pub id: NodeId,
    pub span: Span,

    pub op: BinOp,
    pub lhs: AstId,
    pub rhs: AstId,
}

#[derive(Clone, Debug)]
pub struct ExprLitCharType {
    pub id: NodeId,
    pub span: Span,
    pub value: String,
}

#[derive(Clone, Debug)]
pub struct ExprLitIntType {
    pub id: NodeId,
    pub span: Span,

    pub value: String,
}

#[derive(Clone, Debug)]
pub struct ExprLitFloatType {
    pub id: NodeId,
    pub span: Span,

    pub value: String,
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

    pub parts: Vec<AstId>,
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

    pub stmts: Vec<AstId>,
    pub expr: Option<AstId>,
}

#[derive(Clone, Debug)]
pub struct ExprSelfType {
    pub id: NodeId,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct UpcaseThis {
    pub id: NodeId,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Ident {
    pub id: NodeId,
    pub span: Span,
    pub name: String,
}

#[derive(Clone, Debug)]
pub struct ExprCallType {
    pub id: NodeId,
    pub span: Span,

    pub callee: AstId,
    pub args: Vec<AstId>,
}

impl ExprCallType {
    pub fn object(&self, file: &File) -> Option<AstId> {
        let callee_node = file.node(self.callee);
        if let Some(type_param) = callee_node.to_type_param() {
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

#[derive(Clone, Debug)]
pub struct Argument {
    pub id: NodeId,
    pub span: Span,
    pub name: Option<AstId>,
    pub expr: AstId,
}

#[derive(Clone, Debug)]
pub struct ExprParenType {
    pub id: NodeId,
    pub span: Span,
    pub expr: AstId,
}

#[derive(Clone, Debug)]
pub struct ExprMatchType {
    pub id: NodeId,
    pub span: Span,

    pub expr: AstId,
    pub arms: Vec<AstId>,
}

#[derive(Clone, Debug)]
pub struct MatchArmType {
    pub id: NodeId,
    pub span: Span,

    pub pattern: AstId,
    pub cond: Option<AstId>,
    pub value: AstId,
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

    pub alts: Vec<AstId>,
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
    Constructor(PatternConstructor),
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
            Pattern::Constructor(p) => p.id,
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
            Pattern::Constructor(p) => p.span,
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

#[derive(Clone, Debug)]
pub struct PatternIdent {
    pub id: NodeId,
    pub span: Span,
    pub mutable: bool,
    pub name: AstId,
}

#[derive(Clone, Debug)]
pub struct PatternTuple {
    pub id: NodeId,
    pub span: Span,
    pub params: Vec<AstId>,
}

#[derive(Clone, Debug)]
pub struct PatternConstructor {
    pub id: NodeId,
    pub span: Span,
    pub path: AstId,
    pub params: Option<Vec<AstId>>,
}

#[derive(Clone, Debug)]
pub struct PatternField {
    pub id: NodeId,
    pub span: Span,
    pub ident: Option<AstId>,
    pub pattern: AstId,
}

#[derive(Clone, Debug)]
pub struct PatternParam {
    pub id: NodeId,
    pub span: Span,
    pub mutable: bool,
    pub name: Option<AstId>,
}

#[derive(Clone, Debug)]
pub struct PathData {
    pub id: NodeId,
    pub span: Span,
    pub segments: Vec<AstId>,
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
    pub name: AstId,
}

#[derive(Clone, Debug)]
pub struct ExprTypeParamType {
    pub id: NodeId,
    pub span: Span,
    pub op_span: Span,

    pub callee: AstId,
    pub args: Vec<AstId>,
}

#[derive(Clone, Debug)]
pub struct ExprPathType {
    pub id: NodeId,
    pub span: Span,
    pub op_span: Span,

    pub lhs: AstId,
    pub rhs: AstId,
}

#[derive(Clone, Debug)]
pub struct ExprDotType {
    pub id: NodeId,
    pub span: Span,
    pub op_span: Span,

    pub lhs: AstId,
    pub rhs: AstId,
}
