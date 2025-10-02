use std::sync::Arc;

use dora_parser_derive::AstNode;
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
    RegularType(RegularType),
    TupleType(TupleType),
    LambdaType(LambdaType),
    QualifiedPathType(QualifiedPathType),
    Let(Let),
    ExprStmt(ExprStmt),
    Un(Un),
    Bin(Bin),
    LitChar(LitChar),
    LitInt(LitInt),
    LitFloat(LitFloat),
    LitStr(LitStr),
    Template(Template),
    LitBool(LitBool),
    Ident(Ident),
    Call(Call),
    TypedExpr(TypedExpr),
    Path(Path),
    PathData(PathData),
    Dot(Dot),
    This(This),
    UpcaseThis(UpcaseThis),
    Conv(Conv),
    Is(Is),
    Lambda(Lambda),
    Block(Block),
    If(If),
    For(For),
    While(While),
    Tuple(Tuple),
    Paren(Paren),
    Match(Match),
    MatchArm(Arm),
    Break(Break),
    Continue(Continue),
    Return(Return),
    TypeArgument(TypeArgument),
    Underscore(Underscore),
    LitPattern(LitPattern),
    IdentPattern(IdentPattern),
    TuplePattern(TuplePattern),
    CtorPattern(CtorPattern),
    CtorField(CtorField),
    Rest(Rest),
    Alt(Alt),
    ModifierList(ModifierList),
    Modifier(Modifier),
    Error(Error),
}

impl Ast {
    pub fn span(&self) -> Span {
        match self {
            Ast::Function(node) => node.span,
            Ast::Class(node) => node.span,
            Ast::Struct(node) => node.span,
            Ast::WhereClause(node) => node.span,
            Ast::WhereClauseItem(node) => node.span,
            Ast::Field(node) => node.span,
            Ast::Trait(node) => node.span,
            Ast::Impl(node) => node.span,
            Ast::Global(node) => node.span,
            Ast::Const(node) => node.span,
            Ast::Enum(node) => node.span,
            Ast::Module(node) => node.span,
            Ast::Use(node) => node.span,
            Ast::UsePath(node) => node.span,
            Ast::UseGroup(node) => node.span,
            Ast::UseTargetName(node) => node.span,
            Ast::Extern(node) => node.span,
            Ast::Alias(node) => node.span,
            Ast::Argument(node) => node.span,
            Ast::Param(node) => node.span,
            Ast::RegularType(node) => node.span,
            Ast::TupleType(node) => node.span,
            Ast::LambdaType(node) => node.span,
            Ast::QualifiedPathType(node) => node.span,
            Ast::Let(node) => node.span,
            Ast::ExprStmt(node) => node.span,
            Ast::Un(node) => node.span,
            Ast::Bin(node) => node.span,
            Ast::LitChar(node) => node.span,
            Ast::LitInt(node) => node.span,
            Ast::LitFloat(node) => node.span,
            Ast::LitStr(node) => node.span,
            Ast::Template(node) => node.span,
            Ast::LitBool(node) => node.span,
            Ast::Ident(node) => node.span,
            Ast::Call(node) => node.span,
            Ast::TypedExpr(node) => node.span,
            Ast::Path(node) => node.span,
            Ast::PathData(node) => node.span,
            Ast::Dot(node) => node.span,
            Ast::This(node) => node.span,
            Ast::UpcaseThis(node) => node.span,
            Ast::Conv(node) => node.span,
            Ast::Is(node) => node.span,
            Ast::Lambda(node) => node.span,
            Ast::Block(node) => node.span,
            Ast::If(node) => node.span,
            Ast::Tuple(node) => node.span,
            Ast::Paren(node) => node.span,
            Ast::Match(node) => node.span,
            Ast::MatchArm(node) => node.span,
            Ast::For(node) => node.span,
            Ast::While(node) => node.span,
            Ast::Break(node) => node.span,
            Ast::Continue(node) => node.span,
            Ast::Return(node) => node.span,
            Ast::TypeArgument(node) => node.span,
            Ast::Underscore(node) => node.span,
            Ast::LitPattern(node) => node.span,
            Ast::IdentPattern(node) => node.span,
            Ast::TuplePattern(node) => node.span,
            Ast::CtorPattern(node) => node.span,
            Ast::CtorField(node) => node.span,
            Ast::Rest(node) => node.span,
            Ast::Alt(node) => node.span,
            Ast::ModifierList(node) => node.span,
            Ast::Modifier(node) => node.span,
            Ast::Error(node) => node.span,
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            Ast::Function(node) => node.name(),
            Ast::Class(node) => node.name(),
            Ast::Struct(node) => node.name(),
            Ast::WhereClause(node) => node.name(),
            Ast::WhereClauseItem(node) => node.name(),
            Ast::Field(node) => node.name(),
            Ast::Trait(node) => node.name(),
            Ast::Impl(node) => node.name(),
            Ast::Global(node) => node.name(),
            Ast::Const(node) => node.name(),
            Ast::Enum(node) => node.name(),
            Ast::Module(node) => node.name(),
            Ast::Use(node) => node.name(),
            Ast::UsePath(node) => node.name(),
            Ast::UseGroup(node) => node.name(),
            Ast::UseTargetName(node) => node.name(),
            Ast::Extern(node) => node.name(),
            Ast::Alias(node) => node.name(),
            Ast::Argument(node) => node.name(),
            Ast::Param(node) => node.name(),
            Ast::RegularType(node) => node.name(),
            Ast::TupleType(node) => node.name(),
            Ast::LambdaType(node) => node.name(),
            Ast::QualifiedPathType(node) => node.name(),
            Ast::Let(node) => node.name(),
            Ast::ExprStmt(node) => node.name(),
            Ast::Un(node) => node.name(),
            Ast::Bin(node) => node.name(),
            Ast::LitChar(node) => node.name(),
            Ast::LitInt(node) => node.name(),
            Ast::LitFloat(node) => node.name(),
            Ast::LitStr(node) => node.name(),
            Ast::Template(node) => node.name(),
            Ast::LitBool(node) => node.name(),
            Ast::Ident(node) => node.name(),
            Ast::Call(node) => node.name(),
            Ast::TypedExpr(node) => node.name(),
            Ast::Path(node) => node.name(),
            Ast::PathData(node) => node.name(),
            Ast::Dot(node) => node.name(),
            Ast::This(node) => node.name(),
            Ast::UpcaseThis(node) => node.name(),
            Ast::Conv(node) => node.name(),
            Ast::Is(node) => node.name(),
            Ast::Lambda(node) => node.name(),
            Ast::Block(node) => node.name(),
            Ast::If(node) => node.name(),
            Ast::Tuple(node) => node.name(),
            Ast::Paren(node) => node.name(),
            Ast::Match(node) => node.name(),
            Ast::MatchArm(node) => node.name(),
            Ast::For(node) => node.name(),
            Ast::While(node) => node.name(),
            Ast::Break(node) => node.name(),
            Ast::Continue(node) => node.name(),
            Ast::Return(node) => node.name(),
            Ast::TypeArgument(node) => node.name(),
            Ast::Underscore(node) => node.name(),
            Ast::LitPattern(node) => node.name(),
            Ast::IdentPattern(node) => node.name(),
            Ast::TuplePattern(node) => node.name(),
            Ast::CtorPattern(node) => node.name(),
            Ast::CtorField(node) => node.name(),
            Ast::Rest(node) => node.name(),
            Ast::Alt(node) => node.name(),
            Ast::ModifierList(node) => node.name(),
            Ast::Modifier(node) => node.name(),
            Ast::Error(node) => node.name(),
        }
    }

    pub fn children(&self) -> Vec<AstId> {
        match self {
            Ast::Function(node) => node.children(),
            Ast::Class(node) => node.children(),
            Ast::Struct(node) => node.children(),
            Ast::WhereClause(node) => node.children(),
            Ast::WhereClauseItem(node) => node.children(),
            Ast::Field(node) => node.children(),
            Ast::Trait(node) => node.children(),
            Ast::Impl(node) => node.children(),
            Ast::Global(node) => node.children(),
            Ast::Const(node) => node.children(),
            Ast::Enum(node) => node.children(),
            Ast::Module(node) => node.children(),
            Ast::Use(node) => node.children(),
            Ast::UsePath(node) => node.children(),
            Ast::UseGroup(node) => node.children(),
            Ast::UseTargetName(node) => node.children(),
            Ast::Extern(node) => node.children(),
            Ast::Alias(node) => node.children(),
            Ast::Argument(node) => node.children(),
            Ast::Param(node) => node.children(),
            Ast::RegularType(node) => node.children(),
            Ast::TupleType(node) => node.children(),
            Ast::LambdaType(node) => node.children(),
            Ast::QualifiedPathType(node) => node.children(),
            Ast::Let(node) => node.children(),
            Ast::ExprStmt(node) => node.children(),
            Ast::Un(node) => node.children(),
            Ast::Bin(node) => node.children(),
            Ast::LitChar(node) => node.children(),
            Ast::LitInt(node) => node.children(),
            Ast::LitFloat(node) => node.children(),
            Ast::LitStr(node) => node.children(),
            Ast::Template(node) => node.children(),
            Ast::LitBool(node) => node.children(),
            Ast::Ident(node) => node.children(),
            Ast::Call(node) => node.children(),
            Ast::TypedExpr(node) => node.children(),
            Ast::Path(node) => node.children(),
            Ast::PathData(node) => node.children(),
            Ast::Dot(node) => node.children(),
            Ast::This(node) => node.children(),
            Ast::UpcaseThis(node) => node.children(),
            Ast::Conv(node) => node.children(),
            Ast::Is(node) => node.children(),
            Ast::Lambda(node) => node.children(),
            Ast::Block(node) => node.children(),
            Ast::If(node) => node.children(),
            Ast::Tuple(node) => node.children(),
            Ast::Paren(node) => node.children(),
            Ast::Match(node) => node.children(),
            Ast::MatchArm(node) => node.children(),
            Ast::For(node) => node.children(),
            Ast::While(node) => node.children(),
            Ast::Break(node) => node.children(),
            Ast::Continue(node) => node.children(),
            Ast::Return(node) => node.children(),
            Ast::TypeArgument(node) => node.children(),
            Ast::Underscore(node) => node.children(),
            Ast::LitPattern(node) => node.children(),
            Ast::IdentPattern(node) => node.children(),
            Ast::TuplePattern(node) => node.children(),
            Ast::CtorPattern(node) => node.children(),
            Ast::CtorField(node) => node.children(),
            Ast::Rest(node) => node.children(),
            Ast::Alt(node) => node.children(),
            Ast::ModifierList(node) => node.children(),
            Ast::Modifier(node) => node.children(),
            Ast::Error(node) => node.children(),
        }
    }

    pub fn to_function(&self) -> Option<&Function> {
        match self {
            &Ast::Function(ref fct) => Some(fct),
            _ => None,
        }
    }

    pub fn to_modifier_list(&self) -> Option<&ModifierList> {
        match self {
            &Ast::ModifierList(ref fct) => Some(fct),
            _ => None,
        }
    }

    pub fn to_modifier(&self) -> Option<&Modifier> {
        match self {
            &Ast::Modifier(ref fct) => Some(fct),
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

    pub fn to_match_arm(&self) -> Option<&Arm> {
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

    pub fn to_underscore(&self) -> Option<&Underscore> {
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

    pub fn to_lit_pattern(&self) -> Option<&LitPattern> {
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

    pub fn to_ident_pattern(&self) -> Option<&IdentPattern> {
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

    pub fn to_tuple_pattern(&self) -> Option<&TuplePattern> {
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

    pub fn to_constructor_pattern(&self) -> Option<&CtorPattern> {
        match self {
            &Ast::CtorPattern(ref node) => Some(node),
            _ => None,
        }
    }

    pub fn is_constructor_pattern(&self) -> bool {
        match self {
            &Ast::CtorPattern(..) => true,
            _ => false,
        }
    }

    pub fn to_constructor_field(&self) -> Option<&CtorField> {
        match self {
            &Ast::CtorField(ref node) => Some(node),
            _ => None,
        }
    }

    pub fn is_constructor_field(&self) -> bool {
        match self {
            &Ast::CtorField(..) => true,
            _ => false,
        }
    }

    pub fn to_rest(&self) -> Option<&Rest> {
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

    pub fn to_alt(&self) -> Option<&Alt> {
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
    pub type_params: Option<TypeParams>,
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

    pub fn to_regular(&self) -> Option<&RegularType> {
        match *self {
            Ast::RegularType(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_tuple_type(&self) -> Option<&TupleType> {
        match *self {
            Ast::TupleType(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_fct(&self) -> Option<&LambdaType> {
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

#[derive(Clone, Debug, AstNode)]
pub struct Impl {
    pub declaration_span: Span,
    pub span: Span,

    pub modifiers: Option<AstId>,
    pub type_params: Option<TypeParams>,
    pub trait_type: Option<AstId>,
    pub extended_type: AstId,
    pub where_clause: Option<AstId>,

    pub methods: Vec<AstId>,
}

#[derive(Clone, Debug, AstNode)]
pub struct Trait {
    pub name: Option<AstId>,
    pub modifiers: Option<AstId>,
    pub type_params: Option<TypeParams>,
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
    pub type_params: Option<TypeParams>,
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
    pub type_params: Option<TypeParams>,
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

impl Ast {
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

    pub fn to_let(&self) -> Option<&Let> {
        match *self {
            Ast::Let(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_let(&self) -> bool {
        match *self {
            Ast::Let(_) => true,
            _ => false,
        }
    }

    pub fn to_expr(&self) -> Option<&ExprStmt> {
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

impl Ast {
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

    pub fn to_for(&self) -> Option<&For> {
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

    pub fn to_while(&self) -> Option<&While> {
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

    pub fn to_un(&self) -> Option<&Un> {
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

    pub fn to_bin(&self) -> Option<&Bin> {
        match *self {
            Ast::Bin(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn to_bin_and(&self) -> Option<&Bin> {
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

    pub fn to_is(&self) -> Option<&Is> {
        match *self {
            Ast::Is(ref e) => Some(e),
            _ => None,
        }
    }

    pub fn to_paren(&self) -> Option<&Paren> {
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

    pub fn to_call(&self) -> Option<&Call> {
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

    pub fn to_path(&self) -> Option<&Path> {
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

    pub fn to_type_param(&self) -> Option<&TypedExpr> {
        match *self {
            Ast::TypedExpr(ref val) => Some(val),
            _ => None,
        }
    }

    pub fn is_type_param(&self) -> bool {
        match *self {
            Ast::TypedExpr(_) => true,
            _ => false,
        }
    }

    pub fn to_lit_char(&self) -> Option<&LitChar> {
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

    pub fn to_lit_int(&self) -> Option<&LitInt> {
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

    pub fn to_template(&self) -> Option<&Template> {
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

    pub fn to_lit_float(&self) -> Option<&LitFloat> {
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

    pub fn to_lit_str(&self) -> Option<&LitStr> {
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

    pub fn to_lit_bool(&self) -> Option<&LitBool> {
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

    pub fn to_dot(&self) -> Option<&Dot> {
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

    pub fn to_conv(&self) -> Option<&Conv> {
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

    pub fn to_lambda(&self) -> Option<&Lambda> {
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

    pub fn to_tuple(&self) -> Option<&Tuple> {
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

    pub fn to_block(&self) -> Option<&Block> {
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

    pub fn to_if(&self) -> Option<&If> {
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

    pub fn to_break(&self) -> Option<&Break> {
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

    pub fn to_continue(&self) -> Option<&Continue> {
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

    pub fn to_return(&self) -> Option<&Return> {
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
