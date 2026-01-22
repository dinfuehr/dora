use std::cell::{Cell, OnceCell, RefCell};
use std::rc::Rc;

use id_arena::Arena;

use dora_parser::ast::SyntaxNodePtr;

use crate::sema::{
    ArrayAssignment, CallType, ConstValue, FctDefinitionId, ForTypeInfo, IdentType,
    LazyContextData, LazyLambdaId, NodeMap, TypeRefArena, TypeRefId, VarAccess, VarId,
};
use crate::{SourceType, SourceTypeArray};

use crate::sema::{Expr, ExprId, Pattern, PatternId, Stmt, StmtId};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum UniversalId {
    Expr(ExprId),
    Stmt(StmtId),
    Pattern(PatternId),
    TypeRef(TypeRefId),
}

pub trait ExprMapId {
    fn to_universal_id(self) -> UniversalId;
}

impl ExprMapId for ExprId {
    fn to_universal_id(self) -> UniversalId {
        UniversalId::Expr(self)
    }
}

impl ExprMapId for StmtId {
    fn to_universal_id(self) -> UniversalId {
        UniversalId::Stmt(self)
    }
}

impl ExprMapId for PatternId {
    fn to_universal_id(self) -> UniversalId {
        UniversalId::Pattern(self)
    }
}

impl ExprMapId for TypeRefId {
    fn to_universal_id(self) -> UniversalId {
        UniversalId::TypeRef(self)
    }
}

pub struct ExprArena {
    exprs: Arena<Expr>,
    syntax_node_ptrs: Vec<Option<SyntaxNodePtr>>,
}

impl ExprArena {
    fn empty() -> ExprArena {
        ExprArena {
            exprs: Arena::new(),
            syntax_node_ptrs: Vec::new(),
        }
    }

    pub fn expr(&self, id: ExprId) -> &Expr {
        &self.exprs[id]
    }

    pub fn syntax_node_ptr(&self, id: ExprId) -> SyntaxNodePtr {
        self.syntax_node_ptrs[id.index()].expect("missing syntax node")
    }
}

pub struct ExprArenaBuilder {
    exprs: Arena<Expr>,
    syntax_node_ptrs: Vec<Option<SyntaxNodePtr>>,
}

impl ExprArenaBuilder {
    pub fn new() -> ExprArenaBuilder {
        ExprArenaBuilder {
            exprs: Arena::new(),
            syntax_node_ptrs: Vec::new(),
        }
    }

    pub fn alloc_expr(&mut self, expr: Expr, syntax_node_ptr: Option<SyntaxNodePtr>) -> ExprId {
        let id = self.exprs.alloc(expr);
        self.syntax_node_ptrs.push(syntax_node_ptr);
        debug_assert_eq!(id.index(), self.syntax_node_ptrs.len() - 1);
        id
    }

    pub fn freeze(self) -> Rc<ExprArena> {
        Rc::new(ExprArena {
            exprs: self.exprs,
            syntax_node_ptrs: self.syntax_node_ptrs,
        })
    }
}

pub struct StmtArena {
    stmts: Arena<Stmt>,
    syntax_node_ptrs: Vec<Option<SyntaxNodePtr>>,
}

impl StmtArena {
    fn empty() -> StmtArena {
        StmtArena {
            stmts: Arena::new(),
            syntax_node_ptrs: Vec::new(),
        }
    }

    pub fn stmt(&self, id: StmtId) -> &Stmt {
        &self.stmts[id]
    }

    pub fn syntax_node_ptr(&self, id: StmtId) -> SyntaxNodePtr {
        self.syntax_node_ptrs[id.index()].expect("missing syntax node")
    }
}

pub struct StmtArenaBuilder {
    stmts: Arena<Stmt>,
    syntax_node_ptrs: Vec<Option<SyntaxNodePtr>>,
}

impl StmtArenaBuilder {
    pub fn new() -> StmtArenaBuilder {
        StmtArenaBuilder {
            stmts: Arena::new(),
            syntax_node_ptrs: Vec::new(),
        }
    }

    pub fn alloc_stmt(&mut self, stmt: Stmt, syntax_node_ptr: Option<SyntaxNodePtr>) -> StmtId {
        let id = self.stmts.alloc(stmt);
        self.syntax_node_ptrs.push(syntax_node_ptr);
        debug_assert_eq!(id.index(), self.syntax_node_ptrs.len() - 1);
        id
    }

    pub fn freeze(self) -> Rc<StmtArena> {
        Rc::new(StmtArena {
            stmts: self.stmts,
            syntax_node_ptrs: self.syntax_node_ptrs,
        })
    }
}

pub struct PatternArena {
    patterns: Arena<Pattern>,
    syntax_node_ptrs: Vec<Option<SyntaxNodePtr>>,
}

impl PatternArena {
    fn empty() -> PatternArena {
        PatternArena {
            patterns: Arena::new(),
            syntax_node_ptrs: Vec::new(),
        }
    }

    pub fn pattern(&self, id: PatternId) -> &Pattern {
        &self.patterns[id]
    }

    pub fn syntax_node_ptr(&self, id: PatternId) -> SyntaxNodePtr {
        self.syntax_node_ptrs[id.index()].expect("missing syntax node")
    }
}

pub struct PatternArenaBuilder {
    patterns: Arena<Pattern>,
    syntax_node_ptrs: Vec<Option<SyntaxNodePtr>>,
}

impl PatternArenaBuilder {
    pub fn new() -> PatternArenaBuilder {
        PatternArenaBuilder {
            patterns: Arena::new(),
            syntax_node_ptrs: Vec::new(),
        }
    }

    pub fn alloc_pattern(
        &mut self,
        pattern: Pattern,
        syntax_node_ptr: Option<SyntaxNodePtr>,
    ) -> PatternId {
        let id = self.patterns.alloc(pattern);
        self.syntax_node_ptrs.push(syntax_node_ptr);
        debug_assert_eq!(id.index(), self.syntax_node_ptrs.len() - 1);
        id
    }

    pub fn freeze(self) -> Rc<PatternArena> {
        Rc::new(PatternArena {
            patterns: self.patterns,
            syntax_node_ptrs: self.syntax_node_ptrs,
        })
    }
}

pub struct Body {
    arena: Rc<ExprArena>,
    stmt_arena: Rc<StmtArena>,
    pattern_arena: Rc<PatternArena>,
    type_refs: Rc<TypeRefArena>,
    root_expr_id: Option<ExprId>,
    param_pattern_ids: OnceCell<Vec<PatternId>>,
    has_self: Cell<Option<bool>>,
    map_templates: RefCell<NodeMap<(FctDefinitionId, SourceTypeArray)>>,
    map_calls: RefCell<NodeMap<Rc<CallType>>>,
    map_idents: RefCell<NodeMap<IdentType>>,
    map_tys: RefCell<NodeMap<SourceType>>,
    map_vars: RefCell<NodeMap<VarId>>,
    map_consts: RefCell<NodeMap<ConstValue>>,
    map_fors: RefCell<NodeMap<ForTypeInfo>>,
    map_lambdas: RefCell<NodeMap<LazyLambdaId>>,
    map_block_contexts: RefCell<NodeMap<LazyContextData>>,
    map_argument: RefCell<NodeMap<usize>>,
    map_field_ids: RefCell<NodeMap<usize>>,
    map_array_assignments: RefCell<NodeMap<ArrayAssignment>>,
    vars: RefCell<VarAccess>,
    function_context_data: OnceCell<LazyContextData>,
    needs_context_slot_in_lambda_object: OnceCell<bool>,
    outer_contexts: RefCell<Vec<LazyContextData>>,
}

impl std::fmt::Debug for Body {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Body")
            .field("expr_count", &self.arena.exprs.len())
            .field("stmt_count", &self.stmt_arena.stmts.len())
            .field("pattern_count", &self.pattern_arena.patterns.len())
            .field("root_expr_id", &self.root_expr_id)
            .finish()
    }
}

impl Body {
    pub fn new() -> Body {
        Body::new_with_arenas(
            Rc::new(ExprArena::empty()),
            Rc::new(StmtArena::empty()),
            Rc::new(PatternArena::empty()),
            Rc::new(TypeRefArena::new()),
        )
    }

    pub fn new_with_arena(arena: Rc<ExprArena>) -> Body {
        Body::new_with_arenas(
            arena,
            Rc::new(StmtArena::empty()),
            Rc::new(PatternArena::empty()),
            Rc::new(TypeRefArena::new()),
        )
    }

    pub fn new_with_arenas(
        arena: Rc<ExprArena>,
        stmt_arena: Rc<StmtArena>,
        pattern_arena: Rc<PatternArena>,
        type_refs: Rc<TypeRefArena>,
    ) -> Body {
        Body {
            arena,
            stmt_arena,
            pattern_arena,
            type_refs,
            root_expr_id: None,
            param_pattern_ids: OnceCell::new(),
            has_self: Cell::new(None),
            map_templates: RefCell::new(NodeMap::new()),
            map_calls: RefCell::new(NodeMap::new()),
            map_idents: RefCell::new(NodeMap::new()),
            map_tys: RefCell::new(NodeMap::new()),
            map_vars: RefCell::new(NodeMap::new()),
            map_consts: RefCell::new(NodeMap::new()),
            map_fors: RefCell::new(NodeMap::new()),
            map_lambdas: RefCell::new(NodeMap::new()),
            map_block_contexts: RefCell::new(NodeMap::new()),
            map_argument: RefCell::new(NodeMap::new()),
            map_field_ids: RefCell::new(NodeMap::new()),
            map_array_assignments: RefCell::new(NodeMap::new()),
            vars: RefCell::new(VarAccess::new(Vec::new())),
            function_context_data: OnceCell::new(),
            needs_context_slot_in_lambda_object: OnceCell::new(),
            outer_contexts: RefCell::new(Vec::new()),
        }
    }

    pub(crate) fn arena(&self) -> Rc<ExprArena> {
        self.arena.clone()
    }

    pub(crate) fn stmt_arena(&self) -> Rc<StmtArena> {
        self.stmt_arena.clone()
    }

    pub(crate) fn pattern_arena(&self) -> Rc<PatternArena> {
        self.pattern_arena.clone()
    }

    pub fn exprs(&self) -> &ExprArena {
        self.arena.as_ref()
    }

    pub fn expr(&self, id: ExprId) -> &Expr {
        self.exprs().expr(id)
    }

    pub fn patterns(&self) -> &PatternArena {
        self.pattern_arena.as_ref()
    }

    pub fn pattern(&self, id: PatternId) -> &Pattern {
        self.patterns().pattern(id)
    }

    pub(crate) fn type_ref_arena(&self) -> Rc<TypeRefArena> {
        self.type_refs.clone()
    }

    pub fn type_refs(&self) -> &TypeRefArena {
        self.type_refs.as_ref()
    }

    pub fn stmts(&self) -> &StmtArena {
        self.stmt_arena.as_ref()
    }

    pub fn stmt(&self, id: StmtId) -> &Stmt {
        self.stmts().stmt(id)
    }

    pub fn get_ident<T: ExprMapId>(&self, id: T) -> Option<IdentType> {
        let id = id.to_universal_id();
        self.map_idents.borrow().get(id).cloned()
    }

    pub fn insert_ident<T: ExprMapId>(&self, id: T, ident: IdentType) {
        let id = id.to_universal_id();
        self.map_idents.borrow_mut().insert(id, ident);
    }

    pub fn insert_or_replace_ident<T: ExprMapId>(&self, id: T, ident: IdentType) {
        let id = id.to_universal_id();
        self.map_idents.borrow_mut().insert_or_replace(id, ident);
    }

    pub fn get_template<T: ExprMapId>(&self, id: T) -> Option<(FctDefinitionId, SourceTypeArray)> {
        let id = id.to_universal_id();
        self.map_templates.borrow().get(id).cloned()
    }

    pub fn insert_template<T: ExprMapId>(&self, id: T, data: (FctDefinitionId, SourceTypeArray)) {
        let id = id.to_universal_id();
        self.map_templates.borrow_mut().insert(id, data);
    }

    pub fn insert_template_expr(&self, id: ExprId, data: (FctDefinitionId, SourceTypeArray)) {
        self.insert_template(id, data);
    }

    pub fn get_call_type<T: ExprMapId>(&self, id: T) -> Option<Rc<CallType>> {
        let id = id.to_universal_id();
        self.map_calls.borrow().get(id).cloned()
    }

    pub fn insert_call_type<T: ExprMapId>(&self, id: T, call_type: Rc<CallType>) {
        let id = id.to_universal_id();
        self.map_calls.borrow_mut().insert(id, call_type);
    }

    pub fn insert_call_type_expr(&self, id: ExprId, call_type: Rc<CallType>) {
        self.insert_call_type(id, call_type);
    }

    pub fn insert_or_replace_call_type<T: ExprMapId>(&self, id: T, call_type: Rc<CallType>) {
        let id = id.to_universal_id();
        self.map_calls.borrow_mut().insert_or_replace(id, call_type);
    }

    pub fn insert_or_replace_call_type_expr(&self, id: ExprId, call_type: Rc<CallType>) {
        self.insert_or_replace_call_type(id, call_type);
    }

    pub fn get_var_id<T: ExprMapId>(&self, id: T) -> Option<VarId> {
        let id = id.to_universal_id();
        self.map_vars.borrow().get(id).cloned()
    }

    pub fn insert_var_id<T: ExprMapId>(&self, id: T, var_id: VarId) {
        let id = id.to_universal_id();
        self.map_vars.borrow_mut().insert(id, var_id);
    }

    pub fn get_for_type_info<T: ExprMapId>(&self, id: T) -> Option<ForTypeInfo> {
        let id = id.to_universal_id();
        self.map_fors.borrow().get(id).cloned()
    }

    pub fn insert_for_type_info<T: ExprMapId>(&self, id: T, info: ForTypeInfo) {
        let id = id.to_universal_id();
        self.map_fors.borrow_mut().insert(id, info);
    }

    pub fn get_lambda<T: ExprMapId>(&self, id: T) -> Option<LazyLambdaId> {
        let id = id.to_universal_id();
        self.map_lambdas.borrow().get(id).cloned()
    }

    pub fn insert_lambda<T: ExprMapId>(&self, id: T, lambda: LazyLambdaId) {
        let id = id.to_universal_id();
        self.map_lambdas.borrow_mut().insert(id, lambda);
    }

    pub fn get_block_context<T: ExprMapId>(&self, id: T) -> Option<LazyContextData> {
        let id = id.to_universal_id();
        self.map_block_contexts.borrow().get(id).cloned()
    }

    pub fn insert_block_context<T: ExprMapId>(&self, id: T, context: LazyContextData) {
        let id = id.to_universal_id();
        self.map_block_contexts.borrow_mut().insert(id, context);
    }

    pub fn get_argument<T: ExprMapId>(&self, id: T) -> Option<usize> {
        let id = id.to_universal_id();
        self.map_argument.borrow().get(id).cloned()
    }

    pub fn insert_argument<T: ExprMapId>(&self, id: T, argument: usize) {
        let id = id.to_universal_id();
        self.map_argument.borrow_mut().insert(id, argument);
    }

    pub fn get_field_id<T: ExprMapId>(&self, id: T) -> Option<usize> {
        let id = id.to_universal_id();
        self.map_field_ids.borrow().get(id).cloned()
    }

    pub fn insert_field_id<T: ExprMapId>(&self, id: T, field_id: usize) {
        let id = id.to_universal_id();
        self.map_field_ids.borrow_mut().insert(id, field_id);
    }

    pub fn get_array_assignment<T: ExprMapId>(&self, id: T) -> Option<ArrayAssignment> {
        let id = id.to_universal_id();
        self.map_array_assignments.borrow().get(id).cloned()
    }

    pub fn insert_array_assignment<T: ExprMapId>(&self, id: T, assignment: ArrayAssignment) {
        let id = id.to_universal_id();
        self.map_array_assignments
            .borrow_mut()
            .insert(id, assignment);
    }

    pub fn root_expr_id(&self) -> ExprId {
        self.root_expr_id.expect("missing body expr id")
    }

    pub fn set_root_expr_id(&mut self, id: ExprId) {
        assert!(self.root_expr_id.replace(id).is_none());
    }

    pub fn param_pattern_ids(&self) -> &[PatternId] {
        self.param_pattern_ids
            .get()
            .expect("param_pattern_ids not initialized")
    }

    pub fn set_param_pattern_ids(&self, ids: Vec<PatternId>) {
        assert!(self.param_pattern_ids.set(ids).is_ok());
    }

    pub fn set_has_self(&self, value: bool) {
        self.has_self.set(Some(value));
    }

    pub fn has_self(&self) -> bool {
        self.has_self.get().expect("has_self uninitialized")
    }

    pub fn set_ty<T: ExprMapId>(&self, id: T, ty: SourceType) {
        let id = id.to_universal_id();
        self.map_tys.borrow_mut().insert_or_replace(id, ty);
    }

    pub fn set_const_value<T: ExprMapId>(&self, id: T, value: ConstValue) {
        let id = id.to_universal_id();
        self.map_consts.borrow_mut().insert(id, value);
    }

    pub fn get_const_value<T: ExprMapId>(&self, id: T) -> Option<ConstValue> {
        let id = id.to_universal_id();
        self.map_consts.borrow().get(id).cloned()
    }

    pub fn ty<T: ExprMapId>(&self, id: T) -> SourceType {
        let id = id.to_universal_id();
        self.map_tys
            .borrow()
            .get(id)
            .expect("no type found")
            .clone()
    }

    pub fn ty_opt<T: ExprMapId>(&self, id: T) -> Option<SourceType> {
        let id = id.to_universal_id();
        self.map_tys.borrow().get(id).cloned()
    }

    pub fn set_vars(&self, vars: VarAccess) {
        *self.vars.borrow_mut() = vars;
    }

    pub fn vars(&self) -> std::cell::Ref<'_, VarAccess> {
        self.vars.borrow()
    }

    pub fn function_context_data(&self) -> LazyContextData {
        self.function_context_data
            .get()
            .cloned()
            .expect("missing context")
    }

    pub fn set_function_context_data(&self, data: LazyContextData) {
        assert!(self.function_context_data.set(data).is_ok());
    }

    pub fn needs_context_slot_in_lambda_object(&self) -> bool {
        self.needs_context_slot_in_lambda_object
            .get()
            .cloned()
            .expect("missing value")
    }

    pub fn set_needs_context_slot_in_lambda_object(&self, value: bool) {
        assert!(self.needs_context_slot_in_lambda_object.set(value).is_ok());
    }

    pub fn outer_contexts(&self) -> std::cell::Ref<'_, Vec<LazyContextData>> {
        self.outer_contexts.borrow()
    }

    pub fn set_outer_contexts(&self, contexts: Vec<LazyContextData>) {
        *self.outer_contexts.borrow_mut() = contexts;
    }
}
