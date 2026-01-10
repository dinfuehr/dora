use std::cell::{Cell, OnceCell, RefCell};
use std::sync::Arc;

use id_arena::Arena;

use dora_parser::GreenId;
use dora_parser::ast::{SyntaxNodeId, SyntaxNodePtr};

use crate::sema::{
    ArrayAssignment, CallType, ConstValue, FctDefinitionId, ForTypeInfo, IdentType,
    LazyContextData, LazyLambdaId, NodeMap, VarAccess, VarId,
};
use crate::{SourceType, SourceTypeArray};

use crate::sema::exprs::{Expr, ExprId};

pub struct ExprArena {
    exprs: Arena<Expr>,
    syntax_node_ptrs: Vec<Option<SyntaxNodePtr>>,
    syntax_node_ids: Vec<Option<SyntaxNodeId>>,
    green_ids: Vec<Option<GreenId>>,
    map_expr_ids: NodeMap<ExprId>,
}

impl ExprArena {
    fn empty() -> ExprArena {
        ExprArena {
            exprs: Arena::new(),
            syntax_node_ptrs: Vec::new(),
            syntax_node_ids: Vec::new(),
            green_ids: Vec::new(),
            map_expr_ids: NodeMap::new(),
        }
    }

    fn to_green_id(&self, id: ExprId) -> GreenId {
        self.green_ids[id.index()].expect("missing green id for expr")
    }

    pub fn to_expr_id(&self, id: GreenId) -> ExprId {
        *self
            .map_expr_ids
            .get(id)
            .expect("missing expr id for green id")
    }

    pub fn expr(&self, id: ExprId) -> &Expr {
        &self.exprs[id]
    }

    pub fn syntax_node_id(&self, id: ExprId) -> SyntaxNodeId {
        self.syntax_node_ids[id.index()].expect("missing syntax node")
    }

    pub fn syntax_node_ptr(&self, id: ExprId) -> SyntaxNodePtr {
        self.syntax_node_ptrs[id.index()].expect("missing syntax node")
    }
}

pub struct ExprArenaBuilder {
    exprs: Arena<Expr>,
    syntax_node_ptrs: Vec<Option<SyntaxNodePtr>>,
    syntax_node_ids: Vec<Option<SyntaxNodeId>>,
    green_ids: Vec<Option<GreenId>>,
    map_expr_ids: NodeMap<ExprId>,
}

impl ExprArenaBuilder {
    pub fn new() -> ExprArenaBuilder {
        ExprArenaBuilder {
            exprs: Arena::new(),
            syntax_node_ptrs: Vec::new(),
            syntax_node_ids: Vec::new(),
            green_ids: Vec::new(),
            map_expr_ids: NodeMap::new(),
        }
    }

    pub fn alloc_expr(
        &mut self,
        expr: Expr,
        syntax_node_id: Option<SyntaxNodeId>,
        syntax_node_ptr: Option<SyntaxNodePtr>,
        green_id: Option<GreenId>,
    ) -> ExprId {
        let id = self.exprs.alloc(expr);
        self.syntax_node_ids.push(syntax_node_id);
        self.syntax_node_ptrs.push(syntax_node_ptr);
        self.green_ids.push(green_id);
        if let Some(green_id) = green_id {
            self.map_expr_ids.insert(green_id, id);
        }
        debug_assert_eq!(id.index(), self.syntax_node_ptrs.len() - 1);
        debug_assert_eq!(id.index(), self.green_ids.len() - 1);
        id
    }

    pub fn freeze(self) -> Arc<ExprArena> {
        Arc::new(ExprArena {
            exprs: self.exprs,
            syntax_node_ptrs: self.syntax_node_ptrs,
            syntax_node_ids: self.syntax_node_ids,
            green_ids: self.green_ids,
            map_expr_ids: self.map_expr_ids,
        })
    }
}

pub struct Body {
    arena: Arc<ExprArena>,
    root_expr_id: Option<ExprId>,
    has_self: Cell<Option<bool>>,
    map_templates: RefCell<NodeMap<(FctDefinitionId, SourceTypeArray)>>,
    map_calls: RefCell<NodeMap<Arc<CallType>>>,
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
            .field("root_expr_id", &self.root_expr_id)
            .finish()
    }
}

impl Body {
    fn to_green_id(&self, id: ExprId) -> GreenId {
        self.arena.to_green_id(id)
    }

    pub fn to_expr_id(&self, id: GreenId) -> ExprId {
        self.arena.to_expr_id(id)
    }

    pub fn new() -> Body {
        Body::new_with_arena(Arc::new(ExprArena::empty()))
    }

    pub fn new_with_arena(arena: Arc<ExprArena>) -> Body {
        Body {
            arena,
            root_expr_id: None,
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

    pub(crate) fn arena(&self) -> Arc<ExprArena> {
        self.arena.clone()
    }

    pub fn expr(&self, id: ExprId) -> &Expr {
        self.arena.expr(id)
    }

    pub fn syntax_node_id(&self, id: ExprId) -> SyntaxNodeId {
        self.arena.syntax_node_id(id)
    }

    pub fn syntax_node_ptr(&self, id: ExprId) -> SyntaxNodePtr {
        self.arena.syntax_node_ptr(id)
    }

    pub fn get_ident(&self, id: GreenId) -> Option<IdentType> {
        self.map_idents.borrow().get(id).cloned()
    }

    pub fn insert_ident(&self, id: GreenId, ident: IdentType) {
        self.map_idents.borrow_mut().insert(id, ident);
    }

    pub fn insert_ident_expr(&self, id: ExprId, ident: IdentType) {
        let green_id = self.to_green_id(id);
        self.insert_ident(green_id, ident);
    }

    pub fn insert_or_replace_ident(&self, id: GreenId, ident: IdentType) {
        self.map_idents.borrow_mut().insert_or_replace(id, ident);
    }

    pub fn insert_or_replace_ident_expr(&self, id: ExprId, ident: IdentType) {
        let green_id = self.to_green_id(id);
        self.insert_or_replace_ident(green_id, ident);
    }

    pub fn get_template(&self, id: GreenId) -> Option<(FctDefinitionId, SourceTypeArray)> {
        self.map_templates.borrow().get(id).cloned()
    }

    pub fn insert_template(&self, id: GreenId, data: (FctDefinitionId, SourceTypeArray)) {
        self.map_templates.borrow_mut().insert(id, data);
    }

    pub fn insert_template_expr(&self, id: ExprId, data: (FctDefinitionId, SourceTypeArray)) {
        let green_id = self.to_green_id(id);
        self.insert_template(green_id, data);
    }

    pub fn get_call_type(&self, id: GreenId) -> Option<Arc<CallType>> {
        self.map_calls.borrow().get(id).cloned()
    }

    pub fn insert_call_type(&self, id: GreenId, call_type: Arc<CallType>) {
        self.map_calls.borrow_mut().insert(id, call_type);
    }

    pub fn insert_call_type_expr(&self, id: ExprId, call_type: Arc<CallType>) {
        let green_id = self.to_green_id(id);
        self.insert_call_type(green_id, call_type);
    }

    pub fn insert_or_replace_call_type(&self, id: GreenId, call_type: Arc<CallType>) {
        self.map_calls.borrow_mut().insert_or_replace(id, call_type);
    }

    pub fn insert_or_replace_call_type_expr(&self, id: ExprId, call_type: Arc<CallType>) {
        let green_id = self.to_green_id(id);
        self.insert_or_replace_call_type(green_id, call_type);
    }

    pub fn get_var_id(&self, id: GreenId) -> Option<VarId> {
        self.map_vars.borrow().get(id).cloned()
    }

    pub fn insert_var_id(&self, id: GreenId, var_id: VarId) {
        self.map_vars.borrow_mut().insert(id, var_id);
    }

    pub fn insert_var_id_expr(&self, id: ExprId, var_id: VarId) {
        let green_id = self.to_green_id(id);
        self.insert_var_id(green_id, var_id);
    }

    pub fn get_const_value_opt(&self, id: GreenId) -> Option<ConstValue> {
        self.map_consts.borrow().get(id).cloned()
    }

    pub fn get_for_type_info(&self, id: GreenId) -> Option<ForTypeInfo> {
        self.map_fors.borrow().get(id).cloned()
    }

    pub fn insert_for_type_info(&self, id: GreenId, info: ForTypeInfo) {
        self.map_fors.borrow_mut().insert(id, info);
    }

    pub fn insert_for_type_info_expr(&self, id: ExprId, info: ForTypeInfo) {
        let green_id = self.to_green_id(id);
        self.insert_for_type_info(green_id, info);
    }

    pub fn get_lambda(&self, id: GreenId) -> Option<LazyLambdaId> {
        self.map_lambdas.borrow().get(id).cloned()
    }

    pub fn insert_lambda(&self, id: GreenId, lambda: LazyLambdaId) {
        self.map_lambdas.borrow_mut().insert(id, lambda);
    }

    pub fn insert_lambda_expr(&self, id: ExprId, lambda: LazyLambdaId) {
        let green_id = self.to_green_id(id);
        self.insert_lambda(green_id, lambda);
    }

    pub fn get_block_context(&self, id: GreenId) -> Option<LazyContextData> {
        self.map_block_contexts.borrow().get(id).cloned()
    }

    pub fn insert_block_context(&self, id: GreenId, context: LazyContextData) {
        self.map_block_contexts.borrow_mut().insert(id, context);
    }

    pub fn insert_block_context_expr(&self, id: ExprId, context: LazyContextData) {
        let green_id = self.to_green_id(id);
        self.insert_block_context(green_id, context);
    }

    pub fn get_argument(&self, id: GreenId) -> Option<usize> {
        self.map_argument.borrow().get(id).cloned()
    }

    pub fn insert_argument(&self, id: GreenId, argument: usize) {
        self.map_argument.borrow_mut().insert(id, argument);
    }

    pub fn insert_argument_expr(&self, id: ExprId, argument: usize) {
        let green_id = self.to_green_id(id);
        self.insert_argument(green_id, argument);
    }

    pub fn get_field_id(&self, id: GreenId) -> Option<usize> {
        self.map_field_ids.borrow().get(id).cloned()
    }

    pub fn insert_field_id(&self, id: GreenId, field_id: usize) {
        self.map_field_ids.borrow_mut().insert(id, field_id);
    }

    pub fn insert_field_id_expr(&self, id: ExprId, field_id: usize) {
        let green_id = self.to_green_id(id);
        self.insert_field_id(green_id, field_id);
    }

    pub fn get_array_assignment(&self, id: GreenId) -> Option<ArrayAssignment> {
        self.map_array_assignments.borrow().get(id).cloned()
    }

    pub fn insert_array_assignment(&self, id: GreenId, assignment: ArrayAssignment) {
        self.map_array_assignments
            .borrow_mut()
            .insert(id, assignment);
    }

    pub fn insert_array_assignment_expr(&self, id: ExprId, assignment: ArrayAssignment) {
        let green_id = self.to_green_id(id);
        self.insert_array_assignment(green_id, assignment);
    }

    pub fn root_expr_id(&self) -> ExprId {
        self.root_expr_id.expect("missing body expr id")
    }

    pub fn set_root_expr_id(&mut self, id: ExprId) {
        assert!(self.root_expr_id.replace(id).is_none());
    }

    pub fn set_has_self(&self, value: bool) {
        self.has_self.set(Some(value));
    }

    pub fn has_self(&self) -> bool {
        self.has_self.get().expect("has_self uninitialized")
    }

    pub fn set_ty(&self, id: GreenId, ty: SourceType) {
        self.map_tys.borrow_mut().insert_or_replace(id, ty);
    }

    pub fn set_const_value(&self, id: GreenId, value: ConstValue) {
        self.map_consts.borrow_mut().insert(id, value);
    }

    pub fn const_value(&self, id: GreenId) -> ConstValue {
        self.map_consts
            .borrow()
            .get(id)
            .expect("no literal found")
            .clone()
    }

    pub fn ty(&self, id: GreenId) -> SourceType {
        self.map_tys
            .borrow()
            .get(id)
            .expect("no type found")
            .clone()
    }

    pub fn ty_opt(&self, id: GreenId) -> Option<SourceType> {
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
