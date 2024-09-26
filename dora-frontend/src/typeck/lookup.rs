use crate::error::msg::ErrorMessage;
use crate::interner::Name;
use crate::sema::{
    extension_matches, impl_matches, Candidate, FctDefinitionId, FctParent, Sema, SourceFileId,
    TraitDefinitionId, TypeParamDefinition,
};
use crate::typeck::function::args_compatible_fct;
use crate::typeparamck::{self, ErrorReporting};
use crate::{replace_type, AliasReplacement, SourceType, SourceTypeArray};
use dora_parser::Span;

pub struct MethodLookupResult {
    found_fct_id: Option<FctDefinitionId>,
    found_class_type: Option<SourceType>,
    found_ret: Option<SourceType>,
    found_container_type_params: Option<SourceTypeArray>,
    found_multiple_functions: bool,
    found: bool,
}

impl MethodLookupResult {
    fn new() -> MethodLookupResult {
        MethodLookupResult {
            found_fct_id: None,
            found_class_type: None,
            found_ret: None,
            found_container_type_params: None,
            found_multiple_functions: false,
            found: false,
        }
    }

    pub fn find(&self) -> bool {
        self.found
    }

    pub fn found_fct_id(&self) -> Option<FctDefinitionId> {
        self.found_fct_id
    }

    pub fn found_class_type(&self) -> Option<SourceType> {
        self.found_class_type.clone()
    }

    pub fn found_container_type_params(&self) -> Option<SourceTypeArray> {
        self.found_container_type_params.clone()
    }

    pub fn found_ret(&self) -> Option<SourceType> {
        self.found_ret.clone()
    }
}

#[derive(Clone)]
enum LookupKind {
    Method(SourceType),
    Static(SourceType),
    Self_(TraitDefinitionId),
    Callee(FctDefinitionId),
}

pub struct MethodLookup<'a> {
    sa: &'a Sema,
    file: SourceFileId,
    kind: Option<LookupKind>,
    name: Option<Name>,
    args: Option<&'a [SourceType]>,
    fct_tps: Option<&'a SourceTypeArray>,
    type_param_defs: &'a TypeParamDefinition,
    ret: Option<SourceType>,
    fct_parent: Option<FctParent>,
    span: Option<Span>,
    report_errors: bool,
}

impl<'a> MethodLookup<'a> {
    pub fn new(
        sa: &'a Sema,
        file_id: SourceFileId,
        caller_type_param_defs: &'a TypeParamDefinition,
    ) -> MethodLookup<'a> {
        MethodLookup {
            sa,
            file: file_id,
            kind: None,
            name: None,
            args: None,
            fct_tps: None,
            ret: None,
            fct_parent: None,
            span: None,
            report_errors: true,
            type_param_defs: caller_type_param_defs,
        }
    }

    pub fn callee(mut self, fct_id: FctDefinitionId) -> MethodLookup<'a> {
        self.kind = Some(LookupKind::Callee(fct_id));
        self
    }

    pub fn parent(mut self, parent: FctParent) -> MethodLookup<'a> {
        self.fct_parent = Some(parent);
        self
    }

    pub fn method(mut self, obj: SourceType) -> MethodLookup<'a> {
        let kind = match obj {
            SourceType::This => {
                let parent = self.fct_parent.clone().expect("parent missing");
                match parent {
                    FctParent::Trait(id) => LookupKind::Self_(id),
                    _ => unreachable!(),
                }
            }
            _ => LookupKind::Method(obj),
        };

        self.kind = Some(kind);

        self
    }

    pub fn no_error_reporting(mut self) -> MethodLookup<'a> {
        self.report_errors = false;
        self
    }

    pub fn static_method(mut self, ty: SourceType) -> MethodLookup<'a> {
        self.kind = Some(LookupKind::Static(ty));
        self
    }

    pub fn args(mut self, args: &'a [SourceType]) -> MethodLookup<'a> {
        self.args = Some(args);
        self
    }

    pub fn span(mut self, span: Span) -> MethodLookup<'a> {
        self.span = Some(span);
        self
    }

    pub fn fct_type_params(mut self, fct_tps: &'a SourceTypeArray) -> MethodLookup<'a> {
        self.fct_tps = Some(fct_tps);
        self
    }

    pub fn name(mut self, name: Name) -> MethodLookup<'a> {
        self.name = Some(name);
        self
    }

    pub fn find(&mut self) -> MethodLookupResult {
        let kind = self.kind.clone().expect("kind not set");
        let args = self.args.expect("args not set");
        let mut result = MethodLookupResult::new();

        let fct_id = match kind {
            LookupKind::Callee(fct_id) => Some(fct_id),

            LookupKind::Method(ref obj) => {
                let name = self.name.expect("name not set");
                self.find_method(&mut result, obj.clone(), name, false)
            }

            LookupKind::Self_(trait_id) => {
                let name = self.name.expect("name not set");
                self.find_method_in_trait(trait_id, name, false)
            }

            LookupKind::Static(ref obj) => {
                let name = self.name.expect("name not set");
                self.find_method(&mut result, obj.clone(), name, true)
            }
        };

        result.found_fct_id = fct_id;

        let fct_id = if let Some(fct_id) = fct_id {
            fct_id
        } else if self.report_errors {
            let name = self.name.expect("name not set");

            let name = self.sa.interner.str(name).to_string();
            let param_names = args
                .iter()
                .map(|a| self.ty_name(a))
                .collect::<Vec<String>>();

            let msg = match kind {
                LookupKind::Callee(_) => unreachable!(),
                LookupKind::Method(ref obj) => {
                    let type_name = self.ty_name(obj);

                    if result.found_multiple_functions {
                        ErrorMessage::MultipleCandidatesForMethod(type_name, name, param_names)
                    } else {
                        ErrorMessage::UnknownMethod(type_name, name, param_names)
                    }
                }

                LookupKind::Self_(..) => {
                    ErrorMessage::UnknownMethod("Self".into(), name, param_names)
                }

                LookupKind::Static(ref obj) => {
                    let type_name = self.ty_name(obj);
                    ErrorMessage::UnknownStaticMethod(type_name, name, param_names)
                }
            };

            self.report_error(msg);
            return result;
        } else {
            return result;
        };

        let fct = self.sa.fct(fct_id);

        let container_tps = match kind {
            LookupKind::Method(_) | LookupKind::Static(_) => {
                result.found_container_type_params.clone().unwrap()
            }
            _ => SourceTypeArray::empty(),
        };

        let fct_tps: SourceTypeArray = if let Some(fct_tps) = self.fct_tps {
            fct_tps.clone()
        } else {
            SourceTypeArray::empty()
        };

        let type_params = container_tps.connect(&fct_tps);

        if !self.check_tps(fct.type_params(), &type_params) {
            return result;
        }

        if args.contains(&SourceType::Error) {
            return result;
        }

        if !args_compatible_fct(self.sa, &*fct, args, &type_params, None) {
            if !self.report_errors {
                return result;
            }

            let fct_name = self.sa.interner.str(fct.name).to_string();
            let fct_params = fct
                .params_without_self()
                .iter()
                .map(|a| a.ty().name_fct(self.sa, &*fct))
                .collect::<Vec<_>>();
            let call_types = args.iter().map(|a| self.ty_name(a)).collect::<Vec<_>>();
            let msg = ErrorMessage::ParamTypesIncompatible(fct_name, fct_params, call_types);
            self.report_error(msg);
            return result;
        }

        let cmp_type = {
            let type_list = container_tps.connect(&fct_tps);
            replace_type(
                self.sa,
                fct.return_type(),
                Some(&type_list),
                None,
                AliasReplacement::None,
            )
        };

        if self.ret.is_none() || self.ret.clone().unwrap() == cmp_type {
            result.found_ret = Some(cmp_type);
            result.found = true;
        }

        result
    }

    fn report_error(&mut self, msg: ErrorMessage) {
        self.sa
            .report(self.file, self.span.expect("missing location"), msg);
    }

    fn find_method(
        &mut self,
        result: &mut MethodLookupResult,
        object_type: SourceType,
        name: Name,
        is_static: bool,
    ) -> Option<FctDefinitionId> {
        let candidates = find_method_call_candidates(
            self.sa,
            object_type,
            self.type_param_defs,
            name,
            is_static,
        );

        result.found_multiple_functions = candidates.len() > 1;

        if candidates.len() == 1 {
            let candidate = candidates.first().unwrap();
            result.found_class_type = Some(candidate.object_type.clone());
            result.found_container_type_params = Some(candidate.container_type_params.clone());
            Some(candidate.fct_id)
        } else {
            None
        }
    }

    fn find_method_in_trait(
        &mut self,
        trait_id: TraitDefinitionId,
        name: Name,
        is_static: bool,
    ) -> Option<FctDefinitionId> {
        let trait_ = self.sa.trait_(trait_id);
        trait_.get_method(name, is_static)
    }

    fn check_tps(&self, specified_tps: &TypeParamDefinition, tps: &SourceTypeArray) -> bool {
        let error = if self.report_errors {
            ErrorReporting::Yes(self.file, self.span.expect("no pos"))
        } else {
            ErrorReporting::No
        };

        typeparamck::check_params(self.sa, self.type_param_defs, specified_tps, tps, error)
    }

    fn ty_name(&self, ty: &SourceType) -> String {
        ty.name_with_type_params(self.sa, self.type_param_defs)
    }
}

pub fn find_method_call_candidates(
    sa: &Sema,
    object_type: SourceType,
    type_param_defs: &TypeParamDefinition,
    name: Name,
    is_static: bool,
) -> Vec<Candidate> {
    let mut candidates = Vec::with_capacity(1);

    if let Some(trait_id) = object_type.trait_id() {
        let trait_ = sa.trait_(trait_id);
        if let Some(fct_id) = trait_.get_method(name, false) {
            candidates.push(Candidate {
                object_type: object_type.clone(),
                container_type_params: object_type.type_params(),
                fct_id,
            });
            return candidates;
        }
    }

    for (_id, extension) in sa.extensions.iter() {
        if let Some(bindings) =
            extension_matches(sa, object_type.clone(), type_param_defs, extension.id())
        {
            let table = if is_static {
                &extension.static_names
            } else {
                &extension.instance_names
            };

            if let Some(&fct_id) = table.borrow().get(&name) {
                candidates.push(Candidate {
                    object_type: object_type.clone(),
                    container_type_params: bindings,
                    fct_id,
                });
                return candidates;
            }
        }
    }

    for (_id, impl_) in sa.impls.iter() {
        if let Some(bindings) = impl_matches(sa, object_type.clone(), type_param_defs, impl_.id()) {
            let trait_ = &sa.trait_(impl_.trait_id());

            if let Some(trait_method_id) = trait_.get_method(name, is_static) {
                candidates.push(Candidate {
                    object_type: object_type.clone(),
                    container_type_params: bindings.clone(),
                    fct_id: impl_
                        .get_method_for_trait_method_id(trait_method_id)
                        .expect("missing fct"),
                });
            }
        }
    }

    candidates
}
