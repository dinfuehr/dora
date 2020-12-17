use crate::error::msg::SemError;
use crate::semck::fctbodyck::body::args_compatible;
use crate::semck::specialize::replace_type_param;
use crate::semck::typeparamck::{self, ErrorReporting};
use crate::ty::{SourceType, SourceTypeArray};
use crate::vm::{
    find_methods_in_class, find_methods_in_enum, find_methods_in_struct, ClassId, Fct, FctId,
    FileId, TraitId, TypeParam, TypeParamDefinition, VM,
};

use crate::vm::find_methods_in_module;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

#[derive(Clone)]
enum LookupKind {
    Method(SourceType),
    Static(SourceType),
    Trait(TraitId),
    Callee(FctId),
}

pub struct MethodLookup<'a> {
    vm: &'a VM,
    caller: &'a Fct,
    file: FileId,
    kind: Option<LookupKind>,
    name: Option<Name>,
    args: Option<&'a [SourceType]>,
    fct_tps: Option<&'a SourceTypeArray>,
    type_param_defs: Option<&'a [TypeParam]>,
    type_param_defs2: Option<&'a TypeParamDefinition>,
    ret: Option<SourceType>,
    pos: Option<Position>,
    report_errors: bool,

    found_fct_id: Option<FctId>,
    found_class_type: Option<SourceType>,
    found_ret: Option<SourceType>,
    found_container_type_params: Option<SourceTypeArray>,

    found_multiple_functions: bool,
}

impl<'a> MethodLookup<'a> {
    pub fn new(vm: &'a VM, caller: &'a Fct) -> MethodLookup<'a> {
        MethodLookup {
            vm,
            caller,
            file: caller.file_id,
            kind: None,
            name: None,
            args: None,
            fct_tps: None,
            ret: None,
            pos: None,
            report_errors: true,
            type_param_defs: None,
            type_param_defs2: None,

            found_fct_id: None,
            found_class_type: None,
            found_ret: None,
            found_container_type_params: None,

            found_multiple_functions: false,
        }
    }

    pub fn callee(mut self, fct_id: FctId) -> MethodLookup<'a> {
        self.kind = Some(LookupKind::Callee(fct_id));
        self
    }

    pub fn method(mut self, obj: SourceType) -> MethodLookup<'a> {
        self.kind = if let SourceType::Trait(trait_id, _) = obj {
            Some(LookupKind::Trait(trait_id))
        } else {
            Some(LookupKind::Method(obj))
        };

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

    pub fn pos(mut self, pos: Position) -> MethodLookup<'a> {
        self.pos = Some(pos);
        self
    }

    pub fn fct_type_params(mut self, fct_tps: &'a SourceTypeArray) -> MethodLookup<'a> {
        self.fct_tps = Some(fct_tps);
        self
    }

    pub fn type_param_defs(mut self, tp_defs: &'a [TypeParam]) -> MethodLookup<'a> {
        self.type_param_defs = Some(tp_defs);
        self
    }

    pub fn type_param_defs2(mut self, tp_defs: &'a TypeParamDefinition) -> MethodLookup<'a> {
        self.type_param_defs2 = Some(tp_defs);
        self
    }

    pub fn name(mut self, name: Name) -> MethodLookup<'a> {
        self.name = Some(name);
        self
    }

    pub fn return_type(mut self, ret: SourceType) -> MethodLookup<'a> {
        self.ret = Some(ret);
        self
    }

    pub fn find(&mut self) -> bool {
        let kind = self.kind.clone().expect("kind not set");
        let args = self.args.expect("args not set");

        let fct_id = match kind {
            LookupKind::Callee(fct_id) => Some(fct_id),

            LookupKind::Method(ref obj) => {
                let name = self.name.expect("name not set");
                self.find_method(obj.clone(), name, false)
            }

            LookupKind::Trait(trait_id) => {
                let name = self.name.expect("name not set");
                self.find_method_in_trait(trait_id, name, false)
            }

            LookupKind::Static(ref obj) => {
                let name = self.name.expect("name not set");
                self.find_method(obj.clone(), name, true)
            }
        };

        self.found_fct_id = fct_id;

        let fct_id = if let Some(fct_id) = fct_id {
            fct_id
        } else if self.report_errors {
            let name = self.name.expect("name not set");

            let name = self.vm.interner.str(name).to_string();
            let param_names = args
                .iter()
                .map(|a| a.name_fct(self.vm, self.caller))
                .collect::<Vec<String>>();

            let msg = match kind {
                LookupKind::Callee(_) => unreachable!(),
                LookupKind::Method(ref obj) => {
                    let type_name = obj.name_fct(self.vm, self.caller);

                    if self.found_multiple_functions {
                        SemError::MultipleCandidatesForMethod(type_name, name, param_names)
                    } else {
                        SemError::UnknownMethod(type_name, name, param_names)
                    }
                }

                LookupKind::Trait(trait_id) => {
                    let xtrait = &self.vm.traits[trait_id];
                    let xtrait = xtrait.read();
                    let type_name = self.vm.interner.str(xtrait.name).to_string();
                    SemError::UnknownMethod(type_name, name, param_names)
                }

                LookupKind::Static(ref obj) => {
                    let type_name = obj.name_fct(self.vm, self.caller);
                    SemError::UnknownStaticMethod(type_name, name, param_names)
                }
            };

            self.vm
                .diag
                .lock()
                .report(self.file, self.pos.expect("pos not set"), msg);
            return false;
        } else {
            return false;
        };

        let fct = self.vm.fcts.idx(fct_id);
        let fct = fct.read();

        let container_tps = match kind {
            LookupKind::Method(_) | LookupKind::Static(_) => {
                self.found_container_type_params.clone().unwrap()
            }
            _ => SourceTypeArray::empty(),
        };

        let fct_tps: SourceTypeArray = if let Some(fct_tps) = self.fct_tps {
            fct_tps.clone()
        } else {
            SourceTypeArray::empty()
        };

        let type_params = container_tps.connect(&fct_tps);

        if !self.check_tps(&fct.type_params, &type_params) {
            return false;
        }

        if args.contains(&SourceType::Error) {
            return false;
        }

        if !args_compatible(self.vm, &*fct, args, &type_params, None) {
            if !self.report_errors {
                return false;
            }

            let fct_name = self.vm.interner.str(fct.name).to_string();
            let fct_params = fct
                .params_without_self()
                .iter()
                .map(|a| a.name_fct(self.vm, &*fct))
                .collect::<Vec<_>>();
            let call_types = args
                .iter()
                .map(|a| a.name_fct(self.vm, &*fct))
                .collect::<Vec<_>>();
            let msg = SemError::ParamTypesIncompatible(fct_name, fct_params, call_types);
            self.vm
                .diag
                .lock()
                .report(self.file, self.pos.expect("pos not set"), msg);
            return false;
        }

        let cmp_type = {
            let type_list = container_tps.connect(&fct_tps);
            replace_type_param(self.vm, fct.return_type.clone(), &type_list, None)
        };

        if self.ret.is_none() || self.ret.clone().unwrap() == cmp_type {
            self.found_ret = Some(cmp_type);
            true
        } else {
            false
        }
    }

    fn find_ctor(&self, cls_id: ClassId) -> Option<FctId> {
        let cls = self.vm.classes.idx(cls_id);
        let cls = cls.read();

        cls.constructor
    }

    fn find_method(
        &mut self,
        object_type: SourceType,
        name: Name,
        is_static: bool,
    ) -> Option<FctId> {
        let candidates = if object_type.is_module() {
            find_methods_in_module(self.vm, object_type, name)
        } else if object_type.is_enum() {
            find_methods_in_enum(
                self.vm,
                object_type,
                self.type_param_defs.unwrap(),
                self.type_param_defs2,
                name,
                is_static,
            )
        } else if object_type.is_struct() || object_type.is_primitive() {
            find_methods_in_struct(
                self.vm,
                object_type,
                self.type_param_defs.unwrap(),
                self.type_param_defs2,
                name,
                is_static,
            )
        } else if object_type.is_cls() {
            find_methods_in_class(
                self.vm,
                object_type,
                self.type_param_defs.unwrap(),
                self.type_param_defs2,
                name,
                is_static,
            )
        } else {
            Vec::new()
        };

        self.found_multiple_functions = candidates.len() > 1;

        if candidates.len() == 1 {
            let candidate = candidates.first().unwrap();
            self.found_class_type = Some(candidate.object_type.clone());
            self.found_container_type_params = Some(candidate.container_type_params.clone());
            Some(candidate.fct_id)
        } else {
            None
        }
    }

    fn find_method_in_trait(
        &mut self,
        trait_id: TraitId,
        name: Name,
        is_static: bool,
    ) -> Option<FctId> {
        let xtrait = &self.vm.traits[trait_id];
        let xtrait = xtrait.read();

        xtrait.find_method(self.vm, name, is_static)
    }

    fn check_tps(&self, specified_tps: &[TypeParam], tps: &SourceTypeArray) -> bool {
        let error = if self.report_errors {
            ErrorReporting::Yes(self.file, self.pos.expect("no pos"))
        } else {
            ErrorReporting::No
        };

        typeparamck::check_params(self.vm, self.caller, error, specified_tps, tps)
    }

    pub fn found_fct_id(&self) -> Option<FctId> {
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
