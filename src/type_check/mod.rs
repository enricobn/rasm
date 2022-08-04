pub mod resolved_ast;

use crate::codegen::{EnhancedASTModule, VarContext, VarKind};
use crate::parser::ast::{
    ASTEnumDef, ASTEnumVariantDef, ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef,
    ASTLambdaDef, ASTParameterDef, ASTStructDef, ASTType, ASTTypeRef, BuiltinTypeKind,
};
use crate::type_check::resolved_ast::{
    ASTTypedEnumDef, ASTTypedEnumVariantDef, ASTTypedExpression, ASTTypedFunctionBody,
    ASTTypedFunctionCall, ASTTypedFunctionDef, ASTTypedLambdaDef, ASTTypedModule,
    ASTTypedParameterDef, ASTTypedStructDef, ASTTypedType, ASTTypedTypeRef, BuiltinTypedTypeKind,
};
use linked_hash_map::LinkedHashMap;
use log::{debug, info};
use std::collections::{HashMap, HashSet};

pub struct ToTypedModuleConverter {
    module: EnhancedASTModule,
    /// key=original function name
    /// value=Vector of possible matches between parametric types and effective types and related function definition
    functions: HashMap<String, Vec<ResolvedFunctionDef>>,
    enums: HashMap<String, Vec<ResolvedEnumDef>>,
    count: usize,
}

struct TransformedCall {
    new_function_call: ASTFunctionCall,
    new_function_defs: Option<(usize, Vec<ResolvedFunctionDef>)>,
    new_enums: Option<(usize, Vec<ResolvedEnumDef>)>,
}

#[derive(Debug, Clone)]
struct ResolvedFunctionDef {
    type_parameters: HashMap<String, ASTType>,
    function_def: ASTFunctionDef,
}

#[derive(Debug, Clone)]
struct ResolvedEnumDef {
    original_name: String,
    //type_parameters: HashMap<String, ASTType>,
    enum_def: ASTEnumDef,
    parameter_types: Vec<ASTTypeRef>,
}

impl ToTypedModuleConverter {
    pub fn new(module: EnhancedASTModule) -> Self {
        ToTypedModuleConverter {
            module,
            functions: HashMap::new(),
            count: 0,
            enums: HashMap::new(),
        }
    }

    pub fn convert(&mut self) -> ASTTypedModule {
        let context = VarContext::new(None);

        let module = self.module.clone();

        let mut body = module.body.clone();

        while true {
            debug!("transformed loop");
            let mut temp_body = Vec::new();

            let mut transformed = false;

            for call in body.iter() {
                if let Some(transformed_call) =
                    self.transform_call(self.count, &context, &module, call)
                {
                    if let Some((new_count, resolved_function_defs)) =
                        transformed_call.new_function_defs
                    {
                        self.count = new_count;
                        for resolved_function_def in resolved_function_defs {
                            self.functions
                                .entry(call.function_name.clone())
                                .or_insert_with(Vec::new)
                                .push(resolved_function_def);
                            transformed = true;
                        }
                    }

                    if let Some((_, resolved_enum_defs)) = transformed_call.new_enums {
                        for resolved_enum_def in resolved_enum_defs {
                            debug!("new enum def {:?}", resolved_enum_def.enum_def);
                            self.enums
                                .entry(resolved_enum_def.original_name.clone())
                                .or_insert_with(Vec::new)
                                .push(resolved_enum_def);
                            transformed = true;
                        }
                    }

                    temp_body.push(transformed_call.new_function_call);
                } else {
                    temp_body.push(call.clone());
                }
            }

            body = temp_body;

            if !transformed {
                break;
            }
        }

        let mut functions_by_name = LinkedHashMap::new();

        self.functions
            .values()
            .into_iter()
            .flat_map(|it| it.iter().map(|c| c.function_def.clone()))
            .for_each(|it| {
                let mut context = VarContext::new(Some(&context));

                it.parameters.iter().enumerate().for_each(|(i, par)| {
                    context.insert(par.name.clone(), VarKind::ParameterRef(i, par.clone()));
                });

                match &it.body {
                    ASTFunctionBody::RASMBody(expressions) => {
                        let mut body = Vec::new();
                        for expression in expressions {
                            match expression {
                                ASTExpression::ASTFunctionCallExpression(call) => {
                                    if let Some(transformed_function_call) = self.transform_call(
                                        self.count,
                                        &context,
                                        &self.module,
                                        &call,
                                    ) {
                                        body.push(ASTExpression::ASTFunctionCallExpression(
                                            transformed_function_call.new_function_call,
                                        ));
                                    } else {
                                        body.push(expression.clone());
                                    }
                                }
                                _ => {
                                    body.push(expression.clone());
                                }
                            }
                        }

                        let mut new_function_def = it.clone();
                        new_function_def.body = ASTFunctionBody::RASMBody(body);
                        functions_by_name.insert(new_function_def.name.clone(), new_function_def);
                    }
                    ASTFunctionBody::ASMBody(_) => {
                        functions_by_name.insert(it.name.clone(), it);
                    }
                }
            });

        /*
        self.functions.iter().for_each(|(name, defs)| {
            println!("{name} : {:?}", defs);
        });

         */

        let mut new_module = ASTTypedModule {
            native_body: self.module.native_body.clone(),
            functions_by_name: {
                let mut result = LinkedHashMap::new();

                functions_by_name.iter().for_each(|(name, def)| {
                    result.insert(name.clone(), self.function_def(def));
                });
                self.functions.values().for_each(|it| {
                    it.iter().for_each(|def| {
                        result.insert(
                            def.function_def.name.clone(),
                            self.function_def(&def.function_def),
                        );
                    });
                });

                result
            },
            body: body.iter().map(|it| self.function_call(it)).collect(),
            structs: self
                .module
                .structs
                .iter()
                .map(|it| self.struct_def(it))
                .collect(),
            enums: self
                .enums
                .iter()
                .flat_map(|(_, it)| it.iter().map(|en| self.enum_def(&en.enum_def)))
                .collect(),
            statics: self.module.statics.clone(),
        };

        new_module
    }

    fn function_def(&self, def: &ASTFunctionDef) -> ASTTypedFunctionDef {
        ASTTypedFunctionDef {
            name: def.name.clone(),
            body: self.body(&def.body),
            return_type: def
                .return_type
                .clone()
                .map(|it| self.type_ref(&it, &format!("function {} return type", def.name))),
            inline: def.inline,
            parameters: def
                .parameters
                .iter()
                .map(|it| self.parameter_def(it, &format!("function {}", def.name)))
                .collect(),
        }
    }

    fn parameter_def(
        &self,
        parameter_def: &ASTParameterDef,
        message: &str,
    ) -> ASTTypedParameterDef {
        ASTTypedParameterDef {
            name: parameter_def.name.clone(),
            type_ref: self.type_ref(
                &parameter_def.type_ref,
                &format!("{message}: parameter {}", parameter_def.name),
            ),
        }
    }

    fn type_ref(&self, type_ref: &ASTTypeRef, message: &str) -> ASTTypedTypeRef {
        let ast_type = match &type_ref.ast_type {
            ASTType::Builtin(kind) => match kind {
                BuiltinTypeKind::ASTString => {
                    ASTTypedType::Builtin(BuiltinTypedTypeKind::ASTString)
                }
                BuiltinTypeKind::ASTI32 => ASTTypedType::Builtin(BuiltinTypedTypeKind::ASTI32),
                BuiltinTypeKind::Lambda {
                    return_type,
                    parameters,
                } => ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                    parameters: parameters
                        .iter()
                        .map(|it| self.type_ref(it, &(message.to_owned() + ", lambda parameter")))
                        .collect(),
                    return_type: return_type.clone().map(|it| {
                        Box::new(self.type_ref(&it, &(message.to_owned() + ", lambda return type")))
                    }),
                }),
            },
            ASTType::Parametric(p) => {
                panic!("Unresolved parametric type '{p}': {message}");
            }
            ASTType::Custom { name, param_types } => {
                if let Some(enum_def) = self.module.enums.iter().find(|it| &it.name == name) {
                    ASTTypedType::Enum {
                        name: name.clone(),
                        param_types: param_types
                            .iter()
                            .map(|it| self.type_ref(it, &format!("{message}, enum {name}")))
                            .collect(),
                    }
                } else if let Some(struct_def) =
                    self.module.structs.iter().find(|it| &it.name == name)
                {
                    ASTTypedType::Struct {
                        name: name.clone(),
                        param_types: param_types
                            .iter()
                            .map(|it| self.type_ref(it, &format!("{message}, struct {name}")))
                            .collect(),
                    }
                } else if let Some(resolved_enum_def) = self
                    .enums
                    .iter()
                    .flat_map(|it| it.1.iter())
                    .find(|it| &it.enum_def.name == name)
                {
                    ASTTypedType::Enum {
                        name: name.clone(),
                        param_types: resolved_enum_def
                            .parameter_types
                            .iter()
                            .map(|it| self.type_ref(it, &format!("{message}, enum {name}")))
                            .collect(),
                    }
                } else {
                    panic!("Cannot find Custom type '{name}'");
                }
            }
        };

        ASTTypedTypeRef {
            ast_type,
            ast_ref: type_ref.ast_ref,
        }
    }

    fn expression(&self, expression: &ASTExpression) -> ASTTypedExpression {
        match expression {
            ASTExpression::StringLiteral(s) => ASTTypedExpression::StringLiteral(s.to_string()),
            ASTExpression::ASTFunctionCallExpression(fc) => {
                ASTTypedExpression::ASTFunctionCallExpression(self.function_call(fc))
            }
            ASTExpression::Val(v) => ASTTypedExpression::Val(v.clone()),
            ASTExpression::Number(n) => ASTTypedExpression::Number(*n),
            ASTExpression::Lambda(l) => ASTTypedExpression::Lambda(self.lambda_def(l)),
        }
    }

    fn lambda_def(&self, lambda_def: &ASTLambdaDef) -> ASTTypedLambdaDef {
        ASTTypedLambdaDef {
            body: lambda_def
                .body
                .iter()
                .map(|it| self.expression(it))
                .collect(),
        }
    }

    fn body(&self, body: &ASTFunctionBody) -> ASTTypedFunctionBody {
        match body {
            ASTFunctionBody::RASMBody(body) => {
                ASTTypedFunctionBody::RASMBody(body.iter().map(|it| self.expression(it)).collect())
            }
            ASTFunctionBody::ASMBody(body) => ASTTypedFunctionBody::ASMBody(body.clone()),
        }
    }

    fn struct_def(&self, struct_def: &ASTStructDef) -> ASTTypedStructDef {
        todo!()
    }

    fn enum_def(&self, enum_def: &ASTEnumDef) -> ASTTypedEnumDef {
        ASTTypedEnumDef {
            name: enum_def.name.clone(),
            variants: enum_def
                .variants
                .iter()
                .map(|it| self.enum_variant(it, &format!("enum {}", enum_def.name)))
                .collect(),
        }
    }

    fn enum_variant(&self, variant: &ASTEnumVariantDef, message: &str) -> ASTTypedEnumVariantDef {
        ASTTypedEnumVariantDef {
            name: variant.name.clone(),
            parameters: variant
                .parameters
                .iter()
                .map(|it| self.parameter_def(it, &format!("{message}, variant {}", variant.name)))
                .collect(),
        }
    }

    fn function_call(&self, function_call: &ASTFunctionCall) -> ASTTypedFunctionCall {
        ASTTypedFunctionCall {
            function_name: function_call.function_name.clone(),
            parameters: function_call
                .parameters
                .iter()
                .map(|it| self.expression(it))
                .collect(),
        }
    }

    fn transform_call(
        &self,
        function_count: usize,
        context: &VarContext,
        module: &EnhancedASTModule,
        call: &ASTFunctionCall,
    ) -> Option<TransformedCall> {
        debug!("transform_call {}", call.function_name);
        let mut count = function_count;

        let mut parameters = Vec::new();
        let mut resolved_function_defs = Vec::new();
        let mut resolved_enum_defs = Vec::new();

        for expression in call.parameters.iter() {
            match expression {
                ASTExpression::ASTFunctionCallExpression(call) => {
                    if let Some(transformed_function_call) =
                        self.transform_call(self.count, context, &self.module, call)
                    {
                        parameters.push(ASTExpression::ASTFunctionCallExpression(
                            transformed_function_call.new_function_call,
                        ));
                        if let Some((new_count, mut new_function_defs)) =
                            transformed_function_call.new_function_defs
                        {
                            count = new_count;
                            resolved_function_defs.append(&mut new_function_defs);
                        }

                        if let Some((new_count, mut new_enum_defs)) =
                            transformed_function_call.new_enums
                        {
                            count = new_count;
                            resolved_enum_defs.append(&mut new_enum_defs);
                        }
                    } else {
                        parameters.push(expression.clone());
                    }
                }
                _ => {
                    parameters.push(expression.clone());
                }
            }
        }

        let function_def = if let Some(f) = self
            .functions
            .iter()
            .flat_map(|it| it.1.iter())
            .find(|it| it.function_def.name == call.function_name)
        {
            //println!("found {} in self.functions", call.function_name);
            &f.function_def
        } else if let Some(f) = module
            .functions_by_name
            .iter()
            .find(|(name, it)| name.to_string() == call.function_name)
        {
            //println!("found {} in module.functions_by_name", call.function_name);
            f.1
        } else {
            panic!("Cannot find function {}", call.function_name);
        };

        if function_def.param_types.is_empty() {
            let mut new_call = call.clone();
            new_call.parameters = parameters;
            Some(TransformedCall {
                new_function_call: new_call,
                new_function_defs: Some((count, resolved_function_defs)),
                new_enums: Some((count, resolved_enum_defs)),
            })
        } else {
            let mut param_types_to_effective_types = HashMap::new();
            let mut resolved_enum_defs = Vec::new();

            for (i, param) in function_def.parameters.iter().enumerate() {
                let parametric_types_of_declared_parameter =
                    self.get_parametric_types(&param.type_ref.ast_type);

                if !parametric_types_of_declared_parameter.is_empty() {
                    debug!(
                        "parametric_types {:?}",
                        parametric_types_of_declared_parameter
                    );
                    let parameter_expression = parameters.get(i).unwrap();

                    if let Some(type_of_expression) = self.get_type_of_expr(
                        parameter_expression,
                        context,
                        &resolved_function_defs,
                        &param.type_ref,
                    ) {
                        for (name, effective_type) in self
                            .match_parametric_type_with_effective_type(
                                &param.type_ref.ast_type,
                                &type_of_expression.ast_type,
                            )
                        {
                            param_types_to_effective_types.insert(name, effective_type);
                        }
                    }
                }
            }

            let mut param_types_for_check = function_def.param_types.clone();

            param_types_for_check.retain(|it| param_types_to_effective_types.contains_key(it));

            if param_types_for_check.len() != function_def.param_types.len() {
                panic!(
                    "{} resolved param types do no match {:?}, {:?}",
                    call.function_name, function_def.param_types, param_types_to_effective_types
                )
            }

            let functions = self.functions.get(&function_def.name);

            let mut new_call = call.clone();

            if let Some(resolved_function_def) = functions.and_then(|v| {
                v.iter()
                    .find(|it| it.type_parameters.eq(&param_types_to_effective_types))
            }) {
                new_call.function_name = resolved_function_def.function_def.name.clone();
                new_call.parameters = parameters;

                Some(TransformedCall {
                    new_function_call: new_call,
                    new_function_defs: None,
                    new_enums: None,
                })
            } else {
                let effective_parameters: Vec<ASTParameterDef> = function_def
                    .parameters
                    .iter()
                    .map(|it| {
                        let (effective_type, mut red) = self.substitute(
                            &it.type_ref.ast_type,
                            &param_types_to_effective_types,
                            &resolved_enum_defs,
                        );

                        resolved_enum_defs.append(&mut red);

                        ASTParameterDef {
                            name: it.name.clone(),
                            type_ref: ASTTypeRef {
                                ast_ref: it.type_ref.ast_ref,
                                ast_type: effective_type,
                            },
                        }
                    })
                    .collect();

                let effective_return_type = function_def.return_type.clone().map(|it| {
                    let (ast_type, mut red) = self.substitute(
                        &it.ast_type,
                        &param_types_to_effective_types,
                        &resolved_enum_defs,
                    );

                    resolved_enum_defs.append(&mut red);

                    ASTTypeRef {
                        ast_ref: it.ast_ref,
                        ast_type,
                    }
                });

                let new_name = function_def.name.clone() + "_" + &count.to_string();
                count += 1;

                let mut new_function_def = function_def.clone();
                new_function_def.name = new_name.clone();
                new_function_def.parameters = effective_parameters;
                new_function_def.return_type = effective_return_type;

                let mut param_types = function_def.param_types.clone();
                param_types.retain(|it| !param_types_to_effective_types.contains_key(it));

                new_function_def.param_types = param_types.clone();

                if !&param_types.is_empty() {
                    panic!("new_function_def {:?}", new_function_def);
                } else {
                    debug!("new_function_def {:?}", new_function_def);
                }

                //Option<(usize, (HashMap<String, ASTType>, ASTFunctionDef), ASTFunctionCall)>

                new_call.function_name = new_name;
                new_call.parameters = parameters;

                resolved_function_defs.push(ResolvedFunctionDef {
                    type_parameters: param_types_to_effective_types.clone(),
                    function_def: new_function_def.clone(),
                });

                Some(TransformedCall {
                    new_function_call: new_call,
                    new_function_defs: Some((count, resolved_function_defs)),
                    new_enums: Some((resolved_enum_defs.len(), resolved_enum_defs)),
                })

                //self.functions.insert(function_def.name.clone(), functions);
            }
        }
    }

    fn get_type_of_expr(
        &self,
        expr: &ASTExpression,
        val_context: &VarContext,
        not_added_resolved_function_defs: &Vec<ResolvedFunctionDef>,
        declared_type: &ASTTypeRef,
    ) -> Option<ASTTypeRef> {
        match expr {
            ASTExpression::StringLiteral(_) => {
                return Some(ASTTypeRef {
                    ast_type: ASTType::Builtin(BuiltinTypeKind::ASTString),
                    ast_ref: true,
                });
            }
            ASTExpression::ASTFunctionCallExpression(exp) => {
                if let Some(function_def) = self
                    .module
                    .functions_by_name
                    .values()
                    .find(|it| it.name == exp.function_name)
                {
                    if let Some(tfc) =
                        self.transform_call(self.count, val_context, &self.module, exp)
                    {
                        if let Some((_, mut new_resolved_function_def)) = tfc.new_function_defs {
                            let mut all_resolved_function_defs =
                                not_added_resolved_function_defs.clone();
                            for rfd in self.functions.values() {
                                all_resolved_function_defs.append(&mut rfd.clone());
                            }
                            all_resolved_function_defs.append(&mut new_resolved_function_def);

                            let new_function_def = all_resolved_function_defs
                                .iter()
                                .find(|it| {
                                    it.function_def.name == tfc.new_function_call.function_name
                                })
                                .unwrap();

                            if let Some(declared_return_type) =
                                &new_function_def.function_def.return_type
                            {
                                self.get_type_of_expr(
                                    &ASTExpression::ASTFunctionCallExpression(
                                        tfc.new_function_call,
                                    ),
                                    val_context,
                                    &all_resolved_function_defs,
                                    declared_return_type,
                                );
                            }
                        } else {
                            todo!()
                        }
                    }
                    return function_def.return_type.clone();
                } else if let Some(resolved_function_def) = self
                    .functions
                    .values()
                    .flat_map(|it| it.iter())
                    .find(|it| it.function_def.name == exp.function_name)
                {
                    return resolved_function_def.function_def.return_type.clone();
                } else if let Some(resolved_function_def) = not_added_resolved_function_defs
                    .iter()
                    .find(|it| it.function_def.name == exp.function_name)
                {
                    return resolved_function_def.function_def.return_type.clone();
                } else {
                    panic!("cannot find function {}", exp.function_name);
                }
            }
            ASTExpression::Val(name) => {
                let var_kind = val_context
                    .get(name)
                    .unwrap_or_else(|| panic!("cannot find val {name}"));
                match var_kind {
                    VarKind::ParameterRef(_, def) => {
                        return Some(def.type_ref.clone());
                    }
                }
            }
            ASTExpression::Number(_) => {
                return Some(ASTTypeRef {
                    ast_type: ASTType::Builtin(BuiltinTypeKind::ASTI32),
                    ast_ref: false,
                });
            }
            ASTExpression::Lambda(lambda) => {
                let parametric_types = self.get_parametric_types(&declared_type.ast_type);

                return if parametric_types.is_empty() {
                    Some(declared_type.clone())
                } else if lambda.body.is_empty() {
                    None
                } else if let Some(last_expression) = lambda.body.last() {
                    debug!("last expression of lambda body {:?}", last_expression);
                    let last_expression_type = self.get_type_of_expr(
                        last_expression,
                        val_context,
                        not_added_resolved_function_defs,
                        declared_type,
                    );
                    debug!("  type {:?}", last_expression_type);
                    last_expression_type
                } else {
                    None
                };
                //return Some(ASTTypeRef {ast_type: ASTType::Builtin(BuiltinTypeKind::Lambda {return_type , parameters}), ast_ref: true})
            }
        }
        return None;
    }

    fn get_parametric_types(&self, ast_type: &ASTType) -> Vec<String> {
        return match ast_type {
            ASTType::Builtin(kind) => match kind {
                BuiltinTypeKind::ASTString => {
                    vec![]
                }
                BuiltinTypeKind::ASTI32 => {
                    vec![]
                }
                BuiltinTypeKind::Lambda {
                    parameters,
                    return_type,
                } => {
                    let mut par_types: Vec<String> = parameters
                        .iter()
                        .flat_map(|it| self.get_parametric_types(&it.ast_type))
                        .collect();
                    if let Some(rt) = return_type {
                        par_types.append(&mut self.get_parametric_types(&rt.as_ref().ast_type));
                    }
                    par_types.sort();
                    par_types.dedup();
                    par_types
                }
            },
            ASTType::Parametric(p) => {
                vec![p.into()]
            }
            ASTType::Custom {
                name: _,
                param_types: pt,
            } => {
                let mut result: Vec<String> = pt
                    .iter()
                    .flat_map(|it| match it.clone().ast_type {
                        ASTType::Parametric(name) => {
                            vec![name]
                        }
                        _ => self.get_parametric_types(&it.ast_type),
                    })
                    .collect();
                result.sort();
                result.dedup();
                result
            }
        };
    }

    fn match_parametric_type_with_effective_type(
        &self,
        parametric_type: &ASTType,
        effective_type: &ASTType,
    ) -> HashMap<String, ASTType> {
        debug!("match_parametric_type_with_effective_type:");
        debug!("  {:?}", parametric_type);
        debug!("  {:?}", effective_type);

        let mut result = HashMap::new();

        match effective_type {
            ASTType::Builtin(builtin_type_kind) => match builtin_type_kind {
                BuiltinTypeKind::ASTString => match parametric_type {
                    ASTType::Parametric(name) => {
                        if let Some(old_type) = result.insert(name.into(), effective_type.clone()) {
                            if !old_type.eq(effective_type) {
                                panic!(
                                    "There's already a type for a parameter that does not matches"
                                );
                            }
                        }
                    }
                    _ => {
                        panic!("Expected parametric type");
                    }
                },
                BuiltinTypeKind::ASTI32 => match parametric_type {
                    ASTType::Parametric(name) => {
                        if let Some(old_type) = result.insert(name.into(), effective_type.clone()) {
                            if !old_type.eq(effective_type) {
                                panic!(
                                    "There's already a type for a parameter that does not matches"
                                );
                            }
                        }
                    }
                    _ => {
                        panic!(
                            "Expected type {:?}  got {:?}",
                            parametric_type, effective_type
                        );
                    }
                },
                BuiltinTypeKind::Lambda { .. } => {
                    debug!(
                        "match_parametric_type_with_effective_type lambda {:?}",
                        parametric_type
                    )
                }
            },
            ASTType::Parametric(_) => {}
            ASTType::Custom {
                name: effective_name,
                param_types: effective_param_types,
            } => match parametric_type {
                ASTType::Builtin(_) => {
                    panic!("unexpected builtin type")
                }
                ASTType::Parametric(name) => {
                    if let Some(old_type) = result.insert(name.into(), effective_type.clone()) {
                        if !old_type.eq(effective_type) {
                            panic!("There's already a type for a parameter that does not matches");
                        }
                    }
                }
                ASTType::Custom { name, param_types } => {
                    /*
                    it could be that the original Custom type has been replaced with another with resolved parametric types then it has a different name
                    if name != effective_name {
                        panic!("unmatched custom type {name} {effective_name}");
                    }

                     */
                    if param_types.len() != effective_param_types.len() {
                        panic!("Effective custom parameters count do not match expected");
                    }

                    for (i, param_type) in param_types.iter().enumerate() {
                        let effective_param_type = effective_param_types.get(i).unwrap();

                        for (name, inner_effective_type) in self
                            .match_parametric_type_with_effective_type(
                                &param_type.ast_type,
                                &effective_param_type.ast_type,
                            )
                        {
                            if let Some(old_type) =
                                result.insert(name.into(), inner_effective_type.clone())
                            {
                                if !old_type.eq(&inner_effective_type) {
                                    panic!("There's already a type for a parameter that does not match");
                                }
                            }
                        }
                    }
                }
            },
        }
        result
    }

    fn substitute(
        &self,
        parametric_type: &ASTType,
        param_types_to_effective_types: &HashMap<String, ASTType>,
        resolved_enum_defs: &Vec<ResolvedEnumDef>,
    ) -> (ASTType, Vec<ResolvedEnumDef>) {
        if param_types_to_effective_types.is_empty() {
            return (parametric_type.clone(), Vec::new());
        }

        let mut not_added_resolved_enum_defs = Vec::new();

        let new_type = match parametric_type {
            ASTType::Builtin(kind) => match kind {
                BuiltinTypeKind::ASTString => parametric_type.clone(),
                BuiltinTypeKind::ASTI32 => parametric_type.clone(),
                BuiltinTypeKind::Lambda {
                    parameters,
                    return_type,
                } => {
                    let new_parameters = parameters
                        .iter()
                        .map(|it| {
                            let mut all_resolved_enum_defs = resolved_enum_defs.clone();
                            all_resolved_enum_defs
                                .append(&mut not_added_resolved_enum_defs.clone());

                            for x in self.enums.values() {
                                for y in x {
                                    all_resolved_enum_defs.push(y.clone());
                                }
                            }

                            //all_resolved_enum_defs.append(&mut self_enums);

                            let (ast_type, mut red) = self.substitute(
                                &it.ast_type,
                                param_types_to_effective_types,
                                &all_resolved_enum_defs,
                            );
                            ASTTypeRef {
                                ast_ref: it.ast_ref,
                                ast_type,
                            }
                        })
                        .collect();

                    let new_return_type = return_type.clone().map(|it| {
                        let mut all_resolved_enum_defs = resolved_enum_defs.clone();
                        all_resolved_enum_defs.append(&mut not_added_resolved_enum_defs.clone());

                        let (ast_type, mut red) = self.substitute(
                            &it.ast_type,
                            param_types_to_effective_types,
                            &all_resolved_enum_defs,
                        );

                        not_added_resolved_enum_defs.append(&mut red);

                        Box::new(ASTTypeRef {
                            ast_ref: it.ast_ref,
                            ast_type,
                        })
                    });

                    let new_lambda = BuiltinTypeKind::Lambda {
                        parameters: new_parameters,
                        return_type: new_return_type,
                    };

                    ASTType::Builtin(new_lambda)
                }
            },
            ASTType::Parametric(name) => param_types_to_effective_types.get(name).unwrap().clone(),
            ASTType::Custom { name, param_types } => {
                if param_types
                    .iter()
                    .map(|it| self.get_parametric_types(&it.ast_type).len())
                    .sum::<usize>()
                    == 0
                {
                    parametric_type.clone()
                } else {
                    let new_param_types = param_types
                        .iter()
                        .map(|it| {
                            let mut all_resolved_enum_defs = resolved_enum_defs.clone();
                            all_resolved_enum_defs
                                .append(&mut not_added_resolved_enum_defs.clone());

                            let (ast_type, mut red) = self.substitute(
                                &it.ast_type,
                                param_types_to_effective_types,
                                &all_resolved_enum_defs,
                            );

                            not_added_resolved_enum_defs.append(&mut red);

                            ASTTypeRef {
                                ast_ref: it.ast_ref,
                                ast_type,
                            }
                        })
                        .collect();

                    let mut all_resolved_enum_defs = resolved_enum_defs.clone();
                    all_resolved_enum_defs.append(&mut not_added_resolved_enum_defs.clone());

                    if let Some(resolved_enum_def) = all_resolved_enum_defs.iter().find(|it| {
                        &it.original_name == name && it.parameter_types == new_param_types
                    }) {
                        ASTType::Custom {
                            name: resolved_enum_def.enum_def.name.clone(),
                            param_types: new_param_types,
                        }
                    } else {
                        let enum_def = self
                            .module
                            .enums
                            .iter()
                            .find(|it| &it.name == name)
                            .unwrap();

                        let count = all_resolved_enum_defs.len() + 1;

                        let new_enum_def = ASTEnumDef {
                            type_parameters: Vec::new(),
                            name: name.clone() + &count.to_string(),
                            variants: enum_def
                                .variants
                                .iter()
                                .map(|it| {
                                    let (var_def, mut red) = self.substitute_variant_def(
                                        it,
                                        param_types_to_effective_types,
                                        &all_resolved_enum_defs,
                                        &enum_def,
                                        None,
                                    );

                                    not_added_resolved_enum_defs.append(&mut red);

                                    var_def
                                })
                                .collect(),
                        };

                        let resolved_enum_def = ResolvedEnumDef {
                            parameter_types: new_param_types.clone(),
                            original_name: name.clone(),
                            enum_def: new_enum_def.clone(),
                        };

                        let name = resolved_enum_def.enum_def.name.clone();

                        // I resolve it again since there could be some self references in variant parameters

                        let mut temp_resolved_enum_defs = not_added_resolved_enum_defs.clone();
                        temp_resolved_enum_defs.push(resolved_enum_def);

                        let new_enum_def = ASTEnumDef {
                            type_parameters: Vec::new(),
                            name: name.clone(),
                            variants: enum_def
                                .variants
                                .iter()
                                .map(|it| {
                                    let (var_def, mut red) = self.substitute_variant_def(
                                        it,
                                        param_types_to_effective_types,
                                        &temp_resolved_enum_defs,
                                        &new_enum_def,
                                        Some(&new_param_types),
                                    );

                                    not_added_resolved_enum_defs.append(&mut red);

                                    var_def
                                })
                                .collect(),
                        };

                        let resolved_enum_def = ResolvedEnumDef {
                            parameter_types: new_param_types.clone(),
                            original_name: name.clone(),
                            enum_def: new_enum_def,
                        };

                        not_added_resolved_enum_defs.push(resolved_enum_def);

                        ASTType::Custom {
                            name,
                            param_types: new_param_types,
                        }
                    }
                }
            }
        };

        (new_type, not_added_resolved_enum_defs)
    }

    fn substitute_variant_def(
        &self,
        parametric_type: &ASTEnumVariantDef,
        param_types_to_effective_types: &HashMap<String, ASTType>,
        resolved_enum_defs: &Vec<ResolvedEnumDef>,
        enum_def: &ASTEnumDef,
        option: Option<&Vec<ASTTypeRef>>,
    ) -> (ASTEnumVariantDef, Vec<ResolvedEnumDef>) {
        let mut not_added_resolved_enum_defs = Vec::new();

        let variant_def = ASTEnumVariantDef {
            name: parametric_type.name.clone(),
            parameters: parametric_type
                .parameters
                .iter()
                .map(|par| {
                    let mut all_resolved_enum_defs = resolved_enum_defs.clone();
                    all_resolved_enum_defs.append(&mut not_added_resolved_enum_defs.clone());

                    let it_is_the_same_enum_type =
                        if let ASTType::Custom { name, param_types } = &par.type_ref.ast_type {
                            let parametric_types: HashSet<&String> = HashSet::from_iter(
                                param_types
                                    .iter()
                                    .map(|it| match &it.ast_type {
                                        ASTType::Parametric(p) => Some(p),
                                        _ => None,
                                    })
                                    .flatten(),
                            );

                            &enum_def.name == name
                                && parametric_types == HashSet::from_iter(&enum_def.type_parameters)
                        } else {
                            false
                        };

                    if it_is_the_same_enum_type {
                        let mut cloned = par.clone();

                        if let Some(p) = option {
                            cloned.type_ref.ast_type = ASTType::Custom {
                                name: enum_def.name.clone(),
                                param_types: p.clone(),
                            };
                        }

                        cloned
                    } else {
                        let (ast_param_def, mut red) = self.substitute_param_def(
                            par,
                            param_types_to_effective_types,
                            &all_resolved_enum_defs,
                        );

                        not_added_resolved_enum_defs.append(&mut red);

                        ast_param_def
                    }
                })
                .collect(),
        };

        (variant_def, not_added_resolved_enum_defs)
    }

    fn substitute_param_def(
        &self,
        par: &ASTParameterDef,
        param_types_to_effective_types: &HashMap<String, ASTType>,
        resolved_enum_defs: &Vec<ResolvedEnumDef>,
    ) -> (ASTParameterDef, Vec<ResolvedEnumDef>) {
        let (ast_type, red) = self.substitute(
            &par.type_ref.ast_type,
            param_types_to_effective_types,
            resolved_enum_defs,
        );
        let new_param_def = ASTParameterDef {
            name: par.name.clone(),
            type_ref: ASTTypeRef {
                ast_ref: par.type_ref.ast_ref,
                ast_type,
            },
        };

        (new_param_def, red)
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::backend::BackendAsm386;
    use crate::codegen::EnhancedASTModule;
    use crate::lexer::Lexer;
    use crate::parser::ast::{
        ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTModule,
        ASTParameterDef, ASTTypeRef,
    };
    use crate::parser::Parser;
    use crate::transformations::enum_functions_creator::enum_functions_creator;
    use crate::transformations::struct_functions_creator::struct_functions_creator;
    use crate::type_check::ToTypedModuleConverter;
    use std::path::Path;

    #[test]
    fn test() {
        /*
        let path = Path::new("resources/test/helloworld.rasm");
        let lexer = Lexer::from_file(path).unwrap();
        let mut parser = Parser::new(lexer, path.to_str().map(|it| it.to_string()));
        let module = parser.parse(path);

         */

        let parameter = ASTExpression::Number(10);

        let call = ASTFunctionCall {
            function_name: "consume".into(),
            parameters: vec![parameter],
        };

        let function_def = ASTFunctionDef {
            name: "consume".into(),
            body: ASTFunctionBody::RASMBody(Vec::new()),
            parameters: vec![ASTParameterDef {
                name: "v".into(),
                type_ref: ASTTypeRef::parametric("T", false),
            }],
            inline: false,
            return_type: None,
            param_types: vec!["T".into()],
        };

        let module = ASTModule {
            structs: Vec::new(),
            enums: Vec::new(),
            body: vec![call],
            functions: vec![function_def],
        };

        let mut converter = ToTypedModuleConverter::new(EnhancedASTModule::new(&module));
        let new_module = converter.convert();

        println!("new_module {:?}", new_module);

        assert_eq!(new_module.body.get(0).unwrap().function_name, "consume_0");

        println!("{:?}", new_module);
    }

    #[test]
    fn test_list() {
        let path = Path::new("resources/test/list.rasm");
        let lexer = Lexer::from_file(path).unwrap();
        let mut parser = Parser::new(lexer, path.to_str().map(|it| it.to_string()));
        let module = parser.parse(path);

        let backend = BackendAsm386::new();
        let mut converter = ToTypedModuleConverter::new(struct_functions_creator(
            &backend,
            &enum_functions_creator(&backend, &EnhancedASTModule::new(&module)),
        ));
        let new_module = converter.convert();

        println!(
            "{:?}",
            new_module
                .functions_by_name
                .values()
                .map(|it| &it.name)
                .collect::<Vec<&String>>()
        );

        println!(
            "{:?}",
            new_module
                .enums
                .iter()
                .map(|it| &it.name)
                .collect::<Vec<&String>>()
        );

        println!("{:?}", new_module.enums);
    }

    #[test]
    #[test_env_log::test]
    fn test_list_fmap() {
        let path = Path::new("resources/test/list_fmap.rasm");
        let lexer = Lexer::from_file(path).unwrap();
        let mut parser = Parser::new(lexer, path.to_str().map(|it| it.to_string()));
        let module = parser.parse(path);

        let backend = BackendAsm386::new();
        let mut converter = ToTypedModuleConverter::new(struct_functions_creator(
            &backend,
            &enum_functions_creator(&backend, &EnhancedASTModule::new(&module)),
        ));
        let new_module = converter.convert();

        println!(
            "{:?}",
            new_module
                .functions_by_name
                .values()
                .map(|it| &it.name)
                .collect::<Vec<&String>>()
        );

        println!(
            "{:?}",
            new_module
                .enums
                .iter()
                .map(|it| &it.name)
                .collect::<Vec<&String>>()
        );

        println!("{:?}", new_module.enums);
    }
}
