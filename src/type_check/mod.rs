pub mod resolved_ast;

use crate::codegen::{EnhancedASTModule, VarContext, VarKind};
use crate::parser::ast::{
    ASTEnumDef, ASTEnumVariantDef, ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef,
    ASTLambdaDef, ASTModule, ASTParameterDef, ASTStructDef, ASTType, ASTTypeRef, BuiltinTypeKind,
};
use crate::type_check::resolved_ast::{
    ASTTypedEnumDef, ASTTypedEnumVariantDef, ASTTypedExpression, ASTTypedFunctionBody,
    ASTTypedFunctionCall, ASTTypedFunctionDef, ASTTypedLambdaDef, ASTTypedModule,
    ASTTypedParameterDef, ASTTypedStructDef, ASTTypedType, ASTTypedTypeRef, BuiltinTypedTypeKind,
};
use linked_hash_map::LinkedHashMap;
use log::debug;
use std::collections::HashMap;
use std::env::var;

pub struct ToTypedModuleConverter {
    module: EnhancedASTModule,
    /// key=original function name
    /// value=Vector of possible matches between parametric types and effective types and related function definition
    functions: HashMap<String, Vec<ResolvedFunctionDef>>,
    count: usize,
}

struct TransformedCall {
    new_function_call: ASTFunctionCall,
    new_function_def: Option<(usize, Vec<ResolvedFunctionDef>)>,
}

#[derive(Debug, Clone)]
struct ResolvedFunctionDef {
    type_parameters: HashMap<String, ASTType>,
    function_def: ASTFunctionDef,
}

impl ToTypedModuleConverter {
    pub fn new(module: EnhancedASTModule) -> Self {
        ToTypedModuleConverter {
            module,
            functions: HashMap::new(),
            count: 0,
        }
    }

    pub fn convert(&mut self) -> ASTTypedModule {
        let mut body = Vec::new();

        let context = VarContext::new(None);

        let module = self.module.clone();

        for call in module.body.iter() {
            if let Some(transformed_call) = self.transform_call(self.count, &context, &module, call)
            {
                if let Some((new_count, resolved_function_defs)) = transformed_call.new_function_def
                {
                    /*
                    resolved_function_defs.iter().for_each(|def| {
                        let mut lambda_resolved_par = Vec::new();

                        for par in def.function_def.parameters.iter() {
                            if let ASTType::Builtin(BuiltinTypeKind::Lambda {
                                return_type,
                                parameters,
                            }) = &par.type_ref.ast_type
                            {
                                for lambda_p in parameters.iter() {
                                    if let ASTType::Parametric(p) = &lambda_p.ast_type {
                                        if def.type_parameters.contains_key(p) {
                                            panic!("resolved lambda parameter {p}");
                                        } else {
                                            panic!("unresolved lambda parameter {p}");
                                        }
                                    }
                                }
                            } else {
                                lambda_resolved_par.push(par.clone());
                            }
                        }
                    });

                     */

                    self.count = new_count;
                    for resolved_function_def in resolved_function_defs {
                        self.functions
                            .entry(call.function_name.clone())
                            .or_insert_with(Vec::new)
                            .push(resolved_function_def);
                    }
                }

                body.push(transformed_call.new_function_call);
            } else {
                body.push(call.clone());
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
            body: body
                .iter()
                .map(|it| self.function_call(it))
                .collect(),
            structs: self
                .module
                .structs
                .iter()
                .map(|it| self.struct_def(it))
                .collect(),
            enums: self
                .module
                .enums
                .iter()
                .map(|it| self.enum_def(it))
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

    fn enum_def(&self, struct_def: &ASTEnumDef) -> ASTTypedEnumDef {
        ASTTypedEnumDef {
            name: struct_def.name.clone(),
            variants: struct_def
                .variants
                .iter()
                .map(|it| self.enum_variant(it))
                .collect(),
        }
    }

    fn enum_variant(&self, variant: &ASTEnumVariantDef) -> ASTTypedEnumVariantDef {
        ASTTypedEnumVariantDef {
            name: variant.name.clone(),
            parameters: variant
                .parameters
                .iter()
                .map(|it| self.parameter_def(it, ""))
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
        count: usize,
        context: &VarContext,
        module: &EnhancedASTModule,
        call: &ASTFunctionCall,
    ) -> Option<TransformedCall> {
        let mut count = count;

        let mut parameters = Vec::new();
        let mut resolved_function_defs = Vec::new();

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
                            transformed_function_call.new_function_def
                        {
                            count = new_count;
                            resolved_function_defs.append(&mut new_function_defs);
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

        let function_def = module
            .functions_by_name
            .iter()
            .find(|(name, it)| name.to_string() == call.function_name)
            .map(|it| it.1)
            .unwrap_or_else(|| panic!("Cannot find function {}", call.function_name));

        if function_def.param_types.is_empty() {
            let mut new_call = call.clone();
            new_call.parameters = parameters;
            Some(TransformedCall {
                new_function_call: new_call,
                new_function_def: Some((count, resolved_function_defs)),
            })
        } else {
            let mut param_types_to_effective_types = HashMap::new();

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
                    "{} resolved param types do no match {:?}",
                    call.function_name, param_types_to_effective_types
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
                    new_function_def: None,
                })
            } else {
                let effective_parameters: Vec<ASTParameterDef> = function_def
                    .parameters
                    .iter()
                    .map(|it| {
                        let effective_type =
                            self.substitute(&it.type_ref.ast_type, &param_types_to_effective_types);
                        ASTParameterDef {
                            name: it.name.clone(),
                            type_ref: ASTTypeRef {
                                ast_ref: it.type_ref.ast_ref,
                                ast_type: effective_type,
                            },
                        }
                    })
                    .collect();

                let effective_return_type = function_def.return_type.clone().map(|it| ASTTypeRef {
                    ast_ref: it.ast_ref,
                    ast_type: self.substitute(&it.ast_type, &param_types_to_effective_types),
                });

                let new_name = function_def.name.clone() + "_" + &count.to_string();
                count += 1;

                let mut new_function_def = function_def.clone();
                new_function_def.name = new_name.clone();
                new_function_def.parameters = effective_parameters;
                new_function_def.return_type = effective_return_type;

                //Option<(usize, (HashMap<String, ASTType>, ASTFunctionDef), ASTFunctionCall)>

                new_call.function_name = new_name;
                new_call.parameters = parameters;

                resolved_function_defs.push(ResolvedFunctionDef {
                    type_parameters: param_types_to_effective_types.clone(),
                    function_def: new_function_def.clone(),
                });

                Some(TransformedCall {
                    new_function_call: new_call,
                    new_function_def: Some((count, resolved_function_defs)),
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
                if lambda.body.is_empty() {
                    None
                } else {
                    let last_expression = lambda.body.last().unwrap();
                    println!("{:?}", last_expression);
                    Some(self.get_type_of_expr(
                        last_expression,
                        val_context,
                        not_added_resolved_function_defs,
                    ))
                };
                //return Some(ASTTypeRef {ast_type: ASTType::Builtin(BuiltinTypeKind::Lambda {return_type , parameters}), ast_ref: true})
            }
        }
        return None;
    }

    fn get_parametric_types(&self, ast_type: &ASTType) -> Vec<String> {
        return match ast_type {
            ASTType::Builtin(_) => {
                vec![]
            }
            ASTType::Parametric(p) => {
                vec![p.into()]
            }
            ASTType::Custom {
                name: _,
                param_types: pt,
            } => pt
                .iter()
                .flat_map(|it| match it.clone().ast_type {
                    ASTType::Parametric(name) => {
                        vec![name]
                    }
                    _ => self.get_parametric_types(&it.ast_type),
                })
                .collect(),
        };
    }

    fn match_parametric_type_with_effective_type(
        &self,
        parametric_type: &ASTType,
        effective_type: &ASTType,
    ) -> HashMap<String, ASTType> {
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
                        panic!("Expected parametric type");
                    }
                },
                BuiltinTypeKind::Lambda { .. } => {

                    println!("match_parametric_type_with_effective_type lambda {:?}", parametric_type)

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
                    if name != effective_name {
                        panic!("unmatched custom type");
                    }
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
    ) -> ASTType {
        match parametric_type {
            ASTType::Builtin(kind) => match kind {
                BuiltinTypeKind::ASTString => parametric_type.clone(),
                BuiltinTypeKind::ASTI32 => parametric_type.clone(),
                BuiltinTypeKind::Lambda {
                    parameters,
                    return_type,
                } => {
                    let new_parameters = parameters
                        .iter()
                        .map(|it| ASTTypeRef {
                            ast_ref: it.ast_ref,
                            ast_type: self.substitute(&it.ast_type, param_types_to_effective_types),
                        })
                        .collect();

                    let new_return_type = return_type.clone().map(|it| {
                        Box::new(ASTTypeRef {
                            ast_ref: it.ast_ref,
                            ast_type: self.substitute(&it.ast_type, param_types_to_effective_types),
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
                let new_param_types = param_types
                    .iter()
                    .map(|it| ASTTypeRef {
                        ast_ref: it.ast_ref,
                        ast_type: self.substitute(&it.ast_type, param_types_to_effective_types),
                    })
                    .collect();

                ASTType::Custom {
                    name: name.clone(),
                    param_types: new_param_types,
                }
            }
        }
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
    }
}
