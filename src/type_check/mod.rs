use std::collections::HashMap;
use std::iter;
use crate::codegen::{VarContext, VarKind};
use crate::parser::ast::{ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTModule, ASTParameterDef, ASTType, ASTTypeRef, BuiltinTypeKind};

pub struct ToTypedModuleConverter {
    module: ASTModule,
    /// key=original function name
    /// value=Vector of possible matches between parametric types and effective types and related function definition
    functions: HashMap<String, Vec<(HashMap<String, ASTType>, ASTFunctionDef)>>,
    count: usize
}

impl ToTypedModuleConverter {
    pub fn new(module: ASTModule) -> Self {
        ToTypedModuleConverter { module, functions: HashMap::new(), count: 0 }
    }

    pub fn convert(&mut self) -> ASTModule {
        let mut body = Vec::new();

        let context = VarContext::new(None);

        let module = self.module.clone();

        for call in module.body.iter() {
            body.push(self.transform_call(&context, &module, call));
        }

        let rasm_functions: Vec<ASTFunctionDef> = self.functions.values().into_iter().flat_map(|it| it.iter().map(|c| c.1.clone()))
            .filter(|it| {
                match it.body {
                    ASTFunctionBody::RASMBody(_) => { true}
                    ASTFunctionBody::ASMBody(_) => { false }
                }
            }).collect();

        // TODO

        self.module.clone()
    }

    fn transform_call(&mut self, context: &VarContext, module: &ASTModule, call: &ASTFunctionCall) -> ASTFunctionCall {
        let function_def = module.functions.iter().find(|it| it.name == call.function_name)
            .unwrap_or_else(|| panic!("Cannot find function {}", call.function_name));

        if function_def.param_types.is_empty() {
            call.clone()
        } else {
            let mut param_types_to_effective_types = HashMap::new();

            for (i, param) in function_def.parameters.iter().enumerate() {
                let parametric_types_of_declared_parameter = self.get_parametric_types(&param.type_ref.ast_type);

                if !parametric_types_of_declared_parameter.is_empty() {
                    println!("parametric_types {:?}", parametric_types_of_declared_parameter);
                    let parameter_expression = call.parameters.get(i).unwrap();

                    if let Some(type_of_expression) = self.get_type_of_expr(parameter_expression, &context) {
                        for (name, effective_type) in self.match_parametric_type_with_effective_type(&param.type_ref.ast_type, &type_of_expression.ast_type) {
                            param_types_to_effective_types.insert(name, effective_type);
                        }
                    }
                }
            }

            let mut param_types_for_check = function_def.param_types.clone();

            param_types_for_check.retain(|it| param_types_to_effective_types.contains_key(it));

            if param_types_for_check.len() != function_def.param_types.len() {
                panic!("resolved param types do no match")
            }

            let mut functions = if let Some(functions) = self.functions.get(&function_def.name) {
                functions.clone()
            } else {
                Vec::new()
            };

            let real_function_name = if let Some((_, real_function)) = functions.iter().find(|it| it.0.eq(&param_types_to_effective_types)) {
                real_function.name.clone()
            } else {
                // TODO we have to substitute the parametric types with effective types...
                let effective_parameters: Vec<ASTParameterDef> =
                    function_def.parameters.iter().map(|it| {
                        let effective_type = self.substitute(&it.type_ref.ast_type, &param_types_to_effective_types);
                        ASTParameterDef { name: it.name.clone(), type_ref: ASTTypeRef { ast_ref: it.type_ref.ast_ref, ast_type: effective_type } }
                    }).collect();

                let effective_return_type = function_def.return_type.clone().map(|it| {
                    ASTTypeRef { ast_ref: it.ast_ref, ast_type: self.substitute(&it.ast_type, &param_types_to_effective_types) }
                });

                let new_name = function_def.name.clone() + &self.count.to_string();
                self.count += 1;

                let mut new_function_def = function_def.clone();
                new_function_def.name = new_name.clone();
                new_function_def.parameters = effective_parameters;
                new_function_def.return_type = effective_return_type;
                functions.push((param_types_to_effective_types, new_function_def));

                self.functions.insert(function_def.name.clone(), functions);

                new_name
            };

            let mut new_call =  call.clone();
            new_call.function_name = real_function_name;

            println!("{:?}", new_call);

            new_call
        }
    }

    fn get_type_of_expr(&mut self, expr: &ASTExpression, val_context: &VarContext) -> Option<ASTTypeRef> {
        match expr {
            ASTExpression::StringLiteral(_) => {
                return Some(ASTTypeRef { ast_type: ASTType::Builtin(BuiltinTypeKind::ASTString), ast_ref: true });
            }
            ASTExpression::ASTFunctionCallExpression(_) => {}
            ASTExpression::Val(name) => {
                let var_kind = val_context.get(name).unwrap();
                match var_kind {
                    VarKind::ParameterRef(_, def) => {
                        return Some(def.type_ref.clone());
                    }
                }
            }
            ASTExpression::Number(_) => {
                return Some(ASTTypeRef { ast_type: ASTType::Builtin(BuiltinTypeKind::ASTI32), ast_ref: false });
            }
            ASTExpression::Lambda(lambda) => {
                /*
                let return_type = if lambda.body.is_empty() {
                    None
                } else {
                    let last_expression = lambda.body.last().unwrap();
                    Some(self.get_type_of_expr(last_expression, val_context))
                };
                return ASTTypeRef {ast_type: ASTType::Builtin(BuiltinTypeKind::Lambda {return_type , parameters}), ast_ref: true}

                 */
                return None;
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
            ASTType::Custom { name: _, param_types: pt } => {
                pt.iter().flat_map(|it| {
                    match it.clone().ast_type {
                        ASTType::Parametric(name) => {
                            vec![name]
                        }
                        _ => { self.get_parametric_types(&it.ast_type) }
                    }
                }).collect()
            }
        };
    }

    fn match_parametric_type_with_effective_type(&self, parametric_type: &ASTType, effective_type: &ASTType) -> HashMap<String, ASTType> {
        let mut result = HashMap::new();

        match effective_type {
            ASTType::Builtin(builtin_type_kind) => {
                match builtin_type_kind {
                    BuiltinTypeKind::ASTString => {
                        match parametric_type {
                            ASTType::Parametric(name) => {
                                if let Some(old_type) = result.insert(name.into(), effective_type.clone()) {
                                    if !old_type.eq(effective_type) {
                                        panic!("There's already a type for a parameter that does not matches");
                                    }
                                }
                            }
                            _ => {
                                panic!("Expected parametric type");
                            }
                        }
                    }
                    BuiltinTypeKind::ASTI32 => {
                        match parametric_type {
                            ASTType::Parametric(name) => {
                                if let Some(old_type) = result.insert(name.into(), effective_type.clone()) {
                                    if !old_type.eq(effective_type) {
                                        panic!("There's already a type for a parameter that does not matches");
                                    }
                                }
                            }
                            _ => {
                                panic!("Expected parametric type");
                            }
                        }
                    }
                    BuiltinTypeKind::Lambda { .. } => {}
                }
            }
            ASTType::Parametric(_) => {}
            ASTType::Custom { name: effective_name, param_types: effective_param_types } => {
                match parametric_type {
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

                            for (name, inner_effective_type) in self.match_parametric_type_with_effective_type(&param_type.ast_type,
                                                                                                               &effective_param_type.ast_type) {
                                if let Some(old_type) = result.insert(name.into(), inner_effective_type.clone()) {
                                    if !old_type.eq(&inner_effective_type) {
                                        panic!("There's already a type for a parameter that does not match");
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        result
    }
    fn substitute(&self, parametric_type: &ASTType, param_types_to_effective_types: &HashMap<String, ASTType>) -> ASTType {
        match parametric_type {
            ASTType::Builtin(_) => {
                parametric_type.clone()
            }
            ASTType::Parametric(name) => {
                param_types_to_effective_types.get(name).unwrap().clone()
            }
            ASTType::Custom { name, param_types } => {
                let new_param_types =
                    param_types.iter().map(|it| ASTTypeRef { ast_ref: it.ast_ref, ast_type: self.substitute(&it.ast_type, param_types_to_effective_types) }).collect();

                ASTType::Custom { name: name.clone(), param_types: new_param_types }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;
    use crate::lexer::Lexer;
    use crate::parser::ast::{ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTModule, ASTParameterDef, ASTTypeRef};
    use crate::parser::Parser;
    use crate::type_check::ToTypedModuleConverter;

    #[test]
    fn test() {
        /*
        let path = Path::new("resources/test/helloworld.rasm");
        let lexer = Lexer::from_file(path).unwrap();
        let mut parser = Parser::new(lexer, path.to_str().map(|it| it.to_string()));
        let module = parser.parse(path);

         */

        let parameter = ASTExpression::Number(10);

        let call = ASTFunctionCall { function_name: "consume".into(), parameters: vec![parameter] };

        let function_def = ASTFunctionDef {
            name: "consume".into(),
            body: ASTFunctionBody::RASMBody(Vec::new()),
            parameters: vec![ASTParameterDef {
                name: "v".into(),
                type_ref: ASTTypeRef::parametric("T".into(), false),
            }],
            inline: false,
            return_type: None,
            param_types: vec!["T".into()],
        };

        let module = ASTModule { structs: Vec::new(), enums: Vec::new(), body: vec![call], functions: vec![function_def] };

        let mut converter = ToTypedModuleConverter::new(module);
        converter.convert();
    }
}

