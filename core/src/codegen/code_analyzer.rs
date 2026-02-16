use crate::{
    codegen::{TypedValKind, enh_val_context::TypedValContext},
    enh_type_check::typed_ast::{ASTTypedExpression, ASTTypedParameterDef, ASTTypedStatement},
};

pub fn is_par_reused(
    body: &Vec<ASTTypedStatement>,
    def: &ASTTypedParameterDef,
    context: &TypedValContext,
) -> bool {
    let mut found = false;
    explore_body(body, def, &mut found, context)
}

fn explore_body(
    body: &Vec<ASTTypedStatement>,
    def: &ASTTypedParameterDef,
    found: &mut bool,
    context: &TypedValContext,
) -> bool {
    for statement in body.iter() {
        match statement {
            ASTTypedStatement::Expression(expr) => {
                if explore_expr(expr, def, found, context) {
                    return true;
                }
            }
            ASTTypedStatement::LetStatement(_, expr, _) => {
                if explore_expr(expr, def, found, context) {
                    return true;
                }
            }
            ASTTypedStatement::ConstStatement(_, _, _, _, _) => {}
        }
    }
    false
}

fn explore_expr(
    expr: &ASTTypedExpression,
    def: &ASTTypedParameterDef,
    found: &mut bool,
    context: &TypedValContext,
) -> bool {
    match expr {
        ASTTypedExpression::ASTFunctionCallExpression(call) => {
            for parameter in &call.parameters {
                if explore_expr(parameter, def, found, context) {
                    return true;
                }
            }
        }
        ASTTypedExpression::ValueRef(_, _, _) => {
            if is_ref_to_par(expr, def, context) {
                if *found {
                    return true;
                } else {
                    *found = true;
                }
            }
        }
        ASTTypedExpression::Value(_, _) => {}
        ASTTypedExpression::Lambda(asttyped_lambda_def) => {
            if explore_body(&asttyped_lambda_def.body, def, found, context) {
                return true;
            }
        }
    }
    false
}

fn is_ref_to_par(
    expr: &ASTTypedExpression,
    def: &ASTTypedParameterDef,
    context: &TypedValContext,
) -> bool {
    if let ASTTypedExpression::ValueRef(ref_name, _, _) = expr {
        if ref_name == &def.name {
            if let Some(TypedValKind::ParameterRef(_, pd)) = context.get(&def.name) {
                // println!("found ref to parameter {name}");
                return pd.ast_index == def.ast_index;
            }
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use crate::{
        codegen::{
            c::options::COptions, code_analyzer::is_par_reused, compile_target::CompileTarget,
            enh_val_context::TypedValContext,
        },
        commandline::RasmProfile,
        enh_type_check::typed_ast::{ASTTypedFunctionBody, ASTTypedFunctionDef, ASTTypedStatement},
        project::RasmProject,
        test_utils::project_to_ast_typed_module,
    };

    #[test]
    fn test() {
        let project = RasmProject::new(PathBuf::from("resources/test/reused_parameter.rasm"));

        let function = get_function(&project, "aFunction", &RasmProfile::Main);

        let reused_aram = function
            .parameters
            .iter()
            .find(|it| it.name == "reusedParam")
            .unwrap();
        let reused_param1 = function
            .parameters
            .iter()
            .find(|it| it.name == "reusedParam1")
            .unwrap();
        let reused_param2 = function
            .parameters
            .iter()
            .find(|it| it.name == "reusedParam2")
            .unwrap();
        let param = function
            .parameters
            .iter()
            .find(|it| it.name == "param")
            .unwrap();
        let mut context = TypedValContext::new(None);

        for param in function.parameters.iter() {
            context.insert_par(param.name.clone(), 0, param.clone());
        }

        let body = match &function.body {
            ASTTypedFunctionBody::RASMBody(body) => body,
            _ => panic!(),
        };

        assert!(is_par_reused(body, reused_aram, &context));
        assert!(is_par_reused(body, reused_param1, &context));
        assert!(is_par_reused(body, reused_param2, &context));
        assert!(!is_par_reused(body, param, &context));
    }

    #[test]
    fn test_lexer() {
        let project = RasmProject::new(PathBuf::from("resources/test/reused_parameter.rasm"));

        let function = get_function(&project, "processNumber", &RasmProfile::Main);

        let mut context = TypedValContext::new(None);

        for param in function.parameters.iter() {
            context.insert_par(param.name.clone(), 0, param.clone());
        }

        let body = match &function.body {
            ASTTypedFunctionBody::RASMBody(body) => body,
            _ => panic!(),
        };

        let c = function
            .parameters
            .iter()
            .find(|it| it.name == "c")
            .unwrap();
        let state = function
            .parameters
            .iter()
            .find(|it| it.name == "state")
            .unwrap();

        assert!(is_par_reused(body, c, &context));
        assert!(is_par_reused(body, state, &context));
    }

    fn check_function(
        project: &RasmProject,
        name: &str,
        profile: &RasmProfile,
        f: fn(&Vec<ASTTypedStatement>, &TypedValContext),
    ) {
        let (typed_module, _) = match project_to_ast_typed_module(
            &project,
            &CompileTarget::C(COptions::default()),
            profile,
        ) {
            Ok(it) => it,
            Err(errors) => {
                for error in errors.iter() {
                    println!("{error}");
                }
                panic!();
            }
        };

        let (_, def) = typed_module
            .functions_by_name
            .iter()
            .find(|it| it.1.original_name == name)
            .unwrap();

        if let ASTTypedFunctionBody::RASMBody(body) = &def.body {
            let mut context = TypedValContext::new(None);

            for param in def.parameters.iter() {
                context.insert_par(param.name.clone(), 0, param.clone());
            }

            f(body, &context);
        } else {
            panic!();
        }
    }

    fn get_function<'a>(
        project: &'a RasmProject,
        name: &str,
        profile: &RasmProfile,
    ) -> ASTTypedFunctionDef {
        let (typed_module, _) = match project_to_ast_typed_module(
            &project,
            &CompileTarget::C(COptions::default()),
            profile,
        ) {
            Ok(it) => it,
            Err(errors) => {
                for error in errors.iter() {
                    println!("{error}");
                }
                panic!()
            }
        };

        let (_, def) = typed_module
            .functions_by_name
            .iter()
            .find(|it| it.1.original_name == name)
            .unwrap();
        def.clone()
    }
}
