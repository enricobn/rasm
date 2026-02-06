use crate::{
    codegen::{TypedValKind, enh_val_context::TypedValContext},
    enh_type_check::typed_ast::{ASTTypedExpression, ASTTypedStatement},
};

pub fn is_par_reused(body: &Vec<ASTTypedStatement>, name: &str, context: &TypedValContext) -> bool {
    let mut found = false;
    explore_body(body, name, &mut found, context)
}

fn explore_body(
    body: &Vec<ASTTypedStatement>,
    name: &str,
    found: &mut bool,
    context: &TypedValContext,
) -> bool {
    for statement in body.iter() {
        match statement {
            ASTTypedStatement::Expression(expr) => {
                if explore_expr(expr, name, found, context) {
                    return true;
                }
            }
            ASTTypedStatement::LetStatement(_, expr, _) => {
                if explore_expr(expr, name, found, context) {
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
    name: &str,
    found: &mut bool,
    context: &TypedValContext,
) -> bool {
    match expr {
        ASTTypedExpression::ASTFunctionCallExpression(call) => {
            for parameter in &call.parameters {
                if explore_expr(parameter, name, found, context) {
                    return true;
                }
            }
        }
        ASTTypedExpression::ValueRef(_, _, _) => {
            if is_ref_to_par(expr, name, context) {
                if *found {
                    return true;
                } else {
                    *found = true;
                }
            }
        }
        ASTTypedExpression::Value(_, _) => {}
        ASTTypedExpression::Lambda(asttyped_lambda_def) => {
            if explore_body(&asttyped_lambda_def.body, name, found, context) {
                return true;
            }
        }
    }
    false
}

fn is_ref_to_par(expr: &ASTTypedExpression, name: &str, context: &TypedValContext) -> bool {
    if let ASTTypedExpression::ValueRef(ref_name, _, _) = expr {
        if ref_name == name {
            if let Some(TypedValKind::ParameterRef(_, _)) = context.get(name) {
                // println!("found ref to parameter {name}");
                return true;
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
        enh_type_check::typed_ast::{ASTTypedFunctionBody, ASTTypedStatement},
        project::RasmProject,
        test_utils::project_to_ast_typed_module,
    };

    #[test]
    fn test() {
        let project = RasmProject::new(PathBuf::from("resources/test/reused_parameter.rasm"));

        check_function(
            &project,
            "aFunction",
            &RasmProfile::Main,
            |body, context| {
                assert!(is_par_reused(body, "reusedParam", context));
                assert!(is_par_reused(body, "reusedParam1", context));
                assert!(is_par_reused(body, "reusedParam2", context));
                assert!(!is_par_reused(body, "param", context));
            },
        );
    }

    #[test]
    fn test_lexer() {
        let project = RasmProject::new(PathBuf::from("resources/test/reused_parameter.rasm"));

        check_function(
            &project,
            "processNumber",
            &RasmProfile::Main,
            |body, context| {
                assert!(is_par_reused(body, "c", context));
                assert!(is_par_reused(body, "state", context));
            },
        );
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
                panic!()
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
}
