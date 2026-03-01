use std::{collections::HashSet, iter::zip};

use log::debug;

use crate::codegen::{
    compile_target::CompileTarget,
    enh_ast::{
        EnhASTExpression, EnhASTFunctionBody, EnhASTFunctionCall, EnhASTStatement, EnhASTType,
        EnhBuiltinTypeKind,
    },
    enh_val_context::EnhValContext,
    enhanced_module::EnhancedASTModule,
    statics::Statics,
    typedef_provider::DummyTypeDefProvider,
};

/// Navigate the AST to find all unused functions, navigating the code, starting from the
/// main body.
///
/// It's not used because, at the moment, unused functions
/// are not a problem.
///
/// Returns:
/// - A HashSet of unused function names.
pub fn get_unused_functions_by_navigate(
    target: &CompileTarget,
    module: &EnhancedASTModule,
    debug: bool,
    memory_debug: bool,
) -> HashSet<String> {
    let mut used_functions = HashSet::new();
    for f in target.get_default_functions(memory_debug) {
        used_functions.insert(f.name.clone());
    }

    let mut context = EnhValContext::new(None);
    for st in module.body.iter() {
        statement_used_functions(&mut used_functions, st, &mut context, module);
    }

    let dummy = DummyTypeDefProvider::empty();
    let mut new_used_functions = used_functions.clone();
    let mut statics = Statics::new();
    loop {
        let mut inner_used_functions = HashSet::new();
        for used_function in new_used_functions.iter() {
            if let Some(function) = module.functions_by_name.find_function(used_function) {
                let mut context = EnhValContext::new(None);

                for p in function.parameters.iter() {
                    context.insert_par(p.name.clone(), p.clone()).unwrap();
                }

                match &function.body {
                    EnhASTFunctionBody::RASMBody(body) => {
                        for st in body.iter() {
                            statement_used_functions(
                                &mut inner_used_functions,
                                st,
                                &mut context,
                                module,
                            );
                        }
                    }
                    EnhASTFunctionBody::NativeBody(body) => {
                        let calls = target
                            .called_functions(
                                None,
                                Some(function),
                                body,
                                &context,
                                &dummy,
                                &mut statics,
                                debug,
                                memory_debug,
                            )
                            .unwrap();
                        for (_, call) in calls.iter() {
                            let c = call.to_call(&function);
                            call_used_functions(
                                &mut inner_used_functions,
                                &c.unwrap(),
                                &mut context,
                                module,
                            );
                        }
                    }
                }
            } else {
                debug!("Function {used_function} not found");
            }
        }

        new_used_functions = inner_used_functions.clone();
        new_used_functions.retain(|it| !used_functions.contains(it));

        if new_used_functions.len() == 0 {
            break;
        }

        used_functions.extend(new_used_functions.clone());
    }

    let result: HashSet<String> = module
        .functions()
        .iter()
        .filter(|it| !used_functions.contains(&it.name))
        .map(|it| it.name.clone())
        .collect();

    result
}

fn statement_used_functions(
    used_functions: &mut HashSet<String>,
    statement: &EnhASTStatement,
    context: &mut EnhValContext,
    module: &EnhancedASTModule,
) {
    let mut uf = HashSet::new();
    match statement {
        EnhASTStatement::Expression(enh_astexpression) => {
            expr_used_functions(&mut uf, enh_astexpression, context, module);
        }
        EnhASTStatement::LetStatement(_, enh_astexpression, _) => {
            expr_used_functions(&mut uf, enh_astexpression, context, module);
        }
        EnhASTStatement::ConstStatement(_, enh_astexpression, _, _, _) => {
            expr_used_functions(&mut uf, enh_astexpression, context, module);
        }
    }

    used_functions.extend(uf);
}

fn call_used_functions(
    used_functions: &mut HashSet<String>,
    call: &EnhASTFunctionCall,
    context: &mut EnhValContext,
    module: &EnhancedASTModule,
) {
    if context.get_lambda(&call.function_name).is_some() {
        return;
    }
    used_functions.insert(call.function_name.clone());

    let f = module
        .functions_by_name
        .find_function(&call.function_name)
        .unwrap();

    for (expr, t) in zip(call.parameters.iter(), f.parameters.iter()) {
        if matches!(
            t.ast_type,
            EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
                parameters: _,
                return_type: _
            })
        ) {
            if let EnhASTExpression::ValueRef(name, _, _) = expr {
                if context.get(&name).is_none() {
                    if module.functions_by_name.find_function(name).is_some() {
                        used_functions.insert(name.clone());
                    } else {
                        debug!("Function {name} not found");
                    }
                }
            }
        }

        expr_used_functions(used_functions, expr, context, module);
    }
}

fn expr_used_functions(
    used_functions: &mut HashSet<String>,
    expression: &EnhASTExpression,
    context: &mut EnhValContext,
    module: &EnhancedASTModule,
) {
    match expression {
        EnhASTExpression::ASTFunctionCallExpression(enh_astfunction_call_expression) => {
            call_used_functions(
                used_functions,
                enh_astfunction_call_expression,
                context,
                module,
            );
        }
        EnhASTExpression::ValueRef(_, _, _) => {}
        EnhASTExpression::Value(_, _) => {}
        EnhASTExpression::Lambda(enh_astlambda_def) => {
            for st in enh_astlambda_def.body.iter() {
                statement_used_functions(used_functions, st, context, module);
            }
        }
        EnhASTExpression::Any(_) => {}
    }
}
