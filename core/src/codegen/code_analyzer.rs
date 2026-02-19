use std::collections::HashSet;

use crate::{
    codegen::{TypedValKind, enh_ast::EnhASTIndex, enh_val_context::TypedValContext},
    enh_type_check::{
        traverse_typed_ast::TraverseTypedAST,
        typed_ast::{ASTTypedFunctionBody, ASTTypedFunctionCall, ASTTypedFunctionDef},
    },
};

pub struct ReusedParametersScanner {
    parameters: HashSet<EnhASTIndex>,
    reused: HashSet<EnhASTIndex>,
}

impl ReusedParametersScanner {
    fn new() -> ReusedParametersScanner {
        ReusedParametersScanner {
            parameters: HashSet::new(),
            reused: HashSet::new(),
        }
    }
}

impl TraverseTypedAST for ReusedParametersScanner {
    fn found_value_ref(
        &mut self,
        _name: &str,
        _index: &EnhASTIndex,
        _namespace: &super::enh_ast::EnhASTNameSpace,
        kind: &TypedValKind,
    ) {
        if let TypedValKind::ParameterRef(_, def) = kind {
            if !self.parameters.insert(def.ast_index.clone()) {
                self.reused.insert(def.ast_index.clone());
            }
        }
    }

    fn found_call(&mut self, _call: &ASTTypedFunctionCall) {}

    fn found_let(&mut self, _name: &str, _is_const: bool, _index: &EnhASTIndex) {}

    fn found_function_def(&mut self, _function: &ASTTypedFunctionDef) {}

    fn found_asm(
        &mut self,
        _module: &crate::enh_type_check::typed_ast::ASTTypedModule,
        _function: &ASTTypedFunctionDef,
        _native_code: &str,
    ) {
    }
}

pub fn reused_params(
    def: &ASTTypedFunctionDef,
    val_context: &TypedValContext,
) -> HashSet<EnhASTIndex> {
    if let ASTTypedFunctionBody::RASMBody(body) = &def.body {
        let mut reused = ReusedParametersScanner::new();
        let mut context = val_context.clone();
        reused.traverse_body(body, &mut context);
        reused.reused
    } else {
        HashSet::new()
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use crate::{
        codegen::{
            c::options::COptions, code_analyzer::reused_params, compile_target::CompileTarget,
            enh_val_context::TypedValContext,
        },
        commandline::RasmProfile,
        enh_type_check::typed_ast::ASTTypedFunctionDef,
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

        let reused = reused_params(&function, &mut context);

        assert!(reused.contains(&reused_aram.ast_index));
        assert!(reused.contains(&reused_param1.ast_index));
        assert!(reused.contains(&reused_param2.ast_index));
        assert!(!reused.contains(&param.ast_index));
    }

    #[test]
    fn test_lexer() {
        let project = RasmProject::new(PathBuf::from("resources/test/reused_parameter.rasm"));

        let function = get_function(&project, "processNumber", &RasmProfile::Main);

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

        let mut context = TypedValContext::new(None);

        for param in function.parameters.iter() {
            context.insert_par(param.name.clone(), 0, param.clone());
        }

        let reused = reused_params(&function, &mut context);

        assert!(reused.contains(&c.ast_index));
        assert!(reused.contains(&state.ast_index));
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
