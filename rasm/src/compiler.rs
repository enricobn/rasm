use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::time::Instant;

use log::info;

use rasm_core::codegen::backend::Backend;
use rasm_core::codegen::backend::BackendNasm386;
use rasm_core::codegen::enhanced_module::EnhancedASTModule;
use rasm_core::codegen::statics::Statics;
use rasm_core::codegen::CodeGen;
use rasm_core::errors::{CompilationError, CompilationErrorKind};
use rasm_core::parser::ast::ASTExpression::ASTFunctionCallExpression;
use rasm_core::parser::ast::{
    ASTExpression, ASTFunctionCall, ASTIndex, ASTLambdaDef, ASTModule, ASTStatement, ASTType,
    ValueType,
};
use rasm_core::project::RasmProject;

pub struct Compiler {
    project: RasmProject,
    out: PathBuf,
    is_test: bool,
}

impl Compiler {
    pub fn new(project: RasmProject, out: Option<&String>, is_test: bool) -> Self {
        let out = if let Some(o) = out {
            Path::new(o).to_path_buf()
        } else {
            project
                .out_file(is_test)
                .expect("undefined out in rasm.toml")
        }
        .with_extension("asm");

        info!("out: {}", out.with_extension("").to_string_lossy());

        Self {
            project,
            out,
            is_test,
        }
    }

    pub fn compile(&self, only_compile: bool) {
        if self.out.exists() {
            let _ = fs::remove_file(&self.out);
        }

        let executable = self.out.with_extension("");

        if executable.exists() {
            let _ = fs::remove_file(&executable);
        }

        let object = self.out.with_extension("o");

        if object.exists() {
            let _ = fs::remove_file(&object);
        }

        let start = Instant::now();

        let debug_asm = false;

        let mut backend = BackendNasm386::new(debug_asm);
        let mut statics = Statics::new();
        let (mut modules, mut errors) = self.project.get_all_modules(&mut backend, &mut statics);

        if self.is_test {
            modules.iter_mut().for_each(|it| {
                let new_body = it
                    .body
                    .iter()
                    .filter(|st| {
                        if let ASTStatement::LetStatement(_, _, is_const, _) = st {
                            *is_const
                        } else {
                            false
                        }
                    })
                    .cloned()
                    .collect::<Vec<_>>();
                it.body = new_body;
            });
            let (test_modules, test_errors) = self
                .project
                .get_all_test_modules(&mut backend, &mut statics);

            let test_module = Self::main_test_module(&mut errors, &test_modules);

            modules.push(test_module);
            modules.extend(test_modules);
            errors.extend(test_errors);
        }

        if !errors.is_empty() {
            for error in errors {
                eprintln!("{error}");
            }
            panic!()
        }

        let enhanced_ast_module = EnhancedASTModule::new(modules, &self.project);

        info!("parse ended in {:?}", start.elapsed());

        let mut code_gen = CodeGen::new(
            &backend,
            statics,
            enhanced_ast_module,
            1024 * 1024,
            64 * 1024 * 1024,
            1024 * 1024,
            debug_asm,
            false,
            true,
            false,
        );

        let start = Instant::now();

        let asm = code_gen.asm();

        info!("code generation ended in {:?}", start.elapsed());

        let out_path = Path::new(&self.out);
        File::create(out_path)
            .unwrap_or_else(|_| panic!("cannot create file {}", out_path.to_str().unwrap()))
            .write_all(asm.as_bytes())
            .unwrap();

        if only_compile {
            backend.compile(&self.out);
        } else {
            backend.compile_and_link(&self.out);
        }
    }

    fn main_test_module(
        errors: &mut Vec<CompilationError>,
        test_modules: &Vec<ASTModule>,
    ) -> ASTModule {
        let mut test_main_module_body = Vec::new();
        let mut expr = ASTExpression::Value(ValueType::Boolean(false), ASTIndex::none());

        test_modules
            .iter()
            .flat_map(|it| it.functions.iter())
            .filter(|it| it.name.starts_with("test"))
            .for_each(|it| {
                let valid = if let ASTType::Custom {
                    name,
                    param_types: _,
                    index: _,
                } = &it.return_type
                {
                    name == "Assertions"
                } else {
                    false
                };

                if !valid {
                    errors.push(CompilationError {
                        index: it.index.clone(),
                        error_kind: CompilationErrorKind::Generic(
                            "Test function must return Assertions".to_string(),
                        ),
                    });
                } else {
                    let test_call = ASTFunctionCall {
                        original_function_name: it.original_name.to_string(),
                        function_name: it.name.clone(),
                        parameters: Vec::new(),
                        index: ASTIndex::none(),
                        generics: Vec::new(),
                    };
                    let lambda_def = ASTLambdaDef {
                        parameter_names: Vec::new(),
                        body: vec![ASTStatement::Expression(ASTFunctionCallExpression(
                            test_call,
                        ))],
                        index: ASTIndex::none(),
                    };
                    let run_test_call = ASTFunctionCall {
                        original_function_name: "runTest".to_string(),
                        function_name: "runTest".to_string(),
                        parameters: vec![
                            ASTExpression::StringLiteral(it.name.clone()),
                            ASTExpression::StringLiteral(format!("{}", it.index)),
                            ASTExpression::Lambda(lambda_def),
                        ],
                        index: ASTIndex::none(),
                        generics: Vec::new(),
                    };
                    expr =
                        Self::or_expression(expr.clone(), ASTFunctionCallExpression(run_test_call));
                }
            });

        test_main_module_body.push(ASTStatement::LetStatement(
            "results".to_string(),
            expr,
            false,
            ASTIndex::none(),
        ));

        let panic_call = ASTFunctionCall {
            original_function_name: "panic".to_string(),
            function_name: "panic".to_string(),
            parameters: vec![ASTExpression::StringLiteral("Tests failed.".to_string())],
            index: ASTIndex::none(),
            generics: Vec::new(),
        };
        let panic_lambda = ASTLambdaDef {
            parameter_names: Vec::new(),
            body: vec![ASTStatement::Expression(ASTFunctionCallExpression(
                panic_call,
            ))],
            index: ASTIndex::none(),
        };
        let empty_lambda = ASTLambdaDef {
            parameter_names: Vec::new(),
            body: vec![],
            index: ASTIndex::none(),
        };

        let if_call = ASTFunctionCall {
            original_function_name: "if".to_string(),
            function_name: "if".to_string(),
            parameters: vec![
                ASTExpression::ValueRef("results".to_string(), ASTIndex::none()),
                ASTExpression::Lambda(panic_lambda),
                ASTExpression::Lambda(empty_lambda),
            ],
            index: ASTIndex::none(),
            generics: Vec::new(),
        };

        let if_expression = ASTFunctionCallExpression(if_call);

        test_main_module_body.push(ASTStatement::Expression(if_expression));

        ASTModule {
            path: Default::default(),
            body: test_main_module_body,
            functions: vec![],
            enums: vec![],
            structs: vec![],
            requires: Default::default(),
            externals: Default::default(),
            types: vec![],
        }
    }

    fn or_expression(e1: ASTExpression, e2: ASTExpression) -> ASTExpression {
        let call = ASTFunctionCall {
            original_function_name: "or".to_string(),
            function_name: "or".to_string(),
            parameters: vec![e1, e2],
            index: ASTIndex::none(),
            generics: Vec::new(),
        };
        ASTFunctionCallExpression(call)
    }
}
