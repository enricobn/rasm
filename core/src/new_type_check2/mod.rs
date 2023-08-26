/*
 *     RASM compiler.
 *     Copyright (C) 2022-2023  Enrico Benedetti
 *
 *     This program is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

use crate::parser::ast::{
    ASTExpression, ASTFunctionCall, ASTLambdaDef, ASTModule, ASTStatement, ASTType,
};
use crate::type_check::typed_ast::{
    ASTTypedExpression, ASTTypedFunctionCall, ASTTypedLambdaDef, ASTTypedModule, ASTTypedStatement,
    ASTTypedType,
};

pub struct TypeCheck {
    module: ASTTypedModule,
}

impl TypeCheck {
    pub fn new() -> Self {
        let typed_module = ASTTypedModule {
            body: vec![],
            functions_by_name: Default::default(),
            enums: vec![],
            structs: vec![],
            types: vec![],
        };

        Self {
            module: typed_module,
        }
    }

    pub fn type_check(&mut self, module: ASTModule) -> ASTTypedModule {
        for statement in module.body.into_iter() {
            let typed_statement = self.transform_statement(statement);
            self.module.body.push(typed_statement);
        }

        self.module.clone()
    }

    fn transform_statement(&mut self, statement: ASTStatement) -> ASTTypedStatement {
        match statement {
            ASTStatement::Expression(e) => {
                ASTTypedStatement::Expression(self.transform_expression(e))
            }
            ASTStatement::LetStatement(name, e, is_const, index) => {
                ASTTypedStatement::LetStatement(name, self.transform_expression(e), is_const, index)
            }
        }
    }

    fn transform_expression(&mut self, expression: ASTExpression) -> ASTTypedExpression {
        match expression {
            ASTExpression::StringLiteral(s) => ASTTypedExpression::StringLiteral(s),
            ASTExpression::ASTFunctionCallExpression(call) => {
                ASTTypedExpression::ASTFunctionCallExpression(self.transform_call(call))
            }
            ASTExpression::ValueRef(name, index) => ASTTypedExpression::ValueRef(name, index),
            ASTExpression::Value(value_type, index) => ASTTypedExpression::Value(value_type, index),
            ASTExpression::Lambda(lambda_def) => {
                ASTTypedExpression::Lambda(self.transform_lambda_def(lambda_def))
            }
            ASTExpression::Any(ast_type) => {
                ASTTypedExpression::Any(self.transform_ast_type(ast_type))
            }
        }
    }

    fn transform_ast_type(&mut self, ast_type: ASTType) -> ASTTypedType {
        todo!()
    }

    fn transform_call(&mut self, call: ASTFunctionCall) -> ASTTypedFunctionCall {
        todo!()
    }

    fn transform_lambda_def(&mut self, lambda_def: ASTLambdaDef) -> ASTTypedLambdaDef {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;
    use std::env;
    use std::io::Write;
    use std::path::PathBuf;

    use env_logger::Builder;

    use crate::codegen::backend::BackendNasm386;
    use crate::codegen::statics::Statics;
    use crate::new_type_check2::TypeCheck;
    use crate::parser::ast::ASTModule;
    use crate::project::project::RasmProject;
    use crate::transformations::enrich_module;

    fn init() {
        Builder::from_default_env()
            .format(|buf, record| {
                writeln!(
                    buf,
                    "{} [{}] - {}",
                    chrono::Local::now().format("%Y-%m-%d %H:%M:%S.%3f"),
                    record.level(),
                    record.args()
                )
            })
            .try_init()
            .unwrap_or(());
    }

    fn to_ast_module(test_file: &str) -> ASTModule {
        init();
        env::set_var("RASM_STDLIB", "../../../stdlib");
        let file_name = PathBuf::from(&format!("../rasm/resources/test/{test_file}"));

        let project = RasmProject::new(file_name);

        let mut statics = Statics::new();
        let mut module = project.get_module();

        enrich_module(
            &BackendNasm386::new(HashSet::new(), HashSet::new(), false),
            project.resource_folder(),
            &mut statics,
            &mut module,
        );

        module
    }

    #[test]
    pub fn fibonacci() {
        let module = to_ast_module("fibonacci.rasm");

        let mut type_check = TypeCheck::new();
        let typed_module = type_check.type_check(module);
    }
}
