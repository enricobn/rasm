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

use crate::codegen::enh_ast::EnhASTIndex;
use crate::enh_type_check::typed_ast::{
    ASTTypedExpression, ASTTypedFunctionBody, ASTTypedFunctionCall, ASTTypedFunctionDef,
    ASTTypedModule, ASTTypedStatement,
};

pub trait TraverseTypedAST {
    fn traverse(&mut self, module: &ASTTypedModule) {
        self.traverse_body(&module.body);

        for function in module.functions_by_name.values() {
            match &function.body {
                ASTTypedFunctionBody::RASMBody(rasm_body) => self.traverse_body(rasm_body),
                ASTTypedFunctionBody::NativeBody(native_code) => {
                    self.found_asm(module, function, native_code);
                }
            }
            self.found_function_def(function);
        }
    }

    fn traverse_body(&mut self, body: &[ASTTypedStatement]) {
        for statement in body {
            self.traverse_statement(statement);
        }
    }

    fn traverse_statement(&mut self, statement: &ASTTypedStatement) {
        match statement {
            ASTTypedStatement::Expression(expr) => self.traverse_expression(expr),
            ASTTypedStatement::LetStatement(name, expr, index) => {
                self.traverse_expression(expr);
                self.found_let(name, false, index);
            }
            ASTTypedStatement::ConstStatement(name, expr, index, _modifiers) => {
                self.traverse_expression(expr);
                self.found_let(name, true, index);
            }
        }
    }

    fn traverse_expression(&mut self, expr: &ASTTypedExpression) {
        match expr {
            ASTTypedExpression::ASTFunctionCallExpression(call) => {
                for par in call.parameters.iter() {
                    self.traverse_expression(par);
                }
                self.found_call(call);
            }
            ASTTypedExpression::ValueRef(_, _) => {}
            ASTTypedExpression::Value(_, _) => {}
            ASTTypedExpression::Lambda(lambda_def) => {
                self.traverse_body(&lambda_def.body);
            }
        }
    }

    fn found_call(&mut self, call: &ASTTypedFunctionCall);

    fn found_let(&mut self, name: &str, is_const: bool, index: &EnhASTIndex);

    fn found_function_def(&mut self, function: &ASTTypedFunctionDef);

    fn found_asm(
        &mut self,
        module: &ASTTypedModule,
        function: &ASTTypedFunctionDef,
        native_code: &str,
    );
}
