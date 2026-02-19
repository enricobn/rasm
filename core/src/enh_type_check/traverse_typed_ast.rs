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

use crate::codegen::TypedValKind;
use crate::codegen::enh_ast::{EnhASTIndex, EnhASTNameSpace};
use crate::codegen::enh_val_context::TypedValContext;
use crate::enh_type_check::typed_ast::{
    ASTTypedExpression, ASTTypedFunctionBody, ASTTypedFunctionCall, ASTTypedFunctionDef,
    ASTTypedModule, ASTTypedParameterDef, ASTTypedStatement, ASTTypedType,
};

pub trait TraverseTypedAST {
    fn traverse(&mut self, module: &ASTTypedModule) {
        let mut val_context = TypedValContext::new(None);
        self.traverse_body(&module.body, &mut val_context);

        for function in module.functions_by_name.values() {
            let mut val_context = TypedValContext::new(Some(&val_context));
            for par in function.parameters.iter() {
                val_context.insert_let(par.name.clone(), par.ast_type.clone(), None);
            }
            match &function.body {
                ASTTypedFunctionBody::RASMBody(rasm_body) => {
                    self.traverse_body(rasm_body, &mut val_context)
                }
                ASTTypedFunctionBody::NativeBody(native_code) => {
                    self.found_asm(module, function, native_code);
                }
            }
            self.found_function_def(function);
        }
    }

    fn traverse_body(&mut self, body: &[ASTTypedStatement], val_context: &mut TypedValContext) {
        for statement in body {
            self.traverse_statement(statement, val_context);
        }
    }

    fn traverse_statement(
        &mut self,
        statement: &ASTTypedStatement,
        val_context: &mut TypedValContext,
    ) {
        match statement {
            ASTTypedStatement::Expression(expr) => self.traverse_expression(expr, val_context),
            ASTTypedStatement::LetStatement(name, expr, index) => {
                // TODO this is a hack, we insert always a unit type
                val_context.insert_let(name.clone(), ASTTypedType::Unit, None);
                self.traverse_expression(expr, val_context);
                self.found_let(name, false, index);
            }
            ASTTypedStatement::ConstStatement(name, expr, index, _namespace, _modifiers) => {
                self.traverse_expression(expr, val_context);
                self.found_let(name, true, index);
            }
        }
    }

    fn traverse_expression(
        &mut self,
        expr: &ASTTypedExpression,
        val_context: &mut TypedValContext,
    ) {
        match expr {
            ASTTypedExpression::ASTFunctionCallExpression(call) => {
                for par in call.parameters.iter() {
                    self.traverse_expression(par, val_context);
                }
                self.found_call(call);
            }
            ASTTypedExpression::ValueRef(name, enh_index, namespace) => {
                if let Some(kind) = val_context.get(name) {
                    self.found_value_ref(name, enh_index, namespace, kind);
                }
            }
            ASTTypedExpression::Value(_, _) => {}
            ASTTypedExpression::Lambda(lambda_def) => {
                let mut val_context = TypedValContext::new(Some(val_context));
                for (i, (name, index)) in lambda_def.parameter_names.iter().enumerate() {
                    let par = ASTTypedParameterDef::new(name, ASTTypedType::Unit, index.clone());
                    val_context.insert_par(name.clone(), i, par);
                }
                self.traverse_body(&lambda_def.body, &mut val_context);
            }
        }
    }

    fn found_value_ref(
        &mut self,
        name: &str,
        index: &EnhASTIndex,
        namespace: &EnhASTNameSpace,
        val_kind: &TypedValKind,
    );

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
