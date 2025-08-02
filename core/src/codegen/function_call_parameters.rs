use auto_impl::auto_impl;
use linked_hash_map::LinkedHashMap;

use crate::codegen::enh_ast::EnhASTIndex;
use crate::codegen::enh_val_context::TypedValContext;
use crate::codegen::lambda::LambdaSpace;
use crate::codegen::statics::Statics;
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::codegen::TypedValKind;
use crate::enh_type_check::typed_ast::{
    ASTTypedExpression, ASTTypedFunctionBody, ASTTypedFunctionDef, ASTTypedModule,
    ASTTypedStatement, ASTTypedType,
};
use rasm_parser::parser::ast::ASTValue;

use super::enh_ast::EnhASTNameSpace;

#[auto_impl(Box)]
pub trait FunctionCallParameters<CTX> {
    fn add_label(
        &mut self,
        param_name: &str,
        label: String,
        value: String,
        comment: Option<&str>,
        typed_type: &ASTTypedType,
        statics: &Statics,
        namespace: &EnhASTNameSpace,
    );

    fn add_string_constant(
        &mut self,
        param_name: &str,
        value: &str,
        comment: Option<&str>,
        statics: &mut Statics,
    );

    fn add_function_call(
        &mut self,
        module: &ASTTypedModule,
        comment: &str,
        param_type: ASTTypedType,
        statics: &mut Statics,
        name: String,
        before: String,
        current: String,
        t: &ASTTypedType,
    );

    fn add_lambda(
        &mut self,
        def: &mut ASTTypedFunctionDef,
        parent_lambda_space: Option<&LambdaSpace>,
        context: &TypedValContext,
        comment: Option<&str>,
        statics: &mut Statics,
        module: &ASTTypedModule,
        code_gen_context: &CTX,
        lambda_in_stack: bool,
        param_type: &ASTTypedType,
        name: &str,
    ) -> LambdaSpace;

    fn add_parameter_ref(
        &mut self,
        original_param_name: String,
        val_name: &str,
        index_in_context: usize,
        lambda_space: &Option<&LambdaSpace>,
        indent: usize,
        code_gen_context: &CTX,
        statics: &Statics,
        type_def_provider: &dyn TypeDefProvider,
        typed_type: &ASTTypedType,
    );

    fn add_let_val_ref(
        &mut self,
        original_param_name: String,
        val_name: &str,
        lambda_space: &Option<&LambdaSpace>,
        indent: usize,
        code_gen_context: &CTX,
        ast_index: &EnhASTIndex,
        statics: &Statics,
        type_def_provider: &dyn TypeDefProvider,
        typed_type: &ASTTypedType,
    );

    fn add_value_type(&mut self, name: &str, value_type: &ASTValue);

    fn push(&mut self, s: &str);

    fn add_on_top_of_after(&mut self, s: &str);

    fn before(&self) -> String;

    fn current(&self) -> String;

    fn after(&self) -> Vec<String>;

    fn parameters_values(&self) -> &LinkedHashMap<String, String>;

    fn resolve_native_parameters(
        &self,
        code_gen_context: &CTX,
        body: &str,
        ident: usize,
        return_value: bool,
        is_inner_call: bool,
        return_type: Option<&ASTTypedType>,
        is_lambda: bool,
    ) -> String;

    fn body_references_to_context(
        &self,
        body: &ASTTypedFunctionBody,
        context: &TypedValContext,
    ) -> Vec<(String, TypedValKind)> {
        if context.is_empty() {
            return Vec::new();
        }
        if let ASTTypedFunctionBody::RASMBody(statements) = body {
            statements
                .iter()
                .flat_map(|it| {
                    self.statement_references_to_context(it, context)
                        .into_iter()
                })
                .collect()
        } else {
            Vec::new()
        }
    }

    fn statement_references_to_context(
        &self,
        statement: &ASTTypedStatement,
        context: &TypedValContext,
    ) -> Vec<(String, TypedValKind)> {
        match statement {
            ASTTypedStatement::Expression(expr) => {
                self.expression_references_to_context(expr, context)
            }
            ASTTypedStatement::LetStatement(_, expr, _index) => {
                self.expression_references_to_context(expr, context)
            }
            ASTTypedStatement::ConstStatement(_, _, _, _, _) => Vec::new(),
        }
    }

    fn expression_references_to_context(
        &self,
        expr: &ASTTypedExpression,
        context: &TypedValContext,
    ) -> Vec<(String, TypedValKind)> {
        match expr {
            //ASTTypedExpression::CharLiteral(_) => false,
            ASTTypedExpression::ASTFunctionCallExpression(call) => {
                let mut result = Vec::new();
                if let Some(v) = context.get(&call.function_name) {
                    result.append(&mut vec![(call.function_name.clone(), v.clone())]);
                }
                result.append(
                    &mut call
                        .parameters
                        .iter()
                        .flat_map(|it| {
                            self.expression_references_to_context(it, context)
                                .into_iter()
                        })
                        .collect(),
                );

                result
            }
            ASTTypedExpression::ValueRef(name, _, _) => {
                if let Some(v) = context.get(name) {
                    vec![(name.clone(), v.clone())]
                } else {
                    Vec::new()
                }
            }
            ASTTypedExpression::Value(_, _) => Vec::new(),
            ASTTypedExpression::Lambda(lambda_def) => lambda_def
                .body
                .iter()
                .flat_map(|it| {
                    self.statement_references_to_context(it, context)
                        .into_iter()
                })
                .collect(),
        }
    }

    fn body_reads_from_context(
        &self,
        body: &ASTTypedFunctionBody,
        context: &TypedValContext,
    ) -> bool {
        if let ASTTypedFunctionBody::RASMBody(statements) = body {
            statements
                .iter()
                .any(|it| self.statement_reads_from_context(it, context))
        } else {
            true
        }
    }

    fn statement_reads_from_context(
        &self,
        statement: &ASTTypedStatement,
        context: &TypedValContext,
    ) -> bool {
        match statement {
            ASTTypedStatement::Expression(expr) => {
                self.expression_reads_from_context(expr, context)
            }
            ASTTypedStatement::LetStatement(_, expr, _index) => {
                self.expression_reads_from_context(expr, context)
            }
            ASTTypedStatement::ConstStatement(_, _, _, _, _) => false,
        }
    }

    fn expression_reads_from_context(
        &self,
        expr: &ASTTypedExpression,
        context: &TypedValContext,
    ) -> bool {
        match expr {
            //ASTTypedExpression::CharLiteral(_) => false,
            ASTTypedExpression::ASTFunctionCallExpression(call) => {
                if context.get(&call.function_name).is_some() {
                    true
                } else {
                    call.parameters
                        .iter()
                        .any(|it| self.expression_reads_from_context(it, context))
                }
            }
            ASTTypedExpression::ValueRef(name, _, _) => context.get(name).is_some(),
            ASTTypedExpression::Value(_, _) => false,
            ASTTypedExpression::Lambda(lambda_def) => lambda_def
                .body
                .iter()
                .any(|it| self.statement_reads_from_context(it, context)),
        }
    }
}
