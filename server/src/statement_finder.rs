use std::fmt::Display;

use rasm_core::type_check::ast_modules_container::ASTModulesContainer;
use rasm_parser::{
    catalog::ASTIndex,
    parser::ast::{ASTExpression, ASTFunctionBody, ASTPosition, ASTStatement},
};

pub enum SSPResult {
    Before(ASTPosition),
    Exact(ASTPosition),
    After(ASTPosition),
}

enum SFExprResult {
    InExpr,
    NotInExpr,
    InStatement(ASTPosition),
}

impl Display for SSPResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SSPResult::Before(position) => write!(f, "Before({position})"),
            SSPResult::Exact(position) => write!(f, "Exact({position})"),
            SSPResult::After(position) => write!(f, "After({position})"),
        }
    }
}

pub struct StatementFinder {}

impl StatementFinder {
    /// find the statement start position of the expression that starts at index
    pub fn statement_start_position(
        &self,
        index: &ASTIndex,
        modules_container: &ASTModulesContainer,
    ) -> Option<ASTPosition> {
        if let Some(module) = modules_container.module(index.module_id()) {
            let mut functions_positions = module
                .functions
                .iter()
                .map(|it| it.position.clone())
                .collect::<Vec<_>>();
            functions_positions.sort();
            functions_positions.reverse();

            let mut function = None;
            for fp in functions_positions.iter() {
                if index.position().cmp(fp).is_gt() {
                    function = modules_container.function(&ASTIndex::new(
                        index.module_namespace().clone(),
                        index.module_id().clone(),
                        fp.clone(),
                    ));
                    break;
                }
            }

            if let Some(function) = function {
                if let ASTFunctionBody::RASMBody(ref body) = function.body {
                    if let Some(pos) = self.find_body(index.position(), body) {
                        return Some(pos);
                    }
                }
            }

            if let Some(pos) = self.find_body(index.position(), &module.body) {
                return Some(pos);
            }
        }
        None
    }

    fn find_body(&self, position: &ASTPosition, body: &Vec<ASTStatement>) -> Option<ASTPosition> {
        for statement in body.iter() {
            match statement {
                ASTStatement::Expression(expr) => {
                    let real_position = self.real_position(expr);

                    if &real_position == position {
                        return Some(position.clone());
                    } else {
                        match self.find_expr(position, expr) {
                            SFExprResult::InExpr => return Some(real_position),
                            SFExprResult::NotInExpr => {}
                            SFExprResult::InStatement(astposition) => return Some(astposition),
                        }
                    }
                }
                ASTStatement::LetStatement(_, expr, _, let_position) => {
                    let real_position = self.real_position(expr);
                    let stmt_position = let_position.mv_left(4);
                    if &real_position == position {
                        // because of "let "
                        return Some(stmt_position);
                    } else {
                        match self.find_expr(position, expr) {
                            SFExprResult::InExpr => return Some(stmt_position),
                            SFExprResult::NotInExpr => {}
                            SFExprResult::InStatement(astposition) => return Some(astposition),
                        }
                    }
                }
            }
        }
        None
    }

    fn find_expr(&self, position: &ASTPosition, expr: &ASTExpression) -> SFExprResult {
        if &expr.position() == position {
            return SFExprResult::InExpr;
        }
        match expr {
            ASTExpression::ASTFunctionCallExpression(call) => {
                if position == &call.position {
                    SFExprResult::InExpr
                } else {
                    for e in call.parameters.iter() {
                        match self.find_expr(position, e) {
                            SFExprResult::InExpr => return SFExprResult::InExpr,
                            SFExprResult::NotInExpr => {}
                            SFExprResult::InStatement(astposition) => {
                                return SFExprResult::InStatement(astposition)
                            }
                        }
                    }
                    SFExprResult::NotInExpr
                }
            }
            ASTExpression::ValueRef(_, ref_pos) => {
                if position == ref_pos {
                    SFExprResult::InExpr
                } else {
                    SFExprResult::NotInExpr
                }
            }
            ASTExpression::Value(_, val_pos) => {
                if position == val_pos {
                    SFExprResult::InExpr
                } else {
                    SFExprResult::NotInExpr
                }
            }
            ASTExpression::Lambda(lambda_def) => {
                if let Some(found) = self.find_body(position, &lambda_def.body) {
                    SFExprResult::InStatement(found)
                } else {
                    SFExprResult::NotInExpr
                }
            }
        }
    }

    fn real_position(&self, expr: &ASTExpression) -> ASTPosition {
        if let ASTExpression::ASTFunctionCallExpression(call) = expr {
            // due to dot notation, we calculate the "real" call position
            let mut positions = vec![call.position.clone()];
            positions.append(
                &mut call
                    .parameters
                    .iter()
                    .map(|it| it.position())
                    .collect::<Vec<_>>(),
            );

            positions.iter().min().unwrap().clone()
        } else {
            expr.position()
        }
    }
}
