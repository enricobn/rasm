use rasm_core::type_check::ast_modules_container::ASTModulesContainer;
use rasm_parser::{
    catalog::ASTIndex,
    parser::ast::{
        ASTExpression, ASTFunctionBody, ASTFunctionDef, ASTModule, ASTPosition, ASTStatement,
    },
};

enum SFExprResult {
    InExpr,
    NotInExpr,
    InStatement(ASTPosition),
}

pub enum ModulePosition<'a> {
    Body(&'a ASTModule),
    Function(&'a ASTModule, &'a ASTFunctionDef),
}

impl<'a> ModulePosition<'a> {
    pub fn body(&self) -> &'a Vec<ASTStatement> {
        match self {
            ModulePosition::Body(module) => &module.body,
            ModulePosition::Function(_, ref function_def) => match &function_def.body {
                ASTFunctionBody::RASMBody(vec) => vec,
                ASTFunctionBody::NativeBody(_) => panic!(), // it should not happen
            },
        }
    }
}

pub struct StatementFinder {}

impl StatementFinder {
    /// find the statement start position of the expression that starts at index
    pub fn statement_start_position(
        index: &ASTIndex,
        modules_container: &ASTModulesContainer,
    ) -> Option<ASTPosition> {
        if let Some(module_position) = Self::module_position(index, modules_container) {
            Self::find_body(index.position(), &module_position.body())
        } else {
            None
        }
    }

    pub fn module_position<'a>(
        index: &ASTIndex,
        modules_container: &'a ASTModulesContainer,
    ) -> Option<ModulePosition<'a>> {
        if let Some(module) = modules_container.module(index.module_id()) {
            let mut functions_positions = module
                .functions
                .iter()
                .filter(|it| it.position.builtin.is_none())
                .map(|it| it.position.clone())
                .collect::<Vec<_>>();
            functions_positions.sort();

            if let Some(first_function_pos) = functions_positions.first() {
                if index.position().cmp(&first_function_pos).is_le() {
                    return Some(ModulePosition::Body(module));
                }
            } else {
                return Some(ModulePosition::Body(module));
            }

            functions_positions.reverse();

            let mut function = None;
            for fp in functions_positions.iter() {
                if index.position().cmp(&fp).is_gt() {
                    function = modules_container.function(&ASTIndex::new(
                        index.module_namespace().clone(),
                        index.module_id().clone(),
                        fp.clone(),
                    ));
                    break;
                }
            }

            if let Some(function) = function {
                if let ASTFunctionBody::RASMBody(_) = function.body {
                    return Some(ModulePosition::Function(module, &function));
                }
            }
        }
        None
    }

    fn find_body(position: &ASTPosition, body: &Vec<ASTStatement>) -> Option<ASTPosition> {
        for statement in body.iter() {
            match statement {
                ASTStatement::Expression(expr) => {
                    let real_position = Self::real_position(expr);

                    if &real_position == position {
                        return Some(position.clone());
                    } else {
                        match Self::find_expr(position, expr) {
                            SFExprResult::InExpr => return Some(real_position),
                            SFExprResult::NotInExpr => {}
                            SFExprResult::InStatement(astposition) => return Some(astposition),
                        }
                    }
                }
                ASTStatement::LetStatement(_, expr, _, let_position) => {
                    let real_position = Self::real_position(expr);
                    let stmt_position = let_position.mv_left(4);
                    if &real_position == position {
                        // because of "let "
                        return Some(stmt_position);
                    } else {
                        match Self::find_expr(position, expr) {
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

    pub fn find_expr(position: &ASTPosition, expr: &ASTExpression) -> SFExprResult {
        if &expr.position() == position {
            return SFExprResult::InExpr;
        }
        match expr {
            ASTExpression::ASTFunctionCallExpression(call) => {
                if position == &call.position {
                    SFExprResult::InExpr
                } else {
                    for e in call.parameters.iter() {
                        match Self::find_expr(position, e) {
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
                if let Some(found) = Self::find_body(position, &lambda_def.body) {
                    SFExprResult::InStatement(found)
                } else {
                    SFExprResult::NotInExpr
                }
            }
        }
    }

    fn real_position(expr: &ASTExpression) -> ASTPosition {
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
