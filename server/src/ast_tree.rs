use std::{collections::HashMap, fmt::Display};

use rasm_parser::parser::ast::{ASTExpression, ASTFunctionCall, ASTPosition, ASTStatement};

pub enum ASTElement<'a> {
    Statement(&'a ASTStatement),
    Expression(&'a ASTExpression),
}

impl<'a> ASTElement<'a> {
    pub fn position(&self) -> &ASTPosition {
        match self {
            ASTElement::Statement(statement) => statement.position(),
            ASTElement::Expression(expression) => expression.position(),
        }
    }
}

impl<'a> Display for ASTElement<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTElement::Statement(statement) => writeln!(f, "stmt {statement}"),
            ASTElement::Expression(expression) => writeln!(f, "expr {expression}"),
        }
    }
}

/// A structure to get some infomations, such as the "parent", the root statement,
/// which is the last or the first statement, about the expressions and statements in a block,
/// using the position.
pub struct ASTTree<'a> {
    positions: HashMap<ASTPosition, ASTElement<'a>>,
    parents: HashMap<ASTPosition, ASTPosition>,
}

impl<'a> Display for ASTTree<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for element in self.positions.values() {
            writeln!(f, "{element}")?
        }
        Ok(())
    }
}

impl<'a> ASTTree<'a> {
    pub fn new(statements: &'a Vec<ASTStatement>) -> Self {
        let mut result = Self {
            positions: HashMap::new(),
            parents: HashMap::new(),
        };

        for statement in statements {
            result.internal_add(ASTElement::Statement(statement), None);
            result.add_statement(statement);
        }

        result
    }

    pub fn root(&self, position: &ASTPosition) -> Option<&ASTPosition> {
        if let Some(p) = self.parents.get(position) {
            self.root(p)
        } else {
            self.parents.values().find(|it| it == &position)
        }
    }

    pub fn first_statement(&self) -> Option<&ASTPosition> {
        self.positions
            .iter()
            .filter(|(_pos, element)| match element {
                ASTElement::Statement(_) => true,
                ASTElement::Expression(_) => false,
            })
            .min_by_key(|(pos, _)| *pos)
            .map(|(pos, _)| pos)
    }

    pub fn last_statement(&self) -> Option<&ASTPosition> {
        self.positions
            .iter()
            .filter(|(_pos, element)| match element {
                ASTElement::Statement(_) => true,
                ASTElement::Expression(_) => false,
            })
            .max_by_key(|(pos, _)| *pos)
            .map(|(pos, _)| pos)
    }

    pub fn get_element(&self, position: &ASTPosition) -> Option<&ASTElement> {
        self.positions.get(position)
    }

    pub fn previous_element(&self, position: &ASTPosition) -> Option<&ASTElement> {
        let mut column = position.column;
        while column > 0 {
            if let Some(e) = self.get_element(&ASTPosition::new(position.row, column)) {
                return Some(e);
            }
            column -= 1;
        }
        None
    }

    pub fn enclosing_call(&self, element: &ASTElement) -> Option<&ASTFunctionCall> {
        if let ASTElement::Expression(e) = element {
            if let Some(parent) = self.parents.get(&e.position()) {
                if let Some(p_e) = self.get_element(parent) {
                    let expr = match p_e {
                        ASTElement::Statement(stmt) => match stmt {
                            ASTStatement::ASTExpressionStatement(astexpression) => astexpression,
                            _ => {
                                return None;
                            }
                        },
                        ASTElement::Expression(astexpression) => astexpression,
                    };

                    if let ASTExpression::ASTFunctionCallExpression(call) = expr {
                        return Some(call);
                    }
                }
            }
        }
        None
    }

    pub fn children(&self, position: &ASTPosition) -> Vec<&ASTPosition> {
        self.parents
            .iter()
            .filter(|(_, parent)| parent == &position)
            .map(|(child, _)| child)
            .collect()
    }

    pub fn parent(&self, position: &ASTPosition) -> Option<&ASTPosition> {
        self.parents.get(position)
    }

    fn add_statement(&mut self, statement: &'a ASTStatement) {
        match statement {
            ASTStatement::ASTExpressionStatement(expr) => self.add_expr(expr),
            ASTStatement::ASTLetStatement(_, expr, _) => {
                self.internal_add(
                    ASTElement::Expression(expr),
                    Some(statement.position().clone()),
                );
                self.add_expr(expr);
            }
            ASTStatement::ASTConstStatement(_, expr, _, _) => {
                self.internal_add(
                    ASTElement::Expression(expr),
                    Some(statement.position().clone()),
                );
                self.add_expr(expr);
            }
        }
    }

    fn add_expr(&mut self, expr: &'a ASTExpression) {
        match expr {
            ASTExpression::ASTFunctionCallExpression(function_call) => {
                for e in function_call.parameters().iter() {
                    self.internal_add(ASTElement::Expression(e), Some(expr.position().clone()));
                    self.add_expr(e);
                }
            }
            ASTExpression::ASTValueRefExpression(_, _) => {}
            ASTExpression::ASTValueExpression(_, _) => {}
            ASTExpression::ASTLambdaExpression(lambda_def) => {
                for statement in lambda_def.body.iter() {
                    self.internal_add(
                        ASTElement::Statement(statement),
                        Some(expr.position().clone()),
                    );
                    self.add_statement(statement);
                }
            }
        }
    }

    fn internal_add(&mut self, element: ASTElement<'a>, parent: Option<ASTPosition>) {
        if let Some(p) = parent {
            self.parents.insert(element.position().clone(), p);
        }
        self.positions.insert(element.position().clone(), element);
    }
}
