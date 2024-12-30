use std::{collections::HashMap, fmt::Display};

use rasm_parser::parser::ast::{ASTExpression, ASTPosition, ASTStatement};

pub enum ASTElement<'a> {
    Statement(&'a ASTStatement),
    Expression(&'a ASTExpression),
}

impl<'a> ASTElement<'a> {
    pub fn position(&self) -> ASTPosition {
        match self {
            ASTElement::Statement(statement) => statement.position(),
            ASTElement::Expression(expression) => expression.position(),
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
            match element {
                ASTElement::Statement(statement) => writeln!(f, "stmt {statement}")?,
                ASTElement::Expression(expression) => writeln!(f, "expr {expression}")?,
            }
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
            .filter(|(pos, element)| match element {
                ASTElement::Statement(_) => true,
                ASTElement::Expression(_) => false,
            })
            .min_by_key(|(pos, _)| pos.clone())
            .map(|(pos, _)| pos)
    }

    pub fn last_statement(&self) -> Option<&ASTPosition> {
        self.positions
            .iter()
            .filter(|(pos, element)| match element {
                ASTElement::Statement(_) => true,
                ASTElement::Expression(_) => false,
            })
            .max_by_key(|(pos, _)| pos.clone())
            .map(|(pos, _)| pos)
    }

    pub fn get_element(&self, position: &ASTPosition) -> Option<&ASTElement> {
        self.positions.get(position)
    }

    fn add_statement(&mut self, statement: &'a ASTStatement) {
        match statement {
            ASTStatement::Expression(expr) => self.add_expr(expr),
            ASTStatement::LetStatement(_, expr, _, _) => {
                self.internal_add(ASTElement::Expression(expr), Some(statement.position()));
                self.add_expr(expr);
            }
        }
    }

    fn add_expr(&mut self, expr: &'a ASTExpression) {
        match expr {
            ASTExpression::ASTFunctionCallExpression(function_call) => {
                for e in function_call.parameters.iter() {
                    self.internal_add(ASTElement::Expression(e), Some(expr.position()));
                    self.add_expr(e);
                }
            }
            ASTExpression::ValueRef(_, _) => {}
            ASTExpression::Value(_, _) => {}
            ASTExpression::Lambda(lambda_def) => {
                for statement in lambda_def.body.iter() {
                    self.internal_add(ASTElement::Statement(statement), Some(expr.position()));
                    self.add_statement(statement);
                }
            }
        }
    }

    fn internal_add(&mut self, element: ASTElement<'a>, parent: Option<ASTPosition>) {
        if let Some(p) = parent {
            self.parents.insert(element.position(), p);
        }
        self.positions.insert(element.position(), element);
    }
}
