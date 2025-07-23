use std::collections::HashMap;

use rasm_parser::parser::ast::{
    ASTExpression, ASTFunctionBody, ASTModule, ASTPosition, ASTStatement,
};

#[derive(Clone)]
pub enum ASTElement<'a> {
    Statement(&'a ASTStatement),
    Expression(&'a ASTExpression),
    LambdaParam(String, ASTPosition),
}

impl ASTElement<'_> {
    pub fn position(&self) -> &ASTPosition {
        match self {
            ASTElement::Statement(statement) => statement.position(),
            ASTElement::Expression(expression) => expression.position(),
            ASTElement::LambdaParam(_, astposition) => astposition,
        }
    }
}

#[derive(Clone)]
pub struct ASTModuleTreeItem<'a> {
    pub element: ASTElement<'a>,
    pub parent: Option<usize>,
}

pub struct ASTModuleTree<'a> {
    pub module: &'a ASTModule,
    pub items: HashMap<usize, ASTModuleTreeItem<'a>>,
}

impl<'a> ASTModuleTree<'a> {
    pub fn new(module: &'a ASTModule) -> Self {
        let mut items = HashMap::new();
        Self::build_tree(module, &mut items);
        Self { module, items }
    }

    pub fn get_element_at(&self, position: &ASTPosition) -> Option<&ASTModuleTreeItem<'a>> {
        self.items.get(&position.id)
    }

    pub fn get_elements_at(&self, row: usize, column: usize) -> Vec<&ASTModuleTreeItem<'a>> {
        let mut result = Vec::new();
        for item in self.items.values() {
            if item.element.position().row == row && item.element.position().column == column {
                result.push(item);
            }
        }
        result
    }

    fn build_tree(module: &'a ASTModule, elements: &mut HashMap<usize, ASTModuleTreeItem<'a>>) {
        Self::add_body(&module.body, elements, None);

        for function in module.functions.iter() {
            if let ASTFunctionBody::RASMBody(ref body) = function.body {
                Self::add_body(body, elements, None);
            }
        }
    }

    fn add_body(
        body: &'a Vec<ASTStatement>,
        elements: &mut HashMap<usize, ASTModuleTreeItem<'a>>,
        parent: Option<usize>,
    ) {
        for statement in body.iter() {
            Self::add_statement(statement, elements, parent);
        }
    }

    fn add_statement(
        statement: &'a ASTStatement,
        elements: &mut HashMap<usize, ASTModuleTreeItem<'a>>,
        parent: Option<usize>,
    ) {
        let position = statement.position();
        let element = ASTModuleTreeItem {
            element: ASTElement::Statement(statement),
            parent: parent,
        };

        match statement {
            ASTStatement::Expression(astexpression) => {
                Self::add_expression(astexpression, elements, Some(position.id));
            }
            ASTStatement::LetStatement(_, astexpression, _astposition) => {
                Self::add_expression(astexpression, elements, Some(position.id));
            }
            ASTStatement::ConstStatement(_, astexpression, _astposition, _astmodifiers) => {
                Self::add_expression(astexpression, elements, Some(position.id));
            }
        }

        elements.insert(position.id, element);
    }

    fn add_expression(
        expression: &'a ASTExpression,
        elements: &mut HashMap<usize, ASTModuleTreeItem<'a>>,
        parent: Option<usize>,
    ) {
        let position = expression.position();
        let element = ASTModuleTreeItem {
            element: ASTElement::Expression(expression),
            parent: parent,
        };

        match expression {
            ASTExpression::ASTFunctionCallExpression(ref function_call) => {
                for argument in function_call.parameters().iter() {
                    Self::add_expression(argument, elements, Some(position.id));
                }
            }
            ASTExpression::Lambda(astlambda_def) => {
                for (argument_name, argument_position) in astlambda_def.parameter_names.iter() {
                    let argument_element = ASTModuleTreeItem {
                        element: ASTElement::LambdaParam(
                            argument_name.to_owned(),
                            argument_position.clone(),
                        ),
                        parent: Some(position.id),
                    };
                    elements.insert(argument_position.id, argument_element);
                }
                for statement in astlambda_def.body.iter() {
                    Self::add_statement(statement, elements, Some(position.id));
                }
            }
            _ => {}
        }

        elements.insert(position.id, element);
    }
}
