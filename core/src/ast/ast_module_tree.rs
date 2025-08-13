use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
};

use rasm_parser::parser::ast::{
    ASTExpression, ASTFunctionBody, ASTFunctionDef, ASTModule, ASTPosition, ASTStatement,
};

pub enum ASTModuleTreeLocation {
    Body,
    Function(usize),
}

#[derive(Clone, Debug)]
pub enum ASTElement {
    Statement(ASTStatement),
    Expression(ASTExpression),
    LambdaParam(String, ASTPosition),
    FunctionDef(ASTFunctionDef),
}

impl Display for ASTElement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTElement::Statement(statement) => write!(f, "{}", statement),
            ASTElement::Expression(expression) => write!(f, "{}", expression),
            ASTElement::LambdaParam(name, _) => write!(f, "{}", name),
            ASTElement::FunctionDef(function_def) => write!(f, "{}", function_def),
        }
    }
}

impl ASTElement {
    pub fn position(&self) -> &ASTPosition {
        match self {
            ASTElement::Statement(statement) => statement.position(),
            ASTElement::Expression(expression) => expression.position(),
            ASTElement::LambdaParam(_, astposition) => astposition,
            ASTElement::FunctionDef(function_def) => &function_def.position,
        }
    }
}

#[derive(Clone)]
pub struct ASTModuleTreeItem {
    pub element: ASTElement,
    pub parent: Option<usize>,
}

#[derive(Clone)]
pub struct ASTModuleTree {
    items: HashMap<usize, ASTModuleTreeItem>,
    sorted_functions_positions: Vec<ASTPosition>,
}

impl ASTModuleTree {
    pub fn new(module: &ASTModule) -> Self {
        let mut items = HashMap::new();
        Self::build_tree(module, &mut items);
        let mut functions_positions = module
            .functions
            .iter()
            .filter(|it| it.position.builtin.is_none())
            .map(|it| it.position.clone())
            .collect::<Vec<_>>();
        functions_positions.sort();

        Self {
            items,
            sorted_functions_positions: functions_positions,
        }
    }

    pub fn get_element_at(&self, position: &ASTPosition) -> Option<&ASTModuleTreeItem> {
        self.items.get(&position.id)
    }

    pub fn get(&self, id: usize) -> Option<&ASTModuleTreeItem> {
        self.items.get(&id)
    }

    // TODO optimize
    pub fn get_elements_at(&self, row: usize, column: usize) -> Vec<&ASTModuleTreeItem> {
        let mut result = Vec::new();
        for item in self.items.values() {
            if item.element.position().row == row && item.element.position().column == column {
                result.push(item);
            }
        }
        result
    }

    pub fn get_statement_position(
        &self,
        row: usize,
        column: usize,
    ) -> Option<ASTModuleTreeLocation> {
        let position = ASTPosition::new(row, column);

        if let Some(first_function_pos) = self.sorted_functions_positions.first() {
            if position.cmp(&first_function_pos).is_le() {
                return Some(ASTModuleTreeLocation::Body);
            }
        } else {
            return Some(ASTModuleTreeLocation::Body);
        }

        let mut functions_positions = self.sorted_functions_positions.clone();
        functions_positions.reverse();

        let mut function = None;
        for fp in functions_positions.iter() {
            if position.cmp(&fp).is_gt() {
                if let Some(ASTElement::FunctionDef(found_function)) =
                    self.items.get(&fp.id).map(|it| &it.element)
                {
                    function = Some(found_function);
                }
                break;
            }
        }

        if let Some(function) = function {
            if let ASTFunctionBody::RASMBody(_) = function.body {
                return Some(ASTModuleTreeLocation::Function(function.position.id));
            }
        }

        None
    }

    pub fn get_position_root(&self, position: &ASTPosition) -> Option<&ASTModuleTreeItem> {
        if let Some(e) = self.get_element_at(position) {
            self.get_root(e)
        } else {
            None
        }
    }

    pub fn get_root<'b>(&'b self, item: &'b ASTModuleTreeItem) -> Option<&'b ASTModuleTreeItem> {
        if let Some(p_id) = item.parent {
            if let Some(p) = self.items.get(&p_id) {
                self.get_root(p)
            } else {
                None
            }
        } else {
            Some(item)
        }
    }

    pub fn get_function(&self, id: usize) -> Option<&ASTFunctionDef> {
        self.get(id).and_then(|it| {
            if let ASTElement::FunctionDef(f) = &it.element {
                Some(f)
            } else {
                None
            }
        })
    }

    fn build_tree(module: &ASTModule, elements: &mut HashMap<usize, ASTModuleTreeItem>) {
        Self::add_body(&module.body, elements, None);

        for function in module.functions.iter() {
            let element = ASTModuleTreeItem {
                element: ASTElement::FunctionDef(function.clone()),
                parent: None,
            };
            elements.insert(function.position.id, element);
            if let ASTFunctionBody::RASMBody(ref body) = function.body {
                Self::add_body(body, elements, None);
            }
        }
    }

    fn add_body(
        body: &Vec<ASTStatement>,
        elements: &mut HashMap<usize, ASTModuleTreeItem>,
        parent: Option<usize>,
    ) {
        for statement in body.iter() {
            Self::add_statement(statement, elements, parent);
        }
    }

    fn add_statement(
        statement: &ASTStatement,
        elements: &mut HashMap<usize, ASTModuleTreeItem>,
        parent: Option<usize>,
    ) {
        let position = statement.position();
        let element = ASTModuleTreeItem {
            element: ASTElement::Statement(statement.clone()),
            parent: parent,
        };

        match statement {
            ASTStatement::ASTExpressionStatement(astexpression, _) => {
                Self::add_expression(astexpression, elements, Some(position.id));
            }
            ASTStatement::ASTLetStatement(_, astexpression, _) => {
                Self::add_expression(astexpression, elements, Some(position.id));
            }
            ASTStatement::ASTConstStatement(_, astexpression, _, _) => {
                Self::add_expression(astexpression, elements, Some(position.id));
            }
        }

        elements.insert(position.id, element);
    }

    fn add_expression(
        expression: &ASTExpression,
        elements: &mut HashMap<usize, ASTModuleTreeItem>,
        parent: Option<usize>,
    ) {
        let position = expression.position();
        let element = ASTModuleTreeItem {
            element: ASTElement::Expression(expression.clone()),
            parent: parent,
        };

        match expression {
            ASTExpression::ASTFunctionCallExpression(ref function_call) => {
                for argument in function_call.parameters().iter() {
                    Self::add_expression(argument, elements, Some(position.id));
                }
            }
            ASTExpression::ASTLambdaExpression(astlambda_def) => {
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
