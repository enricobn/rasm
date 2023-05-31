use crate::codegen::TypedValContext;
use crate::type_check::typed_ast::ASTTypedFunctionDef;

#[derive(Debug, Clone)]
pub struct LambdaCall {
    pub def: ASTTypedFunctionDef,
    pub space: LambdaSpace,
}

#[derive(Debug, Clone)]
pub struct LambdaSpace {
    parameters: Vec<String>,
    context: TypedValContext,
}

impl LambdaSpace {
    pub fn new(context: TypedValContext) -> Self {
        LambdaSpace {
            parameters: Vec::new(),
            context,
        }
    }

    pub fn add_context_parameter(&mut self, name: String) {
        self.parameters.push(name);
    }

    pub fn get_index(&self, name: &str) -> Option<usize> {
        return self
            .parameters
            .iter()
            .position(|it| it == name)
            .map(|it| it + 1);
    }

    pub fn is_in_context(&self, name: &str) -> bool {
        self.context.get(name).is_some()
    }

    pub fn get_context(&self) -> &TypedValContext {
        &self.context
    }
}
