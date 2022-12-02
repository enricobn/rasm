use crate::parser::ast::ASTFunctionDef;
use crate::type_check::functions_container::FunctionsContainer;
use linked_hash_map::LinkedHashMap;
use log::debug;

#[derive(Debug, Clone)]
pub struct TypeConversionContext {
    new_function_defs: FunctionsContainer,
}

impl TypeConversionContext {
    pub fn new() -> Self {
        Self {
            new_function_defs: FunctionsContainer::new(),
        }
    }

    pub fn try_add_new(
        &mut self,
        original_name: &String,
        function_def: &ASTFunctionDef,
    ) -> Option<ASTFunctionDef> {
        self.new_function_defs
            .try_add_new(original_name, function_def)
    }

    pub fn replace_body(&mut self, function_def: &ASTFunctionDef) {
        self.new_function_defs.replace_body(function_def);
    }

    pub fn find_function(&self, name: &str) -> Option<&ASTFunctionDef> {
        self.new_function_defs.find_function(name)
    }

    pub fn len(&self) -> usize {
        self.new_function_defs.len()
    }

    pub fn is_empty(&self) -> bool {
        self.new_function_defs.is_empty()
    }

    pub fn functions(&self) -> Vec<&ASTFunctionDef> {
        self.new_function_defs.functions()
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::{ASTFunctionBody, ASTFunctionDef};
    use crate::type_check::typed_context::TypeConversionContext;
    use linked_hash_map::LinkedHashMap;

    #[test]
    fn test() {
        let mut context = TypeConversionContext::new();

        context.try_add_new(&"aFun".to_string(), &simple_function_def("aFun"));

        assert!(context
            .try_add_new(&"f".into(), &simple_function_def("newFun"))
            .is_none());
        assert!(context
            .try_add_new(&"ff".into(), &simple_function_def("anotherNewFun"))
            .is_none());

        assert!(context.find_function("aFun").is_some());
        assert!(context.find_function("newFun").is_some());
        assert!(context.find_function("anotherNewFun").is_some());

        assert_eq!(
            context
                .functions()
                .iter()
                .map(|it| it.name.clone())
                .collect::<Vec<String>>(),
            vec!["aFun", "newFun", "anotherNewFun"]
        );

        assert_eq!(context.len(), 3);
    }

    fn simple_function_def(name: &str) -> ASTFunctionDef {
        ASTFunctionDef {
            name: name.into(),
            body: ASTFunctionBody::ASMBody("".into()),
            parameters: Vec::new(),
            return_type: None,
            inline: false,
            param_types: Vec::new(),
            resolved_generic_types: LinkedHashMap::new(),
        }
    }
}
