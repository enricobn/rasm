use crate::parser::ast::{ASTFunctionCall, ASTFunctionDef, ASTType};
use crate::type_check::functions_container::FunctionsContainer;

#[derive(Debug, Clone)]
pub struct TypeConversionContext {
    functions_by_name: FunctionsContainer,
}

impl TypeConversionContext {
    pub fn new() -> Self {
        Self {
            functions_by_name: FunctionsContainer::new(),
        }
    }

    pub fn try_add_new(
        &mut self,
        original_name: &String,
        function_def: &ASTFunctionDef,
    ) -> Option<ASTFunctionDef> {
        self.functions_by_name
            .try_add_new(original_name, function_def)
    }

    pub fn replace_body(&mut self, function_def: &ASTFunctionDef) {
        self.functions_by_name.replace_body(function_def);
    }

    pub fn find_function(&self, name: &str) -> Option<&ASTFunctionDef> {
        self.functions_by_name.find_function(name)
    }

    pub fn len(&self) -> usize {
        self.functions_by_name.len()
    }

    pub fn is_empty(&self) -> bool {
        self.functions_by_name.is_empty()
    }

    pub fn functions(&self) -> Vec<&ASTFunctionDef> {
        self.functions_by_name.functions()
    }

    pub fn find_call(
        &self,
        call: &ASTFunctionCall,
        parameter_types_filter: Option<Vec<Option<ASTType>>>,
    ) -> Option<&ASTFunctionDef> {
        self.functions_by_name
            .find_call(call, parameter_types_filter)
    }

    pub fn functions_desc(&self) -> Vec<String> {
        self.functions_by_name.functions_desc()
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
