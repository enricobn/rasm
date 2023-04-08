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
        function_name: &str,
        original_function_name: &str,
        parameter_types_filter: Option<Vec<Option<ASTType>>>,
        return_type_filter: Option<Option<ASTType>>,
    ) -> Option<&ASTFunctionDef> {
        self.functions_by_name.find_call(
            function_name,
            original_function_name,
            parameter_types_filter,
            return_type_filter,
            true,
        )
    }

    pub fn find_call_vec(
        &self,
        call: &ASTFunctionCall,
        parameter_types_filter: Option<Vec<Option<ASTType>>>,
        return_type_filter: Option<Option<ASTType>>,
    ) -> Vec<ASTFunctionDef> {
        self.functions_by_name
            .find_call_vec(call, parameter_types_filter, return_type_filter, true)
    }

    pub fn functions_desc(&self) -> Vec<String> {
        self.functions_by_name.functions_desc()
    }

    pub fn debug_i(&self) {
        self.functions_by_name.debug_i("context");
    }
}

#[cfg(test)]
mod tests {
    use linked_hash_map::LinkedHashMap;

    use crate::parser::ast::{ASTFunctionBody, ASTFunctionDef};
    use crate::type_check::typed_context::TypeConversionContext;

    #[test]
    fn test() {
        let mut context = TypeConversionContext::new();

        context.try_add_new(&"aFun".to_string(), &simple_function_def("aFun"));

        assert!(context
            .try_add_new(&"f".into(), &simple_function_def("newFun"))
            .is_some());
        assert!(context
            .try_add_new(&"ff".into(), &simple_function_def("anotherNewFun"))
            .is_some());

        assert!(context.find_function("aFun_0").is_some());
        assert!(context.find_function("newFun_0").is_some());
        assert!(context.find_function("anotherNewFun_0").is_some());

        assert_eq!(
            context
                .functions()
                .iter()
                .map(|it| it.name.clone())
                .collect::<Vec<String>>(),
            vec!["aFun_0", "newFun_0", "anotherNewFun_0"]
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
            generic_types: Vec::new(),
            resolved_generic_types: LinkedHashMap::new(),
            original_name: name.into(),
        }
    }
}
