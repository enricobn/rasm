use crate::parser::ast::{ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTIndex, ASTType};
use crate::type_check::functions_container::{FunctionsContainer, TypeFilter};
use crate::type_check::type_check_error::TypeCheckError;

#[derive(Debug, Clone, PartialEq)]
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
        original_name: &str,
        function_def: &ASTFunctionDef,
    ) -> Option<ASTFunctionDef> {
        self.functions_by_name
            .try_add_new(original_name, function_def)
    }

    pub fn replace_body(
        &mut self,
        function_def: &ASTFunctionDef,
        body: ASTFunctionBody,
    ) -> ASTFunctionDef {
        self.functions_by_name.replace_body(function_def, body)
    }

    pub fn find_function(&self, name: &str) -> Option<&ASTFunctionDef> {
        self.functions_by_name.find_function(name)
    }

    pub fn find_function_by_original_name(&self, name: &str) -> Option<&ASTFunctionDef> {
        self.functions_by_name.find_function_by_original_name(name)
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
        parameter_types_filter: Vec<TypeFilter>,
        return_type_filter: Option<ASTType>,
        index: &ASTIndex,
    ) -> Result<Option<ASTFunctionDef>, TypeCheckError> {
        self.functions_by_name.find_call(
            function_name,
            original_function_name,
            parameter_types_filter,
            return_type_filter,
            true,
            index,
        )
    }

    pub fn find_call_vec(
        &self,
        call: &ASTFunctionCall,
        parameter_types_filter: Vec<TypeFilter>,
        return_type_filter: Option<ASTType>,
    ) -> Result<Vec<ASTFunctionDef>, TypeCheckError> {
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
    use crate::parser::ast::{ASTFunctionBody, ASTFunctionDef, ASTIndex, ASTType};
    use crate::type_check::resolved_generic_types::ResolvedGenericTypes;
    use crate::type_check::typed_context::TypeConversionContext;

    #[test]
    fn test() {
        let mut context = TypeConversionContext::new();

        context.try_add_new("aFun", &simple_function_def("aFun"));

        assert!(context
            .try_add_new("f", &simple_function_def("newFun"))
            .is_some());
        assert!(context
            .try_add_new("ff", &simple_function_def("anotherNewFun"))
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
            return_type: ASTType::Unit,
            inline: false,
            generic_types: Vec::new(),
            resolved_generic_types: ResolvedGenericTypes::new(),
            original_name: name.into(),
            index: ASTIndex::none(),
        }
    }
}
