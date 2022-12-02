use crate::parser::ast::ASTFunctionDef;
use linked_hash_map::LinkedHashMap;
use log::debug;

#[derive(Debug, Clone)]
pub struct TypeConversionContext {
    new_function_defs: LinkedHashMap<String, Vec<ASTFunctionDef>>,
}

impl TypeConversionContext {
    pub fn new() -> Self {
        Self {
            new_function_defs: LinkedHashMap::new(),
        }
    }

    pub fn try_add_new(
        &mut self,
        original_name: &String,
        function_def: &ASTFunctionDef,
    ) -> Option<ASTFunctionDef> {
        debug!("trying to add new function {function_def}");

        if let Some(same_name_functions) = self.new_function_defs.get_mut(original_name) {
            if let Some(already_present) = same_name_functions.iter().find(|it| {
                it.parameters == function_def.parameters
                    && it.return_type == function_def.return_type
            }) {
                debug!("already added as {already_present}");
                Some(already_present.clone())
            } else {
                same_name_functions.push(function_def.clone());
                None
            }
        } else {
            let same_name_functions = vec![function_def.clone()];
            self.new_function_defs
                .insert(original_name.clone(), same_name_functions);
            None
        }
    }

    pub fn replace_body(&mut self, function_def: &ASTFunctionDef) {
        for (_, f_defs) in self.new_function_defs.iter_mut() {
            for mut f_def in f_defs.iter_mut() {
                if f_def.name == function_def.name {
                    f_def.body = function_def.body.clone();
                    return;
                }
            }
        }
        panic!("cannot find function {}", function_def.name)
    }

    pub fn iter(&'_ self) -> impl Iterator<Item = ASTFunctionDef> + '_ {
        TypeConversionContextIterator::new(self)
    }

    pub fn get(&self, name: &str) -> Option<&ASTFunctionDef> {
        let fake_name = name.replace("::", "_");

        self.new_function_defs
            .values()
            .flat_map(|it| it.iter())
            .find(|it| name == it.name || fake_name == it.name)
    }

    pub fn len(&self) -> usize {
        self.new_function_defs.iter().map(|it| it.1.len()).sum()
    }

    pub fn is_empty(&self) -> bool {
        self.new_function_defs.is_empty()
    }
}

struct TypeConversionContextIterator<'a> {
    index: usize,
    context: &'a TypeConversionContext,
}

impl<'a> TypeConversionContextIterator<'a> {
    fn new(context: &'a TypeConversionContext) -> Self {
        Self { index: 0, context }
    }
}

impl<'a> Iterator for TypeConversionContextIterator<'a> {
    type Item = ASTFunctionDef;

    fn next(&mut self) -> Option<Self::Item> {
        let result = self
            .context
            .new_function_defs
            .values()
            .flat_map(|it| it.iter())
            .nth(self.index)
            .cloned();
        self.index += 1;

        result
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

        assert!(context.get("aFun").is_some());
        assert!(context.get("newFun").is_some());
        assert!(context.get("anotherNewFun").is_some());

        assert_eq!(
            context.iter().map(|it| it.name).collect::<Vec<String>>(),
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
