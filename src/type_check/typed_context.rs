use crate::parser::ast::ASTFunctionDef;
use linked_hash_map::LinkedHashMap;
use log::debug;

#[derive(Debug, Clone)]
pub struct TypeConversionContext {
    new_function_defs: LinkedHashMap<String, Vec<ASTFunctionDef>>,
    used_untyped_function_defs: LinkedHashMap<String, ASTFunctionDef>,
}

impl TypeConversionContext {
    pub fn new() -> Self {
        Self {
            new_function_defs: LinkedHashMap::new(),
            used_untyped_function_defs: LinkedHashMap::new(),
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
        if self
            .used_untyped_function_defs
            .contains_key(&function_def.name.clone())
        {
            self.used_untyped_function_defs
                .insert(function_def.name.clone(), function_def.clone());
        } else {
            for (_, f_defs) in self.new_function_defs.iter_mut() {
                for mut f_def in f_defs.iter_mut() {
                    if f_def.name == function_def.name {
                        f_def.body = function_def.body.clone();
                        return;
                    }
                }
            }
            panic!("Cannot find function {}", function_def.name);
        }
    }

    pub fn add_untyped(&mut self, function_def: &ASTFunctionDef) -> bool {
        if self
            .used_untyped_function_defs
            .contains_key(&function_def.name)
        {
            debug!("already existent {}", function_def.name);
            return false;
        }
        self.used_untyped_function_defs
            .insert(function_def.name.clone(), function_def.clone());
        true
    }

    pub fn iter(&'_ self) -> impl Iterator<Item = ASTFunctionDef> + '_ {
        TypeConversionContextIterator::new(self)
    }

    pub fn get(&self, name: &str) -> Option<&ASTFunctionDef> {
        if let Some(function_def) = self.used_untyped_function_defs.get(name) {
            Some(function_def)
        } else {
            let fake_name = name.replace("::", "_");

            if let Some(function_def) = self.used_untyped_function_defs.get(&fake_name) {
                Some(function_def)
            } else {
                self.new_function_defs
                    .values()
                    .flat_map(|it| it.iter())
                    .find(|it| name == it.name)
            }
        }
    }

    pub fn len(&self) -> usize {
        let s: usize = self.new_function_defs.iter().map(|it| it.1.len()).sum();
        self.used_untyped_function_defs.len() + s
    }

    pub fn is_empty(&self) -> bool {
        self.used_untyped_function_defs.is_empty() && self.new_function_defs.is_empty()
    }
}

struct TypeConversionContextIterator<'a> {
    index: usize,
    used: bool,
    context: &'a TypeConversionContext,
}

impl<'a> TypeConversionContextIterator<'a> {
    fn new(context: &'a TypeConversionContext) -> Self {
        Self {
            index: 0,
            used: true,
            context,
        }
    }
}

impl<'a> Iterator for TypeConversionContextIterator<'a> {
    type Item = ASTFunctionDef;

    fn next(&mut self) -> Option<Self::Item> {
        if self.used {
            if let Some((_, found)) = self
                .context
                .used_untyped_function_defs
                .iter()
                .nth(self.index)
            {
                self.index += 1;
                Some(found.clone())
            } else {
                self.used = false;
                self.index = 0;
                self.next()
            }
        } else {
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
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::{ASTFunctionBody, ASTFunctionDef};
    use crate::type_check::typed_context::TypeConversionContext;

    #[test]
    fn test() {
        let mut context = TypeConversionContext::new();

        context.add_untyped(&simple_function_def("aFun"));

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
        }
    }
}
