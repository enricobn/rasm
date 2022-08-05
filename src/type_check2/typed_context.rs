use linked_hash_map::LinkedHashMap;
use log::debug;
use crate::parser::ast::ASTFunctionDef;

#[derive(Debug, Clone)]
pub struct TypeConversionContext {
    new_function_defs: LinkedHashMap<String, ASTFunctionDef>,
    used_untyped_function_defs: LinkedHashMap<String, ASTFunctionDef>,
}

impl TypeConversionContext {
    pub fn new() -> Self {
        Self {
            new_function_defs: LinkedHashMap::new(),
            used_untyped_function_defs: LinkedHashMap::new(),
        }
    }

    pub fn add_new(&mut self, function_def: &ASTFunctionDef) {
        if let Some((_, found)) = self.new_function_defs.iter().find(|(_, it) | it.parameters == function_def.parameters) {
            debug!("similar already existent {}", found.name);
        }
        if let Some(fd) = self.new_function_defs.insert(function_def.name.clone(), function_def.clone()) {
            debug!("already existent {}", fd.name);
        }
    }

    pub fn add_untyped(&mut self, function_def: &ASTFunctionDef) {
        if let Some(fd) = self.used_untyped_function_defs.insert(function_def.name.clone(), function_def.clone()) {
            debug!("already existent {}", fd.name);
        }
    }

    pub fn iter(&'_ self) -> impl Iterator<Item=ASTFunctionDef> + '_ {
        TypeConversionContextIterator::new(self)
    }

    pub fn get(&self, name: &str) -> Option<ASTFunctionDef> {
        if let Some(function_def) = self.used_untyped_function_defs.get(name) {
            Some(function_def.clone())
        } else {
            self.new_function_defs.get(name).cloned()
        }
    }

    pub fn len(&self) -> usize {
        self.used_untyped_function_defs.len() + self.new_function_defs.len()
    }

    pub fn is_empty(&self) -> bool {
        self.used_untyped_function_defs.is_empty() && self.new_function_defs.is_empty()
    }
}

struct TypeConversionContextIterator<'a> {
    index: usize,
    used: bool,
    context: &'a TypeConversionContext
}

impl <'a> TypeConversionContextIterator<'a> {

    fn new(context: &'a TypeConversionContext) -> Self {
        Self { index: 0, used: false, context }
    }

}

impl <'a> Iterator for TypeConversionContextIterator<'a> {
    type Item = ASTFunctionDef;

    fn next(&mut self) -> Option<Self::Item> {
        let vec = if !self.used {
            &self.context.new_function_defs
        } else {
            &self.context.used_untyped_function_defs
        };

        let result = vec.values().nth(self.index);

        self.index += 1;

        result.cloned()
    }
}
