use linked_hash_map::{Iter, LinkedHashMap};

use crate::codegen::enh_val_context::TypedValContext;
use crate::codegen::TypedValKind;
use crate::enh_type_check::typed_ast::{ASTTypedFunctionDef, ASTTypedType};

#[derive(Debug, Clone)]
pub struct LambdaCall {
    pub def: ASTTypedFunctionDef,
    pub space: LambdaSpace,
}

#[derive(Debug, Clone)]
pub struct LambdaSpace {
    values: LinkedHashMap<String, TypedValKind>,
    context: TypedValContext,
    ref_functions: Vec<ASTTypedFunctionDef>,
}

impl LambdaSpace {
    pub fn new(context: TypedValContext) -> Self {
        LambdaSpace {
            values: LinkedHashMap::new(),
            context,
            ref_functions: Vec::new(),
        }
    }

    pub fn add(&mut self, name: String, kind: TypedValKind) {
        self.values.insert(name, kind);
    }

    pub fn get_index(&self, name: &str) -> Option<usize> {
        return self
            .values
            .iter()
            .position(|(n, _)| n == name)
            .map(|it| it + 1);
    }

    pub fn get_type(&self, name: &str) -> Option<&ASTTypedType> {
        return self
            .values
            .iter()
            .find(|(n, _)| n == &name)
            .map(|it| it.1.typed_type());
    }

    pub fn is_in_context(&self, name: &str) -> bool {
        self.context.get(name).is_some()
    }

    pub fn get_context(&self) -> &TypedValContext {
        &self.context
    }

    pub fn iter(&self) -> Iter<String, TypedValKind> {
        self.values.iter()
    }

    pub fn add_ref_function(&mut self, def: ASTTypedFunctionDef) {
        self.ref_functions.push(def);
    }

    pub fn get_ref_functions(&self) -> Vec<ASTTypedFunctionDef> {
        self.ref_functions.clone()
    }

    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    pub fn size(&self) -> usize {
        self.values.len()
    }
}
