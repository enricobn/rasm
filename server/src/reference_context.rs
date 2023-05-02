use std::collections::HashMap;

use rasm_core::parser::ast::ASTIndex;
use rasm_core::type_check::functions_container::TypeFilter;

pub struct ReferenceContext {
    pub value_to_address: HashMap<String, ReferenceKind>,
}

impl ReferenceContext {
    pub fn new(parent: Option<&ReferenceContext>) -> Self {
        if let Some(p) = parent {
            let mut value_to_address = HashMap::new();
            p.value_to_address.iter().for_each(|(key, value)| {
                value_to_address.insert(key.clone(), value.clone());
            });
            Self { value_to_address }
        } else {
            Self {
                value_to_address: HashMap::new(),
            }
        }
    }
}

#[derive(Clone)]
pub struct ReferenceKind {
    pub filter: TypeFilter,
    pub index: ASTIndex,
}

impl ReferenceContext {
    pub fn add(&mut self, key: String, index: ASTIndex, filter: TypeFilter) {
        self.value_to_address
            .insert(key, ReferenceKind { index, filter });
    }

    pub fn get(&self, key: &str) -> Option<&ReferenceKind> {
        self.value_to_address.get(key)
    }
}
