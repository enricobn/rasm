use linked_hash_map::LinkedHashMap;
use rasm_core::{
    codegen::enh_ast::EnhASTIndex, enh_type_check::enh_functions_container::EnhTypeFilter,
};

pub struct ReferenceContext {
    pub value_to_address: LinkedHashMap<String, ReferenceKind>,
}

impl ReferenceContext {
    pub fn new(parent: Option<&ReferenceContext>) -> Self {
        if let Some(p) = parent {
            let mut value_to_address = LinkedHashMap::new();
            p.value_to_address.iter().for_each(|(key, value)| {
                value_to_address.insert(key.clone(), value.clone());
            });
            Self { value_to_address }
        } else {
            Self {
                value_to_address: LinkedHashMap::new(),
            }
        }
    }
}

#[derive(Clone)]
pub struct ReferenceKind {
    pub filter: EnhTypeFilter,
    pub index: EnhASTIndex,
}

impl ReferenceContext {
    pub fn add(&mut self, key: String, index: EnhASTIndex, filter: EnhTypeFilter) {
        self.value_to_address
            .insert(key, ReferenceKind { index, filter });
    }

    pub fn get(&self, key: &str) -> Option<&ReferenceKind> {
        self.value_to_address.get(key)
    }
}
