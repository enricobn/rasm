use linked_hash_map::{Iter, LinkedHashMap};
use log::debug;

use crate::codegen::{TypedValKind, ValKind};
use crate::debug_i;
use crate::parser::ast::{ASTIndex, ASTParameterDef, ASTType, BuiltinTypeKind};
use crate::type_check::typed_ast::{ASTTypedParameterDef, ASTTypedType};

#[derive(Clone, Debug)]
pub struct ValContext {
    pub value_to_address: LinkedHashMap<String, ValKind>,
    let_index: usize,
    par_index: usize,
}

impl ValContext {
    pub fn new(parent_context: Option<&ValContext>) -> Self {
        let mut value_to_address = LinkedHashMap::new();
        if let Some(pc) = parent_context {
            for (key, value) in pc.value_to_address.iter() {
                value_to_address.insert(key.clone(), value.clone());
            }
        }
        Self {
            value_to_address,
            par_index: 0,
            let_index: 0,
        }
    }

    pub fn insert_par(&mut self, key: String, par: ASTParameterDef) -> Option<ValKind> {
        let result = self.value_to_address.insert(
            key.clone(),
            ValKind::ParameterRef(self.par_index, par.clone()),
        );
        self.par_index += 1;
        if result.is_some() {
            panic!("already added {key}: {}", par.ast_index);
        }
        debug_i!("added parameter {key} -> {par} to context");
        result
    }

    pub fn insert_let(
        &mut self,
        key: String,
        ast_type: ASTType,
        ast_index: &ASTIndex,
    ) -> Option<ValKind> {
        debug_i!("adding let val {key} of type {ast_type} to context");

        let result = self.value_to_address.insert(
            key.clone(),
            ValKind::LetRef(self.let_index, ast_type, ast_index.clone()),
        );
        self.let_index += 1;
        if result.is_some() {
            panic!("already added {key}: {}", ast_index);
        }
        result
    }

    pub fn get(&self, key: &str) -> Option<&ValKind> {
        self.value_to_address.get(key)
    }

    pub fn names(&self) -> Vec<&String> {
        self.value_to_address.keys().collect()
    }

    pub fn is_lambda(&self, key: &str) -> bool {
        if let Some(ValKind::ParameterRef(_i, par)) = self.get(key) {
            if let ASTType::Builtin(BuiltinTypeKind::Lambda {
                return_type: _,
                parameters: _,
            }) = &par.ast_type
            {
                return true;
            }
        }

        if let Some(ValKind::LetRef(_i, ast_type, _)) = self.get(key) {
            if let ASTType::Builtin(BuiltinTypeKind::Lambda {
                return_type: _,
                parameters: _,
            }) = ast_type
            {
                return true;
            }
        }
        false
    }

    pub fn get_lambda(&self, key: &str) -> Option<(&Box<ASTType>, &Vec<ASTType>)> {
        if let Some(ValKind::ParameterRef(_i, par)) = self.get(key) {
            if let ASTType::Builtin(BuiltinTypeKind::Lambda {
                return_type,
                parameters,
            }) = &par.ast_type
            {
                return Some((return_type, parameters));
            }
        }

        if let Some(ValKind::LetRef(_i, ast_type, _)) = self.get(key) {
            if let ASTType::Builtin(BuiltinTypeKind::Lambda {
                return_type,
                parameters,
            }) = ast_type
            {
                return Some((return_type, parameters));
            }
        }
        None
    }
}

#[derive(Clone, Debug)]
pub struct TypedValContext {
    pub value_to_address: LinkedHashMap<String, TypedValKind>,
    let_index: usize,
}

impl TypedValContext {
    pub fn new(parent_context: Option<&TypedValContext>) -> Self {
        let mut map = LinkedHashMap::new();
        if let Some(pc) = parent_context {
            for (key, value) in pc.value_to_address.iter() {
                map.insert(key.clone(), value.clone());
            }
        }
        Self {
            value_to_address: map,
            let_index: 0,
        }
    }

    pub fn insert_par(
        &mut self,
        key: String,
        index: usize,
        par: ASTTypedParameterDef,
    ) -> Option<TypedValKind> {
        self.value_to_address
            .insert(key, TypedValKind::ParameterRef(index, par))
    }

    pub fn insert_let(
        &mut self,
        key: String,
        ast_typed_type: ASTTypedType,
        index_relative_to_bp: Option<usize>,
    ) -> Option<TypedValKind> {
        let result = self.value_to_address.insert(
            key,
            TypedValKind::LetRef(
                index_relative_to_bp.unwrap_or(self.let_index + 1),
                ast_typed_type,
            ),
        );
        self.let_index += 1;
        result
    }

    pub fn insert(&mut self, key: String, kind: TypedValKind) {
        self.value_to_address.insert(key, kind);
    }

    pub fn get(&self, key: &str) -> Option<&TypedValKind> {
        self.value_to_address.get(key)
    }

    pub fn iter(&self) -> Iter<String, TypedValKind> {
        self.value_to_address.iter()
    }

    pub fn names(&self) -> Vec<&String> {
        self.value_to_address.keys().collect()
    }

    pub fn is_empty(&self) -> bool {
        self.value_to_address.is_empty()
    }
}
