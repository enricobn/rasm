use std::sync::atomic::{AtomicUsize, Ordering};

use linked_hash_map::{Iter, LinkedHashMap};
use rasm_utils::debug_i;

use crate::codegen::enh_ast::{EnhASTIndex, EnhASTParameterDef, EnhASTType, EnhBuiltinTypeKind};
use crate::codegen::{EnhValKind, TypedValKind};
use crate::enh_type_check::typed_ast::{ASTTypedParameterDef, ASTTypedType};

static COUNT_UNKNOWN_LAMBDA_PAR: AtomicUsize = AtomicUsize::new(0);

#[derive(Clone, Debug)]
pub struct EnhValContext {
    pub value_to_address: LinkedHashMap<String, EnhValKind>,
    let_index: usize,
    par_index: usize,
}

impl EnhValContext {
    pub fn new(parent_context: Option<&EnhValContext>) -> Self {
        let (value_to_address, par_index, let_index) = {
            if let Some(pc) = parent_context {
                (pc.value_to_address.clone(), pc.par_index, pc.let_index)
            } else {
                (LinkedHashMap::new(), 0, 0)
            }
        };
        Self {
            value_to_address,
            par_index,
            let_index,
        }
    }

    pub fn insert_unknown_lambda_par(
        &mut self,
        name: &str,
        index: &EnhASTIndex,
    ) -> Result<Option<EnhValKind>, String> {
        self.insert_par(
            name.to_owned(),
            EnhASTParameterDef {
                name: name.to_owned(),
                ast_type: EnhASTType::Generic(
                    EnhASTIndex::none(),
                    format!(
                        "L_{}",
                        COUNT_UNKNOWN_LAMBDA_PAR.fetch_add(1, Ordering::Relaxed)
                    ),
                    Vec::new(), // TODO type classes
                ),
                ast_index: index.clone(),
            },
        )
    }

    pub fn insert_par(
        &mut self,
        key: String,
        par: EnhASTParameterDef,
    ) -> Result<Option<EnhValKind>, String> {
        let result = self.value_to_address.insert(
            key.clone(),
            EnhValKind::ParameterRef(self.par_index, par.clone()),
        );
        self.par_index += 1;
        if result.is_some() {
            Err(format!("already added {key}: {}", par.ast_index))
        } else {
            debug_i!("added parameter {key} -> {par} to context");
            Ok(result)
        }
    }

    pub fn insert_let(
        &mut self,
        key: String,
        ast_type: EnhASTType,
        ast_index: &EnhASTIndex,
    ) -> Result<Option<EnhValKind>, String> {
        debug_i!("adding let val {key} of type {ast_type} to context");

        let result = self.value_to_address.insert(
            key.clone(),
            EnhValKind::LetRef(self.let_index, ast_type, ast_index.clone()),
        );
        self.let_index += 1;
        if result.is_some() {
            Err(format!("already defined {key}: {}", ast_index))
        } else {
            Ok(result)
        }
    }

    pub fn get(&self, key: &str) -> Option<&EnhValKind> {
        self.value_to_address.get(key)
    }

    pub fn names(&self) -> Vec<&String> {
        self.value_to_address.keys().collect()
    }

    pub fn is_lambda(&self, key: &str) -> bool {
        if let Some(EnhValKind::ParameterRef(_i, par)) = self.get(key) {
            if let EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
                return_type: _,
                parameters: _,
            }) = &par.ast_type
            {
                return true;
            }
        }

        if let Some(EnhValKind::LetRef(_i, ast_type, _)) = self.get(key) {
            if let EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
                return_type: _,
                parameters: _,
            }) = ast_type
            {
                return true;
            }
        }
        false
    }

    pub fn get_lambda(&self, key: &str) -> Option<(&Box<EnhASTType>, &Vec<EnhASTType>)> {
        if let Some(EnhValKind::ParameterRef(_i, par)) = self.get(key) {
            if let EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
                return_type,
                parameters,
            }) = &par.ast_type
            {
                return Some((return_type, parameters));
            }
        }

        if let Some(EnhValKind::LetRef(_i, ast_type, _)) = self.get(key) {
            if let EnhASTType::Builtin(EnhBuiltinTypeKind::Lambda {
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
        self.insert(key, TypedValKind::ParameterRef(index, par))
    }

    pub fn insert_let(
        &mut self,
        key: String,
        ast_typed_type: ASTTypedType,
        index_relative_to_bp: Option<usize>,
    ) -> Option<TypedValKind> {
        let result = self.insert(
            key,
            TypedValKind::LetRef(
                index_relative_to_bp.unwrap_or(self.let_index + 1),
                ast_typed_type,
            ),
        );
        self.let_index += 1;
        result
    }

    pub fn insert(&mut self, key: String, kind: TypedValKind) -> Option<TypedValKind> {
        self.value_to_address.insert(key, kind)
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
