use std::sync::atomic::{AtomicUsize, Ordering};

use linked_hash_map::LinkedHashMap;
use rasm_utils::debug_i;

use rasm_parser::{
    catalog::{ASTIndex, ModuleId, ModuleNamespace},
    parser::ast::{ASTParameterDef, ASTPosition, ASTType, BuiltinTypeKind},
};

static COUNT_UNKNOWN_LAMBDA_PAR: AtomicUsize = AtomicUsize::new(0);

#[derive(Clone, Debug)]
pub enum ValKind {
    ParameterRef(usize, ASTParameterDef),
    LetRef(usize, ASTType, ASTIndex),
}

impl ValKind {
    pub fn ast_type(&self) -> ASTType {
        match self {
            ValKind::ParameterRef(_, par) => par.ast_type.clone(),
            ValKind::LetRef(_, ast_type, _) => ast_type.clone(),
        }
    }

    pub fn index(&self, module_namespace: &ModuleNamespace, module_id: &ModuleId) -> ASTIndex {
        match self {
            ValKind::ParameterRef(_, astparameter_def) => ASTIndex::new(
                module_namespace.clone(),
                module_id.clone(),
                astparameter_def.position.clone(),
            ),
            ValKind::LetRef(_, _, astindex) => astindex.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ValContext {
    pub value_to_address: LinkedHashMap<String, ValKind>,
    let_index: usize,
    par_index: usize,
    unknown_lambda_par_index: usize,
}

impl ValContext {
    pub fn new(parent_context: Option<&ValContext>) -> Self {
        let (value_to_address, par_index, let_index, unknown_lambda_par_index) = {
            if let Some(pc) = parent_context {
                (
                    pc.value_to_address.clone(),
                    pc.par_index,
                    pc.let_index,
                    pc.unknown_lambda_par_index,
                )
            } else {
                (LinkedHashMap::new(), 0, 0, 0)
            }
        };
        Self {
            value_to_address,
            par_index,
            let_index,
            unknown_lambda_par_index,
        }
    }

    pub fn insert_par(
        &mut self,
        key: String,
        par: ASTParameterDef,
        namespace: &ModuleNamespace,
        source: &ModuleId,
    ) -> Result<Option<ValKind>, String> {
        let result = self.value_to_address.insert(
            key.clone(),
            ValKind::ParameterRef(self.par_index, par.clone()),
        );
        self.par_index += 1;
        if result.is_some() {
            Err(format!(
                "already added {key}: {}",
                ASTIndex::new(namespace.clone(), source.clone(), par.position)
            ))
        } else {
            debug_i!("added parameter {key} -> {par} to context");
            Ok(result)
        }
    }

    pub fn insert_let(
        &mut self,
        key: String,
        ast_type: ASTType,
        ast_index: &ASTIndex,
    ) -> Result<Option<ValKind>, String> {
        debug_i!("adding let val {key} of type {ast_type} to context");

        let result = self.value_to_address.insert(
            key.clone(),
            ValKind::LetRef(self.let_index, ast_type, ast_index.clone()),
        );
        self.let_index += 1;
        if result.is_some() {
            Err(format!("already defined {key}: {}", ast_index))
        } else {
            Ok(result)
        }
    }

    pub fn insert_unknown_lambda_parameter(
        &mut self,
        key: String,
        ast_index: &ASTIndex,
    ) -> Result<Option<ValKind>, String> {
        debug_i!("adding unknown lambda parameter {key} to context");

        let par = ASTParameterDef::new(
            &key,
            ASTType::Generic(
                ASTPosition::none(),
                format!(
                    "L_{}",
                    COUNT_UNKNOWN_LAMBDA_PAR.fetch_add(1, Ordering::Relaxed)
                ),
                Vec::new(),
            ),
            ast_index.position().clone(),
        );

        let result = self.value_to_address.insert(
            key.clone(),
            ValKind::ParameterRef(self.unknown_lambda_par_index, par),
        );
        self.unknown_lambda_par_index += 1;
        if result.is_some() {
            Err(format!("already defined {key}: {}", ast_index))
        } else {
            Ok(result)
        }
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
