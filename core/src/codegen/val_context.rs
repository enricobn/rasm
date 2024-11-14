use std::fmt::Display;

use linked_hash_map::LinkedHashMap;
use rasm_utils::debug_i;

use crate::type_check::ast_modules_container::{ModuleId, ModuleInfo, ModuleNamespace};
use rasm_parser::parser::ast::{ASTParameterDef, ASTPosition, ASTType, BuiltinTypeKind};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ASTIndex {
    module_namespace: ModuleNamespace,
    module_id: ModuleId,
    position: ASTPosition,
}

impl ASTIndex {
    pub fn new(
        module_namespace: ModuleNamespace,
        module_id: ModuleId,
        position: ASTPosition,
    ) -> Self {
        Self {
            module_namespace,
            module_id,
            position,
        }
    }

    pub fn none() -> Self {
        Self {
            module_namespace: ModuleNamespace(String::new()),
            module_id: ModuleId(String::new()),
            position: ASTPosition::none(),
        }
    }

    pub fn mv_right(&self, offset: usize) -> Self {
        Self {
            module_namespace: self.module_namespace.clone(),
            module_id: self.module_id.clone(),
            position: self.position.clone().mv_right(offset),
        }
    }

    pub fn module_namespace(&self) -> &ModuleNamespace {
        &self.module_namespace
    }

    pub fn module_id(&self) -> &ModuleId {
        &self.module_id
    }

    pub fn position(&self) -> &ASTPosition {
        &self.position
    }

    pub fn info(&self) -> ModuleInfo {
        ModuleInfo::new(self.module_namespace.clone(), self.module_id.clone())
    }
}

impl Display for ASTIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{}",
            self.module_id, self.position.row, self.position.column
        )
    }
}

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
}

impl ValContext {
    pub fn new(parent_context: Option<&ValContext>) -> Self {
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

    pub fn insert_par(
        &mut self,
        key: String,
        par: ASTParameterDef,
        id: &ModuleNamespace,
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
                ASTIndex::new(id.clone(), source.clone(), par.position)
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
