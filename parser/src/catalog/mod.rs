use std::fmt::Display;

use crate::parser::ast::ASTPosition;

pub mod modules_catalog;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ModuleNamespace(pub String);

impl Display for ModuleNamespace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ModuleId(pub String);

impl Display for ModuleId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ModuleInfo {
    module_id: ModuleId,
    namespace: ModuleNamespace,
}

impl ModuleInfo {
    pub fn new(namespace: ModuleNamespace, module_id: ModuleId) -> Self {
        Self {
            namespace,
            module_id,
        }
    }

    pub fn global() -> Self {
        Self::new(ModuleNamespace(String::new()), ModuleId(String::new()))
    }

    pub fn namespace(&self) -> &ModuleNamespace {
        &self.namespace
    }

    pub fn id(&self) -> &ModuleId {
        &self.module_id
    }
}

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
