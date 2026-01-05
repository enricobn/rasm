use std::fmt::Display;

use crate::parser::ast::ASTPosition;

pub mod modules_catalog;

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ModuleNamespace {
    internal: String,
    rest: String,
}

impl ModuleNamespace {
    pub fn new(internal: String, rest: String) -> Self {
        Self { internal, rest }
    }

    pub fn global() -> Self {
        Self {
            internal: String::new(),
            rest: String::new(),
        }
    }

    pub fn visible_from(
        &self,
        modifiers: &crate::parser::ast::ASTModifiers,
        namespace: &ModuleNamespace,
    ) -> bool {
        match modifiers {
            crate::parser::ast::ASTModifiers::Public => true,
            crate::parser::ast::ASTModifiers::Private => self == namespace,
            crate::parser::ast::ASTModifiers::Internal(internals) => {
                if let Some(internals) = internals {
                    if internals == &namespace.internal {
                        return true;
                    }
                }
                self.internal == namespace.internal
            }
        }
    }

    pub fn internal(&self) -> &str {
        &self.internal
    }

    pub fn safe_name(&self) -> String {
        format!("{}_{}", self.internal, self.rest)
            .replace('/', "_")
            .replace(':', "_")
    }
}

impl Display for ModuleNamespace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{}:{}", self.internal, self.rest))
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ModuleId(pub String);

impl ModuleId {
    pub fn global() -> Self {
        Self(String::new())
    }
}

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

impl Display for ModuleInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} : {}", self.module_id, self.namespace)
    }
}

impl ModuleInfo {
    pub fn new(namespace: ModuleNamespace, module_id: ModuleId) -> Self {
        Self {
            namespace,
            module_id,
        }
    }

    pub fn global() -> Self {
        Self::new(ModuleNamespace::global(), ModuleId::global())
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
            module_namespace: ModuleNamespace::global(),
            module_id: ModuleId::global(),
            position: ASTPosition::none(),
        }
    }

    pub fn mv_right(&self, offset: usize) -> Self {
        Self {
            module_namespace: self.module_namespace.clone(),
            module_id: self.module_id.clone(),
            position: self.position.mv_right(offset),
        }
    }

    pub fn mv_left(&self, offset: usize) -> Self {
        Self {
            module_namespace: self.module_namespace.clone(),
            module_id: self.module_id.clone(),
            position: self.position.mv_left(offset),
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

    pub fn with_position(&self, position: ASTPosition) -> Self {
        Self {
            module_namespace: self.module_namespace.clone(),
            module_id: self.module_id.clone(),
            position,
        }
    }

    pub fn equals_ignoring_builtin(&self, other: &ASTIndex) -> bool {
        self.module_id == other.module_id
            && self.module_namespace == other.module_namespace
            && self.position.row == other.position.row
            && self.position.column == other.position.column
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
