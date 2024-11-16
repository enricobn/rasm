use crate::parser::ast::ASTModule;

use super::{ModuleId, ModuleInfo};

pub trait ModulesCatalog<ID, NAMESPACE> {
    fn module(&self, id: ID) -> Option<&ASTModule>;

    fn info(&self, id: ID) -> Option<ModuleInfo>;

    fn catalog_info(&self, id: ModuleId) -> Option<(ID, NAMESPACE)>;
}
