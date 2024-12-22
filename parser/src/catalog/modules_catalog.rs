use super::{ModuleId, ModuleInfo, ModuleNamespace};

pub trait ModulesCatalog<ID, NAMESPACE>: Send + Sync {
    fn info(&self, id: &ID) -> Option<ModuleInfo>;

    fn catalog_info(&self, id: &ModuleId) -> Option<(&ID, &NAMESPACE)>;

    fn catalog(&self) -> Vec<(&ID, &NAMESPACE, &ModuleId, &ModuleNamespace)>;

    fn is_readonly_module(&self, id: &ModuleId) -> bool;
}
