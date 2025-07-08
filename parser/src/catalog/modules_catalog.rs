use super::{ModuleId, ModuleInfo, ModuleNamespace};

pub trait ModulesCatalog<ID, NAMESPACE>: Send + Sync {
    fn info(&self, id: &ID) -> Option<ModuleInfo>;

    fn catalog_info(&self, id: &ModuleId) -> Option<(&ID, &NAMESPACE)>;

    fn catalog(&self) -> Vec<(&ID, &NAMESPACE, &ModuleId, &ModuleNamespace)>;

    fn namespace(&self, namespace: &NAMESPACE) -> Option<&ModuleNamespace>;

    fn add(&mut self, id: ID, namespace: NAMESPACE);
}
