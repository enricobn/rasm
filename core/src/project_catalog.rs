use std::collections::HashMap;

use rasm_parser::catalog::{
    modules_catalog::ModulesCatalog, ModuleId, ModuleInfo, ModuleNamespace,
};
use rasm_utils::find_one;

use crate::codegen::enh_ast::{EnhASTNameSpace, EnhModuleId, EnhModuleInfo};

struct ModuleEntry {
    id: ModuleId,
    namespace: ModuleNamespace,
    enh_namespace: EnhASTNameSpace,
}

pub struct RasmProjectCatalog {
    map: HashMap<EnhModuleId, ModuleEntry>,
    map_namespaces: HashMap<EnhASTNameSpace, ModuleNamespace>,
}

impl RasmProjectCatalog {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            map_namespaces: HashMap::new(),
        }
    }

    pub fn add(&mut self, info: EnhModuleInfo) {
        if self
            .map
            .insert(
                info.id.clone(),
                ModuleEntry {
                    id: info.module_id(),
                    namespace: info.module_namespace(),
                    enh_namespace: info.namespace.clone(),
                },
            )
            .is_some()
        {
            panic!("already added {}", &info.id);
        }
        if let Some(existing_namespace) = &self
            .map_namespaces
            .insert(info.namespace.clone(), info.module_namespace())
        {
            if existing_namespace != &info.module_namespace() {
                panic!(
                    "already added existing {existing_namespace} {}",
                    info.module_namespace()
                );
            }
        }
    }
}

impl ModulesCatalog<EnhModuleId, EnhASTNameSpace> for RasmProjectCatalog {
    fn info(&self, id: &EnhModuleId) -> Option<ModuleInfo> {
        self.map
            .get(id)
            .map(|it| ModuleInfo::new(it.namespace.clone(), it.id.clone()))
    }

    fn catalog_info(&self, id: &ModuleId) -> Option<(&EnhModuleId, &EnhASTNameSpace)> {
        for (enh_id, info) in self.map.iter() {
            if &info.id == id {
                return Some((enh_id, &info.enh_namespace));
            }
        }
        None
    }

    fn catalog(&self) -> Vec<(&EnhModuleId, &EnhASTNameSpace, &ModuleId, &ModuleNamespace)> {
        self.map
            .iter()
            .map(|(id, entry)| (id, &entry.enh_namespace, &entry.id, &entry.namespace))
            .collect::<Vec<_>>()
    }

    fn namespace(&self, namespace: &EnhASTNameSpace) -> Option<&ModuleNamespace> {
        self.map_namespaces.get(namespace)
        /*
        let o: Vec<(&EnhModuleId, &ModuleEntry)> = self
            .map
            .iter()
            .filter(|(_, entry): &(&EnhModuleId, &ModuleEntry)| &entry.enh_namespace == namespace)
            .collect();
        if o.len() == 1 {
            o.get(0).map(|(id, entry)| &entry.namespace)
        } else {
            None
        }
        */
    }
}
