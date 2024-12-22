use std::collections::HashMap;

use rasm_parser::catalog::{
    modules_catalog::ModulesCatalog, ModuleId, ModuleInfo, ModuleNamespace,
};

use crate::codegen::enh_ast::{EnhASTNameSpace, EnhModuleId, EnhModuleInfo};

struct ModuleEntry {
    id: ModuleId,
    namespace: ModuleNamespace,
    enh_namespace: EnhASTNameSpace,
}

pub struct RasmProjectCatalog {
    map: HashMap<EnhModuleId, ModuleEntry>,
}

impl RasmProjectCatalog {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
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
                    enh_namespace: info.namespace,
                },
            )
            .is_some()
        {
            panic!("already added {}", info.id);
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
}
