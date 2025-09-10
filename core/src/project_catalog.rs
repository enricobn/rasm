use std::collections::HashMap;

use rasm_parser::catalog::{
    modules_catalog::ModulesCatalog, ModuleId, ModuleInfo, ModuleNamespace,
};

use crate::codegen::enh_ast::{EnhASTNameSpace, EnhModuleId, EnhModuleInfo};

#[derive(Clone)]
struct ModuleEntry {
    id: ModuleId,
    namespace: ModuleNamespace,
    enh_namespace: EnhASTNameSpace,
}

#[derive(Clone)]
pub struct RasmProjectCatalog {
    map: HashMap<EnhModuleId, ModuleEntry>,
    map_namespaces: HashMap<EnhASTNameSpace, ModuleNamespace>,
    enh_map: HashMap<ModuleId, EnhModuleInfo>,
}

impl RasmProjectCatalog {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            map_namespaces: HashMap::new(),
            enh_map: HashMap::new(),
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
        self.enh_map.get(id).map(|it| (&it.id, &it.namespace))
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

    fn add(&mut self, id: EnhModuleId, namespace: EnhASTNameSpace) {
        let info = EnhModuleInfo::new(id, namespace);

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

        if self
            .enh_map
            .insert(info.module_id(), info.clone())
            .is_some()
        {
            panic!("already added {}", &info.id);
        }
    }

    fn clone_catalog(&self) -> Box<dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>> {
        Box::new(self.clone())
    }

    fn ids_by_namespace(&self, namespace: &EnhASTNameSpace) -> Vec<(&EnhModuleId, &ModuleId)> {
        self.enh_map
            .iter()
            .filter(|(_, info)| &info.namespace == namespace)
            .map(|(id, info)| (&info.id, id))
            .collect()
    }
}
