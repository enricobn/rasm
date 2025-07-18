use crate::codegen::compile_target::CompileTarget;
use crate::codegen::enh_ast::{EnhASTNameSpace, EnhModuleId, EnhModuleInfo};
use crate::codegen::statics::Statics;

use crate::type_check::ast_modules_container::ASTModulesContainer;
use rasm_parser::catalog::modules_catalog::ModulesCatalog;
use rasm_parser::parser::ast::ASTModule;

pub mod functions_creator;
pub mod globals_creator;
pub mod typed_functions_creator;

pub fn enrich_module(
    target: &CompileTarget,
    statics: &mut Statics,
    module: &mut ASTModule,
    debug: bool,
    info: &EnhModuleInfo,
) {
    target
        .functions_creator(debug)
        .create(module, statics, info);
}

pub fn enrich_container(
    target: &CompileTarget,
    statics: &mut Statics,
    container: ASTModulesContainer,
    catalog: &dyn ModulesCatalog<EnhModuleId, EnhASTNameSpace>,
    debug: bool,
) -> ASTModulesContainer {
    let read_only_modules = container.read_only_modules().clone();

    let modules = container
        .into_modules()
        .into_iter()
        .map(|(id, _, m)| {
            let (e_id, e_ns) = catalog.catalog_info(&id).unwrap();
            let info = EnhModuleInfo::new(e_id.clone(), e_ns.clone());

            let mut module = m;

            enrich_module(target, statics, &mut module, debug, &info);

            (module, info)
        })
        .collect::<Vec<_>>();

    let mut enriched_container = ASTModulesContainer::new();

    for (module, info) in modules.into_iter() {
        let i = catalog.info(&info.id).unwrap();
        enriched_container.add(
            module,
            i.namespace().clone(),
            i.id().clone(),
            false, // what means add_builtin???
            read_only_modules.contains(i.id()),
        );
    }
    enriched_container
}
