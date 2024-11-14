use crate::codegen::compile_target::CompileTarget;
use crate::codegen::enh_ast::EnhModuleInfo;
use crate::codegen::statics::Statics;
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
