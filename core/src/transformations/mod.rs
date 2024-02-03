use crate::codegen::backend::Backend;
use crate::codegen::statics::Statics;
use crate::parser::ast::ASTModule;

pub mod functions_creator;
pub mod globals_creator;
pub mod str_functions_creator;
pub mod type_functions_creator;
pub mod typed_functions_creator;

pub fn enrich_module(backend: &dyn Backend, statics: &mut Statics, module: &mut ASTModule) {
    backend.functions_creator().create(module, statics);
}
