use crate::codegen::statics::Statics;
use crate::codegen::CompileTarget;
use crate::parser::ast::ASTModule;
use crate::transformations::functions_creator::FunctionsCreator;

pub mod functions_creator;
pub mod globals_creator;
pub mod str_functions_creator;
pub mod type_functions_creator;
pub mod typed_functions_creator;

pub fn enrich_module(
    target: &CompileTarget,
    statics: &mut Statics,
    module: &mut ASTModule,
    debug: bool,
) {
    target.functions_creator(debug).create(module, statics);
}
