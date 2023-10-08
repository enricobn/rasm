use crate::codegen::backend::Backend;
use crate::codegen::statics::Statics;
use crate::parser::ast::ASTModule;
use crate::transformations::enum_functions_creator::enum_functions_creator;
use crate::transformations::struct_functions_creator::struct_functions_creator;

pub mod enum_functions_creator;
pub mod globals_creator;
pub mod str_functions_creator;
pub mod struct_functions_creator;
pub mod type_functions_creator;
pub mod typed_enum_functions_creator;
pub mod typed_struct_functions_creator;
pub mod typed_type_functions_creator;

pub fn enrich_module(backend: &dyn Backend, statics: &mut Statics, module: &mut ASTModule) {
    enum_functions_creator(backend, module, statics);
    struct_functions_creator(backend, module);
}
