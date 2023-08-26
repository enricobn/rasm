use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::parser::ast::BuiltinTypeKind;
use crate::type_check::typed_ast::DefaultFunction;

pub fn type_mandatory_functions(module: &EnhancedASTModule) -> Vec<DefaultFunction> {
    let mut result = Vec::new();

    for vec_def in &module.types {
        if !vec_def.type_parameters.is_empty() {
            let name = format!("{}References", vec_def.name);
            result.push(DefaultFunction::new_2(
                &name,
                BuiltinTypeKind::I32,
                BuiltinTypeKind::I32,
            ));
        }
    }

    result
}
