use crate::codegen::EnhancedASTModule;

pub fn type_mandatory_functions(module: &EnhancedASTModule) -> Vec<String> {
    let mut result = Vec::new();

    for vec_def in &module.types {
        if !vec_def.type_parameters.is_empty() {
            result.push(format!("{}References", vec_def.name));
        }
    }

    result
}
