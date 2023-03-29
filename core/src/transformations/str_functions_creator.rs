use linked_hash_map::LinkedHashMap;

use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::parser::ast::{
    ASTFunctionBody, ASTFunctionDef, ASTIndex, ASTParameterDef, ASTType, BuiltinTypeKind,
};

pub fn str_functions_creator(module: &EnhancedASTModule) -> EnhancedASTModule {
    let mut result = module.clone();

    let body = ASTFunctionBody::ASMBody("$call(deref, $s:i32, \"String\")".into());
    let name: String = "str_deref".into();
    let function_def = ASTFunctionDef {
        original_name: name.clone(),
        name,
        parameters: vec![ASTParameterDef {
            name: "s".into(),
            ast_type: ASTType::Builtin(BuiltinTypeKind::String),
            ast_index: ASTIndex::none(),
        }],
        body,
        inline: false,
        return_type: None,
        param_types: Vec::new(),
        resolved_generic_types: LinkedHashMap::new(),
    };

    result.add_function(function_def.name.clone(), function_def);

    let body = ASTFunctionBody::ASMBody("$call(addRef, $s:i32, \"String\")".into());
    let name: String = "str_addRef".into();

    let function_def = ASTFunctionDef {
        original_name: name.clone(),
        name,
        parameters: vec![ASTParameterDef {
            name: "s".into(),
            ast_type: ASTType::Builtin(BuiltinTypeKind::String),
            ast_index: ASTIndex::none(),
        }],
        body,
        inline: false,
        return_type: None,
        param_types: Vec::new(),
        resolved_generic_types: LinkedHashMap::new(),
    };

    result.add_function(function_def.name.clone(), function_def);

    result
}
