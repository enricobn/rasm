use crate::codegen::EnhancedASTModule;
use crate::parser::ast::{
    ASTFunctionBody, ASTFunctionDef, ASTParameterDef, ASTType, BuiltinTypeKind,
};
use linked_hash_map::LinkedHashMap;

pub fn str_functions_creator(module: &EnhancedASTModule) -> EnhancedASTModule {
    let mut functions_by_name = module.functions_by_name.clone();

    let body = ASTFunctionBody::ASMBody("$call(deref, $s, \"String\")".into());
    let function_def = ASTFunctionDef {
        name: "str_deref".into(),
        parameters: vec![ASTParameterDef {
            name: "s".into(),
            ast_type: ASTType::Builtin(BuiltinTypeKind::ASTString),
        }],
        body,
        inline: false,
        return_type: None,
        param_types: Vec::new(),
        resolved_generic_types: LinkedHashMap::new(),
    };
    functions_by_name.insert(function_def.name.clone(), function_def);

    let body = ASTFunctionBody::ASMBody("$call(addRef, $s, \"String\")".into());
    let function_def = ASTFunctionDef {
        name: "str_addRef".into(),
        parameters: vec![ASTParameterDef {
            name: "s".into(),
            ast_type: ASTType::Builtin(BuiltinTypeKind::ASTString),
        }],
        body,
        inline: false,
        return_type: None,
        param_types: Vec::new(),
        resolved_generic_types: LinkedHashMap::new(),
    };
    functions_by_name.insert(function_def.name.clone(), function_def);

    let mut result = module.clone();
    result.functions_by_name = functions_by_name;

    result
}
