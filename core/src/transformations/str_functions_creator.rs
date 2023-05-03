use linked_hash_map::LinkedHashMap;

use crate::parser::ast::{
    ASTFunctionBody, ASTFunctionDef, ASTIndex, ASTModule, ASTParameterDef, ASTType, BuiltinTypeKind,
};

pub fn str_functions_creator(module: &mut ASTModule) {
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
        generic_types: Vec::new(),
        resolved_generic_types: LinkedHashMap::new(),
        index: ASTIndex::none(),
    };

    module.add_function(function_def);

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
        generic_types: Vec::new(),
        resolved_generic_types: LinkedHashMap::new(),
        index: ASTIndex::none(),
    };

    module.add_function(function_def);
}
