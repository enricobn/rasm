use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::parser::ast::{
    ASTFunctionBody, ASTFunctionDef, ASTIndex, ASTModifiers, ASTParameterDef, ASTType,
    BuiltinTypeKind,
};
use crate::type_check::resolved_generic_types::ResolvedGenericTypes;

pub fn str_functions_creator(module: &mut EnhancedASTModule) {
    let body = ASTFunctionBody::ASMBody("$call(deref, $s:i32, \"String\")".into());
    let name: String = "str_deref".into();
    let function_def = ASTFunctionDef {
        original_name: name.clone(),
        name: name.clone(),
        parameters: vec![ASTParameterDef {
            name: "s".into(),
            ast_type: ASTType::Builtin(BuiltinTypeKind::String),
            ast_index: ASTIndex::none(),
        }],
        body,
        inline: false,
        return_type: ASTType::Unit,
        generic_types: Vec::new(),
        resolved_generic_types: ResolvedGenericTypes::new(),
        index: ASTIndex::none(),
        modifiers: ASTModifiers::public(),
    };

    module.add_function(name, function_def);

    let body = ASTFunctionBody::ASMBody("$call(addRef, $s:i32, \"String\")".into());
    let name: String = "str_addRef".into();

    let function_def = ASTFunctionDef {
        original_name: name.clone(),
        name: name.clone(),
        parameters: vec![ASTParameterDef {
            name: "s".into(),
            ast_type: ASTType::Builtin(BuiltinTypeKind::String),
            ast_index: ASTIndex::none(),
        }],
        body,
        inline: false,
        return_type: ASTType::Unit,
        generic_types: Vec::new(),
        resolved_generic_types: ResolvedGenericTypes::new(),
        index: ASTIndex::none(),
        modifiers: ASTModifiers::public(),
    };

    module.add_function(name, function_def);
}
