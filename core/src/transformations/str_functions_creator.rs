use crate::codegen::backend::Backend;
use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::codegen::statics::Statics;
use crate::parser::ast::{
    ASTFunctionBody, ASTFunctionDef, ASTIndex, ASTModifiers, ASTNameSpace, ASTParameterDef,
    ASTType, BuiltinTypeKind,
};
use crate::type_check::resolved_generic_types::ResolvedGenericTypes;

pub fn str_functions_creator(
    module: &mut EnhancedASTModule,
    backend: &dyn Backend,
    statics: &mut Statics,
) {
    let message_key = statics.add_str("String");
    let mut body_src = String::new();
    backend.call_function(
        &mut body_src,
        "deref_0",
        &[("$s", None), (&message_key, None)],
        None,
    );
    let body = ASTFunctionBody::NativeBody(body_src);
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
        namespace: ASTNameSpace::global(),
    };

    module.add_function(name, function_def);

    let mut body_src = String::new();
    backend.call_function(
        &mut body_src,
        "addRef_0",
        &[("$s", None), (&message_key, None)],
        None,
    );

    let body = ASTFunctionBody::NativeBody(body_src);
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
        namespace: ASTNameSpace::global(),
    };

    module.add_function(name, function_def);
}
