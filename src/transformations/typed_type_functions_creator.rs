use crate::codegen::backend::Backend;
use crate::codegen::statics::Statics;
use crate::codegen::CodeGen;
use crate::type_check::typed_ast::{
    ASTTypedFunctionBody, ASTTypedFunctionDef, ASTTypedModule, ASTTypedParameterDef, ASTTypedType,
    ASTTypedTypeDef, ASTTypedTypeRef,
};
use linked_hash_map::LinkedHashMap;
use log::debug;

pub fn typed_type_functions_creator(
    backend: &dyn Backend,
    module: &ASTTypedModule,
    statics: &mut Statics,
) -> ASTTypedModule {
    let mut functions_by_name = module.functions_by_name.clone();
    let native_body = module.native_body.clone();

    for enum_def in module.types.iter() {
        create_free(
            backend,
            &mut functions_by_name,
            enum_def,
            "deref",
            "deref",
            module,
            statics,
        );
        create_free(
            backend,
            &mut functions_by_name,
            enum_def,
            "addRef",
            "addRef",
            module,
            statics,
        );
    }

    let mut result = module.clone();
    result.functions_by_name = functions_by_name;
    result.native_body = native_body;

    result
}

fn create_free(
    backend: &dyn Backend,
    functions_by_name: &mut LinkedHashMap<String, ASTTypedFunctionDef>,
    type_def: &ASTTypedTypeDef,
    asm_function_name: &str,
    function_name: &str,
    module: &ASTTypedModule,
    statics: &mut Statics,
) {
    let ast_type = ASTTypedType::Type {
        name: type_def.name.clone(),
    };
    let type_ref = ASTTypedTypeRef {
        ast_type,
        ast_ref: false,
    };

    let body_str = create_free_body(
        backend,
        type_def,
        asm_function_name,
        function_name,
        module,
        statics,
    );
    let body = ASTTypedFunctionBody::ASMBody(body_str);

    let fun_name = format!("{}_{function_name}", type_def.name);

    let function_def = ASTTypedFunctionDef {
        name: fun_name.clone(),
        parameters: vec![ASTTypedParameterDef {
            name: "address".into(),
            type_ref,
        }],
        body,
        inline: false,
        return_type: None,
    };

    debug!("created function {function_def}");

    functions_by_name.insert(fun_name, function_def);
}

fn create_free_body(
    backend: &dyn Backend,
    type_def: &ASTTypedTypeDef,
    asm_function_name: &str,
    function_name: &str,
    module: &ASTTypedModule,
    statics: &mut Statics,
) -> String {
    let ws = backend.word_size();
    let wl = backend.word_len();

    let mut result = String::new();

    let descr = format!("type {}", type_def.name);
    let key = statics.add_str(&descr);

    CodeGen::add(&mut result, "", Some(&descr), true);
    CodeGen::add(&mut result, &format!("push  {ws} [{key}]"), None, true);
    CodeGen::add(&mut result, &format!("push  {ws} $address"), None, true);
    CodeGen::add(
        &mut result,
        &format!("call  {asm_function_name}"),
        None,
        true,
    );
    CodeGen::add(
        &mut result,
        &format!("add  {},{}", backend.stack_pointer(), wl * 2),
        None,
        true,
    );

    if type_has_references(type_def) {
        // TODO
    }

    result
}

pub fn type_has_references(type_def: &ASTTypedTypeDef) -> bool {
    // TODO I don't know how to make it
    false
}
