use crate::codegen::backend::Backend;
use crate::codegen::CodeGen;
use crate::type_check::typed_ast::{
    ASTTypedFunctionBody, ASTTypedFunctionDef, ASTTypedModule, ASTTypedParameterDef,
    ASTTypedStructDef, ASTTypedType, ASTTypedTypeRef,
};
use linked_hash_map::LinkedHashMap;
use log::debug;

pub fn typed_struct_functions_creator(
    code_gen: &mut CodeGen,
    backend: &dyn Backend,
    module: &ASTTypedModule,
) -> ASTTypedModule {
    let mut functions_by_name = module.functions_by_name.clone();
    let mut native_body = module.native_body.clone();

    for struct_def in module.structs.iter() {
        create_free(
            code_gen,
            backend,
            &mut functions_by_name,
            &struct_def,
            "deref",
            "deref",
        );
        create_free(
            code_gen,
            backend,
            &mut functions_by_name,
            &struct_def,
            "addRef",
            "addRef",
        );
    }

    let mut result = module.clone();
    result.functions_by_name = functions_by_name;
    result.native_body = native_body;

    result
}

fn create_free(
    code_gen: &mut CodeGen,
    backend: &dyn Backend,
    functions_by_name: &mut LinkedHashMap<String, ASTTypedFunctionDef>,
    struct_def: &ASTTypedStructDef,
    asm_function_name: &str,
    function_name: &str,
) {
    let ast_type = ASTTypedType::Struct {
        name: struct_def.name.clone(),
    };
    let type_ref = ASTTypedTypeRef {
        ast_type,
        ast_ref: false,
    };

    let body_str = create_free_body(
        code_gen,
        &backend,
        struct_def,
        asm_function_name,
        function_name,
    );
    let body = ASTTypedFunctionBody::ASMBody(body_str);

    let fun_name = format!("{}_{function_name}", struct_def.name);

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
    code_gen: &mut CodeGen,
    backend: &&dyn Backend,
    struct_def: &ASTTypedStructDef,
    asm_function_name: &str,
    function_name: &str,
) -> String {
    let ws = backend.word_size();
    let wl = backend.word_len();

    let mut result = String::new();

    let descr = format!("type {}", struct_def.name);
    let key = code_gen.statics.add_str(&descr);

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

    //println!("dereferencing enum {type_name}");
    CodeGen::add(&mut result, &format!("push {ws} ebx"), None, true);
    CodeGen::add(&mut result, &format!("push {ws} $address"), None, true);
    CodeGen::add(&mut result, "pop ebx", None, true);
    CodeGen::add(&mut result, &format!("mov {ws} ebx, [ebx]"), None, true);
    for (i, property) in struct_def.clone().properties.iter().enumerate() {
        if let Some(name) = CodeGen::get_reference_type_name(&property.type_ref.ast_type) {
            let free = format!("{name}_{function_name}");
            let descr = format!(
                "{descr}, variant {}, type {name}, par {}",
                property.name, property.name
            );
            let key = code_gen.statics.add_str(&descr);
            //println!("dereferencing par {:?}", par);
            CodeGen::add(
                &mut result,
                &format!("push     {ws} [ebx + {}]", i * wl),
                None,
                true,
            );
            CodeGen::add(&mut result, &format!("call     {free}"), None, true);
            CodeGen::add(&mut result, &format!("add      esp,{}", wl), None, true);
        }
    }

    CodeGen::add(&mut result, "pop ebx", None, true);

    result
}