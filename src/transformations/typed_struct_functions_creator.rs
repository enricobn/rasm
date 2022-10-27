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
    let native_body = module.native_body.clone();

    for struct_def in module.structs.iter() {
        if struct_has_references(struct_def) {
            create_free(
                code_gen,
                backend,
                &mut functions_by_name,
                struct_def,
                "deref",
                "deref",
                module,
            );
            create_free(
                code_gen,
                backend,
                &mut functions_by_name,
                struct_def,
                "addRef",
                "addRef",
                module,
            );
        }
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
    module: &ASTTypedModule,
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
        module,
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
    module: &ASTTypedModule,
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

    if struct_has_references(struct_def) {
        CodeGen::add(&mut result, &format!("push {ws} ebx"), None, true);
        CodeGen::add(&mut result, &format!("mov {ws} ebx, $address"), None, true);
        CodeGen::add(&mut result, &format!("mov {ws} ebx, [ebx]"), None, true);
        for (i, property) in struct_def.clone().properties.iter().enumerate() {
            if let Some(name) = CodeGen::get_reference_type_name(&property.type_ref.ast_type) {
                if function_name == "deref" {
                    result.push_str(&code_gen.call_deref(
                        &format!("[ebx + {}]", i * wl),
                        &name,
                        &format!(
                            "dereferencing {}.{} : {}",
                            struct_def.name, property.name, name
                        ),
                        module,
                    ));
                    result.push('\n');
                } else {
                    code_gen.call_add_ref(
                        &mut result,
                        *backend,
                        &format!("[ebx + {}]", i * wl),
                        &name,
                        "",
                        module,
                    );
                }
            }
        }

        CodeGen::add(&mut result, "pop ebx", None, true);
    }

    result
}

pub fn struct_has_references(stuct_def: &ASTTypedStructDef) -> bool {
    stuct_def
        .properties
        .iter()
        .any(|it| CodeGen::get_reference_type_name(&it.type_ref.ast_type).is_some())
}
