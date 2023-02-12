use linked_hash_map::LinkedHashMap;
use log::debug;

use crate::codegen::backend::Backend;
use crate::codegen::statics::Statics;
use crate::codegen::CodeGen;
use crate::type_check::typed_ast::{
    ASTTypedFunctionBody, ASTTypedFunctionDef, ASTTypedModule, ASTTypedParameterDef,
    ASTTypedStructDef, ASTTypedType,
};

pub fn typed_struct_functions_creator(
    backend: &dyn Backend,
    module: &ASTTypedModule,
    statics: &mut Statics,
) -> ASTTypedModule {
    let mut functions_by_name = module.functions_by_name.clone();
    let native_body = module.native_body.clone();

    for struct_def in module.structs.iter() {
        if struct_has_references(struct_def) {
            create_free(
                backend,
                &mut functions_by_name,
                struct_def,
                "deref",
                "deref",
                module,
                statics,
            );
            create_free(
                backend,
                &mut functions_by_name,
                struct_def,
                "addRef",
                "addRef",
                module,
                statics,
            );
        }
    }

    let mut result = module.clone();
    result.functions_by_name = functions_by_name;
    result.native_body = native_body;

    result
}

fn create_free(
    backend: &dyn Backend,
    functions_by_name: &mut LinkedHashMap<String, ASTTypedFunctionDef>,
    struct_def: &ASTTypedStructDef,
    asm_function_name: &str,
    function_name: &str,
    module: &ASTTypedModule,
    statics: &mut Statics,
) {
    let ast_type = ASTTypedType::Struct {
        name: struct_def.name.clone(),
    };

    let body_str = create_free_body(
        &backend,
        struct_def,
        asm_function_name,
        function_name,
        module,
        statics,
    );
    let body = ASTTypedFunctionBody::ASMBody(body_str);

    let fun_name = format!("{}_{function_name}", struct_def.name);

    let function_def = ASTTypedFunctionDef {
        name: fun_name.clone(),
        parameters: vec![ASTTypedParameterDef {
            name: "address".into(),
            ast_type,
        }],
        body,
        inline: false,
        return_type: None,
        generic_types: LinkedHashMap::new(),
    };

    debug!("created function {function_def}");

    functions_by_name.insert(fun_name, function_def);
}

fn create_free_body(
    backend: &&dyn Backend,
    struct_def: &ASTTypedStructDef,
    asm_function_name: &str,
    function_name: &str,
    module: &ASTTypedModule,
    statics: &mut Statics,
) -> String {
    let ws = backend.word_size();
    let wl = backend.word_len();

    let mut result = String::new();

    let descr = format!("type {}", struct_def.name);
    let key = statics.add_str(&descr);

    CodeGen::add(&mut result, "", Some(&descr), true);
    CodeGen::add(&mut result, &format!("push  {ws} [{key}]"), None, true);
    CodeGen::add(&mut result, &format!("push  {ws} $address"), None, true);
    CodeGen::add(
        &mut result,
        &format!("call  {asm_function_name}_0"),
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
            if let Some(name) = CodeGen::get_reference_type_name(&property.ast_type) {
                let descr = &format!("{}.{} : {}", struct_def.name, property.name, name);
                if function_name == "deref" {
                    result.push_str(&backend.call_deref(
                        &format!("[ebx + {}]", i * wl),
                        &name,
                        descr,
                        module,
                        statics,
                    ));
                    result.push('\n');
                } else {
                    backend.call_add_ref(
                        &mut result,
                        &format!("[ebx + {}]", i * wl),
                        &name,
                        descr,
                        module,
                        statics,
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
        .any(|it| CodeGen::get_reference_type_name(&it.ast_type).is_some())
}
