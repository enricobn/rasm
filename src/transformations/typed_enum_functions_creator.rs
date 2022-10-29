use crate::codegen::backend::Backend;
use crate::codegen::statics::Statics;
use crate::codegen::CodeGen;
use crate::type_check::typed_ast::{
    ASTTypedEnumDef, ASTTypedFunctionBody, ASTTypedFunctionDef, ASTTypedModule,
    ASTTypedParameterDef, ASTTypedType, ASTTypedTypeRef,
};
use linked_hash_map::LinkedHashMap;
use log::debug;

pub fn typed_enum_functions_creator(
    backend: &dyn Backend,
    module: &ASTTypedModule,
    statics: &mut Statics,
) -> ASTTypedModule {
    let mut functions_by_name = module.functions_by_name.clone();
    let native_body = module.native_body.clone();

    for enum_def in module.enums.iter() {
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
    enum_def: &ASTTypedEnumDef,
    asm_function_name: &str,
    function_name: &str,
    module: &ASTTypedModule,
    statics: &mut Statics,
) {
    let ast_type = ASTTypedType::Enum {
        name: enum_def.name.clone(),
    };
    let type_ref = ASTTypedTypeRef {
        ast_type,
        ast_ref: false,
    };

    let body_str = create_free_body(
        backend,
        enum_def,
        asm_function_name,
        function_name,
        module,
        statics,
    );
    let body = ASTTypedFunctionBody::ASMBody(body_str);

    let fun_name = format!("{}_{function_name}", enum_def.name);

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
    enum_def: &ASTTypedEnumDef,
    asm_function_name: &str,
    function_name: &str,
    module: &ASTTypedModule,
    statics: &mut Statics,
) -> String {
    let ws = backend.word_size();
    let wl = backend.word_len();

    let mut result = String::new();

    let descr = format!("type {}", enum_def.name);
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

    if enum_has_references(enum_def) {
        CodeGen::add(&mut result, &format!("push {ws} ebx"), None, true);
        CodeGen::add(&mut result, &format!("mov {ws} ebx,$address"), None, true);
        CodeGen::add(&mut result, &format!("mov {ws} ebx, [ebx]"), None, true);
        for (i, variant) in enum_def.clone().variants.iter().enumerate() {
            if !variant.parameters.is_empty() {
                CodeGen::add(&mut result, &format!("cmp {ws} [ebx], {}", i), None, true);
                CodeGen::add(&mut result, &format!("jnz ._variant_{i}"), None, true);
                for (j, par) in variant.parameters.iter().rev().enumerate() {
                    if let Some(name) = CodeGen::get_reference_type_name(&par.type_ref.ast_type) {
                        if function_name == "deref" {
                            result.push_str(&backend.call_deref(
                                &format!("[ebx + {}]", (j + 1) * wl),
                                &name,
                                &format!("dereferencing {}.{} : {}", enum_def.name, par.name, name),
                                module,
                                statics,
                            ));
                            result.push('\n');
                        } else {
                            backend.call_add_ref(
                                &mut result,
                                &format!("[ebx + {}]", (j + 1) * wl),
                                &name,
                                "",
                                module,
                                statics,
                            );
                        }
                    }
                }
                CodeGen::add(&mut result, &format!("._variant_{i}:"), None, false);
            }
        }

        CodeGen::add(&mut result, "pop ebx", None, true);
    }

    result
}

pub fn enum_has_references(enum_def: &ASTTypedEnumDef) -> bool {
    enum_def
        .variants
        .iter()
        .flat_map(|it| it.parameters.iter())
        .any(|it| CodeGen::get_reference_type_name(&it.type_ref.ast_type).is_some())
}
