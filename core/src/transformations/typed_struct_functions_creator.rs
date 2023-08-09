use linked_hash_map::LinkedHashMap;
use log::debug;

use crate::codegen::backend::Backend;
use crate::codegen::statics::Statics;
use crate::codegen::typedef_provider::TypeDefProvider;
use crate::codegen::CodeGen;
use crate::parser::ast::ASTIndex;
use crate::type_check::typed_ast::{
    ASTTypedFunctionBody, ASTTypedFunctionDef, ASTTypedModule, ASTTypedParameterDef,
    ASTTypedStructDef, ASTTypedType,
};

pub fn typed_struct_functions_creator(
    backend: &dyn Backend,
    module: &mut ASTTypedModule,
    statics: &mut Statics,
) {
    for struct_def in module.structs.clone().iter() {
        if struct_has_references(struct_def, module) {
            create_free(backend, struct_def, "deref", "deref", module, statics);
            create_free(backend, struct_def, "addRef", "addRef", module, statics);
        }
    }
}

fn create_free(
    backend: &dyn Backend,
    struct_def: &ASTTypedStructDef,
    asm_function_name: &str,
    function_name: &str,
    module: &mut ASTTypedModule,
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
            ast_index: ASTIndex::none(),
        }],
        body,
        inline: false,
        return_type: None,
        generic_types: LinkedHashMap::new(),
        index: ASTIndex::none(),
    };

    debug!("created function {function_def}");

    module.functions_by_name.insert(fun_name, function_def);
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

    let descr = if backend.debug_asm() {
        format!("type {}", struct_def.name)
    } else {
        String::new()
    };

    let key = statics.add_str(&descr);

    backend.call_function(
        &mut result,
        &format!("{asm_function_name}_0"),
        &[("$address", None), (&format!("[{key}]"), None)],
        Some(&descr),
    );

    if struct_has_references(struct_def, module) {
        CodeGen::add(&mut result, &format!("push {ws} ebx"), None, true);
        CodeGen::add(&mut result, &format!("mov {ws} ebx, $address"), None, true);
        CodeGen::add(&mut result, &format!("mov {ws} ebx, [ebx]"), None, true);
        for (i, property) in struct_def.clone().properties.iter().enumerate() {
            if let Some(name) = CodeGen::get_reference_type_name(&property.ast_type, module) {
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

pub fn struct_has_references(
    stuct_def: &ASTTypedStructDef,
    type_def_provider: &dyn TypeDefProvider,
) -> bool {
    stuct_def
        .properties
        .iter()
        .any(|it| CodeGen::get_reference_type_name(&it.ast_type, type_def_provider).is_some())
}
