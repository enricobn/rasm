use linked_hash_map::LinkedHashMap;
use log::debug;

use crate::codegen::backend::Backend;
use crate::codegen::statics::Statics;
use crate::codegen::CodeGen;
use crate::parser::ast::ASTIndex;
use crate::type_check::typed_ast::{
    ASTTypedFunctionBody, ASTTypedFunctionDef, ASTTypedModule, ASTTypedParameterDef, ASTTypedType,
    ASTTypedTypeDef, BuiltinTypedTypeKind,
};

pub fn typed_type_functions_creator(
    backend: &dyn Backend,
    module: &mut ASTTypedModule,
    statics: &mut Statics,
) {
    for typed_type_def in module.types.clone().iter() {
        if typed_type_def.is_ref {
            create_free(backend, typed_type_def, "deref", "deref", module, statics);
            create_free(backend, typed_type_def, "addRef", "addRef", module, statics);
        }
    }
}

fn create_free(
    backend: &dyn Backend,
    type_def: &ASTTypedTypeDef,
    asm_function_name: &str,
    function_name: &str,
    module: &mut ASTTypedModule,
    statics: &mut Statics,
) {
    let ast_type = ASTTypedType::Type {
        name: type_def.name.clone(),
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
        original_name: fun_name.clone(),
        parameters: vec![
            ASTTypedParameterDef {
                name: "address".into(),
                ast_type,
                ast_index: ASTIndex::none(),
            },
            ASTTypedParameterDef {
                name: "descr".into(),
                ast_type: ASTTypedType::Builtin(BuiltinTypedTypeKind::String),
                ast_index: ASTIndex::none(),
            },
        ],
        body,
        inline: false,
        return_type: ASTTypedType::Unit,
        generic_types: LinkedHashMap::new(),
        index: ASTIndex::none(),
    };

    debug!("created function {function_def}");

    module.functions_by_name.insert(fun_name, function_def);
}

fn create_free_body(
    backend: &dyn Backend,
    type_def: &ASTTypedTypeDef,
    asm_function_name: &str,
    function_name: &str,
    module: &ASTTypedModule,
    statics: &mut Statics,
) -> String {
    if !type_def.is_ref {
        return String::new();
    }

    let mut result = String::new();
    let descr = format!("type {}", type_def.name);

    backend.call_function(
        &mut result,
        &format!("{asm_function_name}_0"),
        &[("$address", None), ("$descr", None)],
        Some(&descr),
    );

    if type_has_references(type_def) {
        for (i, (_generic_name, generic_type_def)) in type_def.generic_types.iter().enumerate() {
            if let Some(name) = CodeGen::get_reference_type_name(generic_type_def, module) {
                let descr = "$descr";
                let call_deref = if function_name == "deref" {
                    backend.call_deref("[ebx]", &name, descr, module, statics)
                } else {
                    let mut s = String::new();
                    backend.call_add_ref(&mut s, "[ebx]", &name, descr, module, statics);
                    s
                };

                let loop_vec = loop_vec(type_def, backend, call_deref, i);

                result.push_str(&loop_vec);
            }
        }
    }
    result
}

fn loop_vec(
    type_def: &ASTTypedTypeDef,
    backend: &dyn Backend,
    deref_function_call: String,
    generic_n: usize,
) -> String {
    let mut result = String::new();

    CodeGen::add(&mut result, "push  eax", None, true);
    CodeGen::add(&mut result, "push  ebx", None, true);
    CodeGen::add(&mut result, "push  ecx", None, true);

    backend.call_function(
        &mut result,
        &format!("{}References_0", type_def.original_name),
        &[("$address", None), (&format!("{generic_n}"), None)],
        None,
    );

    CodeGen::add(
        &mut result,
        &format!("mov   {} ebx, [eax]", backend.word_size()),
        None,
        true,
    );
    CodeGen::add(
        &mut result,
        &format!("mov   {} ecx, [ebx]", backend.word_size()),
        None,
        true,
    );
    CodeGen::add(
        &mut result,
        &format!("add   ebx, {}", backend.word_len()),
        None,
        true,
    );
    CodeGen::add(&mut result, &format!(".loop_{generic_n}:"), None, false);
    CodeGen::add(
        &mut result,
        &format!("cmp   {} ecx, 0", backend.word_size()),
        None,
        true,
    );
    CodeGen::add(&mut result, &format!("jz   .end_{generic_n}"), None, true);
    result.push_str(&deref_function_call);
    result.push('\n');
    CodeGen::add(
        &mut result,
        &format!("add   ebx, {}", backend.word_len()),
        None,
        true,
    );
    CodeGen::add(&mut result, "dec ecx", None, true);
    CodeGen::add(&mut result, &format!("jmp .loop_{generic_n}"), None, true);
    CodeGen::add(&mut result, &format!(".end_{generic_n}:"), None, false);

    CodeGen::add(&mut result, "pop  ecx", None, true);
    CodeGen::add(&mut result, "pop  ebx", None, true);
    CodeGen::add(&mut result, "pop  eax", None, true);
    result
}

pub fn type_has_references(type_def: &ASTTypedTypeDef) -> bool {
    type_def.is_ref
}
