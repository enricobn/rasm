use linked_hash_map::LinkedHashMap;

use crate::codegen::backend::Backend;
use crate::codegen::CodeGen;
use crate::parser::ast::{
    ASTFunctionBody, ASTFunctionDef, ASTIndex, ASTModule, ASTParameterDef, ASTStructDef,
    ASTStructPropertyDef, ASTType,
};

pub fn struct_functions_creator(backend: &dyn Backend, module: &mut ASTModule) {
    for struct_def in &module.structs.clone() {
        let param_types: Vec<ASTType> = struct_def
            .type_parameters
            .iter()
            .map(|it| ASTType::Generic(it.into()))
            .collect();

        let ast_type = ASTType::Custom {
            name: struct_def.name.clone(),
            param_types: param_types.clone(),
            // TODO for now here's no source fo generated functions
            index: ASTIndex::none(),
        };
        let return_type = Some(ast_type);
        let body_str = struct_constructor_body(backend, struct_def);
        let body = ASTFunctionBody::ASMBody(body_str);

        let parameters = struct_def
            .properties
            .iter()
            .map(|it| ASTParameterDef {
                name: it.name.clone(),
                ast_type: it.ast_type.clone(),
                ast_index: it.index.clone(),
            })
            .collect();

        for (i, property_def) in struct_def.properties.iter().enumerate() {
            let property_function =
                create_function_for_struct_get_property(backend, struct_def, property_def, i);
            module.add_function(property_function);

            let property_setter_function =
                create_function_for_struct_set_property(backend, struct_def, property_def, i);
            module.add_function(property_setter_function);
        }

        let function_def = ASTFunctionDef {
            original_name: struct_def.name.clone(),
            name: struct_def.name.clone(),
            parameters,
            body,
            inline: false,
            return_type,
            generic_types: struct_def.type_parameters.clone(),
            // TODO calculate, even if I don't know if it is useful
            resolved_generic_types: LinkedHashMap::new(),
            index: struct_def.index.clone(),
        };
        module.add_function(function_def);
    }
}

fn struct_constructor_body(backend: &dyn Backend, struct_def: &ASTStructDef) -> String {
    let ws = backend.word_size();
    let wl = backend.word_len();
    let mut body = String::new();
    CodeGen::add(&mut body, "push ebx", None, true);
    CodeGen::add(
        &mut body,
        &format!(
            "$call(malloc, {}, \" for {}\")",
            struct_def.properties.len() * wl,
            struct_def.name
        ),
        None,
        true,
    );
    //CodeGen::add(&mut body, &format!("add esp,{}", wl), None, true);
    CodeGen::add(&mut body, &format!("push {ws} eax"), None, true);
    CodeGen::add(&mut body, &format!("mov {ws} eax, [eax]"), None, true);
    for (i, par) in struct_def.properties.iter().enumerate() {
        CodeGen::add(
            &mut body,
            &format!("mov   ebx, ${}", par.name),
            Some(&format!("property {}", par.name)),
            true,
        );
        CodeGen::add(
            &mut body,
            &format!("mov {}  [eax + {}], ebx", backend.pointer_size(), i * wl),
            None,
            true,
        );
    }
    CodeGen::add(&mut body, "pop   eax", None, true);
    CodeGen::add(&mut body, "pop   ebx", None, true);
    body
}

fn struct_property_body(backend: &dyn Backend, i: usize) -> String {
    let mut body = String::new();
    CodeGen::add(&mut body, "push ebx", None, true);
    CodeGen::add(
        &mut body,
        &format!("mov   {} ebx, $v", backend.word_size()),
        None,
        true,
    );
    // the address points to the heap table
    CodeGen::add(
        &mut body,
        &format!("mov   {} ebx, [ebx]", backend.word_size()),
        None,
        true,
    );
    //CodeGen::add(&mut body, "mov   ebx, $v", None, true);
    CodeGen::add(
        &mut body,
        &format!(
            "mov {}  eax, [ebx + {}]",
            backend.pointer_size(),
            i * backend.word_len()
        ),
        None,
        true,
    );
    CodeGen::add(&mut body, "pop   ebx", None, true);
    body
}

fn struct_setter_body(backend: &dyn Backend, i: usize) -> String {
    let ws = backend.word_size();
    // TODO for now it does not work
    let optimize_clone = false;

    let mut body = String::new();
    CodeGen::add(&mut body, "push   ebx", None, true);
    CodeGen::add(&mut body, "push   ecx", None, true);

    if optimize_clone {
        CodeGen::add(&mut body, &format!("mov    {ws} eax,$receiver"), None, true);
        CodeGen::add(
            &mut body,
            &format!("mov    {ws} eax,[eax + 12]"),
            None,
            true,
        );

        CodeGen::add(&mut body, &format!("cmp    {ws} eax,1"), None, true);
        CodeGen::add(&mut body, "je     .noClone", None, true);
    }

    CodeGen::add(&mut body, "$call(clone,$receiver)", None, true);

    if optimize_clone {
        CodeGen::add(&mut body, "jmp    .set", None, false);
        CodeGen::add(&mut body, ".noClone:", None, false);
        CodeGen::add(&mut body, "$call(println,\"optimized setter\")", None, true);
        CodeGen::add(
            &mut body,
            &format!("mov    {ws} eax, $receiver"),
            None,
            true,
        );
        CodeGen::add(&mut body, ".set:", None, false);
    }

    CodeGen::add(&mut body, &format!("mov   {ws} ebx, $v"), None, true);

    CodeGen::add(&mut body, "push   eax", None, true);
    CodeGen::add(&mut body, &format!("mov {ws} eax,[eax]"), None, true);

    CodeGen::add(
        &mut body,
        &format!("mov {ws}  [eax + {}], ebx", i * backend.word_len()),
        None,
        true,
    );

    CodeGen::add(&mut body, "pop   eax", None, true);

    CodeGen::add(&mut body, "pop   ecx", None, true);
    CodeGen::add(&mut body, "pop   ebx", None, true);
    body
}

fn create_function_for_struct_get_property(
    backend: &dyn Backend,
    struct_def: &ASTStructDef,
    property_def: &ASTStructPropertyDef,
    i: usize,
) -> ASTFunctionDef {
    let param_types = struct_def
        .type_parameters
        .iter()
        .map(|it| ASTType::Generic(it.into()))
        .collect();

    let name = &property_def.name;
    ASTFunctionDef {
        original_name: name.clone(),
        name: name.clone(),
        parameters: vec![ASTParameterDef {
            name: "v".into(),
            ast_type: ASTType::Custom {
                name: struct_def.name.clone(),
                param_types,
                // TODO for now here's no source fo generated functions
                index: ASTIndex::none(),
            },
            ast_index: ASTIndex::none(),
        }],
        return_type: Some(property_def.ast_type.clone()),
        body: ASTFunctionBody::ASMBody(struct_property_body(backend, i)),
        generic_types: struct_def.type_parameters.clone(),
        inline: true,
        resolved_generic_types: LinkedHashMap::new(),
        index: property_def.index.clone(),
    }
}

fn create_function_for_struct_set_property(
    backend: &dyn Backend,
    struct_def: &ASTStructDef,
    property_def: &ASTStructPropertyDef,
    i: usize,
) -> ASTFunctionDef {
    let param_types = struct_def
        .type_parameters
        .iter()
        .map(|it| ASTType::Generic(it.into()))
        .collect();

    let name = &property_def.name;
    let ast_type = ASTType::Custom {
        name: struct_def.name.clone(),
        param_types,
        // TODO for now here's no source fo generated functions
        index: ASTIndex::none(),
    };
    ASTFunctionDef {
        original_name: name.clone(),
        name: name.clone(),
        parameters: vec![
            ASTParameterDef {
                name: "receiver".into(),
                ast_type: ast_type.clone(),
                ast_index: ASTIndex::none(),
            },
            ASTParameterDef {
                name: "v".into(),
                ast_type: property_def.ast_type.clone(),
                ast_index: ASTIndex::none(),
            },
        ],
        return_type: Some(ast_type),
        body: ASTFunctionBody::ASMBody(struct_setter_body(backend, i)),
        generic_types: struct_def.type_parameters.clone(),
        inline: false,
        resolved_generic_types: LinkedHashMap::new(),
        index: struct_def.index.clone(),
    }
}
