use linked_hash_map::LinkedHashMap;

use crate::codegen::backend::Backend;
use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::codegen::CodeGen;
use crate::parser::ast::{
    ASTFunctionBody, ASTFunctionDef, ASTIndex, ASTParameterDef, ASTStructDef, ASTStructPropertyDef,
    ASTType,
};

pub fn struct_functions_creator(
    backend: &dyn Backend,
    module: &EnhancedASTModule,
) -> EnhancedASTModule {
    let mut result = module.clone();

    for struct_def in &module.structs {
        let param_types: Vec<ASTType> = struct_def
            .type_parameters
            .iter()
            .map(|it| ASTType::Parametric(it.into()))
            .collect();

        let ast_type = ASTType::Custom {
            name: struct_def.name.clone(),
            param_types: param_types.clone(),
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
                ast_index: ASTIndex::none(),
            })
            .collect();

        for (i, property_def) in struct_def.properties.iter().enumerate() {
            let property_function =
                create_function_for_struct_property(backend, struct_def, property_def, i);
            result.add_function(
                struct_def.name.clone() + "::" + &property_def.name.clone(),
                property_function,
            );
        }

        let function_def = ASTFunctionDef {
            original_name: struct_def.name.clone(),
            name: struct_def.name.clone(),
            parameters,
            body,
            inline: false,
            return_type,
            param_types: struct_def.type_parameters.clone(),
            // TODO calculate, even if I don't know if it is useful
            resolved_generic_types: LinkedHashMap::new(),
        };
        result.add_function(struct_def.name.clone(), function_def);
    }

    result
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
            i * backend.word_len() as usize
        ),
        None,
        true,
    );
    CodeGen::add(&mut body, "pop   ebx", None, true);
    body
}

fn create_function_for_struct_property(
    backend: &dyn Backend,
    struct_def: &ASTStructDef,
    property_def: &ASTStructPropertyDef,
    i: usize,
) -> ASTFunctionDef {
    let param_types = struct_def
        .type_parameters
        .iter()
        .map(|it| ASTType::Parametric(it.into()))
        .collect();

    let name = struct_def.name.clone() + "_" + &property_def.name;
    ASTFunctionDef {
        original_name: name.clone(),
        name,
        parameters: vec![ASTParameterDef {
            name: "v".into(),
            ast_type: ASTType::Custom {
                name: struct_def.name.clone(),
                param_types,
            },
            ast_index: ASTIndex::none(),
        }],
        return_type: Some(property_def.ast_type.clone()),
        body: ASTFunctionBody::ASMBody(struct_property_body(backend, i)),
        param_types: struct_def.type_parameters.clone(),
        inline: true,
        resolved_generic_types: LinkedHashMap::new(),
    }
}
