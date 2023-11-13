use std::ops::Deref;

use crate::codegen::backend::Backend;
use crate::codegen::CodeGen;
use crate::parser::ast::{
    ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef, ASTIndex, ASTModule,
    ASTParameterDef, ASTStatement, ASTStructDef, ASTStructPropertyDef, ASTType, BuiltinTypeKind,
};
use crate::type_check::resolved_generic_types::ResolvedGenericTypes;

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
        let return_type = ast_type;
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
            let property_functions = create_functions_for_struct_get_property(
                backend,
                struct_def,
                property_def,
                i,
                module,
            );

            for f in property_functions {
                module.add_function(f);
            }

            let property_setter_function = create_function_for_struct_set_property(
                backend,
                struct_def,
                property_def,
                i,
                module,
            );
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
            resolved_generic_types: ResolvedGenericTypes::new(),
            index: struct_def.index.clone(),
            modifiers: struct_def.modifiers.clone(),
            namespace: module.namespace.clone(),
        };
        module.add_function(function_def);
    }
}

fn struct_constructor_body(backend: &dyn Backend, struct_def: &ASTStructDef) -> String {
    let ws = backend.word_size();
    let wl = backend.word_len();
    let mut body = String::new();
    let descr = if backend.debug_asm() {
        format!(" for {}", struct_def.name)
    } else {
        String::new()
    };

    CodeGen::add_rows(
        &mut body,
        vec![
            "push ebx",
            &format!(
                "$call(malloc, {}, \"{}\")",
                struct_def.properties.len() * wl,
                descr
            ),
            &format!("push {ws} eax"),
            &format!("mov {ws} eax, [eax]"),
        ],
        None,
        true,
    );

    for (i, par) in struct_def.properties.iter().enumerate() {
        CodeGen::add_rows(
            &mut body,
            vec![
                &format!("mov   ebx, ${}", par.name),
                &format!("mov {}  [eax + {}], ebx", backend.pointer_size(), i * wl),
            ],
            Some(&format!("property {}", par.name)),
            true,
        );
    }
    CodeGen::add_rows(&mut body, vec!["pop   eax", "pop   ebx"], None, true);
    body
}

fn struct_property_body(backend: &dyn Backend, i: usize) -> String {
    let mut body = String::new();
    CodeGen::add_rows(
        &mut body,
        vec![
            "push ebx",
            &format!("mov   {} ebx, $v", backend.word_size()),
            &format!("mov   {} ebx, [ebx]", backend.word_size()),
            &format!(
                "mov {}  eax, [ebx + {}]",
                backend.pointer_size(),
                i * backend.word_len()
            ),
            "pop   ebx",
        ],
        None,
        true,
    );

    body
}

fn struct_lambda_property_rasm_body(name: &str, parameters: &[ASTType]) -> Vec<ASTStatement> {
    vec![
        ASTStatement::LetStatement(
            "_f".to_owned(),
            ASTExpression::ASTFunctionCallExpression(ASTFunctionCall {
                original_function_name: format!("{name}Fn"),
                function_name: format!("{name}Fn"),
                parameters: vec![ASTExpression::ValueRef("v".to_owned(), ASTIndex::none())],
                index: ASTIndex::none(),
                generics: Vec::new(),
            }),
            false,
            ASTIndex::none(),
        ),
        ASTStatement::Expression(ASTExpression::ASTFunctionCallExpression(ASTFunctionCall {
            original_function_name: "_f".to_owned(),
            function_name: "_f".to_owned(),
            parameters: parameters
                .iter()
                .enumerate()
                .map(|(index, _it)| ASTExpression::ValueRef(format!("p{index}"), ASTIndex::none()))
                .collect(),
            index: ASTIndex::none(),
            generics: Vec::new(),
        })),
    ]
}

fn struct_setter_body(backend: &dyn Backend, i: usize) -> String {
    let ws = backend.word_size();
    // TODO for now it does not work
    let optimize_copy = false;

    let mut body = String::new();
    CodeGen::add(&mut body, "push   ebx", None, true);
    CodeGen::add(&mut body, "push   ecx", None, true);

    if optimize_copy {
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

    CodeGen::add(&mut body, "$call(copy,$receiver)", None, true);

    if optimize_copy {
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

fn create_functions_for_struct_get_property(
    backend: &dyn Backend,
    struct_def: &ASTStructDef,
    property_def: &ASTStructPropertyDef,
    i: usize,
    module: &ASTModule,
) -> Vec<ASTFunctionDef> {
    let param_types: Vec<ASTType> = struct_def
        .type_parameters
        .iter()
        .map(|it| ASTType::Generic(it.into()))
        .collect();

    let name = &property_def.name;

    if let ASTType::Builtin(BuiltinTypeKind::Lambda {
        parameters,
        ref return_type,
    }) = &property_def.ast_type
    {
        let mut f_parameters = vec![ASTParameterDef {
            name: "v".into(),
            ast_type: ASTType::Custom {
                name: struct_def.name.clone(),
                param_types: param_types.clone(),
                index: ASTIndex::none(),
            },
            ast_index: ASTIndex::none(),
        }];

        let mut lambda_parameters = parameters
            .iter()
            .enumerate()
            .map(|(index, ast_type)| ASTParameterDef {
                name: format!("p{index}"),
                ast_type: ast_type.clone(),
                ast_index: ASTIndex::none(),
            })
            .collect::<Vec<_>>();

        f_parameters.append(&mut lambda_parameters);

        let lambda_return_type = return_type.deref().clone();

        let body = struct_lambda_property_rasm_body(name, parameters);

        /*
        print!("lambda get property {name}(");
        for param in f_parameters.iter() {
            print!("{param},");
        }
        println!(") -> {:?}", lambda_return_type);

        for stmt in body.iter() {
            println!("  {stmt}");
        }

         */

        vec![
            create_function_for_struct_get_property(
                backend,
                struct_def,
                property_def,
                i,
                param_types,
                format!("{name}Fn"),
                module,
            ),
            ASTFunctionDef {
                original_name: name.clone(),
                name: name.clone(),
                parameters: f_parameters,
                return_type: lambda_return_type.clone(),
                body: ASTFunctionBody::RASMBody(body),
                generic_types: struct_def.type_parameters.clone(),
                inline: false,
                resolved_generic_types: ResolvedGenericTypes::new(),
                index: property_def.index.clone(),
                modifiers: struct_def.modifiers.clone(),
                namespace: module.namespace.clone(),
            },
        ]
    } else {
        vec![create_function_for_struct_get_property(
            backend,
            struct_def,
            property_def,
            i,
            param_types,
            name.to_owned(),
            module,
        )]
    }
}

fn create_function_for_struct_get_property(
    backend: &dyn Backend,
    struct_def: &ASTStructDef,
    property_def: &ASTStructPropertyDef,
    i: usize,
    param_types: Vec<ASTType>,
    name: String,
    module: &ASTModule,
) -> ASTFunctionDef {
    ASTFunctionDef {
        original_name: name.clone(),
        name,
        parameters: vec![ASTParameterDef {
            name: "v".into(),
            ast_type: ASTType::Custom {
                name: struct_def.name.clone(),
                param_types,
                // TODO for now here's no source for generated functions
                index: ASTIndex::none(),
            },
            ast_index: ASTIndex::none(),
        }],
        return_type: property_def.ast_type.clone(),
        body: ASTFunctionBody::ASMBody(struct_property_body(backend, i)),
        generic_types: struct_def.type_parameters.clone(),
        inline: true,
        resolved_generic_types: ResolvedGenericTypes::new(),
        index: property_def.index.clone(),
        modifiers: struct_def.modifiers.clone(),
        namespace: module.namespace.clone(),
    }
}

fn create_function_for_struct_set_property(
    backend: &dyn Backend,
    struct_def: &ASTStructDef,
    property_def: &ASTStructPropertyDef,
    i: usize,
    module: &ASTModule,
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
        return_type: ast_type,
        body: ASTFunctionBody::ASMBody(struct_setter_body(backend, i)),
        generic_types: struct_def.type_parameters.clone(),
        inline: false,
        resolved_generic_types: ResolvedGenericTypes::new(),
        index: struct_def.index.clone(),
        modifiers: struct_def.modifiers.clone(),
        namespace: module.namespace.clone(),
    }
}
