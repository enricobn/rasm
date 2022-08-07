use linked_hash_map::LinkedHashMap;
use crate::codegen::backend::Backend;
use crate::codegen::{CodeGen, EnhancedASTModule, MemoryValue};
use crate::parser::ast::{
    ASTEnumDef, ASTEnumVariantDef, ASTFunctionBody, ASTFunctionDef, ASTParameterDef, ASTType,
    ASTTypeRef, BuiltinTypeKind,
};

pub fn enum_functions_creator(
    backend: &dyn Backend,
    module: &EnhancedASTModule,
) -> EnhancedASTModule {
    let mut functions_by_name = module.functions_by_name.clone();
    let mut native_body = module.native_body.clone();
    let mut statics = module.statics.clone();

    for enum_def in module.enums.iter() {
        let param_types: Vec<ASTTypeRef> = enum_def
            .type_parameters
            .iter()
            .map(|it| ASTTypeRef::parametric(it, false))
            .collect();

        create_constructors(backend, &mut functions_by_name, &mut native_body, &mut statics, enum_def, &param_types);

        create_match(backend, &mut functions_by_name, &enum_def, &param_types);

        // TODO is very similar to match, the difference is the return type of the lambda, evene the asm code is equal
        create_run(backend, &mut functions_by_name, &enum_def, &param_types);
    }

    let mut result = module.clone();
    result.functions_by_name = functions_by_name;
    result.native_body = native_body;
    result.statics = statics;

    result
}

fn create_run(backend: &dyn Backend, functions_by_name: &mut LinkedHashMap<String, ASTFunctionDef>, enum_def: &&ASTEnumDef, param_types: &Vec<ASTTypeRef>) {
    let return_type = Some(ASTTypeRef::custom(&enum_def.name, false, param_types.clone()));

    let body = enum_match_body(backend, &enum_def);

    let function_body = ASTFunctionBody::ASMBody(body);
    let param_types = enum_def
        .type_parameters
        .iter()
        .map(|it| ASTTypeRef::parametric(it, false))
        .collect();
    let mut parameters = vec![ASTParameterDef {
        name: "value".into(),
        type_ref: ASTTypeRef {
            ast_type: ASTType::Custom {
                name: enum_def.name.clone(),
                param_types,
            },
            ast_ref: true,
        },
    }];
    for variant in enum_def.variants.iter() {
        let ast_type = ASTType::Builtin(BuiltinTypeKind::Lambda {
            return_type: None,
            parameters: variant
                .parameters
                .iter()
                .map(|it| it.type_ref.clone())
                .collect(),
        });
        parameters.push(ASTParameterDef {
            name: variant.name.clone(),
            type_ref: ASTTypeRef {
                ast_type,
                ast_ref: true,
            },
        });
    }
    let function_def = ASTFunctionDef {
        name: enum_def.name.clone() + "Run",
        parameters,
        body: function_body,
        inline: false,
        return_type,
        param_types: enum_def.type_parameters.clone(),
    };
    functions_by_name.insert(enum_def.name.clone() + "::run", function_def);
}

fn create_match(backend: &dyn Backend, functions_by_name: &mut LinkedHashMap<String, ASTFunctionDef>, enum_def: &&ASTEnumDef, param_types: &Vec<ASTTypeRef>) {
    let return_type = Some(ASTTypeRef::custom(&enum_def.name, false, param_types.clone()));

    let body = enum_match_body(backend, &enum_def);

    let function_body = ASTFunctionBody::ASMBody(body);
    let param_types = enum_def
        .type_parameters
        .iter()
        .map(|it| ASTTypeRef::parametric(it, false))
        .collect();
    let mut parameters = vec![ASTParameterDef {
        name: "value".into(),
        type_ref: ASTTypeRef {
            ast_type: ASTType::Custom {
                name: enum_def.name.clone(),
                param_types,
            },
            ast_ref: true,
        },
    }];
    for variant in enum_def.variants.iter() {
        let ast_type = ASTType::Builtin(BuiltinTypeKind::Lambda {
            return_type: return_type.clone().map(Box::new),
            parameters: variant
                .parameters
                .iter()
                .map(|it| it.type_ref.clone())
                .collect(),
        });
        parameters.push(ASTParameterDef {
            name: variant.name.clone(),
            type_ref: ASTTypeRef {
                ast_type,
                ast_ref: true,
            },
        });
    }
    let function_def = ASTFunctionDef {
        name: enum_def.name.clone() + "Match",
        parameters,
        body: function_body,
        inline: false,
        return_type,
        param_types: enum_def.type_parameters.clone(),
    };
    functions_by_name.insert(enum_def.name.clone() + "::match", function_def);
}

fn create_constructors(backend: &dyn Backend, functions_by_name: &mut LinkedHashMap<String, ASTFunctionDef>, mut native_body: &mut String, statics: &mut LinkedHashMap<String, MemoryValue>, enum_def: &ASTEnumDef, param_types: &Vec<ASTTypeRef>) {
    for (variant_num, variant) in enum_def.variants.iter().enumerate() {
        //debug!("variant parameters for {} : {:?}", variant.name, variant.parameters);

        let ast_type = ASTType::Custom {
            name: enum_def.name.clone(),
            param_types: param_types.clone(),
        };
        let type_ref = ASTTypeRef {
            ast_type,
            ast_ref: true,
        };
        let return_type = Some(type_ref);
        let body_str = if variant.parameters.is_empty() {
            let label = format!("_enum_{}_{}", enum_def.name, variant.name);
            statics.insert(label.clone(), MemoryValue::I32Value(0));
            //let all_tab_address_label = format!("_enum_{}_{}_alL_tab_address", enum_def.name, variant.name);
            //self.statics.insert(all_tab_address_label.clone(), MemoryValue::I32Value(0));

            CodeGen::add(
                &mut native_body,
                &format!("push    {} {}", backend.word_size(), backend.word_len()),
                None,
                true,
            );
            CodeGen::add(&mut native_body, "call    malloc", None, true);
            CodeGen::add(
                &mut native_body,
                &format!("add   {}, {}", backend.stack_pointer(), backend.word_len()),
                None,
                true,
            );

            CodeGen::add(
                &mut native_body,
                &format!("mov   {} [{label}], eax", backend.word_size()),
                None,
                true,
            );
            CodeGen::add(
                &mut native_body,
                &format!("mov   {} eax, [eax]", backend.word_size()),
                None,
                true,
            );
            CodeGen::add(
                &mut native_body,
                &format!("mov   {} [eax], {variant_num}", backend.word_size()),
                None,
                true,
            );

            format!("    mov    eax, [{}]\n", label)
        } else {
            enum_parametric_variant_constructor_body(backend, &variant_num, &variant)
        };
        let body = ASTFunctionBody::ASMBody(body_str);

        let function_def = ASTFunctionDef {
            name: enum_def.name.clone() + "_" + &variant.name.clone(),
            parameters: variant.parameters.clone(),
            body,
            inline: false,
            return_type,
            param_types: enum_def.type_parameters.clone(),
        };
        functions_by_name.insert(
            enum_def.name.clone() + "::" + &variant.name.clone(),
            function_def,
        );
    }
}

fn enum_parametric_variant_constructor_body(
    backend: &dyn Backend,
    variant_num: &usize,
    variant: &&ASTEnumVariantDef,
) -> String {
    let word_size = backend.word_size();
    let word_len = backend.word_len();
    let mut body = String::new();
    CodeGen::add(&mut body, "push ebx", None, true);
    CodeGen::add(
        &mut body,
        &format!(
            "push     {}",
            (variant.parameters.len() + 1) * word_len as usize
        ),
        None,
        true,
    );
    CodeGen::add(&mut body, "call malloc", None, true);
    CodeGen::add(&mut body, &format!("add esp,{}", word_len), None, true);
    CodeGen::add(&mut body, &format!("push {word_size} eax"), None, true);
    CodeGen::add(&mut body, &format!("mov {word_size} eax,[eax]"), None, true);
    // I put the variant number in the first location
    CodeGen::add(
        &mut body,
        &format!("mov   [eax], word {}", variant_num),
        None,
        true,
    );
    for (i, par) in variant.parameters.iter().rev().enumerate() {
        CodeGen::add(
            &mut body,
            &format!("mov   ebx, ${}", par.name),
            Some(&format!("parameter {}", par.name)),
            true,
        );
        if let ASTType::Custom {
            name: _,
            param_types: _,
        } = &par.type_ref.ast_type
        {
            CodeGen::call_add_ref(&mut body, backend, "ebx", "");
        } else if let ASTType::Parametric(_name) = &par.type_ref.ast_type {
            //println!("Parametric({name}) variant parameter");
            CodeGen::call_add_ref(&mut body, backend, "ebx", "");
        }
        CodeGen::add(
            &mut body,
            &format!(
                "mov {}  [eax + {}], ebx",
                word_size,
                (i + 1) * word_len as usize
            ),
            None,
            true,
        );
    }
    CodeGen::add(&mut body, "pop   eax", None, true);

    //CodeGen::call_add_ref(&mut body, backend, "eax", "");

    CodeGen::add(&mut body, "pop   ebx", None, true);
    body
}

fn enum_match_body(backend: &dyn Backend, enum_def: &ASTEnumDef) -> String {
    let word_len = backend.word_len();
    let sp = backend.stack_pointer();
    let word_size = backend.word_size();
    let mut body = String::new();

    CodeGen::add(&mut body, "push ebx", None, true);
    CodeGen::add(
        &mut body,
        &format!("mov {word_size} eax, $value"),
        None,
        true,
    );
    // the address is "inside" the allocation table
    CodeGen::add(
        &mut body,
        &format!("mov {word_size} eax, [eax]"),
        None,
        true,
    );

    for (variant_num, variant) in enum_def.variants.iter().enumerate() {
        CodeGen::add(
            &mut body,
            &format!("cmp [eax], word {}", variant_num),
            None,
            true,
        );
        CodeGen::add(
            &mut body,
            &format!("jnz .variant{}", variant_num),
            None,
            true,
        );

        for (i, param) in variant.parameters.iter().enumerate() {
            CodeGen::add(
                &mut body,
                &format!("push dword [eax + {}]", (i + 1) * word_len as usize),
                Some(&format!("param {}", param.name)),
                true,
            );
        }

        CodeGen::add(&mut body, &format!("mov ebx,${}", variant.name), None, true);
        CodeGen::add(&mut body, "push ebx", None, true);
        CodeGen::add(&mut body, "call [ebx]", None, true);
        CodeGen::add(&mut body, &format!("add {}, {}", sp, word_len), None, true);

        if !variant.parameters.is_empty() {
            CodeGen::add(
                &mut body,
                &format!(
                    "add {}, {}",
                    sp,
                    variant.parameters.len() * word_len as usize
                ),
                None,
                true,
            );
        }

        CodeGen::add(&mut body, "jmp .end", None, true);
        CodeGen::add(&mut body, &format!(".variant{}:", variant_num), None, true);
    }
    CodeGen::add(&mut body, ".end:", None, true);
    CodeGen::add(&mut body, "pop ebx", None, true);

    body
}
