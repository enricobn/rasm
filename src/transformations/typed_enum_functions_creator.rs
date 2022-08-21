use crate::codegen::backend::Backend;
use crate::codegen::{CodeGen, MemoryValue};
use linked_hash_map::LinkedHashMap;
use log::debug;
use crate::type_check::typed_ast::{ASTTypedEnumDef, ASTTypedEnumVariantDef, ASTTypedFunctionBody, ASTTypedFunctionDef, ASTTypedModule, ASTTypedParameterDef, ASTTypedType, ASTTypedTypeRef, BuiltinTypedTypeKind};

pub fn typed_enum_functions_creator(
    code_gen: &mut CodeGen,
    backend: &dyn Backend,
    module: &ASTTypedModule,
) -> ASTTypedModule {
    let mut functions_by_name = module.functions_by_name.clone();
    let mut native_body = module.native_body.clone();

    for enum_def in module.enums.iter() {
        create_constructors(
            code_gen,
            backend,
            &mut functions_by_name,
            &mut native_body,
            enum_def,
        );

        create_match_like_function(
            backend,
            &mut functions_by_name,
            &enum_def,
            "Match",
            Some(ASTTypedTypeRef { ast_type: ASTTypedType::Enum { name: enum_def.name.clone() }, ast_ref: false }),
        );

        create_match_like_function(
            backend,
            &mut functions_by_name,
            &enum_def,
            "Run",
            None,
        );

        create_free(code_gen, backend, &mut functions_by_name, &enum_def, "deref", "free");
        create_free(code_gen, backend, &mut functions_by_name, &enum_def, "addRef", "addRef");
    }

    let mut result = module.clone();
    result.functions_by_name = functions_by_name;
    result.native_body = native_body;

    result
}

fn create_match_like_function(
    backend: &dyn Backend,
    functions_by_name: &mut LinkedHashMap<String, ASTTypedFunctionDef>,
    enum_def: &&ASTTypedEnumDef,
    name: &str,
    return_type: Option<ASTTypedTypeRef>,
) {
    let body = enum_match_body(backend, &enum_def);

    let function_body = ASTTypedFunctionBody::ASMBody(body);

    let mut parameters = vec![ASTTypedParameterDef {
        name: "value".into(),
        type_ref: ASTTypedTypeRef {
            ast_type: ASTTypedType::Enum {
                name: enum_def.name.clone(),
            },
            ast_ref: true,
        },
    }];
    for variant in enum_def.variants.iter() {
        let ast_type = ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
            return_type: return_type.clone().map(Box::new),
            parameters: variant
                .parameters
                .iter()
                .map(|it| it.type_ref.clone())
                .collect(),
        });
        parameters.push(ASTTypedParameterDef {
            name: variant.name.clone(),
            type_ref: ASTTypedTypeRef {
                ast_type,
                ast_ref: true,
            },
        });
    }

    let function_def = ASTTypedFunctionDef {
        name: enum_def.name.clone() + name,
        parameters,
        body: function_body,
        inline: false,
        return_type,
    };

    debug!("created function {function_def}");

    functions_by_name.insert(
        enum_def.name.clone() + "_" + &name.to_lowercase(),
        function_def,
    );
}

fn create_constructors(
    code_gen: &mut CodeGen,
    backend: &dyn Backend,
    functions_by_name: &mut LinkedHashMap<String, ASTTypedFunctionDef>,
    native_body: &mut String,
    enum_def: &ASTTypedEnumDef,
) {
    for (variant_num, variant) in enum_def.variants.iter().enumerate() {
        //debug!("variant parameters for {} : {:?}", variant.name, variant.parameters);

        let ast_type = ASTTypedType::Enum {
            name: enum_def.name.clone(),
        };
        let type_ref = ASTTypedTypeRef {
            ast_type,
            ast_ref: false,
        };
        let return_type = Some(type_ref);
        let body_str = if variant.parameters.is_empty() {
            enum_non_parametric_variant_body(code_gen, &backend, native_body, enum_def, variant_num, variant)
        } else {
            enum_parametric_variant_constructor_body(code_gen, backend, &variant_num, &variant)
        };
        let body = ASTTypedFunctionBody::ASMBody(body_str);

        let base_name = enum_def.name.split_at(enum_def.name.clone().find(&"_").unwrap()).0;

        let base_name = format!("{base_name}_{}", variant.name);

        let fun_name_o = {
            let same_function = functions_by_name.values().filter(|it| {
                it.name.starts_with(&base_name) &&
                    it.return_type == return_type
            }).collect::<Vec<_>>();

            if same_function.len() > 1 {
                panic!("find more functions that start with {base_name}");
            } else if let Some(f) = same_function.get(0) {
                Some(f.name.clone())
            } else {
                debug!("cannot find a function that starts with {base_name}");
                None
            }
        };

        if let Some(fun_name) = fun_name_o {
            let function_def = ASTTypedFunctionDef {
                name: fun_name.clone(),
                parameters: variant.parameters.clone(),
                body,
                inline: false,
                return_type,
            };

            debug!("created function {function_def}");

            assert!(functions_by_name.insert(
                fun_name.clone(),
                function_def,
            ).is_some());
        }
    }
}

fn create_free(
    code_gen: &mut CodeGen,
    backend: &dyn Backend,
    functions_by_name: &mut LinkedHashMap<String, ASTTypedFunctionDef>,
    enum_def: &ASTTypedEnumDef,
    asm_function_name: &str, function_name: &str,
) {
    let ast_type = ASTTypedType::Enum {
        name: enum_def.name.clone(),
    };
    let type_ref = ASTTypedTypeRef {
        ast_type,
        ast_ref: false,
    };

    let body_str = enum_free_body(code_gen, &backend, enum_def, asm_function_name, function_name);
    let body = ASTTypedFunctionBody::ASMBody(body_str);

    let fun_name = format!("{}_{function_name}", enum_def.name);

    let function_def = ASTTypedFunctionDef {
        name: fun_name.clone(),
        parameters: vec![ASTTypedParameterDef { name: "address".into(), type_ref }],
        body,
        inline: false,
        return_type: None,
    };

    debug!("created function {function_def}");

    functions_by_name.insert(
        fun_name,
        function_def,
    );
}

fn enum_free_body(code_gen: &mut CodeGen, backend: &&dyn Backend, enum_def: &ASTTypedEnumDef, asm_function_name: &str, function_name: &str) -> String {
    let ws = backend.word_size();
    let wl = backend.word_len();

    let mut result = String::new();

    let descr = format!("type {}", enum_def.name);
    let key = code_gen.statics.add_str(&descr);

    CodeGen::add(&mut result, "", Some(&descr), true);
    CodeGen::add(&mut result, &format!("push  {ws} {key}"), None, true);
    CodeGen::add(&mut result, &format!("push  {ws} $address"), None, true);
    CodeGen::add(&mut result, &format!("call  {asm_function_name}"), None, true);
    CodeGen::add(&mut result, &format!("add  {},{}", backend.stack_pointer(), wl * 2), None, true);

    //println!("dereferencing enum {type_name}");
    CodeGen::add(&mut result, &format!("push {ws} ebx"), None, true);
    CodeGen::add(&mut result, &format!("push {ws} $address"), None, true);
    CodeGen::add(&mut result, "pop ebx", None, true);
    CodeGen::add(&mut result, &format!("mov {ws} ebx, [ebx]"), None, true);
    for (i, variant) in enum_def.clone().variants.iter().enumerate() {
        if !variant.parameters.is_empty() {
            CodeGen::add(&mut result, &format!("cmp {ws} [ebx], {}", i), None, true);
            CodeGen::add(&mut result, &format!("jnz ._variant_{i}"), None, true);
            for (j, par) in variant.parameters.iter().rev().enumerate() {
                if let Some(name) = CodeGen::get_reference_type_name(&par.type_ref.ast_type) {
                    let free = format!("{name}_{function_name}");
                    let descr = format!("{descr}, variant {}, type {name}, par {}", variant.name, par.name);
                    let key = code_gen.statics.add_str(&descr);
                    //println!("dereferencing par {:?}", par);
                    CodeGen::add(&mut result, &format!("push     {ws} [ebx + {}]", (j + 1) * wl), None, true);
                    CodeGen::add(&mut result, &format!("call     {free}"), None, true);
                    CodeGen::add(&mut result, &format!("add      esp,{}", wl), None, true);
                }
            }
            CodeGen::add(&mut result, &format!("._variant_{i}:"), None, false);
        }
    }

    CodeGen::add(&mut result, "pop ebx", None, true);


    result
}

fn enum_non_parametric_variant_body(code_gen: &mut CodeGen, backend: &&dyn Backend, native_body: &mut String,
                                    enum_def: &ASTTypedEnumDef, variant_num: usize, variant: &ASTTypedEnumVariantDef) -> String {
    let label = format!("_enum_{}_{}", enum_def.name, variant.name);
    code_gen.statics.insert(label.clone(), MemoryValue::I32Value(0));
    //let all_tab_address_label = format!("_enum_{}_{}_alL_tab_address", enum_def.name, variant.name);
    //self.statics.insert(all_tab_address_label.clone(), MemoryValue::I32Value(0));

    CodeGen::add(
        native_body,
        &format!("push    {} {}", backend.word_size(), backend.word_len()),
        None,
        true,
    );
    CodeGen::add(native_body, "call    malloc", None, true);
    CodeGen::add(
        native_body,
        &format!("add   {}, {}", backend.stack_pointer(), backend.word_len()),
        None,
        true,
    );

    CodeGen::add(
        native_body,
        &format!("mov   {} [{label}], eax", backend.word_size()),
        None,
        true,
    );
    CodeGen::add(
        native_body,
        &format!("mov   {} eax, [eax]", backend.word_size()),
        None,
        true,
    );
    CodeGen::add(
        native_body,
        &format!("mov   {} [eax], {variant_num}", backend.word_size()),
        None,
        true,
    );

    format!("    mov    eax, [{}]\n", label)
}

fn enum_parametric_variant_constructor_body(
    code_gen: &mut CodeGen,
    backend: &dyn Backend,
    variant_num: &usize,
    variant: &&ASTTypedEnumVariantDef,
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

        /*
        if let Some(name) = CodeGen::get_reference_type_name(&par.type_ref.ast_type) {
            let descr = format!("variant {}, par {}", variant.name, par.name);
            code_gen.call_add_ref(&mut body, backend, "ebx", &name, &descr);
        }

         */

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

fn enum_match_body(backend: &dyn Backend, enum_def: &ASTTypedEnumDef) -> String {
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
