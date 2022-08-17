use crate::codegen::backend::Backend;
use crate::codegen::{CodeGen, EnhancedASTModule, MemoryValue};
use crate::parser::ast::{
    ASTEnumDef, ASTEnumVariantDef, ASTFunctionBody, ASTFunctionDef, ASTParameterDef, ASTType,
    ASTTypeRef, BuiltinTypeKind,
};
use linked_hash_map::LinkedHashMap;
use log::debug;
use crate::codegen::statics::Statics;
use crate::type_check::typed_ast::{ASTTypedEnumDef, ASTTypedEnumVariantDef, ASTTypedFunctionBody, ASTTypedFunctionDef, ASTTypedModule, ASTTypedParameterDef, ASTTypedType, ASTTypedTypeRef, BuiltinTypedTypeKind};

pub fn enum_functions_creator(
    backend: &dyn Backend,
    module: &ASTTypedModule,
) -> ASTTypedModule {
    let mut functions_by_name = module.functions_by_name.clone();
    let mut native_body = module.native_body.clone();
    let mut statics = module.statics.clone();

    for enum_def in module.enums.iter() {
        create_constructors(
            backend,
            &mut functions_by_name,
            &mut native_body,
            &mut statics,
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
    }

    let mut result = module.clone();
    result.functions_by_name = functions_by_name;
    result.native_body = native_body;
    result.statics = statics;

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
    backend: &dyn Backend,
    functions_by_name: &mut LinkedHashMap<String, ASTTypedFunctionDef>,
    native_body: &mut String,
    statics: &mut Statics,
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
            let label = format!("_enum_{}_{}", enum_def.name, variant.name);
            statics.insert(label.clone(), MemoryValue::I32Value(0));
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
        } else {
            enum_parametric_variant_constructor_body(backend, statics, &variant_num, &variant)
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

fn enum_parametric_variant_constructor_body(
    backend: &dyn Backend,
    statics: &mut Statics,
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

        if let Some(_name) = CodeGen::get_reference_type_name(&par.type_ref.ast_type) {
            CodeGen::call_add_ref(statics, &mut body, backend, "ebx", "");
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
