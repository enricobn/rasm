use crate::codegen::backend::Backend;
use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::codegen::statics::Statics;
use crate::codegen::{CodeGen, MemoryValue};
use crate::parser::ast::{
    ASTEnumDef, ASTEnumVariantDef, ASTFunctionBody, ASTFunctionDef, ASTParameterDef, ASTType,
    BuiltinTypeKind,
};
use linked_hash_map::LinkedHashMap;
use log::debug;

pub fn enum_functions_creator(
    backend: &dyn Backend,
    module: &EnhancedASTModule,
    statics: &mut Statics,
) -> EnhancedASTModule {
    let mut result = module.clone();

    let mut native_body = module.native_body.clone();

    for enum_def in module.enums.iter() {
        let param_types: Vec<ASTType> = enum_def
            .type_parameters
            .iter()
            .map(|it| ASTType::Parametric(it.into()))
            .collect();

        create_constructors(
            backend,
            &mut result,
            &mut native_body,
            enum_def,
            &param_types,
            statics,
        );

        create_match_like_function(
            backend,
            &mut result,
            &enum_def,
            "match",
            Some(ASTType::Parametric("_T".into())),
            Some("_T".into()),
        );

        create_match_like_function(backend, &mut result, &enum_def, "run", None, None);
    }

    result.native_body = native_body;

    result
}

fn create_match_like_function(
    backend: &dyn Backend,
    module: &mut EnhancedASTModule,
    enum_def: &&ASTEnumDef,
    name: &str,
    return_type: Option<ASTType>,
    extra_generic: Option<String>,
) {
    let body = enum_match_body(backend, enum_def);

    let function_body = ASTFunctionBody::ASMBody(body);
    let param_types = enum_def
        .type_parameters
        .iter()
        .map(|it| ASTType::Parametric(it.into()))
        .collect();
    let mut parameters = vec![ASTParameterDef {
        name: "value".into(),
        ast_type: ASTType::Custom {
            name: enum_def.name.clone(),
            param_types,
        },
    }];
    for variant in enum_def.variants.iter() {
        let ast_type = ASTType::Builtin(BuiltinTypeKind::Lambda {
            return_type: return_type.clone().map(Box::new),
            parameters: variant
                .parameters
                .iter()
                .map(|it| it.ast_type.clone())
                .collect(),
        });
        parameters.push(ASTParameterDef {
            name: variant.name.clone(),
            ast_type,
        });
    }
    let mut param_types = enum_def.type_parameters.clone();

    if let Some(g) = extra_generic {
        param_types.push(g);
    }

    let function_def = ASTFunctionDef {
        name: format!("{}_{}", enum_def.name, name),
        parameters,
        body: function_body,
        inline: false,
        return_type,
        param_types,
        // TODO calculate, even if I don't know if it's useful
        resolved_generic_types: LinkedHashMap::new(),
    };

    debug!("created function {function_def}");

    module.add_function(format!("{}::{}", enum_def.name, name), function_def);
}

fn create_constructors(
    backend: &dyn Backend,
    module: &mut EnhancedASTModule,
    native_body: &mut String,
    enum_def: &ASTEnumDef,
    param_types: &[ASTType],
    statics: &mut Statics,
) {
    for (variant_num, variant) in enum_def.variants.iter().enumerate() {
        //debug!("variant parameters for {} : {:?}", variant.name, variant.parameters);

        let ast_type = ASTType::Custom {
            name: enum_def.name.clone(),
            param_types: param_types.to_vec(),
        };
        let return_type = Some(ast_type);
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
            CodeGen::add(native_body, "$call(malloc)", None, true);
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
            enum_parametric_variant_constructor_body(backend, &variant_num, &variant)
        };
        let body = ASTFunctionBody::ASMBody(body_str);

        let function_def = ASTFunctionDef {
            name: enum_def.name.clone() + "_" + &variant.name.clone(),
            parameters: variant.parameters.clone(),
            body,
            // TODO we cannot inline parametric variant constructor, but I don't know why
            inline: variant.parameters.is_empty(),
            return_type,
            param_types: enum_def.type_parameters.clone(),
            // TODO calculate, even if I don't know if it is useful
            resolved_generic_types: LinkedHashMap::new(),
        };

        debug!("created function {function_def}");

        module.add_function(
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
    CodeGen::add(&mut body, "$call(malloc)", None, true);
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

        CodeGen::add(
            &mut body,
            &format!(
                "add {}, {}",
                sp,
                (variant.parameters.len() + 1) * word_len as usize
            ),
            None,
            true,
        );

        CodeGen::add(&mut body, "jmp .end", None, true);
        CodeGen::add(&mut body, &format!(".variant{}:", variant_num), None, false);
    }
    CodeGen::add(&mut body, ".end:", None, false);
    CodeGen::add(&mut body, "pop ebx", None, true);

    body
}
