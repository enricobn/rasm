use log::debug;
use std::ops::Deref;

use crate::codegen::backend::BackendAsm;
use crate::codegen::statics::Statics;
use crate::parser::ast::{
    ASTEnumDef, ASTEnumVariantDef, ASTExpression, ASTFunctionBody, ASTFunctionCall, ASTFunctionDef,
    ASTIndex, ASTModule, ASTParameterDef, ASTStatement, ASTStructDef, ASTStructPropertyDef,
    ASTType, BuiltinTypeKind,
};
use crate::type_check::resolved_generic_types::ResolvedGenericTypes;

pub trait FunctionsCreator {
    fn create(&self, module: &mut ASTModule, statics: &mut Statics) {
        for enum_def in module.enums.clone().iter() {
            let param_types: Vec<ASTType> = enum_def
                .type_parameters
                .iter()
                .map(|it| ASTType::Generic(it.into()))
                .collect();

            self.enum_constructors(module, enum_def, &param_types, statics);

            self.create_match_like_function(
                module,
                &enum_def,
                "match",
                ASTType::Generic("_T".into()),
                Some("_T".into()),
            );

            //create_match_like_function(backend, module, &enum_def, "run", ASTType::Unit, None);
        }

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
            let body_str = self.struct_constructor_body(struct_def);
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
                let property_functions = self.create_functions_for_struct_get_property(
                    struct_def,
                    property_def,
                    i,
                    module,
                );

                for f in property_functions {
                    module.add_function(f);
                }

                let property_setter_function = self.create_function_for_struct_set_property(
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

    fn create_match_like_function(
        &self,
        module: &mut ASTModule,
        enum_def: &&ASTEnumDef,
        name: &str,
        return_type: ASTType,
        extra_generic: Option<String>,
    ) {
        let body = self.enum_match_body(name, enum_def);

        let function_body = ASTFunctionBody::ASMBody(body);
        let param_types = enum_def
            .type_parameters
            .iter()
            .map(|it| ASTType::Generic(it.into()))
            .collect();
        let mut parameters = vec![ASTParameterDef {
            name: "value".into(),
            ast_type: ASTType::Custom {
                name: enum_def.name.clone(),
                param_types,
                // TODO for now there's not a source for generated functions
                index: ASTIndex::none(),
            },
            ast_index: ASTIndex::none(),
        }];
        for variant in enum_def.variants.iter() {
            let ast_type = ASTType::Builtin(BuiltinTypeKind::Lambda {
                return_type: Box::new(return_type.clone()),
                parameters: variant
                    .parameters
                    .iter()
                    .map(|it| it.ast_type.clone())
                    .collect(),
            });
            parameters.push(ASTParameterDef {
                name: variant.name.clone(),
                ast_type,
                ast_index: ASTIndex::none(),
            });
        }
        let mut param_types = enum_def.type_parameters.clone();

        if let Some(g) = extra_generic {
            param_types.push(g);
        }

        let function_def = ASTFunctionDef {
            original_name: name.to_owned(),
            name: name.to_owned(),
            parameters,
            body: function_body,
            inline: false,
            return_type,
            generic_types: param_types,
            // TODO calculate, even if I don't know if it's useful
            resolved_generic_types: ResolvedGenericTypes::new(),
            index: ASTIndex::none(),
            modifiers: enum_def.modifiers.clone(),
            namespace: module.namespace.clone(),
        };

        debug!("created function {function_def}");

        module.add_function(function_def);
    }

    fn create_functions_for_struct_get_property(
        &self,
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

            let body = self.struct_lambda_property_rasm_body(name, parameters);

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
                self.create_function_for_struct_get_property(
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
            vec![self.create_function_for_struct_get_property(
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
        &self,
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
            body: ASTFunctionBody::ASMBody(self.struct_property_body(i)),
            generic_types: struct_def.type_parameters.clone(),
            inline: true,
            resolved_generic_types: ResolvedGenericTypes::new(),
            index: property_def.index.clone(),
            modifiers: struct_def.modifiers.clone(),
            namespace: module.namespace.clone(),
        }
    }

    fn create_function_for_struct_set_property(
        &self,
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
            body: ASTFunctionBody::ASMBody(self.struct_setter_body(i)),
            generic_types: struct_def.type_parameters.clone(),
            inline: false,
            resolved_generic_types: ResolvedGenericTypes::new(),
            index: struct_def.index.clone(),
            modifiers: struct_def.modifiers.clone(),
            namespace: module.namespace.clone(),
        }
    }

    fn struct_lambda_property_rasm_body(
        &self,
        name: &str,
        parameters: &[ASTType],
    ) -> Vec<ASTStatement> {
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
                    .map(|(index, _it)| {
                        ASTExpression::ValueRef(format!("p{index}"), ASTIndex::none())
                    })
                    .collect(),
                index: ASTIndex::none(),
                generics: Vec::new(),
            })),
        ]
    }

    fn enum_match_body(&self, name: &str, enum_def: &ASTEnumDef) -> String;

    fn enum_constructors(
        &self,
        module: &mut ASTModule,
        enum_def: &ASTEnumDef,
        param_types: &[ASTType],
        statics: &mut Statics,
    );

    fn struct_constructor_body(&self, struct_def: &ASTStructDef) -> String;

    fn struct_property_body(&self, i: usize) -> String;

    fn struct_setter_body(&self, i: usize) -> String;

    fn enum_parametric_variant_constructor_body(
        &self,
        variant_num: &usize,
        variant: &ASTEnumVariantDef,
        descr_label: &str,
    ) -> String;
}

pub struct FunctionsCreatorAsm<'a> {
    backend: &'a dyn BackendAsm,
}

impl<'a> FunctionsCreatorAsm<'a> {
    pub fn new(backend: &'a dyn BackendAsm) -> Self {
        Self { backend }
    }
}

impl<'a> FunctionsCreator for FunctionsCreatorAsm<'a> {
    fn enum_match_body(&self, name: &str, enum_def: &ASTEnumDef) -> String {
        let word_len = self.backend.word_len();
        let _sp = self.backend.stack_pointer();
        let word_size = self.backend.word_size();
        let mut body = String::new();

        // for debug
        /*
        CodeGen::add(
            &mut body,
            &format!("$call(println,\"executing {}::{}\")", enum_def.name, name),
            None,
            true,
        );

         */

        self.backend.add(&mut body, "push ebx", None, true);
        self.backend.add(
            &mut body,
            &format!("mov {word_size} eax, $value"),
            None,
            true,
        );
        // the address is "inside" the allocation table
        self.backend.add(
            &mut body,
            &format!("mov {word_size} eax, [eax]"),
            None,
            true,
        );

        for (variant_num, variant) in enum_def.variants.iter().enumerate() {
            self.backend.add(
                &mut body,
                &format!("cmp {} [eax], {}", word_size, variant_num),
                None,
                true,
            );
            self.backend.add(
                &mut body,
                &format!("jnz .variant{}", variant_num),
                None,
                true,
            );

            // for debug
            /*
            CodeGen::add(
                &mut body,
                &format!(
                    "$call(println,\"  executing {}::{}\")",
                    enum_def.name, variant.name
                ),
                None,
                true,
            );

             */

            self.backend
                .add(&mut body, &format!("mov ebx,${}", variant.name), None, true);
            self.backend.add(
                &mut body,
                &format!("mov {} ebx,[ebx]", word_size),
                None,
                true,
            );

            let mut args = Vec::new();
            args.push(("ebx".to_owned(), None));

            for (i, param) in variant.parameters.iter().enumerate() {
                args.push((
                    format!("[eax + {}]", (variant.parameters.len() - i) * word_len),
                    Some(format!("param {}", param.name)),
                ));
            }

            self.backend
                .call_function_owned(&mut body, "[ebx]", &args, None);

            self.backend.add(&mut body, "jmp .end", None, true);
            self.backend
                .add(&mut body, &format!(".variant{}:", variant_num), None, false);
        }
        self.backend.add(
            &mut body,
            &format!(
                "$call(print, 1:File, \"{}::{}, invalid value \")",
                enum_def.name, name
            ),
            None,
            false,
        );
        self.backend
            .add(&mut body, "$call(print, 1:File, [eax])", None, false);
        self.backend
            .add(&mut body, "$call(print, 1:File, \"\\n\")", None, false);
        self.backend.add(&mut body, ".end:", None, false);
        self.backend.add(&mut body, "pop ebx", None, true);

        body
    }

    fn enum_constructors(
        &self,
        module: &mut ASTModule,
        enum_def: &ASTEnumDef,
        param_types: &[ASTType],
        statics: &mut Statics,
    ) {
        for (variant_num, variant) in enum_def.variants.iter().enumerate() {
            let ast_type = ASTType::Custom {
                name: enum_def.name.clone(),
                param_types: param_types.to_vec(),
                // TODO for now here's no source fo generated functions
                index: ASTIndex::none(),
            };
            let return_type = ast_type;
            let descr = if self.backend.debug_asm() {
                format!(" for {}::{}", enum_def.name, variant.name)
            } else {
                String::new()
            };

            let body_str = if variant.parameters.is_empty() {
                let label = format!("_enum_{}_{}", enum_def.name, variant.name);
                statics.insert_value_in_heap(&label, &descr, variant_num as i32);

                format!("    mov    eax, [{}]\n", label)
            } else {
                let descr_label = statics.add_str(&descr);
                self.enum_parametric_variant_constructor_body(&variant_num, variant, &descr_label)
            };
            let body = ASTFunctionBody::ASMBody(body_str);

            let name = enum_def.name.clone() + "_" + &variant.name.clone();
            let function_def = ASTFunctionDef {
                original_name: enum_def.name.clone() + "::" + &variant.name.clone(),
                name,
                parameters: variant.parameters.clone(),
                body,
                // TODO we cannot inline parametric variant constructor, but I don't know why
                inline: variant.parameters.is_empty(),
                return_type,
                generic_types: enum_def.type_parameters.clone(),
                // TODO calculate, even if I don't know if it is useful
                resolved_generic_types: ResolvedGenericTypes::new(),
                index: variant.index.clone(),
                modifiers: enum_def.modifiers.clone(),
                namespace: module.namespace.clone(),
            };

            debug!("created function {function_def}");

            module.add_function(function_def);
        }
    }

    fn struct_constructor_body(&self, struct_def: &ASTStructDef) -> String {
        let ws = self.backend.word_size();
        let wl = self.backend.word_len();
        let mut body = String::new();
        let descr = if self.backend.debug_asm() {
            format!(" for {}", struct_def.name)
        } else {
            String::new()
        };

        self.backend.add_rows(
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
            self.backend.add_rows(
                &mut body,
                vec![
                    &format!("mov   ebx, ${}", par.name),
                    &format!(
                        "mov {}  [eax + {}], ebx",
                        self.backend.pointer_size(),
                        i * wl
                    ),
                ],
                Some(&format!("property {}", par.name)),
                true,
            );
        }
        self.backend
            .add_rows(&mut body, vec!["pop   eax", "pop   ebx"], None, true);
        body
    }

    fn struct_property_body(&self, i: usize) -> String {
        let mut body = String::new();
        self.backend.add_rows(
            &mut body,
            vec![
                "push ebx",
                &format!("mov   {} ebx, $v", self.backend.word_size()),
                &format!("mov   {} ebx, [ebx]", self.backend.word_size()),
                &format!(
                    "mov {}  eax, [ebx + {}]",
                    self.backend.pointer_size(),
                    i * self.backend.word_len()
                ),
                "pop   ebx",
            ],
            None,
            true,
        );

        body
    }

    fn struct_setter_body(&self, i: usize) -> String {
        let ws = self.backend.word_size();
        // TODO for now it does not work
        let optimize_copy = false;

        let mut body = String::new();
        self.backend.add(&mut body, "push   ebx", None, true);
        self.backend.add(&mut body, "push   ecx", None, true);

        if optimize_copy {
            self.backend
                .add(&mut body, &format!("mov    {ws} eax,$receiver"), None, true);
            self.backend.add(
                &mut body,
                &format!("mov    {ws} eax,[eax + 12]"),
                None,
                true,
            );

            self.backend
                .add(&mut body, &format!("cmp    {ws} eax,1"), None, true);
            self.backend.add(&mut body, "je     .noClone", None, true);
        }

        self.backend
            .add(&mut body, "$call(copy,$receiver)", None, true);

        if optimize_copy {
            self.backend.add(&mut body, "jmp    .set", None, false);
            self.backend.add(&mut body, ".noClone:", None, false);
            self.backend
                .add(&mut body, "$call(println,\"optimized setter\")", None, true);
            self.backend.add(
                &mut body,
                &format!("mov    {ws} eax, $receiver"),
                None,
                true,
            );
            self.backend.add(&mut body, ".set:", None, false);
        }

        self.backend
            .add(&mut body, &format!("mov   {ws} ebx, $v"), None, true);

        self.backend.add(&mut body, "push   eax", None, true);
        self.backend
            .add(&mut body, &format!("mov {ws} eax,[eax]"), None, true);

        self.backend.add(
            &mut body,
            &format!("mov {ws}  [eax + {}], ebx", i * self.backend.word_len()),
            None,
            true,
        );

        self.backend.add(&mut body, "pop   eax", None, true);

        self.backend.add(&mut body, "pop   ecx", None, true);
        self.backend.add(&mut body, "pop   ebx", None, true);
        body
    }

    fn enum_parametric_variant_constructor_body(
        &self,
        variant_num: &usize,
        variant: &ASTEnumVariantDef,
        descr_label: &str,
    ) -> String {
        let word_size = self.backend.word_size();
        let word_len = self.backend.word_len();
        let mut body = String::new();
        self.backend.add(&mut body, "push ebx", None, true);
        self.backend.add(
            &mut body,
            &format!(
                "$call(malloc, {}, [{descr_label}]: str)",
                (variant.parameters.len() + 1) * word_len
            ),
            None,
            true,
        );
        //CodeGen::add(&mut body, &format!("add esp,{}", word_len), None, true);
        self.backend
            .add(&mut body, &format!("push {word_size} eax"), None, true);
        self.backend
            .add(&mut body, &format!("mov {word_size} eax,[eax]"), None, true);
        // I put the variant number in the first location
        self.backend.add(
            &mut body,
            &format!("mov {}  [eax], {}", word_size, variant_num),
            None,
            true,
        );
        for (i, par) in variant.parameters.iter().rev().enumerate() {
            self.backend.add(
                &mut body,
                &format!("mov   ebx, ${}", par.name),
                Some(&format!("parameter {}", par.name)),
                true,
            );

            self.backend.add(
                &mut body,
                &format!("mov {}  [eax + {}], ebx", word_size, (i + 1) * word_len),
                None,
                true,
            );
        }
        self.backend.add(&mut body, "pop   eax", None, true);

        //CodeGen::call_add_ref(&mut body, backend, "eax", "");

        self.backend.add(&mut body, "pop   ebx", None, true);
        body
    }
}
