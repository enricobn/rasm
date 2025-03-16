use rasm_utils::debug_i;

use crate::codegen::asm::code_gen_asm::CodeGenAsm;
use crate::codegen::backend::{Backend, BackendAsm, BackendNasmi386};
use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::codegen::statics::Statics;
use crate::codegen::CodeGen;
use rasm_parser::parser::ast::{
    ASTBuiltinFunctionType, ASTEnumDef, ASTEnumVariantDef, ASTExpression, ASTFunctionBody,
    ASTFunctionCall, ASTFunctionDef, ASTFunctionSignature, ASTModifiers, ASTModule,
    ASTParameterDef, ASTPosition, ASTStatement, ASTStructDef, ASTStructPropertyDef, ASTType,
    BuiltinTypeKind,
};

use crate::codegen::enh_ast::{
    self, EnhASTFunctionDef, EnhASTNameSpace, EnhModuleId, EnhModuleInfo,
};
use rasm_parser::parser::builtin_functions::BuiltinFunctions;

pub trait FunctionsCreator {
    fn create(&self, module: &mut ASTModule, statics: &mut Statics, info: &EnhModuleInfo) {
        for enum_def in module.enums.clone().iter() {
            self.enum_constructors(module, enum_def, statics, info);

            self.create_match_function(module, enum_def);

            for variant in enum_def.variants.iter() {
                self.create_match_one_function(module, enum_def, variant);
            }
        }

        for struct_def in &module.structs.clone() {
            for (i, property_def) in struct_def.properties.iter().enumerate() {
                let property_functions =
                    self.create_functions_for_struct_get_property(struct_def, property_def, i);

                for f in property_functions {
                    module.add_function(f);
                }

                let property_setter_function =
                    self.create_function_for_struct_set_property(struct_def, property_def, i);
                module.add_function(property_setter_function);

                let property_setter_function = self.create_function_for_struct_set_lambda_property(
                    struct_def,
                    property_def,
                    i,
                );
                module.add_function(property_setter_function);
            }

            let body_str = self.struct_constructor_body(struct_def);
            let body = ASTFunctionBody::NativeBody(body_str);

            let (parameters_names, parameters_positions, signature) =
                BuiltinFunctions::struct_constructor_signature(struct_def);

            let mut position = struct_def.position.clone();
            position.builtin = Some(ASTBuiltinFunctionType::StructConstructor);
            let function_def = ASTFunctionDef::from_signature(
                signature,
                false,
                struct_def.modifiers.public,
                position,
                parameters_names,
                parameters_positions,
                body,
                Some(struct_def.name.clone()),
            );

            module.add_function(function_def);
        }
    }

    fn create_globals(&self, module: &mut EnhancedASTModule, statics: &mut Statics);

    fn create_match_function(&self, module: &mut ASTModule, enum_def: &ASTEnumDef) {
        let name = "match";
        let body = self.enum_match_body(name, enum_def);

        let function_body = ASTFunctionBody::NativeBody(body);

        let (parameters_names, parameters_positions, signature) =
            BuiltinFunctions::match_signature(enum_def);

        let function_def = ASTFunctionDef::from_signature(
            signature,
            false,
            enum_def.modifiers.public,
            ASTPosition::builtin(&enum_def.position, ASTBuiltinFunctionType::Match),
            parameters_names,
            parameters_positions,
            function_body,
            Some(enum_def.name.clone()),
        );

        debug_i!("created function {function_def}");

        module.add_function(function_def);
    }

    fn create_match_one_function(
        &self,
        module: &mut ASTModule,
        enum_def: &ASTEnumDef,
        variant: &ASTEnumVariantDef,
    ) {
        let body = self.enum_match_one_body(enum_def, variant);

        let function_body = ASTFunctionBody::NativeBody(body);

        let (parameters_names, parameters_positions, signature) =
            BuiltinFunctions::match_one_signature(enum_def, variant);

        let function_def = ASTFunctionDef::from_signature(
            signature,
            false,
            enum_def.modifiers.public,
            ASTPosition::builtin(&variant.position, ASTBuiltinFunctionType::MatchOne),
            parameters_names,
            parameters_positions,
            function_body,
            Some(enum_def.name.clone()),
        );

        debug_i!("created function {function_def}");

        module.add_function(function_def);
    }

    fn create_functions_for_struct_get_property(
        &self,
        struct_def: &ASTStructDef,
        property_def: &ASTStructPropertyDef,
        i: usize,
    ) -> Vec<ASTFunctionDef> {
        let mut signatures =
            BuiltinFunctions::struct_get_property_signatures(struct_def, property_def);

        let ((parameters_names, parameters_positions, signature), ft) = signatures.remove(0);

        let mut result = Vec::new();

        if let ASTType::Builtin(BuiltinTypeKind::Lambda {
            parameters,
            return_type: _,
        }) = &property_def.ast_type
        {
            let body = self.struct_lambda_property_rasm_body(&property_def, parameters);

            let ((lambda_parameters_names, lambda_parameters_positions, lambda_signature), ft) =
                signatures.remove(0);

            result.push(ASTFunctionDef::from_signature(
                lambda_signature,
                false,
                struct_def.modifiers.public,
                ASTPosition::builtin(&property_def.position, ft),
                lambda_parameters_names,
                lambda_parameters_positions,
                ASTFunctionBody::RASMBody(body),
                None,
            ));
        }

        result.push(self.create_function_for_struct_get_property(
            struct_def,
            property_def,
            i,
            parameters_names,
            parameters_positions,
            signature,
        ));
        result
    }

    fn create_function_for_struct_get_property(
        &self,
        struct_def: &ASTStructDef,
        property_def: &ASTStructPropertyDef,
        i: usize,
        parameters_names: Vec<String>,
        parameters_positions: Vec<ASTPosition>,
        signature: ASTFunctionSignature,
    ) -> ASTFunctionDef {
        let (native_body, inline) = self.struct_property_body(i, &property_def.name);

        ASTFunctionDef::from_signature(
            signature,
            inline,
            struct_def.modifiers.public,
            ASTPosition::builtin(&property_def.position, ASTBuiltinFunctionType::StructGetter),
            parameters_names,
            parameters_positions,
            ASTFunctionBody::NativeBody(native_body),
            Some(struct_def.name.clone()),
        )
    }

    fn create_function_for_struct_set_property(
        &self,
        struct_def: &ASTStructDef,
        property_def: &ASTStructPropertyDef,
        i: usize,
    ) -> ASTFunctionDef {
        let (parameters_names, parameters_positions, signature) =
            BuiltinFunctions::struct_set_property_signature(struct_def, property_def);

        ASTFunctionDef::from_signature(
            signature,
            false,
            struct_def.modifiers.public,
            ASTPosition::builtin(&property_def.position, ASTBuiltinFunctionType::StructSetter),
            parameters_names,
            parameters_positions,
            ASTFunctionBody::NativeBody(self.struct_setter_body(i, &property_def.name)),
            Some(struct_def.name.clone()),
        )
    }

    fn create_function_for_struct_set_lambda_property(
        &self,
        struct_def: &ASTStructDef,
        property_def: &ASTStructPropertyDef,
        i: usize,
    ) -> ASTFunctionDef {
        let (parameters_names, parameters_positions, signature) =
            BuiltinFunctions::struct_set_property_lambda_signature(struct_def, property_def);

        ASTFunctionDef::from_signature(
            signature,
            false,
            struct_def.modifiers.public,
            ASTPosition::builtin(
                &property_def.position,
                ASTBuiltinFunctionType::StructLambdaSetter,
            ),
            parameters_names,
            parameters_positions,
            ASTFunctionBody::NativeBody(self.struct_setter_lambda_body(i, &property_def.name)),
            Some(struct_def.name.clone()),
        )
    }

    fn struct_lambda_property_rasm_body(
        &self,
        property_def: &ASTStructPropertyDef,
        parameters: &[ASTType],
    ) -> Vec<ASTStatement> {
        vec![
            ASTStatement::LetStatement(
                "_f".to_owned(),
                ASTExpression::ASTFunctionCallExpression(ASTFunctionCall::new(
                    format!("{}", property_def.name),
                    vec![ASTExpression::ValueRef(
                        "v".to_owned(),
                        ASTPosition::builtin(
                            &property_def.position,
                            ASTBuiltinFunctionType::Other("v ref".to_owned()),
                        ),
                    )],
                    ASTPosition::builtin(
                        &property_def.position,
                        ASTBuiltinFunctionType::Other("_f let call".to_owned()),
                    ),
                    Vec::new(),
                    None,
                )),
                false,
                ASTPosition::builtin(
                    &property_def.position,
                    ASTBuiltinFunctionType::Other("_f let".to_owned()),
                ),
            ),
            ASTStatement::Expression(ASTExpression::ASTFunctionCallExpression(
                ASTFunctionCall::new(
                    "_f".to_owned(),
                    parameters
                        .iter()
                        .enumerate()
                        .map(|(index, _it)| {
                            ASTExpression::ValueRef(
                                format!("p{index}"),
                                ASTPosition::builtin(
                                    &property_def.position,
                                    ASTBuiltinFunctionType::Other(format!(
                                        "_f call ref to p{index}"
                                    )),
                                ),
                            )
                        })
                        .collect(),
                    ASTPosition::builtin(
                        &property_def.position,
                        ASTBuiltinFunctionType::Other("_f call".to_owned()),
                    ),
                    Vec::new(),
                    None,
                ),
            )),
        ]
    }

    fn enum_match_body(&self, name: &str, enum_def: &ASTEnumDef) -> String;

    fn enum_match_one_body(&self, enum_def: &ASTEnumDef, variant: &ASTEnumVariantDef) -> String;

    fn enum_constructors(
        &self,
        module: &mut ASTModule,
        enum_def: &ASTEnumDef,
        statics: &mut Statics,
        info: &EnhModuleInfo,
    ) {
        for (variant_num, variant) in enum_def.variants.iter().enumerate() {
            let descr = if self.debug() {
                format!(" for {}::{}", enum_def.name, variant.name)
            } else {
                String::new()
            };

            let (body_str, inline) = self.enum_variant_constructor_body(
                module,
                enum_def,
                statics,
                variant_num,
                variant,
                &descr,
                info,
            );
            let body = ASTFunctionBody::NativeBody(body_str);

            let (parameters_names, parameters_positions, signature) =
                BuiltinFunctions::enum_variant_constructor_signature(enum_def, variant);

            let function_def = ASTFunctionDef::from_signature(
                signature,
                // TODO we cannot inline parametric variant constructor, but I don't know why
                inline && variant.parameters.is_empty(),
                enum_def.modifiers.public,
                variant.position.clone(),
                parameters_names,
                parameters_positions,
                body,
                Some(enum_def.name.clone()),
            );
            debug_i!("created function {function_def}");

            module.add_function(function_def);
        }
    }

    fn debug(&self) -> bool;

    fn enum_variant_constructor_body(
        &self,
        module: &mut ASTModule,
        enum_def: &ASTEnumDef,
        statics: &mut Statics,
        variant_num: usize,
        variant: &ASTEnumVariantDef,
        descr: &String,
        info: &EnhModuleInfo,
    ) -> (String, bool);

    fn struct_constructor_body(&self, struct_def: &ASTStructDef) -> String;

    fn struct_property_body(&self, i: usize, name: &str) -> (String, bool);

    fn struct_setter_body(&self, i: usize, name: &str) -> String;

    fn struct_setter_lambda_body(&self, i: usize, name: &str) -> String;
}

pub struct FunctionsCreatorNasmi386 {
    backend: BackendNasmi386,
    debug: bool,
    code_gen: CodeGenAsm,
}

impl FunctionsCreatorNasmi386 {
    pub fn new(backend: BackendNasmi386, debug: bool, code_gen: CodeGenAsm) -> Self {
        Self {
            backend,
            debug,
            code_gen,
        }
    }

    fn str_deref_body(&self, message_key: &str) -> String {
        let mut body_src = String::new();

        self.code_gen.call_function(
            &mut body_src,
            "deref",
            &[("$s", None), (&format!("[{message_key}]"), None)],
            None,
            false,
            false,
        );

        body_src
    }

    fn str_add_ref_body(&self, message_key: &str) -> String {
        let mut body_src = String::new();

        self.code_gen.call_function(
            &mut body_src,
            "addRef",
            &[("$s", None), (&format!("[{message_key}]"), None)],
            None,
            false,
            false,
        );

        body_src
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
        self.code_gen.add(&mut body, "push ebx", None, true);
        self.code_gen.add(
            &mut body,
            &format!(
                "$call(rasmalloc, {}, [{descr_label}]: str)",
                (variant.parameters.len() + 1) * word_len
            ),
            None,
            true,
        );
        //CodeGen::add(&mut body, &format!("add esp,{}", word_len), None, true);
        self.code_gen
            .add(&mut body, &format!("push {word_size} eax"), None, true);
        self.code_gen
            .add(&mut body, &format!("mov {word_size} eax,[eax]"), None, true);
        // I put the variant number in the first location
        self.code_gen.add(
            &mut body,
            &format!("mov {}  [eax], {}", word_size, variant_num),
            None,
            true,
        );
        for (i, par) in variant.parameters.iter().rev().enumerate() {
            self.code_gen.add(
                &mut body,
                &format!("mov   ebx, ${}", par.name),
                Some(&format!("parameter {}", par.name)),
                true,
            );

            self.code_gen.add(
                &mut body,
                &format!("mov {}  [eax + {}], ebx", word_size, (i + 1) * word_len),
                None,
                true,
            );
        }
        self.code_gen.add(&mut body, "pop   eax", None, true);

        //CodeGen::call_add_ref(&mut body, backend, "eax", "");

        self.code_gen.add(&mut body, "pop   ebx", None, true);
        body
    }
}

impl FunctionsCreator for FunctionsCreatorNasmi386 {
    fn create_globals(&self, module: &mut EnhancedASTModule, statics: &mut Statics) {
        let message_key = statics.add_str("String");
        let body_src = self.str_deref_body(&message_key);
        let body = ASTFunctionBody::NativeBody(body_src);
        let name: String = "str_deref".into();
        let function_def = ASTFunctionDef {
            name: name.clone(),
            parameters: vec![ASTParameterDef {
                name: "s".into(),
                ast_type: ASTType::Builtin(BuiltinTypeKind::String),
                position: ASTPosition::builtin(
                    &ASTPosition::none(),
                    ASTBuiltinFunctionType::Other(format!("{name} param")),
                ),
            }],
            body,
            inline: false,
            return_type: ASTType::Unit,
            generic_types: Vec::new(),
            position: ASTPosition::builtin(
                &ASTPosition::none(),
                ASTBuiltinFunctionType::Other(name.to_owned()),
            ),
            modifiers: ASTModifiers::public(),
            target: None,
        };

        module.add_function(
            name,
            EnhASTFunctionDef::from_ast(
                &EnhModuleId::none(),
                &EnhASTNameSpace::global(),
                function_def,
            ),
        );

        let body_src = self.str_add_ref_body(&message_key);

        let body = ASTFunctionBody::NativeBody(body_src);
        let name: String = "str_addRef".into();

        let function_def = ASTFunctionDef {
            name: name.clone(),
            parameters: vec![ASTParameterDef {
                name: "s".into(),
                ast_type: ASTType::Builtin(BuiltinTypeKind::String),
                position: ASTPosition::builtin(
                    &ASTPosition::none(),
                    ASTBuiltinFunctionType::Other(format!("{name} param")),
                ),
            }],
            body,
            inline: false,
            return_type: ASTType::Unit,
            generic_types: Vec::new(),
            position: ASTPosition::builtin(
                &ASTPosition::none(),
                ASTBuiltinFunctionType::Other(name.to_owned()),
            ),
            modifiers: ASTModifiers::public(),
            target: None,
        };

        module.add_function(
            name,
            enh_ast::EnhASTFunctionDef::from_ast(
                &EnhModuleId::none(),
                &EnhASTNameSpace::global(),
                function_def,
            ),
        );
    }

    fn enum_match_body(&self, name: &str, enum_def: &ASTEnumDef) -> String {
        let word_len = self.backend.word_len();
        let word_size = self.backend.word_size();
        let mut body = String::new();

        self.code_gen.add_rows(
            &mut body,
            vec![
                "push ebx",
                &format!("mov {word_size} eax, $value"),
                &format!("mov {word_size} eax, [eax]"),
            ],
            None,
            true,
        );

        for (variant_num, variant) in enum_def.variants.iter().enumerate() {
            self.code_gen.add_rows(
                &mut body,
                vec![
                    &format!("cmp {} [eax], {}", word_size, variant_num),
                    &format!("jne .variant{}", variant_num),
                    &format!("mov ebx,${}", variant.name),
                    &format!("mov {} ebx,[ebx]", word_size),
                ],
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

            self.code_gen
                .call_function_owned(&mut body, "[ebx]", &args, None, false, false);

            self.code_gen.add_rows(
                &mut body,
                vec!["jmp .end", &format!(".variant{}:", variant_num)],
                None,
                true,
            );
        }
        self.code_gen.add_rows(
            &mut body,
            vec![
                &format!(
                    "$call(print, 1:File, \"{}::{}, invalid value \")",
                    enum_def.name, name
                ),
                "$call(print, 1:File, [eax])",
                "$call(print, 1:File, \"\\n\")",
                ".end:",
                "pop ebx",
            ],
            None,
            false,
        );

        body
    }

    fn enum_match_one_body(&self, enum_def: &ASTEnumDef, variant: &ASTEnumVariantDef) -> String {
        let word_len = self.backend.word_len();
        let word_size = self.backend.word_size();
        let mut body = String::new();

        self.code_gen.add_rows(
            &mut body,
            vec![
                "push ebx",
                &format!("mov {word_size} eax, $value"),
                &format!("mov {word_size} eax, [eax]"),
            ],
            None,
            true,
        );

        let variant_num = enum_def
            .variants
            .iter()
            .enumerate()
            .find(|(_i, it)| it == &variant)
            .unwrap()
            .0;

        self.code_gen.add_rows(
            &mut body,
            vec![
                &format!("cmp {} [eax], {}", word_size, variant_num),
                "jne .else",
                &format!("mov ebx,${}", variant.name),
                &format!("mov {} ebx,[ebx]", word_size),
            ],
            None,
            true,
        );

        let mut args: Vec<(String, Option<String>)> = Vec::new();
        args.push(("ebx".to_owned(), None));

        for (i, param) in variant.parameters.iter().enumerate() {
            args.push((
                format!("[eax + {}]", (variant.parameters.len() - i) * word_len),
                Some(format!("param {}", param.name)),
            ));
        }

        self.code_gen
            .call_function_owned(&mut body, "[ebx]", &args, None, false, false);

        self.code_gen.add_rows(
            &mut body,
            vec![
                "jmp .end",
                ".else:",
                "mov ebx,$elseLambda",
                &format!("mov {} ebx,[ebx]", word_size),
            ],
            None,
            true,
        );

        let mut args: Vec<(String, Option<String>)> = Vec::new();
        args.push(("ebx".to_owned(), None));

        self.code_gen
            .call_function_owned(&mut body, "[ebx]", &args, None, false, false);

        self.code_gen
            .add_rows(&mut body, vec![".end:", "pop ebx"], None, false);

        body
    }

    fn debug(&self) -> bool {
        self.backend.debug_asm()
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

        self.code_gen.add_rows(
            &mut body,
            vec![
                "push ebx",
                &format!(
                    "$call(rasmalloc, {}, \"{}\")",
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
            /*
            let mut add_ref_code = String::new();
            if par.ast_type.is_reference_by_module(module) {
                self.backend.call_add_ref_simple(
                    &mut add_ref_code,
                    "ebx",
                    &format!(" for {} property {}", struct_def.name, par.name),
                    statics,
                );
            }
             */
            self.code_gen.add_rows(
                &mut body,
                vec![
                    &format!("mov   ebx, ${}", par.name),
                    // &add_ref_code,
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
        self.code_gen
            .add_rows(&mut body, vec!["pop   eax", "pop   ebx"], None, true);
        body
    }

    fn struct_property_body(&self, i: usize, name: &str) -> (String, bool) {
        let mut body = String::new();
        self.code_gen.add_rows(
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

        (body, true)
    }

    fn struct_setter_body(&self, i: usize, name: &str) -> String {
        let ws = self.backend.word_size();
        // TODO for now it does not work
        let optimize_copy = false;

        let mut body = String::new();
        self.code_gen.add(&mut body, "push   ebx", None, true);

        if optimize_copy {
            self.code_gen
                .add(&mut body, &format!("mov    {ws} eax,$receiver"), None, true);
            self.code_gen.add(
                &mut body,
                &format!("mov    {ws} eax,[eax + 12]"),
                None,
                true,
            );

            self.code_gen
                .add(&mut body, &format!("cmp    {ws} eax,1"), None, true);
            self.code_gen.add(&mut body, "je     .noClone", None, true);
        }

        self.code_gen
            .add(&mut body, "$call(copy,$receiver)", None, true);

        if optimize_copy {
            self.code_gen.add(&mut body, "jmp    .set", None, false);
            self.code_gen.add(&mut body, ".noClone:", None, false);
            self.code_gen
                .add(&mut body, "$call(println,\"optimized setter\")", None, true);
            self.code_gen.add(
                &mut body,
                &format!("mov    {ws} eax, $receiver"),
                None,
                true,
            );
            self.code_gen.add(&mut body, ".set:", None, false);
        }

        self.code_gen
            .add(&mut body, &format!("mov   {ws} ebx, $v"), None, true);

        self.code_gen.add(&mut body, "push   eax", None, true);
        self.code_gen
            .add(&mut body, &format!("mov {ws} eax,[eax]"), None, true);

        self.code_gen.add(
            &mut body,
            &format!("mov {ws}  [eax + {}], ebx", i * self.backend.word_len()),
            None,
            true,
        );

        self.code_gen.add(&mut body, "pop   eax", None, true);

        self.code_gen.add(&mut body, "pop   ebx", None, true);
        body
    }

    fn struct_setter_lambda_body(&self, i: usize, _name: &str) -> String {
        let ws = self.backend.word_size();
        let wl = self.backend.word_len();

        let mut body = String::new();
        self.code_gen.add_rows(
            &mut body,
            vec![
                "push   ebx",
                "$call(copy,$receiver)",
                "push   eax", // saving return value to the stack
                &format!("mov   {ws} eax, [eax]"),
                &format!("push  {ws} [eax + {}]", i * self.backend.word_len()),
                &format!("mov   {ws} ebx, $f"),
                &format!("mov   {ws} ebx, [ebx]"),
                "push   ebx",
                "call   [ebx]",
                &format!("add     esp, {}", 2 * wl),
                "pop    ebx", // in ebx we have the return value
                "push   ebx", // we push it back to the stack
                &format!("mov {ws} ebx,[ebx]"),
                &format!("mov {ws}  [ebx + {}], eax", i * self.backend.word_len()),
                "pop    eax",
                "pop    ebx",
            ],
            None,
            true,
        );

        body
    }

    fn enum_variant_constructor_body(
        &self,
        module: &mut ASTModule,
        enum_def: &ASTEnumDef,
        statics: &mut Statics,
        variant_num: usize,
        variant: &ASTEnumVariantDef,
        descr: &String,
        info: &EnhModuleInfo,
    ) -> (String, bool) {
        if variant.parameters.is_empty() {
            let label = format!(
                "_enum_{}_{}_{}",
                info.namespace.safe_name(),
                enum_def.name,
                variant.name
            );
            statics.insert_value_in_heap(&label, &descr, variant_num as i32);

            (format!("    mov    eax, [{}]\n", label), true)
        } else {
            let descr_label = statics.add_str(&descr);
            (
                self.enum_parametric_variant_constructor_body(&variant_num, variant, &descr_label),
                true,
            )
        }
    }
}
