use std::ops::Deref;

use itertools::Itertools;

use super::ast::{
    ASTBuiltinFunctionType, ASTEnumDef, ASTEnumVariantDef, ASTFunctionSignature, ASTModifiers,
    ASTParameterDef, ASTPosition, ASTStructDef, ASTStructPropertyDef, ASTType, BuiltinTypeKind,
};

pub struct BuiltinFunctions;

impl BuiltinFunctions {
    pub fn enum_variant_constructor_signature(
        enum_def: &ASTEnumDef,
        variant: &ASTEnumVariantDef,
    ) -> (Vec<String>, Vec<ASTPosition>, ASTFunctionSignature) {
        let name = variant.name.clone();

        let (parameters_types, parameters_names, parameters_positions) =
            Self::split_parameters(&variant.parameters);

        let param_types: Vec<ASTType> = enum_def
            .type_parameters
            .iter()
            .map(|it| {
                let astposition = ASTPosition::builtin(
                    &enum_def.position,
                    ASTBuiltinFunctionType::Other(format!("generic param type {it}")),
                );
                ASTType::Generic(astposition, it.into(), Vec::new())
            })
            .collect();

        let return_type = ASTType::Custom {
            name: enum_def.name.clone(),
            param_types,
            position: ASTPosition::builtin(
                &enum_def.position,
                ASTBuiltinFunctionType::Other("enum type".to_owned()),
            ),
        };

        let signature = ASTFunctionSignature {
            name,
            generics: enum_def.type_parameters.clone(),
            parameters_types,
            return_type,
            modifiers: ASTModifiers::public(),
        };
        (parameters_names, parameters_positions, signature)
    }

    pub fn match_signature(
        enum_def: &ASTEnumDef,
    ) -> (Vec<String>, Vec<ASTPosition>, ASTFunctionSignature) {
        let name = "match";
        let return_type = ASTType::Generic(
            ASTPosition::builtin(
                &enum_def.position,
                ASTBuiltinFunctionType::Other(format!("{name} generic return type")),
            ),
            "_T".to_owned(),
            Vec::new(),
        );
        let extra_generic = Some("_T".into());

        let generic_types = enum_def
            .type_parameters
            .iter()
            .map(|it| {
                let astposition = ASTPosition::builtin(
                    &enum_def.position,
                    ASTBuiltinFunctionType::Other(format!("{name} generic param type {it}")),
                );
                ASTType::Generic(astposition, it.into(), Vec::new())
            })
            .collect();

        let mut parameters = vec![ASTParameterDef {
            name: "value".into(),
            ast_type: ASTType::Custom {
                name: enum_def.name.clone(),
                param_types: generic_types,
                position: ASTPosition::builtin(
                    &enum_def.position,
                    ASTBuiltinFunctionType::Other(format!("{name} value type")),
                ),
            },
            position: ASTPosition::builtin(
                &enum_def.position,
                ASTBuiltinFunctionType::Other(format!("{name} value param")),
            ),
        }];
        for variant in enum_def.variants.iter() {
            let ast_parameter_def = Self::variant_lambda_parameter(&return_type, variant);
            parameters.push(ast_parameter_def);
        }

        let (parameters_types, parameters_names, parameters_positions) =
            Self::split_parameters(&parameters);

        let mut generics = enum_def.type_parameters.clone();

        if let Some(g) = extra_generic {
            generics.push(g);
        }

        let signature = ASTFunctionSignature {
            name: name.to_owned(),
            generics,
            parameters_types,
            return_type,
            modifiers: ASTModifiers::public(),
        };
        (parameters_names, parameters_positions, signature)
    }

    pub fn match_one_signature(
        enum_def: &ASTEnumDef,
        variant: &ASTEnumVariantDef,
    ) -> (Vec<String>, Vec<ASTPosition>, ASTFunctionSignature) {
        let name = &format!("match{}", variant.name);
        let return_type = ASTType::Generic(
            ASTPosition::builtin(
                &enum_def.position,
                ASTBuiltinFunctionType::Other(format!("{name} generic return type")),
            ),
            "_T".to_owned(),
            Vec::new(),
        );
        let extra_generic = Some("_T".into());

        let generic_types = enum_def
            .type_parameters
            .iter()
            .map(|it| {
                let astposition = ASTPosition::builtin(
                    &enum_def.position,
                    ASTBuiltinFunctionType::Other(format!("{name} generic param type {it}")),
                );
                ASTType::Generic(astposition, it.into(), Vec::new())
            })
            .collect();

        let mut parameters = vec![ASTParameterDef {
            name: "value".into(),
            ast_type: ASTType::Custom {
                name: enum_def.name.clone(),
                param_types: generic_types,
                position: ASTPosition::builtin(
                    &enum_def.position,
                    ASTBuiltinFunctionType::Other(format!("{name} value type")),
                ),
            },
            position: ASTPosition::builtin(
                &enum_def.position,
                ASTBuiltinFunctionType::Other(format!("{name} value param")),
            ),
        }];
        let ast_parameter_def = Self::variant_lambda_parameter(&return_type, variant);
        parameters.push(ast_parameter_def);

        let ast_type = ASTType::Builtin(BuiltinTypeKind::Lambda {
            return_type: Box::new(return_type.clone()),
            parameters: Vec::new(),
        });

        parameters.push(ASTParameterDef {
            name: "elseLambda".to_string(),
            ast_type,
            position: ASTPosition::builtin(
                &enum_def.position,
                ASTBuiltinFunctionType::Other(format!("{name} lambda param")),
            ),
        });
        let mut param_types = enum_def.type_parameters.clone();

        if let Some(g) = extra_generic {
            param_types.push(g);
        }

        let (parameters_types, parameters_names, parameters_positions) =
            Self::split_parameters(&parameters);

        let signature = ASTFunctionSignature {
            name: name.to_owned(),
            generics: param_types,
            parameters_types,
            return_type,
            modifiers: ASTModifiers::public(),
        };
        (parameters_names, parameters_positions, signature)
    }

    pub fn enum_signatures(
        enum_def: &ASTEnumDef,
    ) -> Vec<(ASTFunctionSignature, ASTPosition, ASTBuiltinFunctionType)> {
        let mut result = Vec::new();
        result.push((
            Self::match_signature(enum_def).2,
            enum_def.position.clone(),
            ASTBuiltinFunctionType::Match,
        ));
        for variant in enum_def.variants.iter() {
            result.push((
                Self::enum_variant_constructor_signature(enum_def, variant).2,
                variant.position.clone(),
                ASTBuiltinFunctionType::EnumVariantConstructor,
            ));
            result.push((
                Self::match_one_signature(enum_def, variant).2,
                variant.position.clone(),
                ASTBuiltinFunctionType::MatchOne,
            ));
        }
        result
    }

    pub fn struct_signatures(
        struct_def: &ASTStructDef,
    ) -> Vec<(ASTFunctionSignature, ASTPosition, ASTBuiltinFunctionType)> {
        let mut result = Vec::new();

        result.push((
            Self::struct_constructor_signature(struct_def).2,
            struct_def.position.clone(),
            ASTBuiltinFunctionType::StructConstructor,
        ));
        for property in struct_def.properties.iter() {
            result.extend(
                Self::struct_get_property_signatures(struct_def, property)
                    .into_iter()
                    .map(|((_, _, signature), ft)| (signature, property.position.clone(), ft)),
            );

            result.push((
                Self::struct_set_property_signature(struct_def, property).2,
                property.position.clone(),
                ASTBuiltinFunctionType::StructSetter,
            ));
            result.push((
                Self::struct_set_property_lambda_signature(struct_def, property).2,
                property.position.clone(),
                ASTBuiltinFunctionType::StructLambdaSetter,
            ));
        }
        result
    }

    pub fn struct_constructor_signature(
        struct_def: &ASTStructDef,
    ) -> (Vec<String>, Vec<ASTPosition>, ASTFunctionSignature) {
        let parameters = struct_def
            .properties
            .iter()
            .map(|it| ASTParameterDef {
                name: it.name.clone(),
                ast_type: it.ast_type.clone(),
                position: it.position.clone(),
            })
            .collect();

        let (parameters_types, parameters_names, parameters_positions) =
            Self::split_parameters(&parameters);

        let param_types: Vec<ASTType> = struct_def
            .type_parameters
            .iter()
            .map(|it| {
                let astposition = ASTPosition::builtin(
                    &struct_def.position,
                    ASTBuiltinFunctionType::Other(format!("constructor generic param type {it}")),
                );
                ASTType::Generic(astposition, it.into(), Vec::new())
            })
            .collect();

        let return_type = ASTType::Custom {
            name: struct_def.name.clone(),
            param_types: param_types.clone(),
            // TODO for now here's no source fo generated functions
            position: ASTPosition::builtin(
                &struct_def.position,
                ASTBuiltinFunctionType::Other("return type".to_owned()),
            ),
        };

        let signature = ASTFunctionSignature {
            name: struct_def.name.clone(),
            generics: struct_def.type_parameters.clone(),
            parameters_types,
            return_type,
            modifiers: ASTModifiers::public(),
        };
        (parameters_names, parameters_positions, signature)
    }

    pub fn struct_get_property_signatures(
        struct_def: &ASTStructDef,
        property_def: &ASTStructPropertyDef,
    ) -> Vec<(
        (Vec<String>, Vec<ASTPosition>, ASTFunctionSignature),
        ASTBuiltinFunctionType,
    )> {
        let mut result = Vec::new();

        result.push((
            Self::struct_get_property_signature(struct_def, property_def),
            ASTBuiltinFunctionType::StructGetter,
        ));

        if let ASTType::Builtin(BuiltinTypeKind::Lambda {
            parameters,
            ref return_type,
        }) = &property_def.ast_type
        {
            result.push((
                Self::lambda_struct_property_signature(
                    struct_def,
                    parameters,
                    return_type,
                    &format!("call{}", Self::uppercase_first_letter(&property_def.name)),
                ),
                ASTBuiltinFunctionType::StructLambdaCall,
            ));
        }
        result
    }

    // from https://stackoverflow.com/questions/38406793/why-is-capitalizing-the-first-letter-of-a-string-so-convoluted-in-rust
    fn uppercase_first_letter(s: &str) -> String {
        let mut c = s.chars();
        match c.next() {
            None => String::new(),
            Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
        }
    }

    fn lambda_struct_property_signature(
        struct_def: &ASTStructDef,
        parameters: &Vec<ASTType>,
        return_type: &Box<ASTType>,
        name: &String,
    ) -> (Vec<String>, Vec<ASTPosition>, ASTFunctionSignature) {
        let param_types: Vec<ASTType> = struct_def
            .type_parameters
            .iter()
            .map(|it| {
                let astposition = ASTPosition::builtin(
                    &struct_def.position,
                    ASTBuiltinFunctionType::Other(format!("lambda set generic param type {it}")),
                );
                ASTType::Generic(astposition, it.into(), Vec::new())
            })
            .collect();
        let mut f_parameters = vec![ASTParameterDef {
            name: "v".into(),
            ast_type: ASTType::Custom {
                name: struct_def.name.clone(),
                param_types: param_types.clone(),
                position: ASTPosition::builtin(
                    &struct_def.position,
                    ASTBuiltinFunctionType::Other("lambda value type".to_owned()),
                ),
            },
            position: ASTPosition::builtin(
                &struct_def.position,
                ASTBuiltinFunctionType::Other("lambda param".to_owned()),
            ),
        }];

        let mut lambda_parameters = parameters
            .iter()
            .enumerate()
            .map(|(index, ast_type)| ASTParameterDef {
                name: format!("p{index}"),
                ast_type: ast_type.clone(),
                position: ASTPosition::builtin(
                    &struct_def.position,
                    ASTBuiltinFunctionType::Other(format!("lambda param p{index}")),
                ),
            })
            .collect::<Vec<_>>();

        f_parameters.append(&mut lambda_parameters);

        let lambda_return_type = return_type.deref().clone();

        let (parameters_types, parameters_names, parameters_positions) =
            Self::split_parameters(&f_parameters);

        let signature = ASTFunctionSignature {
            name: name.to_owned(),
            generics: struct_def.type_parameters.clone(),
            parameters_types,
            return_type: lambda_return_type,
            modifiers: ASTModifiers::public(),
        };
        (parameters_names, parameters_positions, signature)
    }

    fn struct_get_property_signature(
        struct_def: &ASTStructDef,
        property_def: &ASTStructPropertyDef,
    ) -> (Vec<String>, Vec<ASTPosition>, ASTFunctionSignature) {
        let param_types: Vec<ASTType> = struct_def
            .type_parameters
            .iter()
            .map(|it| {
                let astposition = ASTPosition::builtin(
                    &property_def.position,
                    ASTBuiltinFunctionType::Other(format!("get generic param type {it}")),
                );
                ASTType::Generic(astposition, it.into(), Vec::new())
            })
            .collect();
        let parameters = vec![ASTParameterDef {
            name: "v".into(),
            ast_type: ASTType::Custom {
                name: struct_def.name.clone(),
                param_types,
                // TODO for now here's no source for generated functions
                position: ASTPosition::builtin(
                    &property_def.position,
                    ASTBuiltinFunctionType::Other("get param type".to_owned()),
                ),
            },
            position: ASTPosition::builtin(
                &property_def.position,
                ASTBuiltinFunctionType::Other("get param".to_owned()),
            ),
        }];

        let (parameters_types, parameters_names, parameters_positions) =
            Self::split_parameters(&parameters);

        let signature = ASTFunctionSignature {
            name: property_def.name.clone(),
            generics: struct_def.type_parameters.clone(),
            parameters_types,
            return_type: property_def.ast_type.clone(),
            modifiers: ASTModifiers::public(),
        };
        (parameters_names, parameters_positions, signature)
    }

    pub fn struct_set_property_signature(
        struct_def: &ASTStructDef,
        property_def: &ASTStructPropertyDef,
    ) -> (Vec<String>, Vec<ASTPosition>, ASTFunctionSignature) {
        let param_types: Vec<ASTType> = struct_def
            .type_parameters
            .iter()
            .map(|it| {
                let astposition = ASTPosition::builtin(
                    &property_def.position,
                    ASTBuiltinFunctionType::Other(format!("set generic param type {it}")),
                );
                ASTType::Generic(astposition, it.into(), Vec::new())
            })
            .collect();

        let ast_type = ASTType::Custom {
            name: struct_def.name.clone(),
            param_types,
            position: ASTPosition::builtin(
                &property_def.position,
                ASTBuiltinFunctionType::Other("set receiver type".to_owned()),
            ),
        };

        let parameters = vec![
            ASTParameterDef {
                name: "receiver".into(),
                ast_type: ast_type.clone(),
                position: ASTPosition::builtin(
                    &property_def.position,
                    ASTBuiltinFunctionType::Other("set receiver param".to_owned()),
                ),
            },
            ASTParameterDef {
                name: "v".into(),
                ast_type: property_def.ast_type.clone(),
                position: ASTPosition::builtin(
                    &property_def.position,
                    ASTBuiltinFunctionType::Other("set value param".to_owned()),
                ),
            },
        ];

        let (parameters_types, parameters_names, parameters_positions) =
            BuiltinFunctions::split_parameters(&parameters);

        let signature = ASTFunctionSignature {
            name: property_def.name.clone(),
            generics: struct_def.type_parameters.clone(),
            parameters_types,
            return_type: ast_type,
            modifiers: ASTModifiers::public(),
        };
        (parameters_names, parameters_positions, signature)
    }

    pub fn struct_set_property_lambda_signature(
        struct_def: &ASTStructDef,
        property_def: &ASTStructPropertyDef,
    ) -> (Vec<String>, Vec<ASTPosition>, ASTFunctionSignature) {
        let param_types: Vec<ASTType> = struct_def
            .type_parameters
            .iter()
            .map(|it| {
                let astposition = ASTPosition::builtin(
                    &property_def.position,
                    ASTBuiltinFunctionType::Other(format!("lambda set generic param type {it}")),
                );
                ASTType::Generic(astposition, it.into(), Vec::new())
            })
            .collect();

        let ast_type = ASTType::Custom {
            name: struct_def.name.clone(),
            param_types,
            position: ASTPosition::builtin(
                &property_def.position,
                ASTBuiltinFunctionType::Other("lambda set receiver type".to_owned()),
            ),
        };

        let lambda = ASTType::Builtin(BuiltinTypeKind::Lambda {
            parameters: vec![property_def.ast_type.clone()],
            return_type: Box::new(property_def.ast_type.clone()),
        });

        let parameters = vec![
            ASTParameterDef {
                name: "receiver".into(),
                ast_type: ast_type.clone(),
                position: ASTPosition::builtin(
                    &property_def.position,
                    ASTBuiltinFunctionType::Other("lambda set receiver param".to_owned()),
                ),
            },
            ASTParameterDef {
                name: "f".into(),
                ast_type: lambda,
                position: ASTPosition::builtin(
                    &property_def.position,
                    ASTBuiltinFunctionType::Other("lambda set lambda param".to_owned()),
                ),
            },
        ];

        let (parameters_types, parameters_names, parameters_positions) =
            BuiltinFunctions::split_parameters(&parameters);

        let signature = ASTFunctionSignature {
            name: property_def.name.clone(),
            generics: struct_def.type_parameters.clone(),
            parameters_types,
            return_type: ast_type,
            modifiers: ASTModifiers::public(),
        };
        (parameters_names, parameters_positions, signature)
    }

    pub fn split_parameters(
        parameters: &Vec<ASTParameterDef>,
    ) -> (Vec<ASTType>, Vec<String>, Vec<ASTPosition>) {
        parameters
            .iter()
            .map(|it| (it.ast_type.clone(), it.name.clone(), it.position.clone()))
            .multiunzip()
    }

    fn variant_lambda_parameter(
        return_type: &ASTType,
        variant: &ASTEnumVariantDef,
    ) -> ASTParameterDef {
        let ast_type = ASTType::Builtin(BuiltinTypeKind::Lambda {
            return_type: Box::new(return_type.clone()),
            parameters: variant
                .parameters
                .iter()
                .map(|it| it.ast_type.clone())
                .collect(),
        });
        ASTParameterDef {
            name: variant.name.clone(),
            ast_type,
            position: ASTPosition::builtin(
                &variant.position,
                ASTBuiltinFunctionType::Other("variant lambda param".to_owned()),
            ),
        }
    }
}
