use std::ops::Deref;

use itertools::Itertools;

use super::ast::{
    ASTEnumDef, ASTEnumVariantDef, ASTFunctionSignature, ASTParameterDef, ASTPosition,
    ASTStructDef, ASTStructPropertyDef, ASTType, BuiltinTypeKind,
};

pub struct BuiltinFunctions;

impl BuiltinFunctions {
    pub fn enum_variant_constructor_signature(
        enum_def: &ASTEnumDef,
        variant: &ASTEnumVariantDef,
    ) -> (Vec<String>, Vec<ASTPosition>, ASTFunctionSignature) {
        let name = enum_def.name.clone() + "::" + &variant.name.clone();

        let (parameters_types, parameters_names, parameters_positions) =
            Self::split_parameters(&variant.parameters);

        let param_types: Vec<ASTType> = enum_def
            .type_parameters
            .iter()
            .map(|it| {
                let astposition = ASTPosition::none();
                ASTType::Generic(astposition, it.into())
            })
            .collect();

        let return_type = ASTType::Custom {
            name: enum_def.name.clone(),
            param_types,
            // TODO for now here's no source fo generated functions
            index: ASTPosition::none(),
        };

        let signature = ASTFunctionSignature {
            name,
            generics: enum_def.type_parameters.clone(),
            parameters_types,
            return_type,
        };
        (parameters_names, parameters_positions, signature)
    }

    pub fn match_signature(
        enum_def: &ASTEnumDef,
    ) -> (Vec<String>, Vec<ASTPosition>, ASTFunctionSignature) {
        let name = "match";
        let return_type = ASTType::Generic(ASTPosition::none(), "_T".into());
        let extra_generic = Some("_T".into());

        let generic_types = enum_def
            .type_parameters
            .iter()
            .map(|it| ASTType::Generic(ASTPosition::none(), it.into()))
            .collect();

        let mut parameters = vec![ASTParameterDef {
            name: "value".into(),
            ast_type: ASTType::Custom {
                name: enum_def.name.clone(),
                param_types: generic_types,
                // TODO for now there's not a source for generated functions
                index: ASTPosition::none(),
            },
            index: ASTPosition::none(),
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
        };
        (parameters_names, parameters_positions, signature)
    }

    pub fn match_one_signature(
        enum_def: &ASTEnumDef,
        variant: &ASTEnumVariantDef,
    ) -> (Vec<String>, Vec<ASTPosition>, ASTFunctionSignature) {
        let name = &format!("match{}", variant.name);
        let return_type = ASTType::Generic(ASTPosition::none(), "_T".into());
        let extra_generic = Some("_T".into());

        let generic_types = enum_def
            .type_parameters
            .iter()
            .map(|it| ASTType::Generic(ASTPosition::none(), it.into()))
            .collect();

        let mut parameters = vec![ASTParameterDef {
            name: "value".into(),
            ast_type: ASTType::Custom {
                name: enum_def.name.clone(),
                param_types: generic_types,
                // TODO for now there's not a source for generated functions
                index: ASTPosition::none(),
            },
            index: ASTPosition::none(),
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
            index: ASTPosition::none(),
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
        };
        (parameters_names, parameters_positions, signature)
    }

    pub fn enum_signatures(enum_def: &ASTEnumDef) -> Vec<ASTFunctionSignature> {
        let mut result = Vec::new();
        result.push(Self::match_signature(enum_def).2);
        for variant in enum_def.variants.iter() {
            result.push(Self::enum_variant_constructor_signature(enum_def, variant).2);
            result.push(Self::match_one_signature(enum_def, variant).2);
        }
        result
    }

    pub fn struct_signatures(struct_def: &ASTStructDef) -> Vec<ASTFunctionSignature> {
        let mut result = Vec::new();
        result.push(Self::struct_constructor_signature(struct_def).2);
        for property in struct_def.properties.iter() {
            result.extend(
                Self::struct_get_property_signatures(struct_def, property)
                    .into_iter()
                    .map(|it| it.2),
            );
            result.push(Self::struct_set_property_signature(struct_def, property).2);
            result.push(Self::struct_set_property_lambda_signature(struct_def, property).2);
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
                index: it.index.clone(),
            })
            .collect();

        let (parameters_types, parameters_names, parameters_positions) =
            Self::split_parameters(&parameters);

        let param_types: Vec<ASTType> = struct_def
            .type_parameters
            .iter()
            .map(|it| ASTType::Generic(ASTPosition::none(), it.into()))
            .collect();

        let return_type = ASTType::Custom {
            name: struct_def.name.clone(),
            param_types: param_types.clone(),
            // TODO for now here's no source fo generated functions
            index: ASTPosition::none(),
        };

        let signature = ASTFunctionSignature {
            name: struct_def.name.clone(),
            generics: struct_def.type_parameters.clone(),
            parameters_types,
            return_type,
        };
        (parameters_names, parameters_positions, signature)
    }

    pub fn struct_get_property_signatures(
        struct_def: &ASTStructDef,
        property_def: &ASTStructPropertyDef,
    ) -> Vec<(Vec<String>, Vec<ASTPosition>, ASTFunctionSignature)> {
        let mut result = Vec::new();

        result.push(Self::struct_get_property_signature(
            struct_def,
            property_def,
        ));

        if let ASTType::Builtin(BuiltinTypeKind::Lambda {
            parameters,
            ref return_type,
        }) = &property_def.ast_type
        {
            result.push(Self::lambda_struct_property_signature(
                struct_def,
                parameters,
                return_type,
                &property_def.name,
            ));
        }
        result
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
            .map(|it| ASTType::Generic(ASTPosition::none(), it.into()))
            .collect();
        let mut f_parameters = vec![ASTParameterDef {
            name: "v".into(),
            ast_type: ASTType::Custom {
                name: struct_def.name.clone(),
                param_types: param_types.clone(),
                index: ASTPosition::none(),
            },
            index: ASTPosition::none(),
        }];

        let mut lambda_parameters = parameters
            .iter()
            .enumerate()
            .map(|(index, ast_type)| ASTParameterDef {
                name: format!("p{index}"),
                ast_type: ast_type.clone(),
                index: ASTPosition::none(),
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
            .map(|it| ASTType::Generic(ASTPosition::none(), it.into()))
            .collect();
        let parameters = vec![ASTParameterDef {
            name: "v".into(),
            ast_type: ASTType::Custom {
                name: struct_def.name.clone(),
                param_types,
                // TODO for now here's no source for generated functions
                index: ASTPosition::none(),
            },
            index: ASTPosition::none(),
        }];

        let (parameters_types, parameters_names, parameters_positions) =
            Self::split_parameters(&parameters);

        let signature = ASTFunctionSignature {
            name: property_def.name.clone(),
            generics: struct_def.type_parameters.clone(),
            parameters_types,
            return_type: property_def.ast_type.clone(),
        };
        (parameters_names, parameters_positions, signature)
    }

    pub fn struct_set_property_signature(
        struct_def: &ASTStructDef,
        property_def: &ASTStructPropertyDef,
    ) -> (Vec<String>, Vec<ASTPosition>, ASTFunctionSignature) {
        let param_types = struct_def
            .type_parameters
            .iter()
            .map(|it| ASTType::Generic(ASTPosition::none(), it.into()))
            .collect();

        let ast_type = ASTType::Custom {
            name: struct_def.name.clone(),
            param_types,
            // TODO for now here's no source fo generated functions
            index: ASTPosition::none(),
        };

        let parameters = vec![
            ASTParameterDef {
                name: "receiver".into(),
                ast_type: ast_type.clone(),
                index: ASTPosition::none(),
            },
            ASTParameterDef {
                name: "v".into(),
                ast_type: property_def.ast_type.clone(),
                index: ASTPosition::none(),
            },
        ];

        let (parameters_types, parameters_names, parameters_positions) =
            BuiltinFunctions::split_parameters(&parameters);

        let signature = ASTFunctionSignature {
            name: property_def.name.clone(),
            generics: struct_def.type_parameters.clone(),
            parameters_types,
            return_type: ast_type,
        };
        (parameters_names, parameters_positions, signature)
    }

    pub fn struct_set_property_lambda_signature(
        struct_def: &ASTStructDef,
        property_def: &ASTStructPropertyDef,
    ) -> (Vec<String>, Vec<ASTPosition>, ASTFunctionSignature) {
        let param_types = struct_def
            .type_parameters
            .iter()
            .map(|it| ASTType::Generic(ASTPosition::none(), it.into()))
            .collect();

        let ast_type = ASTType::Custom {
            name: struct_def.name.clone(),
            param_types,
            // TODO for now here's no source fo generated functions
            index: ASTPosition::none(),
        };

        let lambda = ASTType::Builtin(BuiltinTypeKind::Lambda {
            parameters: vec![property_def.ast_type.clone()],
            return_type: Box::new(property_def.ast_type.clone()),
        });

        let parameters = vec![
            ASTParameterDef {
                name: "receiver".into(),
                ast_type: ast_type.clone(),
                index: ASTPosition::none(),
            },
            ASTParameterDef {
                name: "f".into(),
                ast_type: lambda,
                index: ASTPosition::none(),
            },
        ];

        let (parameters_types, parameters_names, parameters_positions) =
            BuiltinFunctions::split_parameters(&parameters);

        let signature = ASTFunctionSignature {
            name: property_def.name.clone(),
            generics: struct_def.type_parameters.clone(),
            parameters_types,
            return_type: ast_type,
        };
        (parameters_names, parameters_positions, signature)
    }

    pub fn split_parameters(
        parameters: &Vec<ASTParameterDef>,
    ) -> (Vec<ASTType>, Vec<String>, Vec<ASTPosition>) {
        parameters
            .iter()
            .map(|it| (it.ast_type.clone(), it.name.clone(), it.index.clone()))
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
            index: ASTPosition::none(),
        }
    }
}
