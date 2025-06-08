use linked_hash_map::LinkedHashMap;

use crate::{
    codegen::typedef_provider::TypeDefProvider,
    enh_type_check::typed_ast::{ASTTypedType, BuiltinTypedTypeKind},
};

/// returns true if the return type of the enclosing function definition,
/// does no contain a lambda, so all the lambdas in the function can be optimized in stack
pub fn can_lambda_be_in_stack(
    function_def_return_type: &ASTTypedType,
    type_def_provider: &dyn TypeDefProvider,
) -> bool {
    let mut already_checked = LinkedHashMap::new();
    can_lambda_be_in_stack_(
        function_def_return_type,
        type_def_provider,
        &mut already_checked,
    )
}

fn can_lambda_be_in_stack_(
    lambda_return_type: &ASTTypedType,
    type_def_provider: &dyn TypeDefProvider,
    already_checked: &mut LinkedHashMap<ASTTypedType, bool>,
) -> bool {
    match lambda_return_type {
        ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda { .. }) => false,
        ASTTypedType::Enum { namespace: _, name } => {
            if let Some(value) = already_checked.get(lambda_return_type) {
                return *value;
            }

            if let Some(e) = type_def_provider.get_enum_def_by_name(name) {
                // for recursion
                already_checked.insert(lambda_return_type.clone(), true);

                let result = e
                    .variants
                    .iter()
                    .flat_map(|it| it.parameters.iter())
                    .all(|it| {
                        can_lambda_be_in_stack_(&it.ast_type, type_def_provider, already_checked)
                    });
                already_checked.insert(lambda_return_type.clone(), result);
                result
            } else {
                panic!();
            }
        }
        ASTTypedType::Struct { namespace: _, name } => {
            if let Some(value) = already_checked.get(lambda_return_type) {
                return *value;
            }

            if let Some(s) = type_def_provider.get_struct_def_by_name(name) {
                // for recursion
                already_checked.insert(lambda_return_type.clone(), true);

                let result = s.properties.iter().all(|it| {
                    can_lambda_be_in_stack_(&it.ast_type, type_def_provider, already_checked)
                });
                already_checked.insert(lambda_return_type.clone(), result);
                result
            } else {
                panic!()
            }
        }
        ASTTypedType::Type {
            namespace: _,
            name,
            is_ref: _,
            native_type: _,
        } => {
            if let Some(value) = already_checked.get(lambda_return_type) {
                return *value;
            }

            if let Some(s) = type_def_provider.get_type_def_by_name(name) {
                // for recursion
                already_checked.insert(lambda_return_type.clone(), true);

                let result = s.generic_types.iter().all(|((_, _), it)| {
                    can_lambda_be_in_stack_(it, type_def_provider, already_checked)
                });
                already_checked.insert(lambda_return_type.clone(), result);
                result
            } else {
                panic!()
            }
        }

        _ => true,
    }
}

#[cfg(test)]
mod tests {
    use rasm_parser::parser::ast::ASTModifiers;

    use crate::{
        codegen::{
            enh_ast::{EnhASTIndex, EnhASTNameSpace, EnhASTType},
            lambda_in_stack::can_lambda_be_in_stack,
            typedef_provider::DummyTypeDefProvider,
        },
        enh_type_check::typed_ast::{
            ASTTypedStructDef, ASTTypedStructPropertyDef, ASTTypedType, ASTTypedTypeDef,
            ResolvedGenericTypedTypes,
        },
    };

    #[test]
    fn test_can_lambda_be_in_stack() {
        let mut structs = Vec::new();

        let ast_typed_type = ASTTypedType::Struct {
            namespace: EnhASTNameSpace::global(),
            name: "Struct".to_owned(),
        };

        structs.push(create_struct(
            "Struct".to_owned(),
            vec![
                ASTTypedStructPropertyDef {
                    name: "s".to_owned(),
                    ast_type: ast_typed_type.clone(),
                },
                ASTTypedStructPropertyDef {
                    name: "f".to_owned(),
                    ast_type: create_lambda(),
                },
            ],
        ));

        let type_def_provider = DummyTypeDefProvider::new(Vec::new(), structs, Vec::new());

        assert!(!can_lambda_be_in_stack(&ast_typed_type, &type_def_provider));
    }

    #[test]
    fn test_can_lambda_be_in_stack_recurse() {
        let mut structs = Vec::new();

        let inner_struct = ASTTypedType::Struct {
            namespace: EnhASTNameSpace::global(),
            name: "InnerStruct".to_owned(),
        };

        let outer_struct = ASTTypedType::Struct {
            namespace: EnhASTNameSpace::global(),
            name: "OuterStruct".to_owned(),
        };

        structs.push(create_struct(
            "InnerStruct".to_owned(),
            vec![ASTTypedStructPropertyDef {
                name: "f".to_owned(),
                ast_type: create_lambda(),
            }],
        ));

        structs.push(create_struct(
            "OuterStruct".to_owned(),
            vec![ASTTypedStructPropertyDef {
                name: "s".to_owned(),
                ast_type: inner_struct,
            }],
        ));

        let type_def_provider = DummyTypeDefProvider::new(Vec::new(), structs, Vec::new());

        assert!(!can_lambda_be_in_stack(&outer_struct, &type_def_provider));
    }

    #[test]
    fn test_can_lambda_be_in_stack_native_type() {
        let mut structs = Vec::new();
        let mut types = Vec::new();

        let outer_type = ASTTypedType::Type {
            namespace: EnhASTNameSpace::global(),
            name: "OuterType".to_owned(),
            is_ref: true,
            native_type: None,
        };

        let inner_struct = ASTTypedType::Struct {
            namespace: EnhASTNameSpace::global(),
            name: "InnerStruct".to_owned(),
        };

        let mut generic_types = ResolvedGenericTypedTypes::new();
        generic_types.insert("T".to_owned(), Vec::new(), inner_struct.clone());

        types.push(ASTTypedTypeDef {
            namespace: EnhASTNameSpace::global(),
            modifiers: ASTModifiers::public(),
            original_name: "OT".to_owned(),
            name: "OuterType".to_owned(),
            generic_types,
            is_ref: true,
            ast_type: EnhASTType::Unit,
            ast_typed_type: outer_type.clone(),
            index: EnhASTIndex::none(),
            native_type: None,
        });

        structs.push(create_struct(
            "InnerStruct".to_owned(),
            vec![ASTTypedStructPropertyDef {
                name: "s".to_owned(),
                ast_type: create_lambda(),
            }],
        ));

        let type_def_provider = DummyTypeDefProvider::new(Vec::new(), structs, types);

        assert!(!can_lambda_be_in_stack(&outer_type, &type_def_provider));
    }

    fn create_struct(
        name: String,
        properties: Vec<ASTTypedStructPropertyDef>,
    ) -> ASTTypedStructDef {
        let ast_typed_type = ASTTypedType::Struct {
            namespace: EnhASTNameSpace::global(),
            name: name.clone(),
        };
        ASTTypedStructDef {
            namespace: EnhASTNameSpace::global(),
            modifiers: ASTModifiers::public(),
            name: name.clone(),
            properties,
            ast_type: EnhASTType::Custom {
                namespace: EnhASTNameSpace::global(),
                name: name,
                param_types: Vec::new(),
                index: EnhASTIndex::none(),
            },
            ast_typed_type: ast_typed_type.clone(),
            index: EnhASTIndex::none(),
        }
    }

    fn create_lambda() -> ASTTypedType {
        ASTTypedType::Builtin(
            crate::enh_type_check::typed_ast::BuiltinTypedTypeKind::Lambda {
                parameters: Vec::new(),
                return_type: Box::new(ASTTypedType::Unit),
            },
        )
    }
}
