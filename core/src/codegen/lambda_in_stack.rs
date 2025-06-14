use std::{collections::HashMap, sync::RwLock};

use once_cell::sync::Lazy;

use crate::{
    codegen::typedef_provider::TypeDefProvider,
    enh_type_check::typed_ast::{ASTTypedType, BuiltinTypedTypeKind},
};

// Global thread-safe cache for type def bodies
static GLOBAL_LAMBDA_IN_STACK: Lazy<RwLock<HashMap<ASTTypedType, bool>>> =
    Lazy::new(|| RwLock::new(HashMap::new()));

/// returns true if the return type of the enclosing function definition,
/// does no contain a lambda, so all the lambdas in the function can be optimized in stack
pub fn can_lambda_be_in_stack(
    function_def_return_type: &ASTTypedType,
    type_def_provider: &dyn TypeDefProvider,
) -> bool {
    can_lambda_be_in_stack_(function_def_return_type, type_def_provider)
}

fn with_cache<F>(key: &ASTTypedType, compute: F) -> bool
where
    F: FnOnce() -> bool,
{
    // Check cache first
    if let Some(cached) = GLOBAL_LAMBDA_IN_STACK.read().unwrap().get(key) {
        return *cached;
    }

    // Set temporary value for recursion protection
    GLOBAL_LAMBDA_IN_STACK
        .write()
        .unwrap()
        .insert(key.clone(), true);

    // Compute actual result
    let result = compute();

    // Update cache with final result
    GLOBAL_LAMBDA_IN_STACK
        .write()
        .unwrap()
        .insert(key.clone(), result);

    result
}

fn can_lambda_be_in_stack_(
    lambda_return_type: &ASTTypedType,
    type_def_provider: &dyn TypeDefProvider,
) -> bool {
    match lambda_return_type {
        ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda { .. }) => false,
        ASTTypedType::Enum { namespace: _, name } => with_cache(lambda_return_type, || {
            type_def_provider
                .get_enum_def_by_name(name)
                .map(|e| {
                    e.variants
                        .iter()
                        .flat_map(|it| it.parameters.iter())
                        .all(|it| can_lambda_be_in_stack_(&it.ast_type, type_def_provider))
                })
                .expect(&format!("Enum {} not found", name))
        }),
        ASTTypedType::Struct { namespace: _, name } => with_cache(lambda_return_type, || {
            type_def_provider
                .get_struct_def_by_name(name)
                .map(|s| {
                    s.properties
                        .iter()
                        .all(|it| can_lambda_be_in_stack_(&it.ast_type, type_def_provider))
                })
                .expect(&format!("Struct {} not found", name))
        }),
        ASTTypedType::Type {
            namespace: _,
            name,
            body: _,
        } => with_cache(lambda_return_type, || {
            type_def_provider
                .get_type_def_by_name(name)
                .map(|t| {
                    t.generic_types
                        .iter()
                        .all(|((_, _), it)| can_lambda_be_in_stack_(&it, type_def_provider))
                })
                .expect(&format!("Type {} not found", name))
        }),

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
            body: String::new(),
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
            ast_type: EnhASTType::Unit,
            ast_typed_type: outer_type.clone(),
            index: EnhASTIndex::none(),
            body: String::new(),
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
