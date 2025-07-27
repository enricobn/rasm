use linked_hash_map::LinkedHashMap;
use rasm_utils::{debug_i, dedent, indent};

use crate::{
    codegen::{
        enh_ast::{
            EnhASTEnumDef, EnhASTEnumVariantDef, EnhASTNameSpace, EnhASTParameterDef,
            EnhASTStructDef, EnhASTStructPropertyDef, EnhASTType, EnhASTTypeDef,
            EnhBuiltinTypeKind,
        },
        enhanced_module::EnhancedASTModule,
        typedef_provider::TypeDefProvider,
    },
    enh_type_check::{
        enh_functions_container::EnhTypeFilter,
        enh_resolved_generic_types::EnhResolvedGenericTypes,
        typed_ast::{
            ASTTypedEnumDef, ASTTypedEnumVariantDef, ASTTypedParameterDef, ASTTypedStructDef,
            ASTTypedStructPropertyDef, ASTTypedType, ASTTypedTypeDef, BuiltinTypedTypeKind,
            ResolvedGenericTypedTypes,
        },
    },
    type_check::substitute,
};

pub struct ConvContext<'a> {
    module: &'a EnhancedASTModule,
    enums: LinkedHashMap<EnhASTType, ASTTypedType>,
    structs: LinkedHashMap<EnhASTType, ASTTypedType>,
    types: LinkedHashMap<EnhASTType, ASTTypedType>,
    pub enum_defs: Vec<ASTTypedEnumDef>,
    pub struct_defs: Vec<ASTTypedStructDef>,
    pub type_defs: Vec<ASTTypedTypeDef>,
    count: usize,
}

impl<'a> TypeDefProvider for ConvContext<'a> {
    fn enums(&self) -> &[ASTTypedEnumDef] {
        &self.enum_defs
    }

    fn structs(&self) -> &[ASTTypedStructDef] {
        &self.struct_defs
    }

    fn types(&self) -> &[ASTTypedTypeDef] {
        &self.type_defs
    }

    fn name(&self) -> String {
        "ConvContext".to_owned()
    }
}

impl<'a> ConvContext<'a> {
    pub fn new(module: &'a EnhancedASTModule) -> Self {
        Self {
            module,
            enums: LinkedHashMap::new(),
            structs: LinkedHashMap::new(),
            types: LinkedHashMap::new(),
            enum_defs: Vec::new(),
            struct_defs: Vec::new(),
            type_defs: Vec::new(),
            count: 0,
        }
    }

    pub fn add_enum(
        &mut self,
        namespace: &EnhASTNameSpace,
        enum_type: &EnhASTType,
        enum_def: &EnhASTEnumDef,
    ) -> ASTTypedType {
        debug_i!("add_enum {enum_type}");
        indent!();

        let result = match enum_type {
            EnhASTType::Custom {
                namespace: _,
                name,
                param_types,
                index: _,
            } => {
                self.count += 1;
                let new_name = format!("{name}_{}", self.count);

                let enum_typed_type = ASTTypedType::Enum {
                    namespace: enum_def.namespace.clone(),
                    name: new_name.clone(),
                };

                self.enums
                    .insert(enum_type.clone(), enum_typed_type.clone());

                let cloned_param_types = param_types.clone();
                let mut generic_to_type = EnhResolvedGenericTypes::new();
                for (i, p) in enum_def.type_parameters.iter().enumerate() {
                    // TODO types classes
                    generic_to_type.insert(
                        p.clone(),
                        Vec::new(),
                        cloned_param_types.get(i).unwrap().clone(),
                    );
                }

                let variants = enum_def
                    .variants
                    .iter()
                    .map(|it| {
                        enum_variant(
                            namespace,
                            self,
                            it,
                            &generic_to_type,
                            enum_type,
                            &enum_typed_type,
                            "",
                        )
                    })
                    .collect();

                let new_enum_def = ASTTypedEnumDef {
                    namespace: enum_def.namespace.clone(),
                    modifiers: enum_def.modifiers.clone(),
                    name: new_name,
                    variants,
                    ast_type: enum_type.clone(),
                    ast_typed_type: enum_typed_type.clone(),
                    index: enum_def.index.clone(),
                };

                self.enum_defs.push(new_enum_def);

                enum_typed_type
            }
            _ => {
                panic!()
            }
        };

        dedent!();
        result
    }

    pub fn get_enum(&self, enum_type: &EnhASTType) -> Option<ASTTypedType> {
        self.get_def_typed_type(enum_type, &self.enums)
    }

    pub fn add_struct(
        &mut self,
        struct_type: &EnhASTType,
        struct_def: &EnhASTStructDef,
    ) -> ASTTypedType {
        debug_i!("add_struct {struct_type}");
        indent!();
        let result = match struct_type {
            EnhASTType::Custom {
                namespace: _,
                name,
                param_types,
                index: _,
            } => {
                self.count += 1;
                let new_name = format!("{name}_{}", self.count);

                let struct_typed_type = ASTTypedType::Struct {
                    namespace: struct_def.namespace.clone(),
                    name: new_name.clone(),
                };

                self.structs
                    .insert(struct_type.clone(), struct_typed_type.clone());

                let cloned_param_types = param_types.clone();
                let mut generic_to_type = EnhResolvedGenericTypes::new();
                for (i, p) in struct_def.type_parameters.iter().enumerate() {
                    // TODO type classes
                    generic_to_type.insert(
                        p.clone(),
                        Vec::new(),
                        cloned_param_types
                            .get(i)
                            .unwrap_or_else(|| {
                                panic!("Cannot find generic type {p} for struct {name}")
                            })
                            .clone(),
                    );
                }

                // we add an incomplete struct for recursion
                let new_struct_def = ASTTypedStructDef {
                    namespace: struct_def.namespace.clone(),
                    modifiers: struct_def.modifiers.clone(),
                    name: new_name.clone(),
                    properties: Vec::new(),
                    ast_type: struct_type.clone(),
                    ast_typed_type: struct_typed_type.clone(),
                    index: struct_def.index.clone(),
                };

                self.struct_defs.push(new_struct_def);

                let properties = struct_def
                    .properties
                    .iter()
                    .map(|it| struct_property(&struct_def.namespace, self, it, &generic_to_type))
                    .collect();

                let new_struct_def = ASTTypedStructDef {
                    namespace: struct_def.namespace.clone(),
                    modifiers: struct_def.modifiers.clone(),
                    name: new_name.clone(),
                    properties,
                    ast_type: struct_type.clone(),
                    ast_typed_type: struct_typed_type.clone(),
                    index: struct_def.index.clone(),
                };

                self.struct_defs = self
                    .struct_defs
                    .iter()
                    .filter(|it| it.name != new_name)
                    .cloned()
                    .collect::<Vec<_>>();

                self.struct_defs.push(new_struct_def);

                struct_typed_type
            }
            _ => {
                panic!()
            }
        };

        dedent!();
        result
    }

    pub fn get_struct(&self, struct_type: &EnhASTType) -> Option<ASTTypedType> {
        self.get_def_typed_type(struct_type, &self.structs)
    }

    pub fn add_type(&mut self, ast_type: &EnhASTType, type_def: &EnhASTTypeDef) -> ASTTypedType {
        debug_i!("add_type {ast_type}");
        indent!();
        let result = match ast_type {
            EnhASTType::Custom {
                namespace: _,
                name,
                param_types,
                index: _,
            } => {
                self.count += 1;
                let new_name = format!("{name}_{}", self.count);

                let type_typed_type = ASTTypedType::Type {
                    namespace: type_def.namespace.clone(),
                    name: new_name.clone(),
                    body: type_def.body.clone(),
                };

                self.types.insert(ast_type.clone(), type_typed_type.clone());

                let cloned_param_types = param_types.clone();
                let mut resolved_generic_typed_types = ResolvedGenericTypedTypes::new();
                for (i, p) in type_def.type_parameters.iter().enumerate() {
                    // TODO type classes
                    resolved_generic_typed_types.insert(
                        p.clone(),
                        Vec::new(),
                        conv_to_typed_type(
                            &type_def.namespace,
                            self,
                            cloned_param_types.get(i).unwrap_or_else(|| {
                                panic!("Cannot find generic type {p} for type {name}")
                            }),
                            "",
                        ),
                    );
                }

                self.type_defs.push(ASTTypedTypeDef {
                    namespace: type_def.namespace.clone(),
                    original_name: name.clone(),
                    name: new_name,
                    generic_types: resolved_generic_typed_types,
                    body: type_def.body.clone(),
                    ast_type: ast_type.clone(),
                    ast_typed_type: type_typed_type.clone(),
                    index: type_def.index.clone(),
                    modifiers: type_def.modifiers.clone(),
                });

                type_typed_type
            }
            _ => {
                panic!()
            }
        };

        dedent!();
        result
    }

    pub fn get_type(&self, type_def_type: &EnhASTType) -> Option<ASTTypedType> {
        self.get_def_typed_type(type_def_type, &self.types)
    }

    fn get_def_typed_type(
        &self,
        ast_type: &EnhASTType,
        ast_type_to_ast_typed_type_map: &LinkedHashMap<EnhASTType, ASTTypedType>,
    ) -> Option<ASTTypedType> {
        match ast_type {
            EnhASTType::Custom {
                namespace: _,
                name: _,
                param_types: _,
                index: _,
            } => ast_type_to_ast_typed_type_map
                .iter()
                .find(|(type_def_ast_type, _ast_typed_type)| {
                    EnhTypeFilter::Exact((*type_def_ast_type).clone())
                        .almost_equal(ast_type, self.module)
                        .unwrap_or(false)
                })
                .map(|(_ast_type, ast_typed_type)| ast_typed_type)
                .cloned(),
            _ => {
                panic!()
            }
        }
    }
}

fn enum_variant(
    namespace: &EnhASTNameSpace,
    conv_context: &mut ConvContext,
    variant: &EnhASTEnumVariantDef,
    generic_to_type: &EnhResolvedGenericTypes,
    enum_type: &EnhASTType,
    enum_typed_type: &ASTTypedType,
    message: &str,
) -> ASTTypedEnumVariantDef {
    debug_i!(
        "variant {variant}, enum_type {enum_type}, enum_typed_type {:?}, {:?}",
        enum_typed_type,
        generic_to_type
    );
    indent!();
    let result = ASTTypedEnumVariantDef {
        name: variant.name.clone(),
        parameters: variant
            .parameters
            .iter()
            .map(|it| {
                debug_i!("param {it} {enum_type}");
                if &it.ast_type == enum_type {
                    ASTTypedParameterDef {
                        name: it.name.clone(),
                        ast_type: enum_typed_type.clone(),
                        ast_index: it.ast_index.clone(),
                    }
                } else if let Some(new_type) = substitute(&it.ast_type, generic_to_type) {
                    debug_i!("new_type {new_type}");

                    if &new_type == enum_type {
                        ASTTypedParameterDef {
                            name: it.name.clone(),
                            ast_type: enum_typed_type.clone(),
                            ast_index: it.ast_index.clone(),
                        }
                    } else {
                        ASTTypedParameterDef {
                            name: it.name.clone(),
                            ast_type: conv_to_typed_type(namespace, conv_context, &new_type, ""),
                            ast_index: it.ast_index.clone(),
                        }
                    }
                } else {
                    conv_to_typed_parameter_def(
                        namespace,
                        conv_context,
                        it,
                        &format!("{message}, variant {}", variant.name),
                    )
                }
            })
            .collect(),
    };
    dedent!();

    result
}

fn struct_property(
    namespace: &EnhASTNameSpace,
    conv_context: &mut ConvContext,
    property: &EnhASTStructPropertyDef,
    generic_to_type: &EnhResolvedGenericTypes,
) -> ASTTypedStructPropertyDef {
    if let Some(new_type) = substitute(&property.ast_type, generic_to_type) {
        ASTTypedStructPropertyDef {
            name: property.name.clone(),
            ast_type: conv_to_typed_type(namespace, conv_context, &new_type, ""),
        }
    } else {
        ASTTypedStructPropertyDef {
            name: property.name.clone(),
            ast_type: conv_to_typed_type(namespace, conv_context, &property.ast_type, ""),
        }
    }
}

pub fn conv_to_typed_type(
    namespace: &EnhASTNameSpace,
    conv_context: &mut ConvContext,
    ast_type: &EnhASTType,
    message: &str,
) -> ASTTypedType {
    let result = match ast_type {
        EnhASTType::Builtin(kind) => match kind {
            EnhBuiltinTypeKind::String => ASTTypedType::Builtin(BuiltinTypedTypeKind::String),
            EnhBuiltinTypeKind::Integer => ASTTypedType::Builtin(BuiltinTypedTypeKind::Integer),
            EnhBuiltinTypeKind::Boolean => ASTTypedType::Builtin(BuiltinTypedTypeKind::Boolean),
            EnhBuiltinTypeKind::Char => ASTTypedType::Builtin(BuiltinTypedTypeKind::Char),
            EnhBuiltinTypeKind::Float => ASTTypedType::Builtin(BuiltinTypedTypeKind::Float),
            EnhBuiltinTypeKind::Lambda {
                return_type,
                parameters,
            } => ASTTypedType::Builtin(BuiltinTypedTypeKind::Lambda {
                parameters: parameters
                    .iter()
                    .map(|it| {
                        conv_to_typed_type(
                            &it.namespace(),
                            conv_context,
                            it,
                            &(message.to_owned() + ", lambda parameter"),
                        )
                    })
                    .collect(),
                return_type: Box::new(conv_to_typed_type(
                    &return_type.namespace(),
                    conv_context,
                    return_type,
                    &(message.to_owned() + ", lambda return type"),
                )),
            }),
        },
        EnhASTType::Generic(index, p, _var_types) => {
            panic!("Unresolved generic type '{p}' {message} : {index}");
        }
        EnhASTType::Custom {
            namespace: _,
            name,
            param_types: _,
            index: _,
        } => {
            if let Some(enum_def) = conv_context.module.enums.iter().find(|it| {
                (it.modifiers.public || &it.namespace == ast_type.namespace()) && &it.name == name
            }) {
                if let Some(e) = conv_context.get_enum(ast_type) {
                    e
                } else {
                    conv_context.add_enum(namespace, ast_type, enum_def)
                }
            } else if let Some(struct_def) = conv_context.module.structs.iter().find(|it| {
                &it.name == name && (it.modifiers.public || &it.namespace == ast_type.namespace())
            }) {
                if let Some(e) = conv_context.get_struct(ast_type) {
                    e
                } else {
                    conv_context.add_struct(ast_type, struct_def)
                }
            } else if let Some(t) = conv_context.module.types.iter().find(|it| {
                (it.modifiers.public || &it.namespace == ast_type.namespace()) && &it.name == name
            }) {
                if let Some(e) = conv_context.get_type(ast_type) {
                    e
                } else {
                    conv_context.add_type(ast_type, t)
                }
            } else {
                println!(
                    "{:?}",
                    conv_context
                        .module
                        .types
                        .iter()
                        .map(|it| format!("{}:{}", it.namespace, it.name))
                        .collect::<Vec<_>>()
                );
                panic!("Cannot find custom type {name} from namespace '{namespace}' {message}");
            }
        }
        EnhASTType::Unit => ASTTypedType::Unit,
    };

    /*
    if format!("{ast_type}").contains("TestModel") {
        println!("  result {result}");
    }

     */

    result
}

pub fn conv_to_typed_parameter_def(
    namespace: &EnhASTNameSpace,
    conv_context: &mut ConvContext,
    parameter_def: &EnhASTParameterDef,
    message: &str,
) -> ASTTypedParameterDef {
    ASTTypedParameterDef {
        name: parameter_def.name.clone(),
        ast_type: conv_to_typed_type(
            namespace,
            conv_context,
            &parameter_def.ast_type,
            &format!("{message}: parameter {}", parameter_def.name),
        ),
        ast_index: parameter_def.ast_index.clone(),
    }
}

#[cfg(test)]
mod tests {
    use linked_hash_map::LinkedHashMap;
    use rasm_parser::parser::ast::ASTModifiers;

    use crate::{
        codegen::{
            enh_ast::{EnhASTEnumDef, EnhASTIndex, EnhASTNameSpace, EnhASTStructDef, EnhASTType},
            enhanced_module::EnhancedASTModule,
        },
        enh_type_check::{
            conv_context::{conv_to_typed_type, ConvContext},
            enh_functions_container::EnhFunctionsContainer,
            typed_ast::ASTTypedType,
        },
    };

    #[test]
    pub fn get_def_typed_type() {
        let first_namespace = EnhASTNameSpace::new("test".to_string(), "first".to_string());

        let second_namespace = EnhASTNameSpace::new("test".to_string(), "second".to_string());

        let module = enhanced_module();
        let sut = ConvContext::new(&module);

        let result_first_ast_type = result_ast_type(
            &first_namespace,
            &simple_custom_ast_type("TestModel", &first_namespace),
        );
        let result_first_typed_type = ASTTypedType::Enum {
            namespace: first_namespace.clone(),
            name: "Result_1".to_string(),
        };

        let result_second_ast_type = result_ast_type(
            &second_namespace,
            &simple_custom_ast_type("TestModel", &second_namespace),
        );
        let result_second_typed_type = ASTTypedType::Enum {
            namespace: second_namespace.clone(),
            name: "Result_2".to_string(),
        };

        let mut ast_type_to_ast_typed_type_map = LinkedHashMap::new();
        ast_type_to_ast_typed_type_map
            .insert(result_first_ast_type.clone(), result_first_typed_type);
        ast_type_to_ast_typed_type_map
            .insert(result_second_ast_type.clone(), result_second_typed_type);

        let result =
            sut.get_def_typed_type(&result_first_ast_type, &ast_type_to_ast_typed_type_map);

        assert_eq!(format!("{}", result.unwrap()), "Result_1");

        let result =
            sut.get_def_typed_type(&result_second_ast_type, &ast_type_to_ast_typed_type_map);

        assert_eq!(format!("{}", result.unwrap()), "Result_2");
    }

    #[test]
    fn test_typed_type() {
        let first_namespace = EnhASTNameSpace::new("test".to_string(), "first".to_string());

        let second_namespace = EnhASTNameSpace::new("test".to_string(), "second".to_string());

        let module = enhanced_module();
        let mut sut = ConvContext::new(&module);

        let _ = conv_to_typed_type(
            &first_namespace,
            &mut sut,
            &simple_custom_ast_type("TestModel", &first_namespace),
            "",
        );

        let _ = conv_to_typed_type(
            &second_namespace,
            &mut sut,
            &simple_custom_ast_type("TestModel", &second_namespace),
            "",
        );

        let _ = conv_to_typed_type(
            &first_namespace,
            &mut sut,
            &result_ast_type(
                &first_namespace,
                &simple_custom_ast_type("TestModel", &first_namespace),
            ),
            "",
        );

        let _ = conv_to_typed_type(
            &second_namespace,
            &mut sut,
            &result_ast_type(
                &second_namespace,
                &simple_custom_ast_type("TestModel", &second_namespace),
            ),
            "",
        );
    }

    fn result_ast_type(namespace: &EnhASTNameSpace, ast_type: &EnhASTType) -> EnhASTType {
        EnhASTType::Custom {
            namespace: namespace.clone(),
            name: "Result".to_string(),
            param_types: vec![ast_type.clone()],
            index: EnhASTIndex::none(),
        }
    }

    fn enhanced_module() -> EnhancedASTModule {
        let result_type_def = result_type_def();
        let first_namespace = EnhASTNameSpace::new("test".to_string(), "first".to_string());

        let second_namespace = EnhASTNameSpace::new("test".to_string(), "second".to_string());

        EnhancedASTModule {
            body: vec![],
            functions_by_name: EnhFunctionsContainer::new(),
            enums: vec![result_type_def.clone()],
            structs: vec![
                simple_struct_def("TestModel", &first_namespace),
                simple_struct_def("TestModel", &second_namespace),
            ],
            types: vec![],
            body_namespace: EnhASTNameSpace::global(),
        }
    }

    fn result_type_def() -> EnhASTEnumDef {
        EnhASTEnumDef {
            namespace: EnhASTNameSpace::new("std".to_string(), "result".to_string()),
            name: "Result".to_string(),
            modifiers: ASTModifiers::public(),
            variants: vec![],
            type_parameters: vec!["T".to_string()],
            index: EnhASTIndex::none(),
        }
    }

    fn simple_custom_ast_type(name: &str, namespace: &EnhASTNameSpace) -> EnhASTType {
        EnhASTType::Custom {
            namespace: namespace.clone(),
            name: name.to_string(),
            param_types: vec![],
            index: EnhASTIndex::none(),
        }
    }

    fn simple_struct_def(name: &str, namespace: &EnhASTNameSpace) -> EnhASTStructDef {
        EnhASTStructDef {
            namespace: namespace.clone(),
            name: name.to_string(),
            type_parameters: vec![],
            properties: vec![],
            index: EnhASTIndex::none(),
            modifiers: ASTModifiers::private(),
        }
    }
}
