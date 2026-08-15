use std::fmt::Display;

use rasm_parser::parser::ast::{ASTBuiltinTypeKind, ASTParameterDef, ASTType};

pub fn add_generic_prefix(t: ASTType, prefix: &dyn Display) -> ASTType {
    if !t.is_generic() {
        return t;
    }
    if format!("{prefix}").contains(":") {
        panic!("unsupported prefix {prefix}");
    }
    if let ASTType::ASTBuiltinType(ASTBuiltinTypeKind::ASTLambdaType {
        parameters,
        return_type,
    }) = t
    {
        return ASTType::ASTBuiltinType(ASTBuiltinTypeKind::ASTLambdaType {
            parameters: parameters
                .into_iter()
                .map(|it| add_generic_prefix(it, prefix))
                .collect(),
            return_type: Box::new(add_generic_prefix(return_type.as_ref().clone(), prefix)),
        });
    } else if let ASTType::ASTGenericType(position, name, var_types) = t {
        if name.contains(":") {
            panic!("generic has already been prefixed");
        }
        return ASTType::ASTGenericType(
            position,
            format!("{prefix}:{name}"),
            var_types
                .into_iter()
                .map(|it| add_generic_prefix(it, prefix))
                .collect(),
        );
    } else if let ASTType::ASTCustomType {
        name,
        param_types,
        position,
    } = t
    {
        return ASTType::ASTCustomType {
            name,
            param_types: param_types
                .into_iter()
                .map(|it| add_generic_prefix(it, prefix))
                .collect(),
            position,
        };
    }

    t
}

pub fn remove_generic_prefix(t: ASTType) -> ASTType {
    if let ASTType::ASTBuiltinType(ASTBuiltinTypeKind::ASTLambdaType {
        parameters,
        return_type,
    }) = t
    {
        ASTType::ASTBuiltinType(ASTBuiltinTypeKind::ASTLambdaType {
            parameters: parameters
                .into_iter()
                .map(|it| remove_generic_prefix(it))
                .collect(),
            return_type: Box::new(remove_generic_prefix(return_type.as_ref().clone())),
        })
    } else if let ASTType::ASTGenericType(ref position, ref name, ref var_types) = t {
        if let Some(original_generic) = get_original_generic(name) {
            ASTType::ASTGenericType(
                position.clone(),
                original_generic.to_owned(),
                var_types
                    .into_iter()
                    .map(|it| remove_generic_prefix(it.clone()))
                    .collect(),
            )
        } else {
            t
        }
    } else if let ASTType::ASTCustomType {
        name,
        param_types,
        position,
    } = t
    {
        ASTType::ASTCustomType {
            name,
            param_types: param_types
                .into_iter()
                .map(|it| remove_generic_prefix(it))
                .collect(),
            position,
        }
    } else {
        t
    }
}

pub fn get_original_generic(name: &str) -> Option<&str> {
    name.find(':').map(|i| name.split_at(i + 1).1)
}

pub fn remove_generic_prefix_from_str(name: &str) -> &str {
    get_original_generic(name).unwrap_or(name)
}

pub fn fix_ast_par_generics(par: ASTParameterDef, prefix: &dyn Display) -> ASTParameterDef {
    let mut result = par;
    result.ast_type = add_generic_prefix(result.ast_type, prefix);
    result
}
