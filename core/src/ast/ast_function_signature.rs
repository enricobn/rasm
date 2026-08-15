use std::{fmt::Display, iter::zip};

use itertools::Itertools;
use rasm_parser::parser::ast::{
    ASTFunctionBody, ASTFunctionDef, ASTModifiers, ASTParameterDef, ASTPosition, ASTType,
};

use crate::ast::generic_prefix::{
    add_generic_prefix, remove_generic_prefix, remove_generic_prefix_from_str,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ASTFunctionSignature {
    pub name: String,
    pub generics: Vec<String>,
    pub parameters_types: Vec<ASTType>,
    pub return_type: ASTType,
    pub modifiers: ASTModifiers,
    pub associated_type: Option<String>,
}

impl Display for ASTFunctionSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let generics = if self.generics.is_empty() {
            ""
        } else {
            &format!("<{}>", self.generics.iter().join(", "))
        };

        write!(
            f,
            "{}{}({})",
            self.name,
            generics,
            self.parameters_types
                .iter()
                .map(|it| format!("{it}"))
                .join(", ")
        )?;
        if !self.return_type.is_unit() {
            write!(f, " -> {}", self.return_type)?;
        }
        Ok(())
    }
}

impl ASTFunctionSignature {
    pub fn from_def(def: &ASTFunctionDef) -> Self {
        ASTFunctionSignature {
            name: def.name.clone(),
            generics: def.generic_types.clone(),
            parameters_types: def
                .parameters
                .iter()
                .map(|it| it.ast_type.clone())
                .collect(),
            return_type: def.return_type.clone(),
            modifiers: def.modifiers.clone(),
            associated_type: def.associated_type.clone(),
        }
    }

    pub fn to_def(
        self,
        modifiers: ASTModifiers,
        position: ASTPosition,
        parameters_names: Vec<String>,
        parameters_positions: Vec<ASTPosition>,
        body: ASTFunctionBody,
        associated_type: Option<String>,
    ) -> ASTFunctionDef {
        assert_eq!(self.parameters_types.len(), parameters_names.len());
        assert_eq!(self.parameters_types.len(), parameters_positions.len());
        ASTFunctionDef {
            name: self.name,
            parameters: zip(
                self.parameters_types,
                zip(parameters_names, parameters_positions),
            )
            .into_iter()
            .map(|(ast_type, (name, position))| ASTParameterDef {
                name,
                ast_type,
                position,
            })
            .collect(),
            return_type: self.return_type,
            body,
            generic_types: self.generics,
            position,
            modifiers,
            associated_type,
        }
    }

    pub fn generics_prefix(&self, prefix: &str) -> String {
        format!("{}_{}", prefix, self.safe_name())
    }

    fn safe_name(&self) -> String {
        match &self.associated_type {
            Some(associated_type) => format!("{}_{}", associated_type, self.name),
            None => self.name.clone(),
        }
    }

    pub fn add_generic_prefix(self, prefix: &str) -> Self {
        let generics_prefix = self.generics_prefix(prefix);
        let mut result = self;
        result.parameters_types = result
            .parameters_types
            .into_iter()
            .map(|it| add_generic_prefix(it, &generics_prefix))
            .collect();
        result.return_type = add_generic_prefix(result.return_type, &generics_prefix);
        result.generics = result
            .generics
            .into_iter()
            .map(|it| format!("{generics_prefix}:{it}"))
            .collect();

        result
    }

    pub fn remove_generic_prefix(self) -> Self {
        let mut result = self;
        result.parameters_types = result
            .parameters_types
            .into_iter()
            .map(|it| remove_generic_prefix(it))
            .collect();
        result.return_type = remove_generic_prefix(result.return_type);
        result.generics = result
            .generics
            .into_iter()
            .map(|it| remove_generic_prefix_from_str(&it).to_owned())
            .collect();

        result
    }

    pub fn is_generic(&self) -> bool {
        !self.generics.is_empty()
    }
}

#[cfg(test)]
pub mod tests {
    use rasm_parser::parser::ast::{ASTBuiltinTypeKind, ASTModifiers, ASTPosition, ASTType};

    use crate::ast::ast_function_signature::ASTFunctionSignature;

    #[test]
    fn function_signature_display() {
        let os = ASTType::ASTCustomType {
            name: "Option".to_owned(),
            param_types: vec![ASTType::ASTBuiltinType(ASTBuiltinTypeKind::ASTStringType)],
            position: ASTPosition::none(),
        };
        let ot = ASTType::ASTCustomType {
            name: "Option".to_owned(),
            param_types: vec![ASTType::ASTGenericType(
                ASTPosition::none(),
                "T".to_string(),
                vec![],
            )],
            position: ASTPosition::none(),
        };
        let fs = ASTFunctionSignature {
            return_type: ot,
            name: "aFunction".to_owned(),
            generics: vec!["T".to_string()],
            parameters_types: vec![os],
            modifiers: ASTModifiers::Public,
            associated_type: None,
        };

        assert_eq!(format!("{fs}"), "aFunction<T>(Option<str>) -> Option<T>");
    }
}
