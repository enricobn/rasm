use std::path::PathBuf;

use crate::parser::ast::{ASTExpression, ASTIndex, ASTModule, ASTStatement};

pub fn add_rasm_resource_folder(module: &mut ASTModule, resource_folder: PathBuf) {
    module.body.insert(
        0,
        ASTStatement::LetStatement(
            "RASMRESOURCEFOLDER".to_owned(),
            ASTExpression::StringLiteral(
                resource_folder
                    .canonicalize()
                    .unwrap_or_else(|_| panic!("{:?}", resource_folder))
                    .to_str()
                    .unwrap()
                    .to_owned(),
            ),
            true,
            ASTIndex::none(),
        ),
    );
}
