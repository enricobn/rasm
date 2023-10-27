use std::path::PathBuf;

use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::parser::ast::{ASTExpression, ASTIndex, ASTStatement};

pub fn add_rasm_resource_folder(module: &mut EnhancedASTModule, resource_folder: PathBuf) {
    module.body.insert(
        0,
        ASTStatement::LetStatement(
            "RASMRESOURCEFOLDER".to_owned(),
            ASTExpression::StringLiteral(
                resource_folder
                    .canonicalize()
                    .unwrap_or_else(|_| {
                        panic!("Cannot find resource folder: {:?}", resource_folder)
                    })
                    .to_str()
                    .unwrap()
                    .to_owned(),
            ),
            true,
            ASTIndex::none(),
        ),
    );
}
