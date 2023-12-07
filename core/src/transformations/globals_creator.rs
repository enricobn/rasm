use std::path::PathBuf;

use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::parser::ast::{ASTExpression, ASTIndex, ASTStatement};

pub fn add_folder(
    module: &mut EnhancedASTModule,
    source_folder_name: &str,
    resource_folder: PathBuf,
) {
    if resource_folder.exists() {
        module.body.insert(
            0,
            ASTStatement::LetStatement(
                source_folder_name.to_owned(),
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
}
