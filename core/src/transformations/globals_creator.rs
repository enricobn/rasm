use std::path::PathBuf;

use crate::codegen::eh_ast::{ASTExpression, ASTIndex, ASTStatement};
use crate::codegen::enhanced_module::EnhancedASTModule;

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
                    ASTIndex::none(),
                ),
                true,
                ASTIndex::none(),
            ),
        );
    }
}
