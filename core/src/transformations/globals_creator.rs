use std::path::PathBuf;

use rasm_parser::parser::ast::{ASTModifiers, ASTValueType};

use crate::codegen::enh_ast::{EnhASTExpression, EnhASTIndex, EnhASTStatement};
use crate::codegen::enhanced_module::EnhancedASTModule;

pub fn add_folder(
    module: &mut EnhancedASTModule,
    source_folder_name: &str,
    resource_folder: PathBuf,
) {
    if resource_folder.exists() {
        module.body.insert(
            0,
            EnhASTStatement::ConstStatement(
                source_folder_name.to_owned(),
                EnhASTExpression::Value(
                    ASTValueType::String(
                        resource_folder
                            .canonicalize()
                            .unwrap_or_else(|_| {
                                panic!("Cannot find resource folder: {:?}", resource_folder)
                            })
                            .to_str()
                            .unwrap()
                            .to_owned(),
                    ),
                    EnhASTIndex::none(),
                ),
                EnhASTIndex::none(),
                ASTModifiers::public(),
            ),
        );
    }
}
