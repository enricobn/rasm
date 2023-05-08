use crate::parser::ast::{ASTExpression, ASTIndex, ASTModule, ASTStatement};

pub fn add_rasm_resource_folder(module: &mut ASTModule, resource_folder: String) {
    module.body.insert(
        0,
        ASTStatement::LetStatement(
            "RASMRESOURCEFOLDER".to_owned(),
            ASTExpression::StringLiteral(resource_folder),
            true,
            ASTIndex::none(),
        ),
    );
}
