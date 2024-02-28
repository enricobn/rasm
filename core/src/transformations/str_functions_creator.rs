use crate::codegen::backend::Backend;
use crate::codegen::enhanced_module::EnhancedASTModule;
use crate::codegen::statics::Statics;
use crate::parser::ast::{
    ASTFunctionBody, ASTFunctionDef, ASTIndex, ASTModifiers, ASTNameSpace, ASTParameterDef,
    ASTType, BuiltinTypeKind,
};
use crate::type_check::resolved_generic_types::ResolvedGenericTypes;
