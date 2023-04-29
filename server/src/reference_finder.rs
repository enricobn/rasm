use std::path::Path;

use rasm_core::codegen::{CodeGen, TypedValContext, TypedValKind, ValContext, ValKind};
use rasm_core::lexer::Lexer;
use rasm_core::parser::ast::{
    ASTExpression, ASTFunctionBody, ASTFunctionDef, ASTIndex, ASTModule, ASTStatement, ASTType,
    BuiltinTypeKind,
};
use rasm_core::type_check::typed_ast::{
    ASTTypedExpression, ASTTypedFunctionBody, ASTTypedFunctionDef, ASTTypedModule,
    ASTTypedStatement, ASTTypedType, BuiltinTypedTypeKind,
};
use rasm_core::type_check::typed_context::TypeConversionContext;

pub struct ReferenceFinder {
    selectable_items: Vec<SelectableItem>,
}

impl ReferenceFinder {
    pub fn new(module: &ASTModule) -> Self {
        let selectable_items = Self::get_selectable_items(module);
        Self { selectable_items }
    }

    pub fn find(&self, index: &ASTIndex) -> Vec<ASTIndex> {
        let mut result = Vec::new();

        for selectable_item in self.selectable_items.iter() {
            if selectable_item.matches(index) {
                //println!("found {:?}", selectable_item);
                result.push(selectable_item.point_to.clone());
            }

            if selectable_item.min.file_name == index.file_name {
                //println!("NOT found {:?}", selectable_item.min.row);
            }
        }

        result
    }

    fn get_selectable_items(module: &ASTModule) -> Vec<SelectableItem> {
        let mut val_context = ValContext::new(None);

        let mut result = Vec::new();

        Self::process_statements(module, &module.body, &mut result, &mut val_context);

        result.append(
            &mut module
                .functions
                .iter()
                .flat_map(|it| Self::get_selectable_items_fn(it, module))
                .collect(),
        );

        result
    }

    fn get_selectable_items_fn(
        function: &ASTFunctionDef,
        module: &ASTModule,
    ) -> Vec<SelectableItem> {
        if let ASTFunctionBody::RASMBody(statements) = &function.body {
            let mut result = Vec::new();
            let mut val_context = ValContext::new(None);

            for par in function.parameters.iter() {
                val_context.insert_par(par.name.clone(), par.clone());
            }

            Self::process_statements(&module, statements, &mut result, &mut val_context);

            result
        } else {
            Vec::new()
        }
    }

    fn process_statements(
        module: &ASTModule,
        statements: &Vec<ASTStatement>,
        result: &mut Vec<SelectableItem>,
        mut val_context: &mut ValContext,
    ) {
        for stmt in statements {
            if let ASTStatement::LetStatement(name, expr, is_const, index) = stmt {
                // TODO it's not always a string!!!
                val_context.insert_let(
                    name.clone(),
                    ASTType::Builtin(BuiltinTypeKind::String),
                    index,
                );
            }
            result.append(&mut Self::get_selectable_items_stmt(
                stmt,
                &mut val_context,
                module,
            ));
        }
    }

    fn get_selectable_items_stmt(
        stmt: &ASTStatement,
        val_context: &mut ValContext,
        module: &ASTModule,
    ) -> Vec<SelectableItem> {
        match stmt {
            ASTStatement::Expression(expr) => {
                Self::get_selectable_items_expr(expr, val_context, module)
            }
            ASTStatement::LetStatement(name, expr, _, index) => {
                Self::get_selectable_items_expr(expr, val_context, module)
            }
        }
    }

    fn get_selectable_items_expr(
        expr: &ASTExpression,
        val_context: &mut ValContext,
        module: &ASTModule,
    ) -> Vec<SelectableItem> {
        let mut result = Vec::new();
        match expr {
            ASTExpression::StringLiteral(_) => {}
            ASTExpression::ASTFunctionCallExpression(call) => {
                // TODO call
                let mut v = call
                    .parameters
                    .iter()
                    .flat_map(|it| {
                        Self::get_selectable_items_expr(it, val_context, module).into_iter()
                    })
                    .collect::<Vec<_>>();
                result.append(&mut v);
            }
            ASTExpression::ValueRef(name, index) => {
                if let Some(v) = val_context.get(&name) {
                    match v {
                        ValKind::ParameterRef(_, def) => {
                            result.push(SelectableItem {
                                min: index.mv(-(name.len() as i32)),
                                max: index.clone(),
                                point_to: def.ast_index.clone(),
                            });
                        }
                        ValKind::LetRef(_, def) => {
                            if let Some(type_index) = match def {
                                ASTType::Builtin(_) => None,
                                ASTType::Custom { name, param_types } => {
                                    Self::get_enum(module, name)
                                }
                                ASTType::Generic(_) => None,
                            } {
                                result.push(SelectableItem {
                                    min: index.mv(-(name.len() as i32)),
                                    max: index.clone(),
                                    point_to: type_index,
                                });
                            }
                        }
                    }
                }
            }
            ASTExpression::Value(value_type, index) => {}
            ASTExpression::Lambda(def) => {
                // TODO for we should add the parameter names in the context
                let mut v = def
                    .body
                    .iter()
                    .flat_map(|it| {
                        Self::get_selectable_items_stmt(it, val_context, module).into_iter()
                    })
                    .collect::<Vec<_>>();
                result.append(&mut v);
            }
            ASTExpression::Any(_) => {}
        }
        result
    }

    fn get_enum(module: &ASTModule, name: &String) -> Option<ASTIndex> {
        module
            .enums
            .iter()
            .find(|it| &it.name == name)
            .map(|it| it.index.clone())
    }

    fn get_struct(module: &ASTModule, name: &String) -> Option<ASTIndex> {
        module
            .structs
            .iter()
            .find(|it| &it.name == name)
            .map(|it| it.index.clone())
    }

    fn get_type(module: &ASTModule, name: &String) -> Option<ASTIndex> {
        module
            .types
            .iter()
            .find(|it| &it.name == name)
            .map(|it| it.index.clone())
    }
}

#[derive(Debug, Clone)]
pub struct SelectableItem {
    min: ASTIndex,
    max: ASTIndex,
    point_to: ASTIndex,
}

impl SelectableItem {
    pub fn matches(&self, index: &ASTIndex) -> bool {
        index.file_name == self.min.file_name
            && index.file_name == self.max.file_name
            && index.row == self.min.row
            && index.row == self.max.row
            && index.column >= self.min.column
            && index.column <= self.max.column
    }
}
#[cfg(test)]
mod tests {
    use std::env;
    use std::io::Write;
    use std::path::Path;

    use env_logger::Builder;
    use log::info;

    use rasm_core::codegen::backend::BackendAsm386;
    use rasm_core::codegen::statics::Statics;
    use rasm_core::codegen::CodeGen;
    use rasm_core::lexer::Lexer;
    use rasm_core::parser::ast::ASTIndex;
    use rasm_core::parser::Parser;
    use rasm_core::type_check::typed_ast::ASTTypedModule;
    use rasm_core::type_check::typed_context::TypeConversionContext;

    use crate::get_module;
    use crate::reference_finder::ReferenceFinder;

    fn init() {
        Builder::from_default_env()
            .format(|buf, record| {
                writeln!(
                    buf,
                    "{} [{}] - {}",
                    chrono::Local::now().format("%Y-%m-%d %H:%M:%S.%3f"),
                    record.level(),
                    record.args()
                )
            })
            .init();
    }

    #[test]
    fn simple() {
        init();
        env::set_var("RASM_STDLIB", "../stdlib");

        let module = get_module("resources/simple.rasm");

        let finder = ReferenceFinder::new(&module);

        let file_name = "resources/simple.rasm".to_owned();
        let row = 5;
        let column = 15;

        let index = ASTIndex {
            file_name: Some(file_name),
            row,
            column,
        };

        finder.find(&index);
    }
}
