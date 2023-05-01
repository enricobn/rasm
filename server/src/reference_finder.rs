use std::path::Path;

use rasm_core::codegen::{CodeGen, TypedValContext, TypedValKind, ValContext, ValKind};
use rasm_core::lexer::Lexer;
use rasm_core::parser::ast::{
    ASTEnumDef, ASTExpression, ASTFunctionBody, ASTFunctionDef, ASTIndex, ASTModule, ASTStatement,
    ASTStructDef, ASTType, ASTTypeDef, BuiltinTypeKind,
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
                Self::process_type(&module, &par.ast_type, &mut result);
            }

            Self::process_statements(&module, statements, &mut result, &mut val_context);

            result
        } else {
            Vec::new()
        }
    }

    fn process_type(module: &ASTModule, ast_type: &ASTType, result: &mut Vec<SelectableItem>) {
        if let ASTType::Custom {
            name,
            param_types,
            index,
        } = ast_type
        {
            Self::process_custom_type(module, result, name, index);
            param_types
                .iter()
                .for_each(|it| Self::process_type(module, it, result));
        }
    }

    fn process_custom_type(
        module: &ASTModule,
        result: &mut Vec<SelectableItem>,
        name: &String,
        index: &ASTIndex,
    ) {
        let min = index.mv(-(name.len() as i32));
        if let Some(def) = Self::get_enum(module, name) {
            result.push(SelectableItem::new(min, index.clone(), def.index));
        } else if let Some(def) = Self::get_struct(module, name) {
            result.push(SelectableItem::new(min, index.clone(), def.index));
        } else if let Some(def) = Self::get_type(module, name) {
            result.push(SelectableItem::new(min, index.clone(), def.index));
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
                            result.push(SelectableItem::new(
                                index.mv(-(name.len() as i32)),
                                index.clone(),
                                def.ast_index.clone(),
                            ));
                        }
                        ValKind::LetRef(_, def, let_index) => {
                            result.push(SelectableItem::new(
                                index.mv(-(name.len() as i32)),
                                index.clone(),
                                let_index.clone(),
                            ));
                        }
                    }
                }
            }
            ASTExpression::Value(value_type, index) => {}
            ASTExpression::Lambda(def) => {
                let mut lambda_context = ValContext::new(Some(val_context));
                let mut lambda_result = Vec::new();
                Self::process_statements(
                    module,
                    &def.body,
                    &mut lambda_result,
                    &mut lambda_context,
                );
                result.append(&mut lambda_result);
            }
            ASTExpression::Any(_) => {}
        }
        result
    }

    fn get_enum(module: &ASTModule, name: &String) -> Option<ASTEnumDef> {
        module
            .enums
            .iter()
            .find(|it| &it.name == name)
            .map(|it| it.clone())
    }

    fn get_struct(module: &ASTModule, name: &String) -> Option<ASTStructDef> {
        module
            .structs
            .iter()
            .find(|it| &it.name == name)
            .map(|it| it.clone())
    }

    fn get_type(module: &ASTModule, name: &String) -> Option<ASTTypeDef> {
        module
            .types
            .iter()
            .find(|it| &it.name == name)
            .map(|it| it.clone())
    }
}

#[derive(Debug, Clone)]
pub struct SelectableItem {
    min: ASTIndex,
    max: ASTIndex,
    point_to: ASTIndex,
}

impl SelectableItem {
    pub fn new(min: ASTIndex, max: ASTIndex, point_to: ASTIndex) -> Self {
        SelectableItem { min, max, point_to }
    }

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
    use rasm_core::utils::SliceDisplay;

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
        let file_name = "resources/simple.rasm".to_owned();

        let module = get_module(&file_name);

        let finder = ReferenceFinder::new(&module);

        let row = 5;
        let column = 15;

        finder.selectable_items.iter().for_each(|it| {
            if it.min.file_name == Some(file_name.clone()) {
                println!("{} {} -> {}", &it.min, &it.max, &it.point_to);
            }
        });

        assert_eq!(
            finder.find(&ASTIndex::new(Some(file_name.clone()), 5, 15,)),
            vec![ASTIndex::new(Some(file_name.clone()), 3, 10)]
        );

        assert_eq!(
            finder.find(&ASTIndex::new(Some(file_name.clone()), 8, 15,)),
            vec![ASTIndex::new(Some(file_name), 7, 21)]
        );
    }

    #[test]
    fn types() {
        init();
        env::set_var("RASM_STDLIB", "../stdlib");
        let file_name = "resources/types.rasm".to_owned();

        let module = get_module(&file_name);

        let finder = ReferenceFinder::new(&module);

        let row = 5;
        let column = 15;

        finder.selectable_items.iter().for_each(|it| {
            if it.min.file_name == Some(file_name.clone()) {
                println!("{} {} -> {}", &it.min, &it.max, &it.point_to);
            }
        });

        assert_eq!(
            finder.find(&ASTIndex::new(Some(file_name.clone()), 15, 23,)),
            vec![ASTIndex::new(
                Some("../stdlib/option.rasm".to_owned()),
                1,
                5
            )]
        );

        assert_eq!(
            finder.find(&ASTIndex::new(Some(file_name.clone()), 19, 23,)),
            vec![ASTIndex::new(Some(file_name.clone()), 3, 7)]
        );

        assert_eq!(
            finder.find(&ASTIndex::new(Some(file_name.clone()), 23, 23,)),
            vec![ASTIndex::new(Some("../stdlib/vec.rasm".to_owned()), 1, 5)]
        );
    }
}
