use std::io::Write;
use std::path::Path;

use clap::{Arg, Command};
use env_logger::Builder;
use log::info;

use rasm_core::codegen::backend::BackendAsm386;
use rasm_core::codegen::statics::Statics;
use rasm_core::codegen::{CodeGen, TypedValContext, TypedValKind};
use rasm_core::lexer::Lexer;
use rasm_core::parser::ast::{ASTIndex, ASTModule};
use rasm_core::parser::Parser;
use rasm_core::type_check::typed_ast::{
    ASTTypedExpression, ASTTypedFunctionBody, ASTTypedFunctionDef, ASTTypedModule,
    ASTTypedStatement, ASTTypedType, BuiltinTypedTypeKind,
};
use rasm_core::type_check::typed_context::TypeConversionContext;
use rasm_core::utils::SliceDisplay;

use crate::reference_finder::ReferenceFinder;

pub mod reference_finder;

fn main() {
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

    let matches = Command::new("RASM lang")
        .version("0.1.0-alpha.0")
        .arg(
            Arg::new("SRC")
                .help("Sets the input file to use")
                .required(true)
                .index(1),
        )
        .arg(
            Arg::new("message-format")
                .help("for vscode")
                .long("message-format")
                .required(false),
        )
        .get_matches();

    let src = matches.get_one::<String>("SRC").unwrap();

    let (typed_module, type_conversion_context) = get_module(src);

    let file_name = "rasm/resources/examples/breakout/breakout.rasm".to_owned();
    let row = 59;
    let column = 91;

    let index = ASTIndex {
        file_name: Some(file_name),
        row,
        column,
    };

    let finder = ReferenceFinder::new(&typed_module);

    println!("result {}", SliceDisplay(&finder.find(&index)));
}

fn get_module(src: &str) -> (ASTTypedModule, TypeConversionContext) {
    let file_path = Path::new(src);
    let std_lib_path = CodeGen::get_std_lib_path();
    let module = match Lexer::from_file(file_path) {
        Ok(lexer) => {
            info!("Lexer ended");
            let mut parser = Parser::new(lexer, file_path.to_str().map(|it| it.to_string()));
            parser.parse(file_path, Path::new(&std_lib_path))
        }
        Err(err) => {
            panic!("An error occurred: {}", err)
        }
    };

    let backend = BackendAsm386::new(module.requires.clone(), module.externals.clone());
    let mut statics = Statics::new();

    CodeGen::get_typed_module(&backend, module, false, false, true, false, &mut statics)
}
