use std::collections::{HashMap, HashSet, LinkedList};
use std::env;
use std::fs::File;
use std::io::{Read, Write};
use std::net::SocketAddr;
use std::path::Path;
use std::sync::Arc;

use axum::extract::State;
use axum::response::Html;
use axum::{
    http::StatusCode,
    response::IntoResponse,
    routing::{get, post},
    Json, Router,
};
use clap::{Arg, Command};
use env_logger::Builder;
use log::info;
use serde::{Deserialize, Serialize};

use rasm_core::codegen::backend::{Backend, BackendAsm386};
use rasm_core::codegen::statics::Statics;
use rasm_core::codegen::{CodeGen, TypedValContext, TypedValKind};
use rasm_core::lexer::tokens::{BracketKind, BracketStatus, PunctuationKind, Token, TokenKind};
use rasm_core::lexer::Lexer;
use rasm_core::parser::ast::{ASTIndex, ASTModule};
use rasm_core::parser::Parser;
use rasm_core::transformations::enum_functions_creator::enum_functions_creator;
use rasm_core::transformations::str_functions_creator::str_functions_creator;
use rasm_core::transformations::struct_functions_creator::struct_functions_creator;
use rasm_core::type_check::typed_ast::{
    ASTTypedExpression, ASTTypedFunctionBody, ASTTypedFunctionDef, ASTTypedModule,
    ASTTypedStatement, ASTTypedType, BuiltinTypedTypeKind,
};
use rasm_core::type_check::typed_context::TypeConversionContext;
use rasm_core::utils::SliceDisplay;

use crate::reference_finder::ReferenceFinder;

pub mod reference_context;
pub mod reference_finder;

#[tokio::main]
async fn main() {
    //init_log();
    // initialize tracing
    tracing_subscriber::fmt::init();

    let src = get_src();
    info!("starting server for {src}");

    let server_state = ServerState::new(src, &BackendAsm386::new(HashSet::new(), HashSet::new()));

    let app_state = Arc::new(server_state);

    // build our application with a route
    let app = Router::new()
        .route("/", get(root))
        .route("/file/*src", get(file))
        .with_state(app_state);

    // run our app with hyper
    // `axum::Server` is a re-export of `hyper::Server`
    let addr = SocketAddr::from(([127, 0, 0, 1], 3000));
    info!("listening on http://{}", addr);
    axum::Server::bind(&addr)
        .serve(app.into_make_service())
        .await
        .unwrap();
}

struct ServerState {
    src: String,
    finder: ReferenceFinder,
    module: ASTModule,
}

impl ServerState {
    fn new(src: String, backend: &dyn Backend) -> Self {
        let module = get_module(&src, backend);
        let finder = ReferenceFinder::new(&module);
        Self {
            src,
            finder,
            module,
        }
    }
}

async fn root(State(state): State<Arc<ServerState>>) -> Html<String> {
    info!("start rendering root");

    let mut html = String::new();

    html.push_str(&format!(
        "<b><A href=\"/file/{}\">{}</A></b></br>",
        state.src, state.src
    ));

    html.push_str("</br>");

    let mut paths = Vec::new();

    paths.extend(&mut state.module.included_files.iter());

    paths.sort();

    for included_file in paths
        .iter()
        .filter(|it| !it.starts_with("stdlib") && it.to_owned() != &&state.src)
    {
        html.push_str(&format!(
            "<A href=\"/file/{}\">{}</A></br>",
            included_file, included_file
        ));
    }

    let result = Html(html);
    info!("end rendering root");
    result
}

async fn file(
    axum::extract::Path(src): axum::extract::Path<String>,
    State(state): State<Arc<ServerState>>,
) -> Html<String> {
    info!("start rendering {}", src);

    let root_path = Path::new(&state.src).parent().unwrap();

    let file_path = if src.starts_with("stdlib") {
        let path = Path::new(&src).strip_prefix("stdlib").unwrap();
        let std_lib_path_s = CodeGen::get_std_lib_path();
        let std_lib_path = Path::new(&std_lib_path_s).clone();
        std_lib_path.join(path).to_str().unwrap().to_owned()
    } else {
        src.clone()
    };

    let file_path = Path::new(&file_path);

    let result = if let Ok(mut file) = File::open(file_path.clone()) {
        let mut s = String::new();
        file.read_to_string(&mut s).unwrap();

        let mut html = format!("<b>{src}</b></br></br>");
        html.push_str("<pre>\n");

        let source_file = file_path.to_str().unwrap().to_owned();
        let lexer = Lexer::new(s, source_file.clone());

        lexer.for_each(|it| {
            let index = ASTIndex {
                file_name: Some(source_file.clone()),
                row: it.row,
                column: it.column,
            };
            let vec = state.finder.find(&index);
            let name = format!("_{}_{}", it.row, it.column);
            if vec.len() > 0 {
                if let Some(file_name) = &vec.first().cloned().and_then(|it| it.file_name) {
                    let ref_name = format!(
                        "/file/{}#_{}_{}",
                        file_name,
                        vec.first().unwrap().row,
                        vec.first().unwrap().column
                    );
                    html.push_str(&format!("<!-- {},{} -->", it.row, it.column));
                    html.push_str(&format!(
                        "<A HREF=\"{ref_name}\" NAME={name}>{}</A>",
                        token_to_string(&it)
                    ));
                } else {
                    html.push_str(&format!("<!-- {},{} -->", it.row, it.column));
                    html.push_str(&format!("<A NAME={name}>{}</A>", token_to_string(&it)));
                }
            } else {
                html.push_str(&format!("<!-- {},{} -->", it.row, it.column));
                html.push_str(&format!("<A NAME={name}>{}</A>", token_to_string(&it)));
            }
        });
        html.push_str("</pre>\n");
        Html(format!("{html}"))
    } else {
        Html(format!("Error loading {}", src))
    };
    info!("end rendering {}", src);
    result
}

fn token_to_string(token: &Token) -> String {
    match &token.kind {
        TokenKind::AlphaNumeric(s) => s.clone(),
        TokenKind::AsmBLock(s) => s.clone(),
        TokenKind::Bracket(kind, status) => match kind {
            BracketKind::Angle => match status {
                BracketStatus::Close => "&gt;".to_owned(),
                BracketStatus::Open => "&lt;".to_owned(),
            },
            BracketKind::Brace => match status {
                BracketStatus::Close => "}".to_owned(),
                BracketStatus::Open => "{".to_owned(),
            },
            BracketKind::Round => match status {
                BracketStatus::Close => ")".to_owned(),
                BracketStatus::Open => "(".to_owned(),
            },
            BracketKind::Square => match status {
                BracketStatus::Close => "]".to_owned(),
                BracketStatus::Open => "[".to_owned(),
            },
        },
        TokenKind::Comment(s) => format!("// {s}"),
        TokenKind::EndOfLine => "</br>".to_owned(),
        TokenKind::KeyWord(keyword) => format!("{:?}", keyword).to_lowercase(),
        TokenKind::Number(n) => n.to_string(),
        TokenKind::Punctuation(kind) => match kind {
            PunctuationKind::Dot => ".".to_owned(),
            PunctuationKind::Colon => ":".to_owned(),
            PunctuationKind::Comma => ",".to_owned(),
            PunctuationKind::Equal => "=".to_owned(),
            PunctuationKind::RightArrow => "->".to_owned(),
            PunctuationKind::SemiColon => ";".to_owned(),
        },
        TokenKind::StringLiteral(s) => format!("\"{s}\""),
        TokenKind::CharLiteral(c) => format!("'{c}'"),
        TokenKind::WhiteSpaces(s) => s.clone(),
    }
}

fn get_src() -> String {
    let matches = Command::new("RASM server")
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

    matches.get_one::<String>("SRC").unwrap().to_owned()
}

fn get_module(src: &str, backend: &dyn Backend) -> ASTModule {
    let file_path = Path::new(src);
    let std_lib_path = CodeGen::get_std_lib_path();
    let mut module = match Lexer::from_file(file_path) {
        Ok(lexer) => {
            info!("Lexer ended");
            let mut parser = Parser::new(lexer, file_path.to_str().map(|it| it.to_string()));
            parser.parse(file_path, Path::new(&std_lib_path))
        }
        Err(err) => {
            panic!("An error occurred: {}", err)
        }
    };

    let mut statics = Statics::new();

    enum_functions_creator(backend, &mut module, &mut statics);
    struct_functions_creator(backend, &mut module);
    str_functions_creator(&mut module);

    module
}
