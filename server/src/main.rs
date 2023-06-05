use std::collections::{HashMap, HashSet, LinkedList};
use std::env;
use std::fs::File;
use std::io::{Read, Write};
use std::net::SocketAddr;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use axum::extract::{Query, State};
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
use walkdir::WalkDir;

use rasm_core::codegen::backend::{Backend, BackendAsm386};
use rasm_core::codegen::statics::Statics;
use rasm_core::codegen::{CodeGen, TypedValKind};
use rasm_core::lexer::tokens::{BracketKind, BracketStatus, PunctuationKind, Token, TokenKind};
use rasm_core::lexer::Lexer;
use rasm_core::parser::ast::{ASTIndex, ASTModule};
use rasm_core::parser::Parser;
use rasm_core::project;
use rasm_core::project::project::RasmProject;
use rasm_core::transformations::enrich_module;
use rasm_core::transformations::enum_functions_creator::enum_functions_creator;
use rasm_core::transformations::str_functions_creator::str_functions_creator;
use rasm_core::transformations::struct_functions_creator::struct_functions_creator;
use rasm_core::type_check::typed_ast::{
    ASTTypedExpression, ASTTypedFunctionBody, ASTTypedFunctionDef, ASTTypedModule,
    ASTTypedStatement, ASTTypedType, BuiltinTypedTypeKind,
};
use rasm_core::type_check::typed_context::TypeConversionContext;
use rasm_core::utils::SliceDisplay;
use rasm_server::reference_finder::ReferenceFinder;

#[tokio::main]
async fn main() {
    //init_log();
    // initialize tracing
    tracing_subscriber::fmt::init();

    let src = Path::new(&get_src()).to_path_buf();
    info!("starting server for {:?}", src);

    let project = RasmProject::new(src.clone());

    let server_state = ServerState::new(src, &BackendAsm386::new(HashSet::new(), HashSet::new()));

    let app_state = Arc::new(server_state);

    // build our application with a route
    let app = Router::new()
        .route("/", get(root))
        .route("/file", get(file))
        .with_state(app_state);

    let addr = SocketAddr::from(([127, 0, 0, 1], 3000));
    info!("listening on http://{}", addr);
    // run our app with hyper
    // `axum::Server` is a re-export of `hyper::Server`
    axum::Server::bind(&addr)
        .serve(app.into_make_service())
        .await
        .unwrap();
}

#[derive(Deserialize)]
struct FileQueryParams {
    src: String,
}

struct ServerState {
    src: PathBuf,
    finder: ReferenceFinder,
    module: ASTModule,
}

impl ServerState {
    fn new(src: PathBuf, backend: &dyn Backend) -> Self {
        let project = RasmProject::new(src.clone());

        let mut statics = Statics::new();
        let module = project.get_module(
            &BackendAsm386::new(HashSet::new(), HashSet::new()),
            &mut statics,
        );
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

    let project = RasmProject::new(state.src.clone());

    let root_file = project
        .relative_to_root(project.main_src_file().as_path())
        .unwrap();

    info!("root file {}", root_file.to_str().unwrap());

    html.push_str(&format!(
        "<b><A href=\"/file?src={}\">{}</A></b></br>",
        root_file.to_str().unwrap(),
        root_file.to_str().unwrap()
    ));

    html.push_str("</br>");

    let mut paths: Vec<PathBuf> = Vec::new();

    for entry in WalkDir::new(&project.source_folder())
        .into_iter()
        .filter_map(Result::ok)
        .filter(|e| !e.file_type().is_dir() && e.file_name().to_str().unwrap().ends_with(".rasm"))
    {
        let f_name = entry.into_path();
        let file = project.relative_to_root(f_name.as_path()).unwrap();
        paths.push(file);
    }

    //paths.extend(&mut state.module.included_files.iter());

    paths.sort();

    for included_file in paths
        .iter()
        .filter(|it| !it.starts_with("stdlib") && it != &&root_file)
    {
        html.push_str(&format!(
            "<A href=\"/file?src={}\">{}</A></br>",
            included_file.to_str().unwrap(),
            included_file.to_str().unwrap()
        ));
    }

    let result = Html(html);
    info!("end rendering root");
    result
}

async fn file(
    //axum::extract::Path(src): axum::extract::Path<String>,
    src: Query<FileQueryParams>,
    State(state): State<Arc<ServerState>>,
) -> Html<String> {
    let src = src.src.clone();
    info!("start rendering {}", src);

    let project = RasmProject::new(state.src.clone());

    let file_path = project.from_relative_to_root(Path::new(&src));

    let result = if let Ok(mut file) = File::open(file_path.clone()) {
        let mut s = String::new();
        file.read_to_string(&mut s).unwrap();

        let mut html = format!("<b>{src}</b></br></br>");
        html.push_str("<pre>\n");

        let source_file = file_path.to_str().unwrap().to_owned();

        let lexer = Lexer::new(s, Some(file_path.to_path_buf()));

        lexer.for_each(|it| {
            let index = ASTIndex {
                file_name: Some(file_path.to_path_buf()),
                row: it.row,
                column: it.column,
            };
            let vec = state.finder.find(&index);
            let name = format!("_{}_{}", it.row, it.column);
            if vec.len() > 0 {
                if let Some(file_name) = &vec.first().cloned().and_then(|it| it.file_name) {
                    let ref_name = format!(
                        "/file?src={}#_{}_{}",
                        project
                            .relative_to_root(file_name.as_path())
                            .expect(&format!("{:?}", file_name))
                            .to_str()
                            .unwrap(),
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
        TokenKind::Comment(s) => {
            format!("{s}")
        }
        TokenKind::MultiLineComment(s) => {
            format!("<pre>{s}</pre>")
        }
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
