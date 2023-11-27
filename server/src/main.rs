use std::fs::File;
use std::io::Read;
use std::net::SocketAddr;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use axum::extract::{Query, State};
use axum::response::Html;
use axum::{routing::get, Router};
use clap::{Arg, Command};
use log::info;
use serde::Deserialize;
use walkdir::WalkDir;

use rasm_core::codegen::backend::{Backend, BackendNasmI386};
use rasm_core::codegen::enhanced_module::EnhancedASTModule;
use rasm_core::codegen::statics::Statics;
use rasm_core::lexer::tokens::{BracketKind, BracketStatus, PunctuationKind, Token, TokenKind};
use rasm_core::lexer::Lexer;
use rasm_core::parser::ast::ASTIndex;
use rasm_core::project::RasmProject;
use rasm_core::type_check::type_check_error::TypeCheckError;
use rasm_server::reference_finder::ReferenceFinder;

#[tokio::main]
async fn main() {
    //init_log();
    // initialize tracing
    tracing_subscriber::fmt::init();

    let src = Path::new(&get_src()).to_path_buf();
    info!("starting server for {:?}", src);

    let project = RasmProject::new(src.clone());

    let mut backend = BackendNasmI386::new(false);
    let server_state = ServerState::new(src, &mut backend).unwrap();

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
}

impl ServerState {
    fn new(src: PathBuf, backend: &mut dyn Backend) -> Result<Self, TypeCheckError> {
        let project = RasmProject::new(src.clone());

        let mut statics = Statics::new();
        let (modules, errors) = project.get_all_modules(backend, &mut statics, false);

        // TODO errors

        let enhanced_astmodule = EnhancedASTModule::new(modules, &project, backend, &mut statics);

        let finder = ReferenceFinder::new(enhanced_astmodule)?;
        Ok(Self { src, finder })
    }
}

async fn root(State(state): State<Arc<ServerState>>) -> Html<String> {
    info!("start rendering root");

    let mut html = String::new();

    let project = RasmProject::new(state.src.clone());

    let root_file = project
        .relative_to_root_src(
            project
                .main_src_file()
                .expect("undefined main in rasm.toml")
                .as_path(),
        )
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
        let file = project.relative_to_root_src(f_name.as_path()).unwrap();
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

        let lexer = Lexer::new(s, Some(file_path.clone()));

        // TODO errors

        let (tokens, errors_) = lexer.process();

        tokens.into_iter().for_each(|it| {
            let index = ASTIndex {
                file_name: Some(file_path.to_path_buf()),
                row: it.row,
                column: it.column,
            };
            let vec = state.finder.find(&index).unwrap();
            let name = format!("_{}_{}", it.row, it.column);
            if vec.len() > 0 {
                if let Some(file_name) = &vec.first().cloned().and_then(|it| it.point_to.file_name)
                {
                    let ref_name = format!(
                        "/file?src={}#_{}_{}",
                        project
                            .relative_to_root_src(file_name.as_path())
                            .unwrap_or_else(|| panic!("{:?}", file_name))
                            .to_str()
                            .unwrap(),
                        vec.first().unwrap().point_to.row,
                        vec.first().unwrap().point_to.column
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
        Html(html)
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
        TokenKind::Comment(s) => s.to_string(),
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
