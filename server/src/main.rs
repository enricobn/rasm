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

use rasm_core::codegen::backend::BackendAsm386;
use rasm_core::codegen::statics::Statics;
use rasm_core::codegen::{CodeGen, TypedValContext, TypedValKind};
use rasm_core::lexer::tokens::{BracketKind, BracketStatus, PunctuationKind, Token, TokenKind};
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

#[tokio::main]
async fn main() {
    //init_log();
    // initialize tracing
    tracing_subscriber::fmt::init();

    let src = get_src();
    info!("starting server for {src}");

    let server_state = ServerState::new(src);

    let app_state = Arc::new(server_state);

    // build our application with a route
    let app = Router::new()
        // `GET /` goes to `root`
        .route("/", get(root))
        // `POST /users` goes to `create_user`
        .route("/users", post(create_user))
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
}

impl ServerState {
    fn new(src: String) -> Self {
        let module = get_module(&src);
        let finder = ReferenceFinder::new(&module);
        Self { src, finder }
    }
}

// root handler
async fn root(State(state): State<Arc<ServerState>>) -> Html<String> {
    info!("start rendering {}", state.src);

    let file_path = Path::new(&state.src);

    let result = if let Ok(mut file) = File::open(file_path) {
        let mut s = String::new();
        file.read_to_string(&mut s).unwrap();

        let mut html = String::new();
        html.push_str("<pre>\n");

        let lexer = Lexer::new(s, state.src.clone());

        lexer.for_each(|it| {
            let index = ASTIndex {
                file_name: Some(state.src.clone()),
                row: it.row,
                column: it.column,
            };
            let vec = state.finder.find(&index);
            if vec.len() > 0 {
                html.push_str(&format!("<b>{}</b>", token_to_string(&it)));
            } else {
                html.push_str(&format!("{}", token_to_string(&it)));
            }
        });
        html.push_str("</pre>\n");
        Html(format!("{html}"))
    } else {
        Html(format!("Error loading {}", state.src))
    };
    info!("end rendering {}", state.src);
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

async fn create_user(
    // this argument tells axum to parse the request body
    // as JSON into a `CreateUser` type
    Json(payload): Json<CreateUser>,
) -> (StatusCode, Json<User>) {
    // insert your application logic here
    let user = User {
        id: 1337,
        username: payload.username,
    };

    // this will be converted into a JSON response
    // with a status code of `201 Created`
    (StatusCode::CREATED, Json(user))
}

// the input to our `create_user` handler
#[derive(Deserialize)]
struct CreateUser {
    username: String,
}

// the output to our `create_user` handler
#[derive(Serialize)]
struct User {
    id: u64,
    username: String,
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

fn get_module(src: &str) -> ASTModule {
    let file_path = Path::new(src);
    let std_lib_path = CodeGen::get_std_lib_path();
    match Lexer::from_file(file_path) {
        Ok(lexer) => {
            info!("Lexer ended");
            let mut parser = Parser::new(lexer, file_path.to_str().map(|it| it.to_string()));
            parser.parse(file_path, Path::new(&std_lib_path))
        }
        Err(err) => {
            panic!("An error occurred: {}", err)
        }
    }
}
