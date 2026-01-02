/*
 *     RASM compiler.
 *     Copyright (C) 2022-2023  Enrico Benedetti
 *
 *     This program is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

use std::fs::File;
use std::io::Read;
use std::net::SocketAddr;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use axum::extract::{Query, State};
use axum::response::Html;
use axum::{Router, routing::get};
use log::info;

use rasm_core::codegen::c::options::COptions;
use rasm_core::codegen::enh_ast::EnhASTIndex;
use rasm_core::commandline::RasmProfile;
use rasm_parser::catalog::ASTIndex;
use serde::Deserialize;
use walkdir::WalkDir;

use crate::ide_helper::IDEHelper;

use rasm_core::codegen::compile_target::CompileTarget;
use rasm_core::enh_type_check::enh_type_check_error::EnhTypeCheckError;
use rasm_core::project::RasmProject;
use rasm_parser::lexer::Lexer;
use rasm_parser::lexer::tokens::{BracketKind, BracketStatus, PunctuationKind, Token, TokenKind};

pub fn rasm_server(project: RasmProject) {
    //init_log();
    // initialize tracing
    //tracing_subscriber::fmt::init();

    info!("starting server for {:?}", project);

    tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .unwrap()
        .block_on(async {
            run_server(project).await;
        })
}

async fn run_server(project: RasmProject) {
    let server_state = ServerState::new(project).unwrap();

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
    project: RasmProject,
    target: CompileTarget,
    profile: RasmProfile,
}

impl ServerState {
    fn new(project: RasmProject) -> Result<Self, EnhTypeCheckError> {
        Ok(Self {
            project,
            target: CompileTarget::C(COptions::default()),
            profile: RasmProfile::Main,
        })
    }
}

async fn root(State(state): State<Arc<ServerState>>) -> Html<String> {
    info!("start rendering root");

    let mut html = String::new();

    let project = &state.project;

    let root_file = project
        .relative_to_main_rasm_source_folder(
            &state.profile.principal_sub_project(),
            project
                .main_src_file(&state.profile.principal_sub_project())
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

    for entry in WalkDir::new(
        &project
            .rasm_source_folder(&state.profile.principal_sub_project())
            .unwrap(),
    )
    .into_iter()
    .filter_map(Result::ok)
    .filter(|e| !e.file_type().is_dir() && e.file_name().to_str().unwrap().ends_with(".rasm"))
    {
        let f_name = entry.into_path();
        let file = project
            .relative_to_main_rasm_source_folder(
                &state.profile.principal_sub_project(),
                f_name.as_path(),
            )
            .unwrap();
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

async fn file<'a>(
    //axum::extract::Path(src): axum::extract::Path<String>,
    src: Query<FileQueryParams>,
    State(state): State<Arc<ServerState>>,
) -> Html<String> {
    let src = src.src.clone();
    info!("start rendering {}", src);

    let project = &state.project;
    let profile = &state.profile;
    let target = &state.target;

    let file_path = project
        .from_relative_to_main_src(&profile.principal_sub_project(), Path::new(&src))
        .unwrap()
        .canonicalize()
        .unwrap();

    let (_module, _errors, info) = &state
        .project
        .get_module(
            file_path.as_path(),
            target,
            &profile.principal_sub_project(),
            true,
        )
        .unwrap();

    /*
    let (container, catalog, _) = project.container_and_catalog(&profile, &target);

    let container = enrich_container(
        &target,
        &mut Statics::new(),
        container,
        &catalog,
        false,
        false,
    );
    */

    let ide_helper = IDEHelper::from_project(project);

    let result = if let Ok(mut file) = File::open(file_path.clone()) {
        let mut s = String::new();
        file.read_to_string(&mut s).unwrap();

        let mut html = format!("<b>{src}</b></br></br>");
        html.push_str("<pre>\n");

        let lexer = Lexer::new(s);

        // TODO errors

        let (tokens, _errors) = lexer.process();

        let mut row = 0;
        let mut last_is_multiline = false;

        tokens.into_iter().for_each(|it| {
            // mv_left because the index of the token is at the end of the token itself, hence outside the text of the token...
            let index = EnhASTIndex::new(Some(file_path.to_path_buf()), it.position.clone());

            let ast_index = ASTIndex::new(
                info.module_namespace().clone(),
                info.module_id().clone(),
                it.position.clone(),
            );

            if row != index.position.row {
                row = index.position.row;
                if !(is_multiline(&it)
                    || last_is_multiline && matches!(it.kind, TokenKind::EndOfLine))
                {
                    html.push_str(&format!("{: >6} ", row));
                }
            }
            let mut vec = ide_helper.find(&ast_index);
            let name = format!("_{}_{}", it.position.row, it.position.column);
            if !vec.is_empty() {
                let item = vec.remove(0);
                if let Some((file_name, row, column)) =
                    item.target.and_then(|it| it.index()).and_then(|index| {
                        let info = ide_helper.catalog_info(index.module_id());
                        info.and_then(|(id, _)| {
                            id.path()
                                .map(|path| (path, index.position().row, index.position().column))
                        })
                    })
                {
                    let ref_name = format!(
                        "/file?src={}#_{}_{}",
                        project
                            .relative_to_main_rasm_source_folder(
                                &state.profile.principal_sub_project(),
                                file_name.as_path()
                            )
                            .unwrap_or_else(|| panic!("{:?}", file_name))
                            .to_str()
                            .unwrap(),
                        row,
                        column
                    );
                    html.push_str(&format!(
                        "<!-- {} {},{} -->",
                        it.kind, it.position.row, it.position.column
                    ));
                    html.push_str(&format!(
                        "<A HREF=\"{ref_name}\" NAME={name}>{}</A>",
                        token_to_string(&it, row)
                    ));
                } else {
                    html.push_str(&format!(
                        "<!-- {},{} -->",
                        it.position.row, it.position.column
                    ));
                    html.push_str(&format!("<A NAME={name}>{}</A>", token_to_string(&it, row)));
                }
            } else {
                html.push_str(&format!(
                    "<!-- {},{} -->",
                    it.position.row, it.position.column
                ));
                html.push_str(&format!("<A NAME={name}>{}</A>", token_to_string(&it, row)));
            }
            last_is_multiline = is_multiline(&it);
        });
        html.push_str("</pre>\n");
        Html(html)
    } else {
        Html(format!("Error loading {}", src))
    };
    info!("end rendering {}", src);
    result
}

fn is_multiline(token: &Token) -> bool {
    matches!(token.kind, TokenKind::MultiLineComment(_))
        || matches!(token.kind, TokenKind::NativeBlock(_))
}

fn token_to_string(token: &Token, row: usize) -> String {
    match &token.kind {
        TokenKind::AlphaNumeric(s) => s.clone(),
        TokenKind::NativeBlock(s) => {
            let mut new_s = format!("{s}}}/");
            if new_s.starts_with('\n') {
                new_s.remove(0);
            }
            format!("/{{</br>{}", multiline_to_string(&new_s, row))
        }
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
        TokenKind::Comment(s) => format!("{s}</br>"),
        TokenKind::MultiLineComment(s) => multiline_to_string(s, row),
        TokenKind::EndOfLine => "</br>".to_owned(),
        TokenKind::KeyWord(keyword) => format!("{:?}", keyword).to_lowercase(),
        TokenKind::Number(n) => n.to_string(),
        TokenKind::Punctuation(kind) => match kind {
            PunctuationKind::At => "@".to_owned(),
            PunctuationKind::Dot => ".".to_owned(),
            PunctuationKind::Colon => ":".to_owned(),
            PunctuationKind::Comma => ",".to_owned(),
            PunctuationKind::Equal => "=".to_owned(),
            PunctuationKind::RightArrow => "->".to_owned(),
            PunctuationKind::SemiColon => ";".to_owned(),
            PunctuationKind::Esclamation => "!".to_owned(),
        },
        TokenKind::StringLiteral(s) => format!("\"{s}\""),
        TokenKind::CharLiteral(c) => format!("'{c}'"),
        TokenKind::WhiteSpaces(s) => s.clone(),
        TokenKind::Reserved(reserved) => format!("{reserved}"),
    }
}

fn multiline_to_string(s: &str, row: usize) -> String {
    let mut row = row;
    let string = s.to_string();
    let lines = string.lines().collect::<Vec<_>>();
    lines
        .iter()
        .enumerate()
        .map(|(index, it)| {
            let result = if index == lines.len() - 1 {
                format!("{: >6} {it}", (row as i32) - lines.len() as i32 + 1)
            } else {
                row += 1;
                format!("{: >6} {it}</br>", (row as i32) - lines.len() as i32)
            };

            result
        })
        .collect()
}
