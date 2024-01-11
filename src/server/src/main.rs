/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENCE file in the repository root for full licence text.
 */

use std::convert::Infallible;
use log::info;
use serde::Serialize;
use server::{handler, index_html};
use warp::Filter;
use warp::http::StatusCode;
use server::config::Config;
use server::lobby::{lobbies};
use server::lobby::filters::with_lobby_container;
use server::lobby::models::LobbyContainer;

#[derive(Debug)]
enum ApiErrors {
    NotAuthorized(String)
}

impl warp::reject::Reject for ApiErrors { }

#[derive(Debug, Serialize)]
struct ApiErrorResult {
    details: String
}

const API_TOKEN_USER: &'static str = "loonacuse";
const API_TOKEN: &'static str = "12345";


#[tokio::main]
async fn main() -> Result<(), dotenvy::Error> {
    dotenvy::dotenv()?;
    env_logger::init_from_env(env_logger::Env::new().default_filter_or("info"));

    let config = Config::init();

    info!("Using config {:?}", config);

    let lobby_container = LobbyContainer::new();

    // lobby/:uuid
    let lobby_ws = warp::path!("lobby" / String)
        .and(ensure_auth().await)
        .and(warp::ws())
        .and(with_lobby_container(lobby_container.clone()))
        .and_then(handler::ws);

    let index = warp::path::end().map(|| warp::reply::html(index_html));

    let lobby_api = lobbies(lobby_container);

    let routes = index.or(lobby_ws).or(lobby_api).recover(handle_rejection);

    warp::serve(routes).run(([127, 0, 0, 1], 3030)).await;

    Ok(())
}

async fn handle_rejection(err: warp::reject::Rejection) -> Result<impl warp::reply::Reply, Infallible> {
    let code;
    let message;

    if err.is_not_found() {
        code = StatusCode::NOT_FOUND;
        message = "Not Found";
    } else if let Some(_) = err.find::<warp::filters::body::BodyDeserializeError>() {
        code = StatusCode::BAD_REQUEST;
        message = "Invalid Body";
    } else if let Some(e) = err.find::<ApiErrors>() {
        match e {
            ApiErrors::NotAuthorized(_err) => {
                code = StatusCode::UNAUTHORIZED;
                message = "Not Authorized";
            }
        }
    } else if let Some(_) = err.find::<warp::reject::MethodNotAllowed>() {
        code = StatusCode::METHOD_NOT_ALLOWED;
        message = "Method Not Allowed";
    } else {
        log::error!("unhandled rejection: {:?}", err);
        code = StatusCode::INTERNAL_SERVER_ERROR;
        message = "Internal Server Error"
    }

    let json = warp::reply::json(&ApiErrorResult { details: message.into() });

    Ok(warp::reply::with_status(json, code))
}

fn check_token(header: String) -> Option<String> {
    let parts: Vec<&str> = header.split(" ").collect();
    if parts.len() == 2 && parts[0] == "Token" && parts[1] == API_TOKEN {
        return Some(API_TOKEN_USER.to_string());
    }

    None
}

async fn ensure_auth() -> impl Filter<Extract = (String,), Error = warp::reject::Rejection> + Clone {
    warp::header::optional::<String>("Authorization").and_then(|auth_header: Option<String>| async move {
        log::info!("doing dummy validation of auth header");

        if let Some(header) = auth_header {
            log::info!("got auth header, verifying: {}", header);
            if let Some(user) = check_token(header) {
                return Ok(user);
            }
        }

        Err(warp::reject::custom(ApiErrors::NotAuthorized(
            "Not Authorized".to_string()
        )))
    })
}
