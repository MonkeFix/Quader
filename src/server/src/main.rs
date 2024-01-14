/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENSE file in the repository root for full licence text.
 */

use std::convert::Infallible;
use server::{index_html};
use warp::Filter;
use warp::http::StatusCode;
use server::auth::{ApiErrorResult, ApiErrors};
use server::config::Config;
use server::lobby::lobbies;
use server::lobby::models::LobbyContainer;
use server::ws::models::SessionStorage;
use server::ws::ws_lobby;


#[tokio::main]
async fn main() -> Result<(), dotenvy::Error> {
    dotenvy::dotenv()?;
    env_logger::init_from_env(env_logger::Env::new().default_filter_or("info"));

    let config = Config::init();

    log::info!("Using config {:?}", config);

    let lobby_container = LobbyContainer::new();
    let session_storage = SessionStorage::new();

    // lobby/:uuid
    let lobby_ws = ws_lobby(lobby_container.clone(), session_storage).await;

    let index = warp::path::end().map(|| warp::reply::html(index_html));

    let lobby_api = lobbies(lobby_container).await;

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



