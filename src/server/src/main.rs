/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENCE file in the repository root for full licence text.
 */

use log::info;
use server::{handler, index_html};
use warp::Filter;
use server::config::Config;
use server::filter::{get_lobby_list, update_lobby, with_lobby_filter, with_lobby_settings};
use server::lobby::{Lobby, LobbyContainer};


#[tokio::main]
async fn main() -> Result<(), dotenvy::Error> {
    dotenvy::dotenv()?;
    env_logger::init_from_env(env_logger::Env::new().default_filter_or("info"));

    let config = Config::init();

    info!("Using config {:?}", config);

    let chat = warp::path("chat")
        .and(warp::ws())
        .and_then(handler::ws);

    let index = warp::path::end().map(|| warp::reply::html(index_html));
    let lobby_container = LobbyContainer::new();
    let create_lobby = warp::post()
        .and(warp::path("v1"))
        .and(warp::path("lobby"))
        .and(warp::path::end())
        .and(with_lobby_settings())
        .and(with_lobby_filter(lobby_container.clone()))
        .and_then(update_lobby);

    let get_lobbies = warp::get()
        .and(warp::path("v1"))
        .and(warp::path("lobby"))
        .and(warp::path::end())
        .and(with_lobby_filter(lobby_container.clone()))
        .and_then(get_lobby_list);

    let routes = index.or(chat).or(create_lobby).or(get_lobbies);

    warp::serve(routes).run(([127, 0, 0, 1], 3030)).await;

    Ok(())
}
