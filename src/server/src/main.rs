/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENCE file in the repository root for full licence text.
 */

use log::info;
use server::{handler, index_html};
use warp::Filter;
use server::config::Config;
use server::lobby::{lobbies};
use server::lobby::filters::with_lobby_container;
use server::lobby::models::LobbyContainer;


#[tokio::main]
async fn main() -> Result<(), dotenvy::Error> {
    dotenvy::dotenv()?;
    env_logger::init_from_env(env_logger::Env::new().default_filter_or("info"));

    let config = Config::init();

    info!("Using config {:?}", config);

    let lobby_container = LobbyContainer::new();

    // lobby/:uuid
    let chat = warp::path!("lobby" / String)
        .and(warp::ws())
        .and(with_lobby_container(lobby_container.clone()))
        .and_then(handler::ws);

    let index = warp::path::end().map(|| warp::reply::html(index_html));
    
    let lobby_api = lobbies(lobby_container);

    let routes = index.or(chat).or(lobby_api);

    warp::serve(routes).run(([127, 0, 0, 1], 3030)).await;

    Ok(())
}
