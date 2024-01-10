/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENCE file in the repository root for full licence text.
 */

use std::convert::Infallible;
use warp::{Filter, http};
use crate::client::Clients;
use crate::lobby::{Lobby, LobbyContainer, LobbySettings};

pub fn with_clients(clients: Clients) -> impl Filter<Extract=(Clients,), Error=Infallible> + Clone {
    warp::any().map(move || clients.clone())
}

pub fn with_lobby_settings() -> impl Filter<Extract = (LobbySettings,), Error = warp::Rejection> + Clone {
    warp::body::content_length_limit(1024 * 16).and(warp::body::json())
}

pub fn with_lobby_filter(lobby_container: LobbyContainer) -> impl Filter<Extract=(LobbyContainer,), Error=Infallible> + Clone {
    warp::any().map(move || lobby_container.clone())
}

pub async fn update_lobby(lobby_settings: LobbySettings, lobby_container: LobbyContainer) -> Result<impl warp::Reply, warp::Rejection> {
    lobby_container.lobby_list
        .write()
        .unwrap()
        .insert(lobby_settings.name.clone(), Lobby::new(lobby_settings.name));

    Ok(warp::reply::with_status(
       "Success",
        http::StatusCode::CREATED
    ))
}

pub async fn get_lobby_list(lobby_container: LobbyContainer) -> Result<impl warp::Reply, warp::Rejection> {
    let r = lobby_container.lobby_list.read().unwrap();
    Ok(warp::reply::json(&*r))
}