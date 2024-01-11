/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENSE file in the repository root for full licence text.
 */


use std::convert::Infallible;
use warp::Filter;
use crate::auth::ensure_auth;
use crate::lobby::handlers::{create_lobby, delete_lobby, list_lobbies, update_lobby};
use crate::lobby::models::{LobbyContainer, LobbySettings};

/// GET /lobby
pub async fn lobby_list(
    lobby_container: LobbyContainer
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone
{
    warp::path!("lobbies")
        .and(ensure_auth().await)
        .and(warp::get())
        .and(with_lobby_container(lobby_container))
        .and_then(list_lobbies)
}

/// POST /lobby
pub async fn lobby_create(
    lobby_container: LobbyContainer
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone
{
    warp::path!("lobbies")
        .and(ensure_auth().await)
        .and(warp::post())
        .and(json_body())
        .and(with_lobby_container(lobby_container))
        .and_then(create_lobby)
}

/// PUT /lobby/:uuid
pub async fn lobby_update(
    lobby_container: LobbyContainer
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone
{
    warp::path!("lobbies" / String)
        .and(ensure_auth().await)
        .and(warp::put())
        .and(json_body())
        .and(with_lobby_container(lobby_container))
        .and_then(update_lobby)
}

/// DELETE /lobby/:uuid
pub async fn lobby_delete(
    lobby_container: LobbyContainer
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone
{
    warp::path!("lobbies" / String)
        .and(ensure_auth().await)
        .and(warp::delete())
        .and(with_lobby_container(lobby_container))
        .and_then(delete_lobby)
}

fn json_body() -> impl Filter<Extract = (LobbySettings,), Error = warp::Rejection> + Clone {
    warp::body::content_length_limit(1024 * 16).and(warp::body::json())
}

pub fn with_lobby_container(
    lobby_container: LobbyContainer
) -> impl Filter<Extract=(LobbyContainer,), Error=Infallible> + Clone
{
    warp::any().map(move || lobby_container.clone())
}