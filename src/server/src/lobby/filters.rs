/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENCE file in the repository root for full licence text.
 */


use std::convert::Infallible;
use warp::Filter;
use crate::lobby::handlers::{create_lobby, delete_lobby, list_lobbies, update_lobby};
use crate::lobby::models::{LobbyContainer, LobbySettings};

pub fn lobby_list(
    lobby_container: LobbyContainer
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone
{
    warp::path!("lobbies")
        .and(warp::get())
        .and(with_lobby_container(lobby_container))
        .and_then(list_lobbies)
}

pub fn lobby_create(
    lobby_container: LobbyContainer
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone
{
    warp::path!("lobbies")
        .and(warp::post())
        .and(json_body())
        .and(with_lobby_container(lobby_container))
        .and_then(create_lobby)
}

pub fn lobby_update(
    lobby_container: LobbyContainer
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone
{
    warp::path!("lobbies" / String)
        .and(warp::put())
        .and(json_body())
        .and(with_lobby_container(lobby_container))
        .and_then(update_lobby)
}

pub fn lobby_delete(
    lobby_container: LobbyContainer
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone
{
    let admin_only = warp::header::exact("authorization", "Bearer admin");

    warp::path!("lobbies" / String)
        .and(admin_only)
        .and(warp::delete())
        .and(with_lobby_container(lobby_container))
        .and_then(delete_lobby)
}

fn json_body() -> impl Filter<Extract = (LobbySettings,), Error = warp::Rejection> + Clone {
    warp::body::content_length_limit(1024 * 16).and(warp::body::json())
}

fn with_lobby_container(
    lobby_container: LobbyContainer
) -> impl Filter<Extract=(LobbyContainer,), Error=Infallible> + Clone
{
    warp::any().map(move || lobby_container.clone())
}