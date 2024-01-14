/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENSE file in the repository root for full license text.
 */

use std::convert::Infallible;
use warp::Filter;
use crate::auth::ensure_auth;
use crate::lobby::filters::with_lobby_container;
use crate::lobby::models::LobbyContainer;
use crate::ws::handlers;
use crate::ws::models::SessionStorage;

pub async fn ws_lobby_connect(
    lobby_container: LobbyContainer,
    session_storage: SessionStorage
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone
{
    warp::path!("lobby" / String)
        .and(ensure_auth().await)
        .and(warp::ws())
        .and(with_lobby_container(lobby_container))
        .and(with_session_storage(session_storage))
        .and_then(handlers::ws)
}

pub fn with_session_storage(
    session_storage: SessionStorage
) -> impl Filter<Extract=(SessionStorage,), Error=Infallible> + Clone
{
    warp::any().map(move || session_storage.clone())
}