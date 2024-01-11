/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENSE file in the repository root for full license text.
 */

use warp::Filter;
use crate::auth::ensure_auth;
use crate::lobby::filters::with_lobby_container;
use crate::lobby::models::LobbyContainer;
use crate::ws::handlers;

pub async fn ws_lobby_connect(
    lobby_container: LobbyContainer
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone
{
    warp::path!("lobby" / String)
        .and(ensure_auth().await)
        .and(warp::ws())
        .and(with_lobby_container(lobby_container))
        .and_then(handlers::ws)
}