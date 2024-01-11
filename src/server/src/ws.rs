/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENSE file in the repository root for full licence text.
 */

use warp::Filter;
use crate::lobby::models::LobbyContainer;
use crate::ws::filters::ws_lobby_connect;

pub mod handlers;
pub mod models;
pub mod board;
pub mod filters;

pub async fn ws_lobby(
    lobby_container: LobbyContainer
)  -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone
{
    ws_lobby_connect(lobby_container).await
}
