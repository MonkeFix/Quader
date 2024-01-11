/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENCE file in the repository root for full licence text.
 */

use warp::Filter;
use crate::lobby::filters::{lobby_create, lobby_delete, lobby_list, lobby_update};
use crate::lobby::models::LobbyContainer;

pub mod board;
pub mod filters;
pub mod models;
pub mod handlers;


pub async fn lobbies(
    lobby_container: LobbyContainer
) -> impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone
{
    lobby_list(lobby_container.clone()).await
        .or(lobby_create(lobby_container.clone()).await)
        .or(lobby_update(lobby_container.clone()).await)
        .or(lobby_delete(lobby_container).await)
}



