/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENCE file in the repository root for full licence text.
 */

use std::collections::HashMap;
use std::convert::Infallible;
use uuid::Uuid;
use warp::http::StatusCode;
use crate::auth::UserInfo;
use crate::lobby::models::{Lobby, LobbyContainer, LobbySettings};


pub async fn list_lobbies(
    _user_info: UserInfo,
    lobby_container: LobbyContainer
) -> Result<impl warp::Reply, warp::Rejection>
{
    let lobbies = lobby_container.lobby_list.read().unwrap();
    let lobbies: HashMap<String, Lobby> = lobbies
        .clone()
        .into_iter() // TODO: Add filters here, e.g. not full
        .collect();
    Ok(warp::reply::json(&lobbies))
}

pub async fn create_lobby(
    user_info: UserInfo,
    lobby_settings: LobbySettings,
    lobby_container: LobbyContainer
) -> Result<impl warp::Reply, Infallible>
{
    log::debug!("creating lobby with settings: {:?}", lobby_settings);

    let uuid = Uuid::new_v4();
    let lobby = Lobby::new(user_info.username, uuid.clone(), lobby_settings.name, lobby_settings.player_limit);

    lobby_container.lobby_list.write().unwrap().insert(uuid.clone().to_string(), lobby);

    let response = uuid.to_string();
    Ok(warp::reply::with_status(response, StatusCode::CREATED))
}

pub async fn update_lobby(
    uuid: String,
    user_info: UserInfo,
    lobby_settings: LobbySettings,
    lobby_container: LobbyContainer
) -> Result<impl warp::Reply, warp::Rejection>
{
    log::debug!("update_lobby: uuid={:?}, lobby_settings={:?}", uuid, lobby_settings);
    let mut lobby_container = lobby_container.lobby_list.write().unwrap();

    if let Some(lobby) = lobby_container.get_mut(&uuid) {
        // deny removal of the lobby if issuer is not the creator of the lobby
        if lobby.creator_username != user_info.username && user_info.role != "admin" {
            return Ok(StatusCode::FORBIDDEN);
        }

        lobby.lobby_name = lobby_settings.name;
        lobby.player_limit = lobby_settings.player_limit;

        return Ok(StatusCode::OK);
    }

    Ok(StatusCode::NOT_FOUND)
}

pub async fn delete_lobby(
    uuid: String,
    user_info: UserInfo,
    lobby_container: LobbyContainer
) -> Result<impl warp::Reply, Infallible>
{
    log::debug!("delete_lobby: uuid={:?}", uuid);
    let mut lobby_container = lobby_container.lobby_list.write().unwrap();

    if let Some(lobby) = lobby_container.get(&uuid) {
        // deny removal of the lobby if issuer is not the creator of the lobby
        if lobby.creator_username != user_info.username && user_info.role != "admin" {
            return Ok(StatusCode::FORBIDDEN);
        }

        lobby_container.remove(&uuid);
        Ok(StatusCode::OK)
    } else {
        Ok(StatusCode::NOT_FOUND)
    }
}