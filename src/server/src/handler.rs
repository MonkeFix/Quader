/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENCE file in the repository root for full licence text.
 */

use warp::{filters::ws::Ws, reject, Rejection, reply::Reply};
use warp::http::StatusCode;
use warp::reject::Reject;

use crate::client::client_connected;
use crate::lobby::models::LobbyContainer;
use crate::Result;

pub async fn ws(uuid: String, ws: Ws, lobby_container: LobbyContainer) -> Result<impl Reply> {
    log::debug!("connecting to lobby with uuid={:?}", uuid);

    let mut lobby_container = lobby_container.lobby_list.write().unwrap();
    if !lobby_container.contains_key(&uuid) {
        log::debug!("lobby with uuid={:?} not found", uuid);
        return Err(reject::not_found());
    }

    let mut lobby = lobby_container.get_mut(&uuid).unwrap();
    lobby.player_list.push("test".into());

    Ok(ws.on_upgrade(move |socket| client_connected(socket)))
}


#[derive(Debug)]
struct LobbyNotFound;

impl Reject for LobbyNotFound { }