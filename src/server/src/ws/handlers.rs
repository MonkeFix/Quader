/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENSE file in the repository root for full license text.
 */

use warp::{filters::ws::Ws, reject, reply::Reply};
use crate::auth::UserInfo;

use crate::lobby::models::LobbyContainer;
use crate::Result;
use crate::ws::models::client_connected;

pub async fn ws(uuid: String, user: UserInfo, ws: Ws, lobby_container: LobbyContainer) -> Result<impl Reply> {
    log::debug!("connecting to lobby with uuid={:?}, UserInfo={:?}", uuid, user);

    let mut lobby_container = lobby_container.lobby_list.write().unwrap();
    if !lobby_container.contains_key(&uuid) {
        log::debug!("lobby with uuid={:?} not found", uuid);
        return Err(reject::not_found());
    }

    let lobby = lobby_container.get_mut(&uuid).unwrap();
    lobby.player_list.push("test".into());

    Ok(ws.on_upgrade(move |socket| client_connected(socket)))
}