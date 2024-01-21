/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENSE file in the repository root for full license text.
 */

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use tokio::task::JoinHandle;
use uuid::Uuid;

use crate::{
    ws::wsboard::{WsBoardMgr, WsBoardMgrHandle},
    LobbyName, LobbyUuid,
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LobbySettings {
    pub name: String,
    pub player_limit: usize,
    // TODO: This:
    // pub game_settings: GameSettings
}

#[derive(Debug, Clone, Serialize)]
pub struct LobbyListing {
    pub uuid: String,
    pub name: String,
    pub player_limit: usize,
    pub player_count: usize,
}

impl LobbySettings {
    pub fn new(name: String, player_limit: usize) -> Self {
        Self { name, player_limit }
    }
}

#[derive(Debug)]
pub struct LobbyContainer {
    pub lobby_map: HashMap<String, Lobby>,
}

impl LobbyContainer {
    pub fn new() -> Self {
        Self {
            lobby_map: HashMap::new(),
        }
    }

    pub fn contains_id(&self, uuid: &str) -> bool {
        self.lobby_map.contains_key(uuid)
    }

    pub fn add_lobby(&mut self, lobby: Lobby) -> Option<Lobby> {
        let uuid = lobby.uuid.clone();

        self.lobby_map.insert(uuid, lobby)
    }

    pub fn delete_lobby(&mut self, uuid: &str) -> Option<Lobby> {
        self.lobby_map.remove(uuid)
    }

    /// Tries to add a new player to lobby with specified `uuid`.
    /// If the lobby is full, returns `Err(())`.
    /// Returns `Ok(())` otherwise.
    pub fn add_player(&mut self, uuid: &str, username: String) -> Result<(), ()> {
        let lobby = self.lobby_map.get_mut(uuid).unwrap();
        lobby.add_player(username)
    }

    pub fn remove_player(&mut self, uuid: &str, username: &str) -> Result<(), ()> {
        let lobby = self.lobby_map.get_mut(uuid).unwrap();
        lobby.remove_player(username)
    }

    pub fn list_lobbies(&self) -> Vec<LobbyListing> {
        self.lobby_map
            .iter()
            .map(|kv| LobbyListing {
                uuid: kv.0.clone(),
                name: kv.1.lobby_name.clone(),
                player_limit: kv.1.player_limit,
                player_count: kv.1.player_count(),
            })
            .collect()
    }
}

#[derive(Debug)]
pub struct Lobby {
    pub uuid: String,
    pub lobby_name: String,
    pub player_limit: usize,
    pub player_list: Vec<String>,
    pub creator_username: String,
    pub is_started: bool,
    board_mgr: WsBoardMgrHandle,
    //board_mgr_task: JoinHandle<()>,
}

impl Lobby {
    pub fn new(
        creator_username: String,
        uuid: String,
        lobby_name: String,
        player_limit: usize,
    ) -> Self {
        let (board_mgr, handle) = WsBoardMgr::new();
        //let board_mgr = tokio::spawn(board_mgr.run());

        Self {
            uuid,
            lobby_name,
            player_limit,
            player_list: vec![],
            creator_username,
            is_started: false,
            board_mgr: handle,
            //board_mgr_task: board_mgr,
        }
    }

    pub fn start(mut self) {}

    pub fn from_settings(lobby_settings: LobbySettings, creator_username: String) -> Self {
        Lobby::from_settings_with_id(lobby_settings, creator_username, Uuid::new_v4().to_string())
    }

    pub fn from_settings_with_id(
        lobby_settings: LobbySettings,
        creator_username: String,
        uuid: String,
    ) -> Self {
        Self::new(
            creator_username,
            uuid,
            lobby_settings.name,
            lobby_settings.player_limit,
        )
    }

    pub fn player_count(&self) -> usize {
        self.player_list.len()
    }

    /// Tries to add a new player. If the lobby is full, returns `Err(())`.
    /// Returns `Ok(())` otherwise.
    pub fn add_player(&mut self, username: String) -> Result<(), ()> {
        if self.player_count() == self.player_limit {
            return Err(());
        }

        self.player_list.push(username);
        Ok(())
    }

    pub fn remove_player(&mut self, username: &str) -> Result<(), ()> {
        let index = self.player_list.iter().position(|x| x == username);
        if let Some(index) = index {
            self.player_list.remove(index);
            return Ok(());
        }

        Err(())
    }
}
