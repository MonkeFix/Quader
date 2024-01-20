/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENSE file in the repository root for full license text.
 */

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use uuid::Uuid;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LobbySettings {
    pub name: String,
    pub player_limit: usize,
    // TODO: This:
    // pub game_settings: GameSettings
}

impl LobbySettings {
    pub fn new(name: String, player_limit: usize) -> Self {
        Self { name, player_limit }
    }
}

#[derive(Debug, Clone)]
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

    pub fn add_player(&mut self, uuid: &str, username: String) {
        let lobby = self.lobby_map.get_mut(uuid).unwrap();
        lobby.add_player(username);
    }

    pub fn remove_player(&mut self, uuid: &str, username: &str) {
        let lobby = self.lobby_map.get_mut(uuid).unwrap();
        lobby.remove_player(username);
    }
}

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct Lobby {
    pub uuid: String,
    pub lobby_name: String,
    pub player_limit: usize,
    pub player_list: Vec<String>,
    pub creator_username: String,
    pub is_started: bool,
}

impl Lobby {
    pub fn new(
        creator_username: String,
        uuid: String,
        lobby_name: String,
        player_limit: usize,
    ) -> Self {
        Self {
            uuid,
            lobby_name,
            player_limit,
            player_list: vec![],
            creator_username,
            is_started: false,
        }
    }

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

    pub fn add_player(&mut self, username: String) {
        self.player_list.push(username);
    }

    pub fn remove_player(&mut self, username: &str) {
        let index = self.player_list.iter().position(|x| x == username).unwrap();
        self.player_list.remove(index);
    }
}