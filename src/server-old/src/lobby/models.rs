/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENSE file in the repository root for full licence text.
 */

use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use serde::{Deserialize, Serialize};
use uuid::Uuid;
//use quader_engine::game_settings::GameSettings;

pub type Lobbies = Arc<RwLock<HashMap<String, Lobby>>>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LobbySettings {
    pub name: String,
    pub player_limit: usize,
    // TODO: This:
    // pub game_settings: GameSettings
}

impl LobbySettings {
    pub fn new(name: String, player_limit: usize) -> Self {
        Self {
            name,
            player_limit
        }
    }
}

#[derive(Clone)]
pub struct LobbyContainer {
    pub lobby_list: Lobbies
}

impl LobbyContainer {
    pub fn new() -> Self {
        Self {
            lobby_list: Arc::new(RwLock::new(HashMap::new()))
        }
    }

    pub fn contains_id(&self, uuid: &str) -> bool {
        self.lobby_list.read().unwrap().contains_key(uuid)
    }

    pub fn add_player(&mut self, uuid: &str, username: String) {
        let mut binding = self.lobby_list.write().unwrap();
        let lobby = binding.get_mut(uuid).unwrap();
        lobby.add_player(username);
    }

    pub fn remove_player(&mut self, uuid: &str, username: &str) {
        let mut binding = self.lobby_list.write().unwrap();
        let lobby = binding.get_mut(uuid).unwrap();
        lobby.remove_player(username);
    }
}

#[derive(Default, Clone, Serialize, Deserialize)]
pub struct Lobby {
    pub uuid: Uuid,
    pub lobby_name: String,
    pub player_limit: usize,
    pub player_list: Vec<String>,
    pub creator_username: String,
    pub is_started: bool
}

impl Lobby {
    pub fn new(creator_username: String, uuid: Uuid, lobby_name: String, player_limit: usize) -> Self {
        Self {
            uuid,
            lobby_name,
            player_limit,
            player_list: vec![],
            creator_username,
            is_started: false
        }
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
