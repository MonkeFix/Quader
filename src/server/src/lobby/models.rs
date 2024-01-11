/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENCE file in the repository root for full licence text.
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
}

#[derive(Default, Clone, Serialize, Deserialize)]
pub struct Lobby {
    pub uuid: Uuid,
    pub lobby_name: String,
    pub player_limit: usize,
    pub player_list: Vec<String>,
    pub creator_username: String
}

impl Lobby {
    pub fn new(creator_username: String, uuid: Uuid, lobby_name: String, player_limit: usize) -> Self {
        Self {
            uuid,
            lobby_name,
            player_limit,
            player_list: vec![],
            creator_username
        }
    }
}
