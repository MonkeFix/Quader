use std::sync::Arc;

use quader_engine::prelude::*;

pub(crate) mod controller;
pub(crate) mod manager;
pub(crate) mod renderer;

#[derive(Debug, Clone)]
pub struct ControllerSettings {
    pub game_settings: GameSettings,
    pub seed: u64,
    pub wkd: Arc<WallKickData>,
}

impl Default for ControllerSettings {
    fn default() -> Self {
        let seed = RngManager::from_entropy().gen();
        Self {
            game_settings: GameSettings::default(),
            seed,
            wkd: Arc::new(WallKickData::new(WallKickMode::Standard)),
        }
    }
}
