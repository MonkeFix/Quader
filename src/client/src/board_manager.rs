/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENSE file in the repository root for full licence text.
 */

use std::sync::{Arc};
use macroquad::miniquad::log;
use macroquad::prelude::{info, is_key_pressed, KeyCode};
use quader_engine::game_settings::GameSettings;
use quader_engine::rng_manager::RngManager;
use quader_engine::time_mgr::TimeMgr;
use quader_engine::wall_kick_data::WallKickData;
use crate::assets::Assets;
use crate::board_controller::BoardController;
use crate::board_controller_bot::BoardControllerBot;

pub struct BoardManager {
    pub player_board: Box<BoardController>,
    pub bot_board: Box<BoardControllerBot>,
    pub game_settings: GameSettings,
    pub time_mgr: TimeMgr,
    pub assets: Option<Assets>
}

impl BoardManager {
    pub fn new() -> Self {

        let game_settings = GameSettings::default();
        let seed = RngManager::from_entropy().gen();
        let wkd = Arc::new(WallKickData::new(game_settings.wall_kick_data_mode));

        let time_mgr = TimeMgr::new();

        let player_board = BoardController::new(
            300., 
            128., 
            game_settings, 
            seed, 
            Arc::clone(&wkd)
        );

        let bot_board = BoardControllerBot::new(
            1200.,
            128.,
            game_settings,
            seed,
            Arc::clone(&wkd),
            1.25
        );

        Self {
            player_board: Box::new(player_board),
            bot_board: Box::new(bot_board),
            game_settings,
            time_mgr,
            assets: None
        }
    }

    pub async fn load_content(&mut self) {
        self.assets = Some(Assets::load().await);
    }

    pub fn update(&mut self, dt: f32) {
        self.time_mgr.update(dt);

        if is_key_pressed(KeyCode::R) {
            let seed = RngManager::from_entropy().gen::<u64>();

            self.time_mgr.reset();

            self.player_board.reset(Some(seed));
            self.bot_board.reset(Some(seed));
        }

        if let Some(hd) = self.player_board.update(&self.time_mgr) {
            match hd {
                Ok(hd) => {
                    if hd.attack.out_damage > 0 {
                        let _ = &self.bot_board.bot_board.engine_board.attack(hd.attack.out_damage);
                    }
                }
                Err(_) => {
                    info!("Player is dead.");
                    self.player_board.board.disable();
                    self.bot_board.bot_board.engine_board.disable();
                }
            }
        }
        if let Some(hd) = self.bot_board.update(&self.time_mgr) {
            match hd {
                Ok(hd) => {
                    if hd.attack.out_damage > 0 {
                        let _ = &self.player_board.board.attack(hd.attack.out_damage);
                    }
                }
                Err(_) => {
                    info!("Bot is dead.");
                    self.player_board.board.disable();
                    self.bot_board.bot_board.engine_board.disable();
                }
            }
        }
    }

    pub fn render(&self) {
        if let Some(assets) = &self.assets {
            self.player_board.render(assets);
            self.bot_board.render(assets);
        } else {
            panic!("assets are not loaded!");
        }
    }
}