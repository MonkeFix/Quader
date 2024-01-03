use std::sync::Arc;
use macroquad::prelude::{is_key_pressed, KeyCode};
use quader_engine::game_settings::GameSettings;
use quader_engine::rng_manager::RngManager;
use quader_engine::wall_kick_data::WallKickData;
use crate::board_controller::BoardController;
use crate::board_controller_bot::BoardControllerBot;

pub struct BoardManager {
    pub player_board: Box<BoardController>,
    pub bot_board: Box<BoardControllerBot>,
    pub game_settings: GameSettings,
}

impl BoardManager {
    pub fn new() -> Self {

        let game_settings = GameSettings::default();
        let seed = RngManager::from_entropy().gen();
        let wkd = Arc::new(WallKickData::new(game_settings.wall_kick_data_mode));

        let player_board = BoardController::new(300., 128., game_settings, seed, Arc::clone(&wkd));
        let bot_board = BoardControllerBot::new(
            1200.,
            128.,
            game_settings,
            seed,
            Arc::clone(&wkd),
            0.5
        );

        Self {
            player_board: Box::new(player_board),
            bot_board: Box::new(bot_board),
            game_settings,
        }
    }

    pub async fn load_content(&mut self) {
        self.player_board.load_content().await;
        self.bot_board.load_content().await;
    }

    pub fn update(&mut self, dt: f32) {
        if is_key_pressed(KeyCode::R) {
            let seed = RngManager::from_entropy().gen::<u64>();

            self.player_board.reset(Some(seed));
            self.bot_board.reset(Some(seed));
        }

        if let Some(hd) = self.player_board.update(dt) {
            match hd {
                Ok(hd) => {
                    if hd.attack.out_damage > 0 {
                        let _ = &self.bot_board.bot_board.engine_board.attack(hd.attack.out_damage);
                    }
                }
                Err(_) => {}
            }
        }
        if let Some(hd) = self.bot_board.update(dt) {
            match hd {
                Ok(hd) => {
                    if hd.attack.out_damage > 0 {
                        let _ = &self.player_board.board.attack(hd.attack.out_damage);
                    }
                }
                Err(_) => {}
            }
        }
    }

    pub fn render(&self) {
        self.player_board.render();
        self.bot_board.render();
    }
}