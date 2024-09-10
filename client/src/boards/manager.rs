use std::sync::Arc;

use macroquad::prelude::*;
use quader_engine::prelude::*;

use crate::assets::Assets;

use super::{
    controller::{Controller, ControllerBot, ControllerPlayer},
    renderer::{DefaultRenderer, Renderer},
    ControllerSettings,
};

struct BoardCouple {
    pub controller: Box<dyn Controller>,
    pub renderer: Box<DefaultRenderer>,
}

pub struct Manager {
    player_board: BoardCouple,
    bot_board: BoardCouple,
    pub game_settings: GameSettings,
    pub time_mgr: TimeMgr,
    pub assets: Option<Assets>,
}

impl Manager {
    pub fn new() -> Self {
        let game_settings = GameSettings::default();
        let time_mgr = TimeMgr::new();
        let seed = RngManager::from_entropy().gen();
        let wkd = Arc::new(WallKickData::new(game_settings.wall_kick_mode));

        let settings = ControllerSettings {
            game_settings,
            seed,
            wkd: wkd.clone(),
        };

        let player_board = BoardCouple {
            controller: Box::new(ControllerPlayer::new(settings.clone())),
            renderer: Box::new(DefaultRenderer::new(300., 128., game_settings.board.height)),
        };
        let bot_board = BoardCouple {
            controller: Box::new(ControllerBot::new(settings.clone(), 1.3)),
            renderer: Box::new(DefaultRenderer::new(
                1200.,
                128.,
                game_settings.board.height,
            )),
        };

        Self {
            player_board,
            bot_board,
            game_settings,
            time_mgr,
            assets: None,
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

            self.player_board.controller.reset(Some(seed));
            self.bot_board.controller.reset(Some(seed));
        }

        if let Some(hd) = self.player_board.controller.update(&self.time_mgr) {
            match hd {
                Ok(hd) => {
                    if hd.attack.out_damage > 0 {
                        let _ = &self
                            .bot_board
                            .controller
                            .board_mut()
                            .attack(hd.attack.out_damage);
                    }
                }
                Err(err) => {
                    match err {
                        BoardErrorReason::CannotApplyPiece
                        | BoardErrorReason::BoardDead
                        | BoardErrorReason::CannotSpawnPiece => {
                            info!("Player is dead. {:?}", err);
                        }
                        BoardErrorReason::BoardDisabled => {
                            info!("Player's board is disabled. {:?}", err);
                        }
                    }

                    self.player_board.controller.disable();
                    self.bot_board.controller.disable();
                }
            }
        }
        if let Some(hd) = self.bot_board.controller.update(&self.time_mgr) {
            match hd {
                Ok(hd) => {
                    if hd.attack.out_damage > 0 {
                        let _ = &self
                            .player_board
                            .controller
                            .board_mut()
                            .attack(hd.attack.out_damage);
                    }
                }
                Err(err) => {
                    match err {
                        BoardErrorReason::CannotApplyPiece
                        | BoardErrorReason::BoardDead
                        | BoardErrorReason::CannotSpawnPiece => {
                            info!("Bot is dead. {:?}", err);
                        }
                        BoardErrorReason::BoardDisabled => {
                            info!("Bot's board is disabled. {:?}", err);
                        }
                    }
                    self.player_board.controller.disable();
                    self.bot_board.controller.disable();
                }
            }
        }
    }

    pub fn render(&self) {
        if let Some(assets) = &self.assets {
            let board = self.player_board.controller.board();
            self.player_board.renderer.render(assets, board);

            let board = self.bot_board.controller.board();
            self.bot_board.renderer.render(assets, board);
        } else {
            panic!("assets are not loaded!");
        }
    }
}
