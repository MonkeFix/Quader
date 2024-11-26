use std::sync::Arc;

use ::rand::{thread_rng, Rng};
use macroquad::prelude::*;
use quader_engine::prelude::*;

use crate::assets::Assets;

use super::{
    controller::{Controller, ControllerBot, ControllerPlayer},
    renderer::{DefaultRenderer, Renderer},
    ControllerSettings,
};

struct BoardCouple {
    pub is_player: bool,
    pub controller: Box<dyn Controller>,
    pub renderer: Box<dyn Renderer>,
    pub is_disabled: bool,
}

pub struct Manager {
    //player_board: BoardCouple,
    //bot_board: BoardCouple,
    boards: Vec<BoardCouple>,
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
            is_player: true,
            controller: Box::new(ControllerPlayer::new(settings.clone())),
            renderer: Box::new(DefaultRenderer::new(300., 128., game_settings.board.height)),
            is_disabled: false,
        };
        let bot_board = BoardCouple {
            is_player: false,
            controller: Box::new(ControllerBot::new(settings.clone(), 1.3)),
            renderer: Box::new(DefaultRenderer::new(
                1200.,
                128.,
                game_settings.board.height,
            )),
            is_disabled: false,
        };
        let bot_board_2 = BoardCouple {
            is_player: false,
            controller: Box::new(ControllerBot::new(settings.clone(), 1.0)),
            renderer: Box::new(DefaultRenderer::new(800., 128., game_settings.board.height)),
            is_disabled: false,
        };

        let boards = vec![player_board, bot_board, bot_board_2];

        Self {
            boards,
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

            self.boards.iter_mut().for_each(|b| {
                b.controller.reset(Some(seed));
                b.is_disabled = false;
            });
        }

        // firstly, update all the boards and collect move results
        let mut results = vec![];

        let len = self.boards.len();

        for (index, board) in self.boards.iter_mut().enumerate() {
            if let Some(hd) = board.controller.update(&self.time_mgr) {
                results.push((index, hd));
            }
        }

        for (index, result) in results {
            match result {
                Ok(hd) => {
                    if hd.attack.out_damage > 0 {
                        // attack a random board, but not self and not disabled one
                        let mut rand_index = -1;
                        while rand_index == -1
                            || rand_index == index as i32
                            || self.boards[rand_index as usize].is_disabled
                        {
                            rand_index = thread_rng().gen_range(0..len) as i32;
                        }

                        self.boards[rand_index as usize]
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
                            info!("Someone is dead. Index: {}, Error: {:?}", index, err);
                        }
                        BoardErrorReason::BoardDisabled => {
                            info!(
                                "Player's board is disabled. Index: {}, Error: {:?}",
                                index, err
                            );
                        }
                    }
                    self.boards[index].is_disabled = true;
                    self.boards[index].controller.disable();
                }
            }
        }

        let mut enabled = 0;
        let mut is_player_enabled = false;
        for b in self.boards.iter() {
            if !b.is_disabled {
                enabled += 1;
                if b.is_player {
                    is_player_enabled = true;
                }
            }
        }

        if enabled <= 1 && !is_player_enabled {
            //info!("Game over! But who won???");
            self.boards.iter_mut().for_each(|b| {
                b.is_disabled = true;
                b.controller.disable();
            });
        }
    }

    pub fn render(&self) {
        if let Some(assets) = &self.assets {
            self.boards.iter().for_each(|b| {
                let board = b.controller.board();
                b.renderer.render(assets, board);
            });
        } else {
            panic!("assets are not loaded!");
        }
    }
}
