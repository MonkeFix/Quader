/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENSE file in the repository root for full licence text.
 */

use std::sync::{Arc};
use macroquad::prelude::*;

use quader_engine::board::{Board};
use quader_engine::game_settings::{GameSettings};
use quader_engine::piece::{RotationDirection};
use quader_engine::piece_mgr::BoardErrorReason;
use quader_engine::replays::MoveResult;
use quader_engine::rng_manager::RngManager;
use quader_engine::time_mgr::TimeMgr;
use quader_engine::wall_kick_data::WallKickData;
use crate::assets::Assets;
use crate::board_renderer::BoardRenderer;

struct PieceMover {
    elapsed: f32,
    #[allow(dead_code)]
    arr: f32,
    das: f32,
    sdf: u32,
    is_left_down: bool,
    is_right_down: bool
}

impl PieceMover {
    pub fn move_left(&self, board: &mut Board) {
        board.move_left(1);
    }

    pub fn move_right(&self, board: &mut Board) {
        board.move_right(1);
    }

    pub fn reset(&mut self) {
        self.elapsed = 0.0;
        self.is_left_down = false;
        self.is_right_down = false;
    }
}

pub struct BoardController {
    pub board: Board,
    piece_mover: PieceMover,
    board_renderer: BoardRenderer
    //wkd: Arc<WallKickData>
}

impl BoardController {
    pub fn new(x: f32, y: f32, game_settings: GameSettings, seed: u64, wkd: Arc<WallKickData>) -> Self {

        let board = Board::new(game_settings, Arc::clone(&wkd), seed);

        dbg!(&game_settings);

        BoardController {
            board,
            piece_mover: PieceMover {
                elapsed: 0.0,
                arr: 0.0,
                das: 128.0,
                sdf: u32::MAX,
                is_left_down: false,
                is_right_down: false
            },
            board_renderer: BoardRenderer::new(x, y, game_settings.board.height)
            //wkd
        }
    }

    pub fn render(&self, assets: &Assets) {
        self.board_renderer.render(assets, &self.board);
    }

    pub fn update(&mut self, time_mgr: &TimeMgr) -> Option<Result<MoveResult, BoardErrorReason>> {
        let elapsed = time_mgr.last_dt * 1000.0; // convert to milliseconds
        let mut result = None;

        if is_key_pressed(KeyCode::Left) {
            self.piece_mover.move_left(&mut self.board);
        }
        if is_key_down(KeyCode::Left) {
            self.piece_mover.is_left_down = true;
            self.piece_mover.elapsed += elapsed;
        }
        if is_key_released(KeyCode::Left) {
            self.piece_mover.is_left_down = false;
            self.piece_mover.elapsed = 0.0;
        }

        if is_key_pressed(KeyCode::Right) {
            self.piece_mover.move_right(&mut self.board);
        }
        if is_key_down(KeyCode::Right) {
            self.piece_mover.is_right_down = true;
            self.piece_mover.elapsed += elapsed;
        }
        if is_key_released(KeyCode::Right) {
            self.piece_mover.is_right_down = false;
            self.piece_mover.elapsed = 0.0;
        }

        if self.piece_mover.elapsed >= self.piece_mover.das {
            let moves = 10;

            for _ in 0..moves {
                if self.piece_mover.is_left_down {
                    self.piece_mover.move_left(&mut self.board);
                }
                if self.piece_mover.is_right_down {
                    self.piece_mover.move_right(&mut self.board);
                }
            }
        }

        if is_key_down(KeyCode::Down) {
            self.board.soft_drop(self.piece_mover.sdf);
        }
        if is_key_pressed(KeyCode::Space) {
            result = Some(self.board.hard_drop());
        }
        if is_key_pressed(KeyCode::Z) {
            self.board.rotate(RotationDirection::CounterClockwise);
        }
        if is_key_pressed(KeyCode::X) {
            self.board.rotate(RotationDirection::Clockwise);
        }
        if is_key_pressed(KeyCode::F) {
            self.board.rotate(RotationDirection::Deg180);
        }
        if is_key_pressed(KeyCode::C) {
            self.board.try_hold_piece();
        }

        if is_key_pressed(KeyCode::T) {
            let mut rng = RngManager::from_entropy();
            self.board.attack(rng.gen_range(0..6));
        }

        if let Some(res) = self.board.update(time_mgr) {
            result = Some(res);
        }

        result
    }

    pub fn reset(&mut self, new_seed: Option<u64>) {
        self.board.reset(new_seed);
        self.piece_mover.reset();
    }
}
