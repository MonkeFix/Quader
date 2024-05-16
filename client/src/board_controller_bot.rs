/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENSE file in the repository root for full licence text.
 */

use crate::assets::Assets;
use crate::board_renderer::BoardRenderer;
use quader_engine::board::piece::manager::BoardErrorReason;
use quader_engine::board::piece::wall_kick::WallKickData;
use quader_engine::board::replays::MoveResult;
use quader_engine::{settings::GameSettings, time::TimeMgr};
use quader_skynet::{BotBoard, BotSettings};
use std::sync::Arc;

pub struct BoardControllerBot {
    pub bot_board: Box<BotBoard>,
    board_renderer: BoardRenderer,
}

impl BoardControllerBot {
    pub fn new(
        x: f32,
        y: f32,
        game_settings: GameSettings,
        seed: u64,
        wkd: Arc<WallKickData>,
        target_pps: f32,
    ) -> Self {
        Self {
            bot_board: Box::new(BotBoard::new(
                game_settings,
                wkd,
                seed,
                BotSettings { target_pps },
            )),
            board_renderer: BoardRenderer::new(x, y, game_settings.board.height),
        }
    }

    pub fn render(&self, assets: &Assets) {
        self.board_renderer
            .render(assets, &self.bot_board.engine_board)
    }

    pub fn update(&mut self, time_mgr: &TimeMgr) -> Option<Result<MoveResult, BoardErrorReason>> {
        self.bot_board.update(time_mgr)
    }

    pub fn reset(&mut self, new_seed: Option<u64>) {
        self.bot_board.reset(new_seed);
    }
}
