use std::sync::Arc;
use quader_engine::game_settings::GameSettings;
use quader_engine::wall_kick_data::WallKickData;
use quader_skynet::bot_board::{BotBoard, BotSettings};
use crate::board_renderer::BoardRenderer;

pub struct BoardControllerBot {
    game_settings: GameSettings,
    bot_board: Box<BotBoard>,
    board_renderer: BoardRenderer
}

impl BoardControllerBot {
    pub fn new(x: f32, y: f32, game_settings: GameSettings, seed: u64, wkd: Arc<WallKickData>, target_pps: f32) -> Self {
        Self {
            game_settings,
            bot_board: Box::new(BotBoard::new(game_settings, wkd, seed, BotSettings { target_pps })),
            board_renderer: BoardRenderer::new(x, y, game_settings.board.height)
        }
    }

    pub async fn load_content(&mut self) {
        self.board_renderer.load_content().await;
    }

    pub fn render(&self) {
        self.board_renderer.render(&self.bot_board.engine_board)
    }

    pub fn update(&mut self, dt: f32) {
        self.bot_board.update(dt);
    }
}