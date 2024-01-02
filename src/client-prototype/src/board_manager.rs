use std::sync::Arc;
use quader_engine::game_settings::GameSettings;
use quader_engine::rng_manager::RngManager;
use quader_engine::wall_kick_data::WallKickData;
use crate::board_controller::BoardController;
use crate::board_controller_bot::BoardControllerBot;
use crate::renderable::Renderable;
use crate::updatable::Updatable;

pub struct BoardManager {
    pub player_board: Box<BoardController>,
    pub bot_board: Box<BoardControllerBot>,
    pub game_settings: GameSettings,
    wkd: Arc<WallKickData>
}

impl BoardManager {
    pub fn new() -> Self {

        let game_settings = GameSettings::default();
        let seed = RngManager::from_entropy().gen();
        let wkd = Arc::new(WallKickData::new(game_settings.wall_kick_data_mode));

        let player_board = BoardController::new(300., 128., game_settings, seed, Arc::clone(&wkd));
        let bot_board = BoardControllerBot::new(1200., 128., game_settings, seed, Arc::clone(&wkd), 4.0);

        Self {
            player_board: Box::new(player_board),
            bot_board: Box::new(bot_board),
            game_settings,
            wkd
        }
    }

    pub async fn load_content(&mut self) {
        self.player_board.load_content().await;
        self.bot_board.load_content().await;
    }

    pub fn update(&mut self, dt: f32) {
        self.player_board.update(dt);
        self.bot_board.update(dt);
    }

    pub fn render(&self) {
        self.player_board.render();
        self.bot_board.render();
    }
}