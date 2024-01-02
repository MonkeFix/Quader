use std::sync::Arc;
use cold_clear::{BotPollState, Info, Interface};
use quader_engine::game_settings::GameSettings;
use quader_engine::piece::PieceType;
use quader_engine::wall_kick_data::WallKickData;

pub struct BotBoard {
    pub engine_board: quader_engine::board::Board,
    pub game_settings: GameSettings,
    pub bot_interface: Box<Interface>
}

#[derive(Debug)]
pub enum BotStatus {
    Waiting, Dead
}

pub fn piece_type_to_piece(piece_type: PieceType) -> libtetris::Piece {
    match piece_type {
        PieceType::I => libtetris::Piece::I,
        PieceType::O => libtetris::Piece::O,
        PieceType::T => libtetris::Piece::T,
        PieceType::L => libtetris::Piece::L,
        PieceType::J => libtetris::Piece::J,
        PieceType::S => libtetris::Piece::S,
        PieceType::Z => libtetris::Piece::Z,
        PieceType::Pixel => panic!("Invalid conversion from pixel")
    }
}

impl BotBoard {
    pub fn new(game_settings: GameSettings, wkd: Arc<WallKickData>, seed: u64) -> Self {

        let board = quader_engine::board::Board::new(game_settings, wkd, seed);

        let mut bot_board = libtetris::Board::new();
        bot_board.add_next_piece(piece_type_to_piece(board.piece_mgr.cur_piece.get_type()));
        for pt in board.piece_mgr.piece_queue.queue {
            bot_board.add_next_piece(piece_type_to_piece(pt));
        }

        let bot_interface = Interface::launch(
            bot_board,
            cold_clear::Options::default(),
            cold_clear::evaluation::Standard::default(),
            None
        );

        Self {
            engine_board: board,
            bot_interface: Box::new(bot_interface),
            game_settings
        }
    }

    pub fn reset(&self, field: [[bool; 10]; 40], b2b_active: bool, combo: u32) {
        self.bot_interface.reset(field, b2b_active, combo);
    }

    pub fn add_next_piece(&self, piece_type: PieceType) {
        self.bot_interface.add_next_piece(piece_type_to_piece(piece_type));
    }

    pub fn request_next_move(&self, incoming_garbage: u32) {
        self.bot_interface.suggest_next_move(incoming_garbage);
    }

    pub fn poll_next_move(&self) -> Result<(libtetris::Move, Info), BotPollState> {
        self.bot_interface.poll_next_move()
    }

    pub fn block_next_move(&self) -> Option<(libtetris::Move, Info)> {
        self.bot_interface.block_next_move()
    }

    pub fn play_next_move(&self, falling_piece: libtetris::FallingPiece) {
       self.bot_interface.play_next_move(falling_piece);
    }
}