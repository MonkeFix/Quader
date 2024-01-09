/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENCE file in the repository root for full licence text.
 */

mod bot_board;

pub use bot_board::BotBoard;
use quader_engine::piece::PieceType;

#[derive(Debug, Copy, Clone)]
pub struct BotSettings {
    /// Target Pieces Per Second.
    pub target_pps: f32
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