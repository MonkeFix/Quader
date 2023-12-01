use serde::{Deserialize, Serialize};
use crate::board::GameState;
use crate::piece::{PieceType, RotationDirection};

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum BoardMoveDir {
    Left, Right
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum BoardCommandType {
    // move direction, delta
    Move(BoardMoveDir, i32),
    Rotate(RotationDirection),
    HardDrop,
    // delta
    SoftDrop(u32),
    // amount, messiness
    SendGarbage(u32, u32)
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum BoardMessage {
    NewPieceInQueue(PieceType),
    PieceUpdated,
    // Amount, hole x position
    GarbageReceived(u32, u32),
    GameStateChanged(GameState),
    PlayerRemoved,
    BoardUpdated
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct BoardCommand {
    command_type: BoardCommandType
}

impl BoardCommand {
    pub fn new(command_type: BoardCommandType) -> Self {
        Self {
            command_type
        }
    }

    pub fn get_type(&self) -> &BoardCommandType {
        &self.command_type
    }
}