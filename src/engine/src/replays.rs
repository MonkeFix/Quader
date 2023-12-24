use std::fmt::{Display, Formatter};
use serde::{Deserialize, Serialize};

/// Represents just a move done by a player.
/// This includes lines cleared
#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
pub struct HardDropInfo {
    pub lines_cleared: u32
}

impl Default for HardDropInfo {
    fn default() -> Self {
        Self {
            lines_cleared: 0
        }
    }
}

///
#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
pub struct MoveInfo {
    pub timestamp: f64,
    pub lines_cleared: u32,
    pub mod_bits: u32,
    pub b2b: u32,
    pub combo: u32,
    pub is_success: bool,
    pub attack: u32
}

impl Display for MoveInfo {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: LC: {}, Mods: {}, B2B: {}, Combo: {}, Success: {}, Attack: {}",
            self.timestamp,
            self.lines_cleared,
            self.mod_bits,
            self.b2b,
            self.combo,
            self.is_success,
            self.attack
        )
    }
}

impl Default for MoveInfo {
    fn default() -> Self {
        Self {
            timestamp: 0.0,
            lines_cleared: 0,
            mod_bits: 0,
            b2b: 0,
            combo: 0,
            is_success: false,
            attack: 0
        }
    }
}