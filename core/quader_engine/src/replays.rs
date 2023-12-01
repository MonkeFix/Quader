use std::fmt::{Display, Formatter};
use serde::{Deserialize, Serialize};

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