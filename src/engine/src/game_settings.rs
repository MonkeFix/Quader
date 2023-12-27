use serde::{Deserialize, Serialize};
use crate::wall_kick_data::{WallKickDataMode};

//pub const BOARD_WIDTH: usize = 10;
//pub const BOARD_HEIGHT: usize = 40;
//pub const BOARD_FULL_HEIGHT: usize = BOARD_HEIGHT * 2;

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct GravitySettings {
    pub grav_const: f32,
    pub grav_base: f32,
    pub grav_incr: f32,
    pub lock_delay: f32,
    pub lock_prolong_amount: f32
}

impl Default for GravitySettings {
    fn default() -> Self {
        Self {
            grav_const: 0.0,
            grav_base: 0.8,
            grav_incr: 0.007,
            lock_delay: 1.0,
            lock_prolong_amount: 0.02
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize, Deserialize)]
pub struct AttackSettings {
    pub lines_0: u32,
    pub lines_1: u32,
    pub lines_2: u32,
    pub lines_3: u32,
    pub lines_4: u32,
    pub t_spin_single: u32,
    pub t_spin_double: u32,
    pub t_spin_triple: u32,
    pub t_spin_single_mini: u32,
    pub all_clear: u32,
    pub b2bs: [u32; 5],
    pub combos: [u32; 5],
    pub garbage_delay_ms: u32
}

impl Default for AttackSettings {
    fn default() -> Self {
        Self {
            lines_0: 0,
            lines_1: 0,
            lines_2: 1,
            lines_3: 2,
            lines_4: 4,
            t_spin_single: 4,
            t_spin_double: 6,
            t_spin_triple: 2,
            t_spin_single_mini: 1,
            all_clear: 10,
            b2bs: [1,2,3,4,5],
            combos: [1,2,3,4,5],
            garbage_delay_ms: 500,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct BoardSettings {
    pub width: usize,
    pub height: usize
}

impl Default for BoardSettings {
    fn default() -> Self {
        Self {
            width: 10,
            height: 20
        }
    }
}

impl BoardSettings {
    pub fn full_height(&self) -> usize {
        self.height * 2
    }
}

#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
pub struct GameSettings {
    pub gravity: GravitySettings,
    pub board: BoardSettings,
    pub attack: AttackSettings,
    pub wall_kick_data_mode: WallKickDataMode
}
impl Default for GameSettings {
    fn default() -> Self {
        Self {
            gravity: GravitySettings::default(),
            board: BoardSettings::default(),
            attack: AttackSettings::default(),
            wall_kick_data_mode: WallKickDataMode::Standard
        }
    }
}