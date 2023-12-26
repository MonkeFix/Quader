use std::collections::VecDeque;
use serde::{Deserialize, Serialize};
use crate::replays::{HardDropInfo, LastMoveType};

#[derive(Debug, Copy, Clone, PartialEq, Serialize, Deserialize)]
pub enum TSpinStatus {
    /// T-Spin wasn't performed.
    None,
    /// Full T-Spin: at least 3 occupied cells around the corners of the T piece.
    Full,
    /// Mini T-Spin: 2 occupied cells around the corners of the T piece.
    Mini
}

pub mod thresholds {
    pub const COMBO_1_THRESHOLD: u32 = 1;
    pub const COMBO_2_THRESHOLD: u32 = 6;
    pub const COMBO_3_THRESHOLD: u32 = 10;
    pub const COMBO_4_THRESHOLD: u32 = 15;
    pub const COMBO_5_THRESHOLD: u32 = 18;


    pub const B2B_1_THRESHOLD: u32 = 0;
    pub const B2B_2_THRESHOLD: u32 = 5;
    pub const B2B_3_THRESHOLD: u32 = 10;
    pub const B2B_4_THRESHOLD: u32 = 30;
    pub const B2B_5_THRESHOLD: u32 = 60;
}

pub mod damage_mods {
    pub const NONE: u32 = 0;

    pub const T_SPIN_MINI: u32 = 1 << 1;
    pub const T_SPIN_FULL: u32 = 1 << 2;

    pub const SINGLE: u32 = 1 << 3;
    pub const DOUBLE: u32 = 1 << 4;
    pub const TRIPLE: u32 = 1 << 5;
    pub const QUAD: u32 = 1 << 6;

    pub const ALL_CLEAR: u32 = 1 << 7;

    pub const B2B_1: u32 = 1 << 8;
    pub const B2B_2: u32 = 1 << 9;
    pub const B2B_3: u32 = 1 << 10;
    pub const B2B_4: u32 = 1 << 11;
    pub const B2B_5: u32 = 1 << 12;

    pub const COMBO_1: u32 = 1 << 13;
    pub const COMBO_2: u32 = 1 << 14;
    pub const COMBO_3: u32 = 1 << 15;
    pub const COMBO_4: u32 = 1 << 16;
    pub const COMBO_5: u32 = 1 << 17;
}

pub fn has_flag(value: u32, flag: u32) -> bool {
    value & flag != 0
}

#[derive(Default, Debug, Clone, Serialize, Deserialize)]
pub struct DamageMgr {
    attack_queue: VecDeque<u32>,
    incoming_damage: Vec<u32>,
    last_garbage_x: u32,
    cur_garbage_cd: f32
}

impl DamageMgr {
    pub fn new() -> Self {
        Self {
            attack_queue: VecDeque::new(),
            incoming_damage: Vec::new(),
            last_garbage_x: 0,
            cur_garbage_cd: 0.0
        }
    }
}

#[derive(Default, Debug, Copy, Clone, Serialize, Deserialize)]
pub struct ScoringMgr {
    pub combo: u32,
    pub b2b: u32
}

impl ScoringMgr {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn hard_drop(&mut self, hard_drop_info: &HardDropInfo) {
        // Do not break B2B if and only if the player:
        //  - haven't cleared any lines,
        //  - cleared exactly 4 lines,
        //  - performed a T-Spin which must include a rotation of the piece.
        // Otherwise break it.
        if hard_drop_info.lines_cleared == 4 ||
            hard_drop_info.last_move_type == LastMoveType::Rotation &&
                (
                    hard_drop_info.lines_cleared >= 1 ||
                        hard_drop_info.tspin_status == TSpinStatus::Mini ||
                        hard_drop_info.tspin_status == TSpinStatus::Full
                ) {
            self.b2b += 1;

        } else if hard_drop_info.lines_cleared != 0 {
            self.b2b = 0;
        }

        // Combos are much easier:
        // if a player cleared 1 or more lines in a row, for each hard drop combo increases by one,
        // otherwise if a player didn't clear any lines, combo resets back to 0.
        if hard_drop_info.lines_cleared > 0 {
            self.combo += 1;
        } else {
            self.combo = 0;
        }
    }

    pub fn reset(&mut self) {
        self.combo = 0;
        self.b2b = 0;
    }
}