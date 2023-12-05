use std::collections::VecDeque;
use crate::board::BoardComponent;

pub(crate) enum TSpinStatus {
    None, Full, Mini
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

#[derive(Default)]
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

#[derive(Default)]
pub struct ScoringMgr {
    pub combo: u32,
    pub b2b: u32
}

impl ScoringMgr {
    pub fn new() -> Self {
        Self::default()
    }
}

impl BoardComponent for ScoringMgr {
    fn get_name(&self) -> &'static str {
        "scoring_mgr"
    }

    fn reset(&mut self) {
        self.combo = 0;
        self.b2b = 0;
    }
}