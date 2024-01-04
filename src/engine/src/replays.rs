use serde::{Deserialize, Serialize};
use crate::cell_holder::CellHolder;
use crate::damage_calculation::{calculate_damage, create_board_move_bits};
use crate::game_settings::AttackSettings;
use crate::garbage_mgr::{GarbageHardDropResult, GarbageMgr};
use crate::scoring::{ScoringMgr, TSpinStatus};
use crate::time_mgr::{TimeMgr};

/// Represents just a move done by a player.
/// This includes lines cleared
#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
pub struct HardDropInfo {
    pub lines_cleared: u32,
    pub tspin_status: TSpinStatus,
    pub last_move_type: LastMoveType,
    pub occupied_cells_left: u32
}

#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
pub enum MoveAction {
    MoveLeft,
    MoveRight,

    RotateCW,
    RotateCCW,
    RotateDeg180,

    SoftDrop,
    HardDrop,

    HoldPiece
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize, Deserialize)]
pub enum LastMoveType {
    None, Rotation, Movement
}

impl Default for HardDropInfo {
    fn default() -> Self {
        Self {
            lines_cleared: 0,
            tspin_status: TSpinStatus::None,
            last_move_type: LastMoveType::None,
            occupied_cells_left: 0
        }
    }
}

///
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MoveResult {
    pub timestamp: f32,
    pub mod_bits: u32,
    pub b2b: u32,
    pub combo: u32,
    /// False if the piece fails to spawn or apply to the cell holder. True otherwise.
    pub is_success: bool,
    pub attack: GarbageHardDropResult,

    pub hard_drop_info: HardDropInfo,
    pub move_queue: Vec<MoveAction>
}

/*impl Display for MoveResult {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: Mods: {}, B2B: {}, Combo: {}, Success: {}, Attack: {}",
            self.timestamp,
            self.mod_bits,
            self.b2b,
            self.combo,
            self.is_success,
            self.attack
        )
    }
}*/

impl Default for MoveResult {
    fn default() -> Self {
        Self {
            timestamp: 0.0,
            mod_bits: 0,
            b2b: 0,
            combo: 0,
            is_success: false,
            attack: GarbageHardDropResult::default(),
            move_queue: vec![],
            hard_drop_info: HardDropInfo::default()
        }
    }
}

impl MoveResult {
    pub fn new(
        scoring_mgr: &ScoringMgr,
        hard_drop_info: HardDropInfo,
        attack_settings: &AttackSettings,
        garbage_mgr: &mut GarbageMgr,
        cell_holder: &CellHolder,
        move_queue: Vec<MoveAction>,
        cur_sec: f32
    ) -> MoveResult {
        let mut result = MoveResult {
            is_success: true,
            b2b: scoring_mgr.b2b,
            combo: scoring_mgr.combo,
            hard_drop_info,
            move_queue,
            timestamp: cur_sec,
            ..Default::default()
        };

        let bits = create_board_move_bits(
            cell_holder.get_occupied_cell_count() as u32,
            &result,
            hard_drop_info.tspin_status
        );
        result.mod_bits = bits;

        let dmg = calculate_damage(attack_settings, &result);
        result.attack = garbage_mgr.hard_drop(hard_drop_info.lines_cleared, dmg as i32);

        result
    }
}

#[derive(Default, Debug, Copy, Clone, Serialize, Deserialize)]
pub struct BoardStats {
    /// Total time spent in current game in seconds
    pub elapsed_seconds: f32,
    /// Attack Per Minute
    pub apm: f32,
    /// Pieces Per Second
    pub pps: f32,
    /// Total pieces placed on board in current game
    pub total_pieces: u32,

    pub singles: u32,
    pub doubles: u32,
    pub triples: u32,
    pub quads: u32,
    pub tspins: u32,
    pub tspin_minis: u32,
    pub tspin_singles: u32,
    pub tspin_doubles: u32,
    pub tspin_triples: u32,
    pub all_clears: u32,
    pub max_combo: u32,
    pub max_b2b: u32
}

impl BoardStats {
    pub fn new() -> Self {
        Self::default()
    }

    /// Updates elapsed seconds to correctly calculate APM and PPS.
    pub fn update(&mut self, time_mgr: &TimeMgr) {
        self.elapsed_seconds = time_mgr.elapsed_sec;
        self.pps = self.total_pieces as f32 / self.elapsed_seconds;
        self.apm = self.total_pieces as f32 / (self.elapsed_seconds / 60.0);
    }

    /// Updates all current stats using data from `HardDropInfo` and `ScoringMgr`.
    pub fn hard_drop(&mut self, hard_drop_info: &HardDropInfo, scoring_mgr: &ScoringMgr) {
        self.total_pieces += 1;
        
        match hard_drop_info.lines_cleared {
            1 => self.singles += 1,
            2 => self.doubles += 1,
            3 => self.triples += 1,
            4 => self.quads += 1,
            _ => {}
        };

        match hard_drop_info.tspin_status {
            TSpinStatus::None => {}
            TSpinStatus::Full => {
                self.tspins += 1;
                match hard_drop_info.lines_cleared {
                    1 => self.tspin_singles += 1,
                    2 => self.tspin_doubles += 1,
                    3 => self.tspin_triples += 1,
                    _ => {}
                }
            }
            TSpinStatus::Mini => {
                self.tspin_minis += 1;
                match hard_drop_info.lines_cleared {
                    1 => self.tspin_singles += 1,
                    2 => self.tspin_doubles += 1,
                    _ => {}
                }
            }
        }

        if hard_drop_info.occupied_cells_left == 0 {
            self.all_clears += 1;
        }

        self.max_b2b = std::cmp::max(self.max_b2b, scoring_mgr.b2b);
        self.max_combo = std::cmp::max(self.max_combo, scoring_mgr.combo);
    }

    pub fn reset(&mut self) {
        let default = Self::default();

        self.elapsed_seconds = default.elapsed_seconds;
        self.apm = default.apm;
        self.pps = default.pps;
        self.total_pieces = default.total_pieces;
        self.doubles = default.doubles;
        self.triples = default.triples;
        self.quads = default.quads;
        self.tspins = default.tspins;
        self.tspin_minis = default.tspin_minis;
        self.tspin_singles = default.tspin_singles;
        self.tspin_doubles = default.tspin_doubles;
        self.tspin_triples = default.tspin_triples;
        self.all_clears = default.all_clears;
        self.max_combo = default.max_combo;
        self.max_b2b = default.max_b2b;
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReplayMove {
    pub action: MoveAction,
    pub timestamp: f32
}

#[derive(Default, Debug, Clone, Serialize, Deserialize)]
pub struct ReplayMgr {
    pub moves: Vec<ReplayMove>,
    pub cur_move_queue: Vec<MoveAction>
}

impl ReplayMgr {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push_move(&mut self, timestamp: f32, move_action: MoveAction) {
        self.moves.push(ReplayMove {
            action: move_action,
            timestamp
        });
        self.cur_move_queue.push(move_action);
    }

    pub fn end_move(&mut self) -> Vec<MoveAction> {
        let res = self.cur_move_queue.clone();
        self.cur_move_queue.clear();

        res
    }

    pub fn reset(&mut self) {
        self.moves.clear();
        self.cur_move_queue.clear();
    }
}