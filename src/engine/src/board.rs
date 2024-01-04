use std::sync::{Arc, Mutex, RwLock};
use crate::cell_holder::{CellHolder};
use crate::game_settings::{GameSettings};
use crate::garbage_mgr::GarbageMgr;
use crate::gravity_mgr::{GravityMgr, GravityUpdateResult};
use crate::piece::{Piece, PieceType, RotationDirection, RotationState};
use crate::piece_mgr::{PieceMgr, UpdateErrorReason};
use crate::replays::{BoardStats, MoveAction, MoveResult, ReplayMgr};
use crate::scoring::{ScoringMgr};
use crate::time_mgr::TimeMgr;
use crate::wall_kick_data::{WallKickData};

#[derive(Debug)]
pub struct Board {
    pub game_settings: GameSettings,

    pub(crate) gravity_mgr: GravityMgr,
    pub piece_mgr: Box<PieceMgr>,
    pub(crate) is_enabled: bool,

    wkd: Arc<WallKickData>,
    pub(crate) scoring_mgr: ScoringMgr,
    pub board_stats: BoardStats,
    pub is_dead: bool,
    pub garbage_mgr: GarbageMgr,
    pub replay_mgr: ReplayMgr,

    cur_sec: f32
}

impl Board {

    pub fn new(game_settings: GameSettings, wkd: Arc<WallKickData>, seed: u64) -> Self {

        let gravity_mgr = GravityMgr::new(&game_settings.gravity);
        let piece_mgr = Box::new(PieceMgr::new(&game_settings, seed));

        Self {
            game_settings,
            gravity_mgr,
            piece_mgr,
            is_enabled: true,
            wkd: Arc::clone(&wkd),
            scoring_mgr: ScoringMgr::new(),
            board_stats: BoardStats::default(),
            is_dead: false,
            garbage_mgr: GarbageMgr::new(&game_settings.attack),
            replay_mgr: ReplayMgr::default(),
            cur_sec: 0.0
        }
    }

    /// Updates `GravityMgr` by sending delta time `dt` and updating its current variables:
    /// lock, gravity. Force hard drops the piece if `GravityMgr` requests it to.
    pub fn update(&mut self, time_mgr: &TimeMgr) -> Option<Result<MoveResult, UpdateErrorReason>> {
        if !self.is_enabled {
            return None;
        }

        if self.is_dead {
            return Some(Err(UpdateErrorReason::BoardDead));
        }

        self.cur_sec = time_mgr.elapsed_sec;

        self.board_stats.update(time_mgr);
        self.garbage_mgr.update(time_mgr);

        let res = self.gravity_mgr.update(&self.piece_mgr, time_mgr);
        match res {
            GravityUpdateResult::None => None,
            GravityUpdateResult::SoftDrop(dt) => {
                self.soft_drop(dt);
                None
            }
            GravityUpdateResult::HardDrop => {
                Some(self.hard_drop())
            }
        }
    }

    /// Tries to move current piece to the left by amount `delta`.
    pub fn move_left(&mut self, delta: u32) -> u32 {
        let mut moves_count = 0;

        for _ in 0..delta {
            if self.piece_mgr.move_left() {
                self.replay_mgr.push_move(self.cur_sec, MoveAction::MoveLeft);
                moves_count += 1;
            }
        }

        moves_count
    }

    /// Tries to move current piece to the right by amount `delta`.
    pub fn move_right(&mut self, delta: u32) -> u32 {
        let mut moves_count = 0;

        for _ in 0..delta {
            if self.piece_mgr.move_right() {
                self.replay_mgr.push_move(self.cur_sec, MoveAction::MoveRight);
                moves_count += 1;
            }
        }

        moves_count
    }

    /// Tries to rotate current piece to direction `RotationDirection`
    pub fn rotate(&mut self, direction: RotationDirection) -> Option<RotationState> {
        if self.piece_mgr.rotate(&self.wkd, direction) {
            // prolong lock only if rotation was successful
            self.gravity_mgr.prolong_lock();

            let action = match direction {
                RotationDirection::Clockwise => MoveAction::RotateCW,
                RotationDirection::CounterClockwise => MoveAction::RotateCCW,
                RotationDirection::Deg180 => MoveAction::RotateDeg180
            };
            self.replay_mgr.push_move(self.cur_sec, action);

            return Some(self.piece_mgr.cur_piece.current_rotation)
        }

        None
    }

    /// Tries to hold current piece. Doesn't do anything if it fails.
    /// It may fail if the player has already held the piece during his turn.
    pub fn hold_piece(&mut self) -> Option<&Piece> {
        
        let result = self.piece_mgr.hold_piece();

        if let Some(_) = result {
            self.replay_mgr.push_move(self.cur_sec, MoveAction::HoldPiece);
        }

        result
    }

    /// Returns currently hold `PieceType`. If there's none, returns `None`.
    pub fn get_hold_piece(&self) -> Option<PieceType> {
        self.piece_mgr.get_hold_piece()
    }

    /// Executes specified `MoveAction` once. Returns `None` if a simple action occurs,
    /// for example, `MoveLeft` or `MoveRight`.
    pub fn exec_action(&mut self, action: MoveAction) -> Option<Result<MoveResult, UpdateErrorReason>> {
        match action {
            MoveAction::MoveLeft => { self.move_left(1); },
            MoveAction::MoveRight => { self.move_right(1); },
            MoveAction::RotateCW => { self.rotate(RotationDirection::Clockwise); },
            MoveAction::RotateCCW => { self.rotate(RotationDirection::CounterClockwise); },
            MoveAction::RotateDeg180 => { self.rotate(RotationDirection::Deg180); },
            MoveAction::SoftDrop => { self.soft_drop(1); },
            MoveAction::HardDrop => { return Some(self.hard_drop()); }
            MoveAction::HoldPiece => { self.hold_piece(); }
        }

        None
    }

    /// Performs a hard drop and at same time updates all the board's internals.
    /// That includes applying piece onto the board, updating current combo and b2b,
    /// updating player's statistics, pushing the move to the replay manager, 
    /// and composing the final `MoveResult`.
    pub fn hard_drop(&mut self) -> Result<MoveResult, UpdateErrorReason> {
        if !self.is_enabled {
            return Err(UpdateErrorReason::BoardDisabled);
        }
        if self.is_dead {
            return Err(UpdateErrorReason::BoardDead);
        }

        let piece_mgr = &mut self.piece_mgr;
        // apply the piece onto board
        let hard_drop_info = piece_mgr.hard_drop()?;
        // update combo and b2b
        self.scoring_mgr.hard_drop(&hard_drop_info);
        // update board stats (apm, pps, etc.)
        self.board_stats.hard_drop(&hard_drop_info, &self.scoring_mgr);
        // add the move to the replay manager
        self.replay_mgr.push_move(self.cur_sec, MoveAction::HardDrop);

        let move_queue = self.replay_mgr.end_move();

        // initializing the move result
        let move_result = MoveResult::new(
            &self.scoring_mgr,
            hard_drop_info,
            &self.game_settings.attack,
            &mut self.garbage_mgr,
            &self.piece_mgr.cell_holder,
            move_queue,
            self.cur_sec
        );

        // if the attack is negative, the board received damage; pushing garbage then
        move_result.attack.in_damage_queue
            .iter()
            .for_each(|dmg| {
                self.garbage_mgr.push_garbage_at(dmg.amount as u32, dmg.hole_x, &mut self.piece_mgr.cell_holder);
                self.piece_mgr.update_nearest_y();
            });

        Ok(move_result)
    }

    /// Soft drops current piece. That means moving it down by amount `delta`.
    /// Returns an `u32` which indicates how many times the piece was successfully
    /// soft dropped.
    pub fn soft_drop(&mut self, delta: u32) -> u32 {
        let dt = std::cmp::min(delta, self.game_settings.board.full_height() as u32);
        let mut amount_moved = 0;

        for _ in 0..dt {
            if self.piece_mgr.soft_drop() {
                self.gravity_mgr.reset_lock();
                self.replay_mgr.push_move(self.cur_sec, MoveAction::SoftDrop);
                amount_moved += 1;
            }
        }

        amount_moved
    }

    /// Sends garbage onto current board with specified `amount` of garbage rows and `messiness`.
    /// The higher the messiness, the more random the holes are.
    /// Messiness = 0 means that the hole will be at the same x coordinate within
    /// pending garbage rows.
    pub fn push_garbage(&mut self, amount: u32, messiness: u32) {
        self.garbage_mgr.push_garbage(amount, messiness, &mut self.piece_mgr.cell_holder);
    }

    /// Pushes damage onto board. The difference between this method and `push_garbage()`
    /// is that `push_garbage()` adds garbage immediately, whereas this method
    /// adds damage into the damage queue.
    pub fn attack(&mut self, damage: i32) {
        self.garbage_mgr.attack(self.game_settings.board.width, damage);
    }

    pub fn get_cell_holder(&self) -> &CellHolder {
        &self.piece_mgr.cell_holder
    }

    pub fn get_piece_mgr(&self) -> &PieceMgr {
        &self.piece_mgr
    }

    /// Returns nearest Y coordinate which the piece fits at.
    /// May be useful for rendering ghost piece.
    pub fn find_nearest_y(&self) -> u32 {
        self.get_piece_mgr().find_nearest_y()
    }

    /// Completely resets the state of the board.
    pub fn reset(&mut self, new_seed: Option<u64>) {
        self.gravity_mgr.reset();
        self.scoring_mgr.combo = 0;
        self.scoring_mgr.b2b = 0;
        self.board_stats.reset();
        self.piece_mgr.reset(new_seed);
        self.is_dead = false;
        //self.time_mgr.reset();
        self.replay_mgr.reset();
        self.garbage_mgr.reset();
    }

    /// Enables current board. All BoardCommands will execute normally.
    pub fn enable(&mut self) {
        if self.is_enabled {
            return;
        }

        self.is_enabled = true;
        self.gravity_mgr.enable();
        //self.time_mgr.enable();
    }

    /// Disables current board. Most BoardCommands will stop execution.
    pub fn disable(&mut self) {
        if !self.is_enabled {
            return;
        }

        self.is_enabled = false;
        self.gravity_mgr.disable();
        //self.time_mgr.disable();
    }
}

/// Simplified or lightweight version of `Board` which contains only the essentials.
/// It can only execute a `MoveAction`, doesn't track any scores, but it can handle incoming garbage.
/// Piece can be moved and rotated freely without doing any checks.
#[derive(Debug)]
pub struct BoardSimple {
    pub piece_mgr: Box<PieceMgr>,
    pub is_enabled: bool,
    pub garbage_mgr: GarbageMgr,
}

impl BoardSimple {
    pub fn new(game_settings: GameSettings, seed: u64) -> Self {
        Self {
            piece_mgr: Box::new(PieceMgr::new(&game_settings, seed)),
            is_enabled: true,
            garbage_mgr: GarbageMgr::new(&game_settings.attack)
        }
    }

    /// Executes a `MoveAction` once. Does nothing if the board is disabled.
    pub fn exec_action(&mut self, move_action: MoveAction) {
        if !self.is_enabled {
            return;
        }

        match move_action {
            MoveAction::MoveLeft => { self.piece_mgr.move_left_force(); }
            MoveAction::MoveRight => { self.piece_mgr.move_right_force(); }
            MoveAction::RotateCW => { self.piece_mgr.rotate_force(RotationDirection::Clockwise); }
            MoveAction::RotateCCW => { self.piece_mgr.rotate_force(RotationDirection::CounterClockwise); }
            MoveAction::RotateDeg180 => { self.piece_mgr.rotate_force(RotationDirection::Deg180); }
            MoveAction::SoftDrop => { self.piece_mgr.soft_drop_force(); }
            MoveAction::HardDrop => {
                self.piece_mgr.hard_drop().ok();
            }
            MoveAction::HoldPiece => {
                self.piece_mgr.hold_piece();
            }
        }
    }

    /// Sends `amount` rows of garbage with a hole at `hole_x`.
    pub fn send_garbage(&mut self, amount: u32, hole_x: u32) {
        self.garbage_mgr.push_garbage_at(amount, hole_x, &mut self.piece_mgr.cell_holder);
    }
}