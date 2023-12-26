use std::sync::Arc;
use serde::{Deserialize, Serialize};
use crate::cell_holder::{CellHolder};
use crate::game_settings::{GameSettings};
use crate::garbage_mgr::GarbageMgr;
use crate::gravity_mgr::{GravityMgr, GravityUpdateResult};
use crate::piece::{Piece, PieceType, RotationDirection, RotationState};
use crate::piece_mgr::{PieceMgr, UpdateErrorReason};
use crate::replays::{BoardStats, MoveResult};
use crate::scoring::{ScoringMgr};
use crate::time_mgr::TimeMgr;
use crate::wall_kick_data::{WallKickData};

#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
pub enum MoveAction {
    MoveLeft,
    MoveRight,

    RotateCW,
    RotateCCW,
    RotateDeg180,

    SoftDrop,
    HardDrop,
}

#[derive(Debug)]
pub struct Board {
    game_settings: GameSettings,

    pub(crate) gravity_mgr: GravityMgr,
    pub(crate) piece_mgr: Box<PieceMgr>,
    pub(crate) is_enabled: bool,

    wkd: Arc<WallKickData>,
    pub(crate) scoring_mgr: ScoringMgr,
    pub board_stats: BoardStats,
    pub is_dead: bool,
    pub move_queue: Vec<MoveAction>,
    pub garbage_mgr: GarbageMgr,

    pub time_mgr: TimeMgr
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
            move_queue: Vec::default(),
            garbage_mgr: GarbageMgr::new(&game_settings.attack),
            time_mgr: TimeMgr::new()
        }
    }

    // Executes specified `BoardCommand`.
/*    pub fn exec_cmd(&mut self, cmd: &BoardCommand) -> Option<HardDropInfo> {
        let mut res = None;

        match cmd {
            BoardCommand::Move(dir, delta) => {
                match dir {
                    BoardMoveDir::Left => self.move_left(*delta),
                    BoardMoveDir::Right => self.move_right(*delta)
                }
            },
            BoardCommand::Rotate(dir) => self.rotate(*dir),
            BoardCommand::HardDrop => {
                res = Some(self.hard_drop().unwrap_or_else(|_err| {
                    //println!("cannot apply piece!");
                    HardDropInfo::default()
                }));
            },
            BoardCommand::SoftDrop(delta) => self.soft_drop(*delta),
            BoardCommand::SendGarbage(amount, _messiness) => self.attack(*amount as i32), //self.push_garbage(*amount, *messiness),
            BoardCommand::Update(dt) => {
                let r = self.update(*dt);
                if let Some(update_res) = r {
                    res = Some(update_res);
                }
            },
            BoardCommand::HoldPiece => self.hold_piece(),
            BoardCommand::RequestBoardLayout => {}
        }

        res
    }*/

    /// Updates `GravityMgr` by sending delta time `dt` and updating its current variables:
    /// lock, gravity. Force hard drops the piece if `GravityMgr` requests it to.
    pub fn update(&mut self, dt: f32) -> Option<Result<MoveResult, UpdateErrorReason>> {
        if !self.is_enabled {
            return None;
        }

        if self.is_dead {
            return Some(Err(UpdateErrorReason::BoardDead));
        }

        self.time_mgr.update(dt);
        self.garbage_mgr.update((dt * 1000.0) as u32);
        match self.gravity_mgr.update(&self.piece_mgr, dt) {
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
                self.move_queue.push(MoveAction::MoveLeft);
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
                self.move_queue.push(MoveAction::MoveRight);
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
            self.move_queue.push(action);

            return Some(self.piece_mgr.curr_piece.current_rotation)
        }

        None
    }

    /// Tries to hold current piece. Doesn't do anything if it fails.
    /// It may fail if the player has already held the piece during his turn.
    pub fn hold_piece(&mut self) -> Option<&Piece> {
        self.piece_mgr.hold_piece()
    }

    /// Returns currently hold `PieceType`. If there's none, returns `None`.
    pub fn get_hold_piece(&self) -> Option<PieceType> {
        self.piece_mgr.get_hold_piece()
    }

    pub fn hard_drop(&mut self) -> Result<MoveResult, UpdateErrorReason> {
        if !self.is_enabled {
            return Err(UpdateErrorReason::BoardDisabled);
        }
        if self.is_dead {
            return Err(UpdateErrorReason::BoardDead);
        }

        let piece_mgr = &mut self.piece_mgr;
        let hard_drop_info = piece_mgr.hard_drop()?;

        self.scoring_mgr.hard_drop(&hard_drop_info);
        self.board_stats.hard_drop(&hard_drop_info, &self.scoring_mgr);

        self.move_queue.push(MoveAction::HardDrop);

        let move_result = MoveResult::new(
            &self.scoring_mgr,
            hard_drop_info,
            &self.game_settings.attack,
            &mut self.garbage_mgr,
            &mut self.piece_mgr.cell_holder,
            &self.move_queue,
            &self.time_mgr
        );

        self.move_queue.clear();

        Ok(move_result)
    }

    /// Soft drops current piece. That means moving it down by amount `delta`.
    /// Returns an `u32` which indicates how many times the piece was successfully
    /// soft dropped.
    pub fn soft_drop(&mut self, delta: u32) -> u32 {
        let dt = std::cmp::min(delta, self.game_settings.board.height as u32);
        let mut amount_moved = 0;

        for _ in 0..dt {
            if self.piece_mgr.soft_drop() {
                self.gravity_mgr.reset_lock();
                self.move_queue.push(MoveAction::SoftDrop);
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
        self.garbage_mgr.attack(damage);
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
    pub fn reset(&mut self) {
        self.gravity_mgr.reset();
        self.scoring_mgr.combo = 0;
        self.scoring_mgr.b2b = 0;
        self.board_stats.reset();
        self.piece_mgr.reset();
        self.is_dead = false;
        self.time_mgr.reset();
    }

    /// Enables current board. All BoardCommands will execute normally.
    pub fn enable(&mut self) {
        if self.is_enabled {
            return;
        }

        self.is_enabled = true;
        self.gravity_mgr.enable();
        self.time_mgr.enable();
    }

    /// Disables current board. Most BoardCommands will stop execution.
    pub fn disable(&mut self) {
        if !self.is_enabled {
            return;
        }

        self.is_enabled = false;
        self.gravity_mgr.disable();
        self.time_mgr.disable();
    }
}
