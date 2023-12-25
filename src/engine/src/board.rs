use std::collections::VecDeque;
use std::sync::Arc;
use crate::board_command::{BoardCommand, BoardMoveDir};
use crate::cell_holder::{CellHolder};
use crate::game_settings::{GameSettings};
use crate::garbage_mgr::GarbageMgr;
use crate::gravity_mgr::{GravityMgr, GravityUpdateResult};
use crate::piece::{PieceType, RotationDirection};
use crate::piece_mgr::{PieceMgr, UpdateErrorReason};
use crate::replays::{BoardStats, HardDropInfo, MoveInfo};
use crate::scoring::{ScoringMgr};
use crate::wall_kick_data::{WallKickData};

#[derive(Debug, Copy, Clone)]
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
    // used for generating garbage holes

    wkd: Arc<WallKickData>,
    pub(crate) scoring_mgr: ScoringMgr,
    pub board_stats: BoardStats,
    pub is_dead: bool,
    pub move_queue: VecDeque<MoveAction>,
    pub prev_move_queue: Option<VecDeque<MoveAction>>,
    pub garbage_mgr: GarbageMgr
}

impl Board {

    // TODO: Use attacks and send garbage (DamageMgr)
    // TODO: Add replays

    pub fn new(game_settings: GameSettings, wkd: Arc<WallKickData>, seed: u64) -> Self {

        // TODO: Maybe create a new thread in which every tick there will be time pulled and GravityMgr updated with correct dt
        // TODO: and sends commands to the Board
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
            move_queue: VecDeque::new(),
            prev_move_queue: None,
            garbage_mgr: GarbageMgr::new(&game_settings.attack)
        }
    }

    /// Executes specified `BoardCommand`.
    pub fn exec_cmd(&mut self, cmd: &BoardCommand) -> Option<HardDropInfo> {
        let mut res = HardDropInfo::default();

        match cmd {
            BoardCommand::Move(dir, delta) => {
                match dir {
                    BoardMoveDir::Left => self.move_left(*delta),
                    BoardMoveDir::Right => self.move_right(*delta)
                }
            },
            BoardCommand::Rotate(dir) => self.rotate(*dir),
            BoardCommand::HardDrop => {
                res = self.hard_drop().unwrap_or_else(|_err| {
                    //println!("cannot apply piece!");
                    HardDropInfo::default()
                });
            },
            BoardCommand::SoftDrop(delta) => self.soft_drop(*delta),
            BoardCommand::SendGarbage(amount, _messiness) => self.attack(*amount as i32), //self.push_garbage(*amount, *messiness),
            BoardCommand::Update(dt) => { res = self.update(*dt).unwrap_or_default(); },
            BoardCommand::HoldPiece => self.hold_piece(),
            BoardCommand::RequestBoardLayout => {}
        }

        Some(res)
    }

    /// Updates `GravityMgr` by sending delta time `dt` and updating its current variables:
    /// lock, gravity. Force hard drops the piece if `GravityMgr` requests it to.
    pub fn update(&mut self, dt: f32) -> Option<HardDropInfo> {
        self.garbage_mgr.update((dt * 1000.0) as u32);
        match self.gravity_mgr.update(&self.piece_mgr, dt) {
            GravityUpdateResult::None => None,
            GravityUpdateResult::SoftDrop(dt) => {
                self.soft_drop(dt);
                None
            }
            GravityUpdateResult::HardDrop => {
                self.hard_drop().ok()
            }
        }
    }

    /// Tries to move current piece to the left by amount `delta`.
    pub fn move_left(&mut self, delta: u32) {
        for _ in 0..delta {
            if self.piece_mgr.move_left() {
                self.move_queue.push_back(MoveAction::MoveLeft);
            }
        }
    }

    /// Tries to move current piece to the right by amount `delta`.
    pub fn move_right(&mut self, delta: u32) {
        for _ in 0..delta {
            if self.piece_mgr.move_right() {
                self.move_queue.push_back(MoveAction::MoveRight);
            }
        }
    }

    /// Tries to rotate current piece to direction `RotationDirection`
    pub fn rotate(&mut self, direction: RotationDirection) {
        if self.piece_mgr.rotate(&self.wkd, direction) {
            // prolong lock only if rotation was successful
            self.gravity_mgr.prolong_lock();

            let action = match direction {
                RotationDirection::Clockwise => MoveAction::RotateCW,
                RotationDirection::CounterClockwise => MoveAction::RotateCCW,
                RotationDirection::Deg180 => MoveAction::RotateDeg180
            };
            self.move_queue.push_back(action);
        }
    }

    /// Tries to hold current piece. Doesn't do anything if it fails.
    /// It may fail if the player has already held the piece during his turn.
    pub fn hold_piece(&mut self) {
        if let Some(_p) = self.piece_mgr.hold_piece() {
            // TODO: Send a signal to client that the hold and current piece were updated
        }
    }

    /// Returns currently hold `PieceType`. If there's none, returns `None`.
    pub fn get_hold_piece(&self) -> Option<PieceType> {
        self.piece_mgr.get_hold_piece()
    }

    pub fn hard_drop(&mut self) -> Result<HardDropInfo, UpdateErrorReason> {
        let piece_mgr = &mut self.piece_mgr;
        let result = piece_mgr.hard_drop()?;

        self.scoring_mgr.hard_drop(&result);

        self.board_stats.hard_drop(&result, &self.scoring_mgr);

        let move_info = MoveInfo::new(
            &self.scoring_mgr,
            &result,
            &self.game_settings.attack,
            &mut self.garbage_mgr,
            &mut self.piece_mgr.cell_holder
        );

        dbg!(move_info);

        self.move_queue.push_back(MoveAction::HardDrop);

        self.prev_move_queue = Some(self.move_queue.iter().copied().collect::<VecDeque<MoveAction>>());

        self.move_queue.clear();

        Ok(result)
    }

    /// Soft drops current piece. That means moving it down by amount `delta`.
    pub fn soft_drop(&mut self, delta: u32) {
        let dt = std::cmp::min(delta, self.game_settings.board.height as u32);

        for _ in 0..dt {
            if self.piece_mgr.soft_drop() {
                self.gravity_mgr.reset_lock();
                self.move_queue.push_back(MoveAction::SoftDrop);
            }
        }
    }

    /// Sends garbage onto current board with specified `amount` of garbage rows and `messiness`.
    /// The higher the messiness, the more random the holes are.
    /// Messiness = 0 means that the hole will be at the same x coordinate within
    /// pending garbage rows.
    pub fn push_garbage(&mut self, amount: u32, messiness: u32) {
        self.garbage_mgr.push_garbage(amount, messiness, &mut self.piece_mgr.cell_holder);
    }

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
    }

    /// Enables current board. All BoardCommands will execute normally.
    pub fn enable(&mut self) {
        if self.is_enabled {
            return;
        }

        self.is_enabled = true;
        self.gravity_mgr.enable();
    }

    /// Disables current board. Most BoardCommands will stop execution.
    pub fn disable(&mut self) {
        if !self.is_enabled {
            return;
        }

        self.is_enabled = false;
        self.gravity_mgr.disable();
    }
}
