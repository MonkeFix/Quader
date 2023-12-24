use rand::{Rng, SeedableRng};
use rand_chacha::ChaCha8Rng;
use serde::{Deserialize, Serialize};
use crate::board_command::{BoardCommand, BoardMoveDir};
use crate::cell_holder::{CellHolder};
use crate::game_settings::{GameSettings};
use crate::gravity_mgr::GravityMgr;
use crate::piece::{PieceType, RotationDirection};
use crate::piece_mgr::{PieceMgr, UpdateErrorReason};
use crate::replays::{BoardStats, HardDropInfo, LastMoveType};
use crate::scoring::{ScoringMgr, TSpinStatus};
use crate::wall_kick_data::{WallKickData};

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum GameState {

}

pub struct Board {
    game_settings: GameSettings,

    gravity_mgr: Box<GravityMgr>,
    pub(crate) is_enabled: bool,

    last_garbage_x: Option<u32>,
    // used for generating garbage holes
    rng: ChaCha8Rng,
    wkd: WallKickData,
    pub(crate) scoring_mgr: ScoringMgr,
    pub board_stats: BoardStats
}

impl Board {

    // TODO: Use attacks and send garbage (DamageMgr)
    // TODO: Add replays

    pub fn new(game_settings: GameSettings, seed: u64) -> Self {

        let gravity_mgr = Box::new(GravityMgr::new(&game_settings, seed));

        Self {
            game_settings,
            gravity_mgr,
            is_enabled: true,
            last_garbage_x: None,
            // used for generating garbage holes, so we can safely use entropy here instead of set seed
            rng: SeedableRng::from_entropy(),
            wkd: WallKickData::new(game_settings.wall_kick_data_mode),
            scoring_mgr: ScoringMgr::new(),
            board_stats: BoardStats::default()
        }
    }

    /// Executes specified `BoardCommand`.
    pub fn exec_cmd(&mut self, cmd: &BoardCommand) {
        match cmd {
            BoardCommand::Move(dir, delta) => {
                match dir {
                    BoardMoveDir::Left => self.move_left(*delta),
                    BoardMoveDir::Right => self.move_right(*delta)
                }
            },
            BoardCommand::Rotate(dir) => self.rotate(*dir),
            BoardCommand::HardDrop => {
                self.hard_drop().unwrap_or_else(|_err| {
                    //println!("cannot apply piece!");
                    HardDropInfo::default()
                });
            },
            BoardCommand::SoftDrop(delta) => self.soft_drop(*delta),
            BoardCommand::SendGarbage(amount, messiness) => self.send_garbage(*amount, *messiness),
            BoardCommand::Update(dt) => self.update(*dt),
            BoardCommand::HoldPiece => self.hold_piece(),
            BoardCommand::RequestBoardLayout => {}
        }
    }

    /// Updates `GravityMgr` by sending delta time `dt` and updating its current variables:
    /// lock, gravity. Force hard drops the piece if `GravityMgr` requests it to.
    pub fn update(&mut self, dt: f32) {
        if self.gravity_mgr.update(dt) {
            // hard drop requested
            self.exec_cmd(&BoardCommand::HardDrop)
        }
    }

    /// Tries to move current piece to the left by amount `delta`.
    pub fn move_left(&mut self, delta: i32) {
        self.gravity_mgr.piece_mgr.move_left(delta);
    }

    /// Tries to move current piece to the right by amount `delta`.
    pub fn move_right(&mut self, delta: i32) {
        self.gravity_mgr.piece_mgr.move_right(delta);
    }

    /// Tries to rotate current piece to direction `RotationDirection`
    pub fn rotate(&mut self, direction: RotationDirection) {
        if self.gravity_mgr.piece_mgr.rotate(&self.wkd, direction) {
            // prolong lock only if rotation was successful
            self.gravity_mgr.prolong_lock();
        }
    }

    /// Tries to hold current piece. Doesn't do anything if it fails.
    /// It may fail if the player has already held the piece during his turn.
    pub fn hold_piece(&mut self) {
        if let Some(_p) = self.gravity_mgr.piece_mgr.hold_piece() {
            // TODO: Send a signal to client that the hold and current piece were updated
        }
    }

    /// Returns currently hold `PieceType`. If there's none, returns `None`.
    pub fn get_hold_piece(&self) -> Option<PieceType> {
        self.gravity_mgr.piece_mgr.get_hold_piece()
    }

    pub fn hard_drop(&mut self) -> Result<HardDropInfo, UpdateErrorReason> {
        let piece_mgr = &mut self.gravity_mgr.piece_mgr;
        let result = piece_mgr.hard_drop()?;

        // Do not break B2B if and only if the player:
        //  - haven't cleared any lines,
        //  - cleared exactly 4 lines,
        //  - performed a T-Spin which must include a rotation of the piece.
        // Otherwise break it.
        if result.lines_cleared == 4 ||
            result.last_move_type == LastMoveType::Rotation &&
                (
                    result.lines_cleared >= 1 ||
                    result.tspin_status == TSpinStatus::Mini ||
                    result.tspin_status == TSpinStatus::Full
                ) {
            self.scoring_mgr.b2b += 1;

        } else if result.lines_cleared != 0 {
            self.scoring_mgr.b2b = 0;
        }

        // Combos are much easier:
        // if a player cleared 1 or more lines in a row, for each hard drop combo increases by one,
        // otherwise if a player didn't clear any lines, combo resets back to 0.
        if result.lines_cleared > 0 {
            self.scoring_mgr.combo += 1;
        } else {
            self.scoring_mgr.combo = 0;
        }

        self.board_stats.hard_drop(result, &self.scoring_mgr);

        /*let piece = piece_mgr.get_piece();
        let t_spin_status = check_t_overhang(
            self.game_settings.get_board(),
            piece.get_x() as i32, piece.get_y() as i32,
            |p| { self.get_cell_holder().intersects(&p) }
        );
        let bits = create_board_move_bits(
            self.get_cell_holder().get_occupied_cell_count() as u32,
            &mut result,
            t_spin_status);

        result.mod_bits = bits;

        let dmg = calculate_damage(self.game_settings.get_attack(), &result);
        result.attack = dmg;

        println!("hard drop: {:?}", result);*/

        // TODO: Move damage calculation and move creation to BoardManager.
        // TODO: Attacks should be calculated in the server.

        Ok(result)
    }

    /// Soft drops current piece. That means moving it down by amount `delta`.
    pub fn soft_drop(&mut self, delta: u32) {
        if self.gravity_mgr.piece_mgr.soft_drop(delta) {
            self.gravity_mgr.reset_lock();
        }
    }

    /// Sends garbage onto current board with specified `amount` of garbage rows and `messiness`.
    /// The higher the messiness, the more random the holes are.
    /// Messiness = 0 means that the hole will be at the same x coordinate within
    /// pending garbage rows.
    pub fn send_garbage(&mut self, amount: u32, _messiness: u32) {
        let ch = &mut self.gravity_mgr.piece_mgr.cell_holder;

        let width = self.game_settings.board.width as u32;

        let garbage_hole_x: u32 = if let Some(gx) = self.last_garbage_x {
            // TODO: Use messiness
            gx
        } else {
            self.rng.gen_range(0..width)
        };

        for _ in 0..amount {
            ch.push_garbage(garbage_hole_x);
        }
    }

    pub fn get_cell_holder(&self) -> &CellHolder {
        &self.gravity_mgr.piece_mgr.cell_holder
    }

    pub fn get_piece_mgr(&self) -> &PieceMgr {
        &self.gravity_mgr.piece_mgr
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



