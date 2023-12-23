use rand::{Rng, SeedableRng};
use rand_chacha::ChaCha8Rng;
use serde::{Deserialize, Serialize};
use crate::board_command::{BoardCommand, BoardCommandType, BoardMoveDir};
use crate::cell_holder::{CellHolder};
use crate::game_settings::{GameSettings};
use crate::gravity_mgr::GravityMgr;
use crate::piece::{PieceType, RotationDirection};
use crate::piece_mgr::PieceMgr;
use crate::wall_kick_data::{WallKickData};

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum GameState {

}

pub struct Board {
    game_settings: GameSettings,

    gravity_mgr: Box<GravityMgr>,
    is_enabled: bool,

    last_garbage_x: Option<u32>,
    // used for generating garbage holes
    rng: ChaCha8Rng,
    wkd: WallKickData,
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
            wkd: WallKickData::new(*game_settings.get_wall_kick_data_mode())
        }
    }

    pub fn exec_cmd(&mut self, cmd: &BoardCommand) {
        match cmd.get_type() {
            BoardCommandType::Move(dir, delta) => {
                match dir {
                    BoardMoveDir::Left => self.move_left(*delta),
                    BoardMoveDir::Right => self.move_right(*delta)
                }
            },
            BoardCommandType::Rotate(dir) => self.rotate(*dir),
            BoardCommandType::HardDrop => self.hard_drop(),
            BoardCommandType::SoftDrop(delta) => self.soft_drop(*delta),
            BoardCommandType::SendGarbage(amount, messiness) => self.send_garbage(*amount, *messiness),
            BoardCommandType::Update(dt) => self.update(*dt),
            BoardCommandType::HoldPiece => self.hold_piece()
        }
    }

    pub fn update(&mut self, dt: f32) {
        if let Some(_res) = self.gravity_mgr.update(dt) {

        }

        if let Some(_res) = self.gravity_mgr.piece_mgr.update(dt) {

        }
    }

    pub fn move_left(&mut self, delta: i32) {
        self.gravity_mgr.piece_mgr.move_left(delta);
    }

    pub fn move_right(&mut self, delta: i32) {
        self.gravity_mgr.piece_mgr.move_right(delta);
    }

    pub fn rotate(&mut self, direction: RotationDirection) {
        self.gravity_mgr.piece_mgr.rotate(&self.wkd, direction);
        self.gravity_mgr.prolong_lock();
    }

    pub fn hold_piece(&mut self) {
        if let Some(_p) = self.gravity_mgr.piece_mgr.hold_piece() {
            // TODO: Send a signal to client that the hold and current piece were updated
        }
    }

    pub fn get_hold_piece(&self) -> Option<PieceType> {
        self.gravity_mgr.piece_mgr.get_hold_piece()
    }

    pub fn hard_drop(&mut self) {
        let piece_mgr = &mut self.gravity_mgr.piece_mgr;
        let _lines_cleared = piece_mgr.hard_drop().unwrap_or(0);
    }

    pub fn soft_drop(&mut self, delta: u32) {
        self.gravity_mgr.piece_mgr.soft_drop(delta);
    }

    pub fn send_garbage(&mut self, amount: u32, _messiness: u32) {
        let ch = &mut self.gravity_mgr.piece_mgr.cell_holder;

        let width = self.game_settings.get_board().width as u32;

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

    pub fn find_nearest_y(&self) -> u32 {
        self.get_piece_mgr().find_nearest_y()
    }
}

pub trait BoardStateful {
    fn reset(&mut self);
    fn enable(&mut self) { }
    fn disable(&mut self) { }
    fn is_enabled(&self) -> bool;
}
impl BoardStateful for Board {
    fn reset(&mut self) {
        self.gravity_mgr.reset();
    }

    fn enable(&mut self) {
        if self.is_enabled {
            return;
        }

        self.gravity_mgr.enable();

        self.is_enabled = true;
    }

    fn disable(&mut self) {
        if !self.is_enabled {
            return;
        }

        self.gravity_mgr.disable();

        self.is_enabled = false;
    }

    fn is_enabled(&self) -> bool {
        self.is_enabled
    }
}

pub enum UpdateErrorReason {
    CannotApplyPiece
}

pub trait BoardComponent {
    fn get_name(&self) -> &'static str;
    fn reset(&mut self);
    fn enable(&mut self) { }
    fn disable(&mut self) { }
    /// u32 in the Result is lines cleared
    fn update(&mut self, _dt: f32) -> Option<Result<u32, UpdateErrorReason>> {
        None
    }
}


