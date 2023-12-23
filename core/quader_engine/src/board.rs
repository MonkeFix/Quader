use std::cell::{Ref, RefCell, RefMut};
use std::rc::Rc;
use rand::{Rng, SeedableRng};
use rand_chacha::ChaCha8Rng;
use serde::{Deserialize, Serialize};
use crate::board_command::{BoardCommand, BoardCommandType, BoardMoveDir};
use crate::cell_holder::{CellHolder};
use crate::game_settings::{GameSettings};
use crate::gravity_mgr::GravityMgr;
use crate::piece::{RotationDirection};
use crate::piece_generators::{PieceGenerator, PieceGeneratorBag7};
use crate::piece_mgr::PieceMgr;
use crate::wall_kick_data::{WallKickData};

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum GameState {

}

pub struct Board {
    game_settings: GameSettings,

    cell_holder: Rc<RefCell<CellHolder>>,
    piece_mgr: Rc<RefCell<PieceMgr>>,
    gravity_mgr: Rc<RefCell<GravityMgr>>,
    is_enabled: bool,
    // seed: u64,
    piece_generator: PieceGeneratorBag7,
    last_garbage_x: Option<u32>,
    // used for generating garbage holes
    rng: ChaCha8Rng,
    wkd: WallKickData,
}

impl Board {

    // TODO: Add PieceQueue
    // TODO: Add HoldPiece
    // TODO: Use attacks and send garbage (DamageMgr)
    // TODO: Add replays

    pub fn new(game_settings: GameSettings, seed: u64) -> Self {

        let cell_holder = Rc::new(RefCell::new(CellHolder::new(game_settings.get_board())));
        let piece_mgr =  Rc::new(RefCell::new(PieceMgr::new(&game_settings, Rc::clone(&cell_holder))));
        let gravity_mgr = Rc::new(RefCell::new(GravityMgr::new(game_settings.get_gravity(), Rc::clone(&piece_mgr))));
        let mut piece_gen = PieceGeneratorBag7::new(seed);
        // TODO: Actually use the queue. Move it to separate struct
        let queue = piece_gen.init();

        piece_mgr.borrow_mut().create_piece(queue[0]);

        Self {
            game_settings,
            cell_holder,
            piece_mgr,
            gravity_mgr,
            is_enabled: true,
            piece_generator: piece_gen,
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
        if let Some(_res) = self.gravity_mgr.borrow_mut().update(dt) {

        }

        if let Some(_res) = self.piece_mgr.borrow_mut().update(dt) {

        }
    }

    pub fn move_left(&mut self, delta: i32) {
        self.piece_mgr.borrow_mut().move_left(delta);
    }

    pub fn move_right(&mut self, delta: i32) {
        self.piece_mgr.borrow_mut().move_right(delta);
    }

    pub fn rotate(&mut self, direction: RotationDirection) {
        self.piece_mgr.borrow_mut().rotate(&self.wkd, direction);
        self.gravity_mgr.borrow_mut().prolong_lock();
    }

    pub fn hold_piece(&mut self) {
        let mut piece_mgr = self.piece_mgr.borrow_mut();

        if let Some(p) = piece_mgr.hold_piece(|| self.piece_generator.next()) {

        }
    }

    pub fn hard_drop(&mut self) {
        let mut piece_mgr = self.piece_mgr.borrow_mut();
        let _lines_cleared = piece_mgr.hard_drop().unwrap_or(0);

        let next_piece = self.piece_generator.next();

        piece_mgr.create_piece(next_piece);
    }

    pub fn soft_drop(&mut self, delta: u32) {
        self.piece_mgr.borrow_mut().soft_drop(delta);
    }

    pub fn send_garbage(&mut self, amount: u32, _messiness: u32) {
        let mut ch = self.cell_holder.borrow_mut();

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

    pub fn get_cell_holder(&self) -> Ref<CellHolder> {
        self.cell_holder.borrow()
    }

    pub fn get_piece_mgr(&self) -> Ref<PieceMgr> {
        self.piece_mgr.borrow()
    }

    pub fn find_nearest_y(&self) -> u32 {
        self.get_piece_mgr().find_nearest_y()
    }

    pub(crate) fn get_piece_mgr_mut(&self) -> RefMut<PieceMgr> {
        self.piece_mgr.borrow_mut()
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
        self.piece_mgr.borrow_mut().reset();
        self.gravity_mgr.borrow_mut().reset();
        self.cell_holder.borrow_mut().reset();
    }

    fn enable(&mut self) {
        if self.is_enabled {
            return;
        }

        self.piece_mgr.borrow_mut().enable();
        self.gravity_mgr.borrow_mut().enable();
        self.cell_holder.borrow_mut().enable();

        self.is_enabled = true;
    }

    fn disable(&mut self) {
        if !self.is_enabled {
            return;
        }

        self.piece_mgr.borrow_mut().disable();
        self.gravity_mgr.borrow_mut().disable();
        self.cell_holder.borrow_mut().disable();

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


