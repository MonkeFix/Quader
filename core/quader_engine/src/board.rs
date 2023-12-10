use std::any::Any;
use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use std::marker::PhantomData;
use std::ops::{Add, AddAssign};
use std::rc::Rc;
use serde::{Deserialize, Serialize};
use crate::board_command::{BoardCommand, BoardCommandType, BoardMoveDir};
use crate::cell_holder::{CellHolder, CellType, Row};
use crate::damage_calculator::DamageCalculator;
use crate::game_settings::{BOARD_VISIBLE_HEIGHT, BOARD_WIDTH, BoardSettings, GameSettings};
use crate::gravity_mgr::GravityMgr;
use crate::piece::{OffsetType, Piece, PieceType, RotationDirection, WallKickCheckParams};
use crate::piece_generators::{PieceGenerator, PieceGeneratorBag7};
use crate::piece_mgr::PieceMgr;
use crate::primitives::Point;
use crate::replays::MoveInfo;
use crate::rng_manager::RngManager;
use crate::scoring::{DamageMgr, ScoringMgr};
use crate::time_mgr::TimeMgr;
use crate::utils::{adjust_positions_clone, piece_type_to_cell_type};
use crate::wall_kick_data::WallKickData;

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum GameState {

}

struct All {
    game_settings: GameSettings,

    // INNER LOGIC:
    test_queue: VecDeque<Vec<Point>>,
    cell_holder: CellHolder,
    current_piece: Piece,

    // SCORING:
    // ScoringMgr | combo: u32,
    // ScoringMgr | b2b: u32,
    last_move: MoveInfo,
    //replay: Replay,
    // DamageMgr | attack_queue: VecDeque<u32>,
    // DamageMgr | incoming_damage: Vec<u32>,
    // DamageMgr | last_garbage_x: u32,

    // UPDATABLE
    // GravityMgr | cur_gravity: f32,
    // GravityMgr | cur_lock: f32,
    // GravityMgr | intermediate_y: f32,
    // GravityMgr | y_needs_update: bool,
    // GravityMgr | y_to_check: u32,
    // DamageMgr | cur_garbage_cd: f32,
    // TimeMgr | cur_tick: f64,
}

pub struct Board {
    game_settings: GameSettings,
    /*gravity_mgr: GravityMgr<'a, 'a>,
    piece_mgr: PieceMgr<'a>,
    scoring_mgr: ScoringMgr,
    time_mgr: TimeMgr,
    damage_mgr: DamageMgr,
    cell_holder: CellHolder,
    rng_mgr: RngManager,
    damage_calculator: DamageCalculator<'a>,*/

    components: Vec<Box<dyn Any>>,
    is_enabled: bool
}

impl Board {
    fn for_each<F: Fn(&Box<dyn BoardComponent>)>(&self, action: F) {
        for c in self.components.iter() {
            if let Some(c) = c.downcast_ref::<Box<dyn BoardComponent>>() {
                action(c);
            }
        }
    }

    fn for_each_mut<F: FnMut(&mut Box<dyn BoardComponent>)>(&mut self, action: &mut F) {
        for c in self.components.iter_mut() {
            if let Some(c) = c.downcast_mut::<Box<dyn BoardComponent>>() {
                action(c);
            }
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
            BoardCommandType::Rotate(dir) => self.rotate(&WallKickData::default(), dir),
            BoardCommandType::HardDrop => self.hard_drop(),
            BoardCommandType::SoftDrop(delta) => self.soft_drop(*delta),
            BoardCommandType::SendGarbage(amount, messiness) => self.send_garbage(*amount, *messiness),
            BoardCommandType::Update(dt) => self.update(*dt)
        }
    }

    pub fn move_left(&mut self, delta: i32) {
        let cell_holder = self.get_component::<CellHolder>("cell_holder").unwrap();

        let mut c = self.get_component_mut::<PieceMgr>("piece_mgr");
        //c.as_mut().unwrap().move_left(&cell_holder, delta);
    }

    pub fn move_right(&mut self, delta: i32) {
        let cell_holder = self.get_component::<CellHolder>("cell_holder").unwrap();

        let mut c = self.get_component_mut::<PieceMgr>("piece_mgr");
        //c.unwrap().move_right(&cell_holder, delta);
    }

    pub fn rotate(&mut self, wkd: &WallKickData, direction: &RotationDirection) {
        let cell_holder = self.get_component::<CellHolder>("cell_holder").unwrap();

        let mut c = self.get_component::<PieceMgr>("piece_mgr");
        //c.unwrap().rotate(&cell_holder, wkd, direction);
    }

    pub fn hard_drop(&mut self) {

    }

    pub fn soft_drop(&mut self, delta: u32) {

    }

    pub fn send_garbage(&mut self, amount: u32, messiness: u32) {

    }
}

pub trait BoardStateful {
    fn reset(&mut self);
    fn enable(&mut self) { }
    fn disable(&mut self) { }
    fn is_enabled(&self) -> bool;
}

pub trait BoardEntity {
    fn new(game_settings: GameSettings) -> Self;
    fn add_component<T>(&mut self, component: T) where T: BoardComponent + Any;
    fn get_component<T: BoardComponent + Any>(&self, comp_name: &str) -> Option<&T>;
    fn get_component_mut<T: BoardComponent + Any>(&mut self, comp_name: &str) -> Option<&mut T>;
    fn update(&mut self, dt: f32);
}
impl BoardEntity for Board {
    fn new(game_settings: GameSettings) -> Self {
        Self {
            game_settings,
            components: Vec::default(),
            is_enabled: true
        }
    }

    fn add_component<T>(&mut self, component: T) where T: BoardComponent + Any {
        self.components.push(Box::new(component));
    }

    fn get_component<T: BoardComponent + Any>(&self, comp_name: &str) -> Option<&T> {
        if let Some(c) = self.components
            .iter()
            .find(|c| c.downcast_ref::<T>().map(|x| x.get_name()) == Some(comp_name)) {
            return c.downcast_ref::<T>();
        }
        None
    }

    fn get_component_mut<T: BoardComponent + Any>(&mut self, comp_name: &str) -> Option<&mut T> {
        if let Some(c) = self.components
            .iter_mut()
            .find(|c| c.downcast_ref::<T>().map(|x| x.get_name()) == Some(comp_name)) {
            return c.downcast_mut::<T>();
        }
        None
    }

    fn update(&mut self, dt: f32) {
        self.for_each_mut(&mut |c| c.as_mut().update(dt));
    }
}

impl BoardStateful for Board {
    fn reset(&mut self) {
        /*for c in self.components.iter_mut() {
            let c = c.downcast_mut::<Box<dyn BoardComponent>>();
            c.unwrap().as_mut().reset();
        }*/
        self.for_each_mut(&mut |c| c.as_mut().reset());
    }

    fn enable(&mut self) {
        if self.is_enabled {
            return;
        }

        self.for_each_mut(&mut |c| c.as_mut().enable());

        self.is_enabled = true;
    }

    fn disable(&mut self) {
        if !self.is_enabled {
            return;
        }

        self.for_each_mut(&mut |c| c.as_mut().disable());

        self.is_enabled = false;
    }

    fn is_enabled(&self) -> bool {
        self.is_enabled
    }
}

pub trait BoardComponent {
    fn get_name(&self) -> &'static str;
    fn reset(&mut self);
    fn enable(&mut self) { }
    fn disable(&mut self) { }
    fn update(&mut self, dt: f32) { }
}

pub struct BoardOld {
    width: usize,
    height: usize,
    cell_holder: Box<CellHolder>,
    cur_piece: Option<Box<Piece>>,
    gravity: f32,
    lock: f32,
    intermediate_y: f32,
    y_needs_update: bool,
    y_to_check: u32,
    cells_on_board: usize,
    wkd: Rc<WallKickData>,
    piece_gen: Box<dyn PieceGenerator>,
    piece_queue: Vec<Piece>,
    rng: Rc<RefCell<RngManager>>
}


impl Default for BoardOld {
    fn default() -> Self {
        BoardOld::new(&BoardSettings::default())
    }
}

impl BoardOld {
    pub fn new(settings: &BoardSettings) -> Self {

        let rng = Rc::new(RefCell::new(RngManager::new(12345)));
        let mut piece_gen = PieceGeneratorBag7::new(&rng);
        let pieces = piece_gen.init();

        BoardOld {
            width: settings.width,
            height: settings.height,
            cell_holder: Box::<CellHolder>::default(),
            cur_piece: None,
            gravity: 0.0,
            lock: 0.0,
            intermediate_y: 0.0,
            y_needs_update: true,
            y_to_check: 0,
            cells_on_board: 0,
            wkd: Rc::new(WallKickData::default()),
            piece_gen: Box::new(piece_gen),
            piece_queue: pieces,
            rng
        }


    }

    pub fn get_queue(&self) -> &[Piece] {
        &self.piece_queue
    }

    /*pub fn set_piece(&mut self, piece: &mut Piece) {
        self.reset_piece(piece);
        let p = Box::new(piece);
        self.cur_piece = Some(p);
    }*/

    pub fn create_piece(&mut self, piece_type: PieceType) {
        let piece = Piece::new(piece_type);

        let p = Box::new(piece);
        self.cur_piece = Some(p);
        self.reset_piece();
    }

    pub fn reset_piece(&mut self) {
        self.intermediate_y = 0.0;
        let piece = self.cur_piece.as_mut().unwrap();

        match piece.get_offset_type() {
            OffsetType::Cell => piece.set_x(BOARD_WIDTH as u32 / 2 - 1),
            OffsetType::BetweenCells => piece.set_x(((BOARD_WIDTH as f32) / 2.0).round() as u32)
        }

        if piece.get_type() == &PieceType::I {
            piece.set_y(BOARD_VISIBLE_HEIGHT as u32 + 1);
        } else {
            piece.set_y(BOARD_VISIBLE_HEIGHT as u32);
        }

        piece.reset();
    }

    pub fn get_piece(&self) -> Option<&Piece> {
        match &self.cur_piece {
            None => None,
            Some(piece) => Some(piece.as_ref())
        }
    }

    pub fn move_left(&mut self, delta: i32) {
        if self.test_movement(-1, 0) {
            self.cur_piece.as_mut().expect("Piece must be set").move_left();
        }
    }

    pub fn move_right(&mut self, delta: i32) {
        if self.test_movement(1, 0) {
            self.cur_piece.as_mut().expect("Piece must be set").move_right();
        }
    }

    pub fn rotate(&mut self, rotation: &RotationDirection) {
        let piece = self.cur_piece.as_ref().expect("Piece must be set");
        let rot_type = piece.get_rotation_type(&rotation);
        let wkd = &self.wkd;
        let tests = &wkd.get(piece.get_wall_kick_type())[&rot_type.0];

        let test = self.test_rotation(WallKickCheckParams {
            tests,
            expected_pos: rot_type.1
        });

        if let Some(point) = test {
            self.cur_piece
                .as_mut()
                .unwrap()
                .rotate(rotation, point.x, point.y);
        }
    }

    pub fn update(&mut self, dt: f32) {
        if self.cur_piece.is_none() {
            return;
        }

        self.intermediate_y += self.gravity * dt;
        if self.y_needs_update {
            self.y_to_check = self.find_nearest_y();
            self.y_needs_update = false;
        }

        if self.intermediate_y > 1.0f32 {
            let diff = std::cmp::max((self.intermediate_y - 1.0f32) as i32, 1);
            for i in 0..diff {
                self.gravity_drop();
                self.y_needs_update = true;
            }
        }

        if self.y_to_check == self.cur_piece.as_ref().unwrap().get_y() {
            self.lock -= 1f32 * dt;
        }

        if self.lock <= 0f32 {
            // self.hard_drop();
        }

        // TODO: Change zero
        self.gravity += 0f32 * (dt * 10f32);
    }

    pub fn find_nearest_y(&self) -> u32 {

        if let Some(piece) = self.cur_piece.as_ref() {
            let mut y = piece.get_y();
            let points = piece.get_positions();


            for i in piece.get_y()..=(self.height as u32) {
                let offset: Point<i32> = Point::new(piece.get_x() as i32, i as i32);
                let new_points = adjust_positions_clone(points, offset);
                if self.cell_holder.intersects_any(&new_points) {
                    break;
                }

                y = i;
            }

            return y;
        }

        0
    }

    fn test_rotation(&self, kick_params: WallKickCheckParams) -> Option<Point> {
        let tests = kick_params.tests;
        let expected_pos = kick_params.expected_pos;
        let piece = self.cur_piece.as_ref().unwrap();

        for t in tests {
            let test = Point::new(t.x, -t.y);

            let adjusted = adjust_positions_clone(
                expected_pos,
                Point::new(piece.get_x() as i32 + test.x, piece.get_y() as i32 + test.y)
            );

            if !self.cell_holder.intersects_any(&adjusted) {
                return Some(test);
            }
        }

        None
    }

    pub fn get_layout(&self) -> &[Row] {
        self.cell_holder.get_layout()
    }

    fn gravity_drop(&self) {
        todo!();
    }

    pub fn soft_drop(&mut self, delta: u32) {
        if self.y_needs_update {
            self.y_to_check = self.find_nearest_y();
            self.y_needs_update = false;
        }

        if self.test_movement(0, 1) {
            self.cur_piece.as_mut().unwrap().move_down();
        }

        self.y_needs_update = true;
    }

    pub fn hard_drop(&mut self) {
        let nearest_y = self.find_nearest_y();

        if !self.try_apply_piece(nearest_y) {
            return;
        }

        let lines_cleared = self.cell_holder.check_row_clears(None);

        if nearest_y <= BOARD_VISIBLE_HEIGHT as u32 && lines_cleared.is_empty() {
            return;
        }

        self.cell_holder.clear_rows(&lines_cleared);
        if !lines_cleared.is_empty() {
            // self.cells_on_board -= lines_cleared.len() * self.width as usize;
        }

        self.reset_piece();
    }

    pub fn test_movement(&self, x: i32, y: i32) -> bool {
        let piece = self.cur_piece.as_ref().unwrap();
        let b = piece.get_bounds();

        if b.x + x < 0 || b.x + b.width as i32 + x > self.width as i32 {
            return false;
        }
        if b.y + b.height as i32 + y > self.height as i32 {
            return false;
        }

        let pos = piece.get_positions();
        let offset: Point<i32> = Point::new(piece.get_x() as i32 + x, piece.get_y() as i32 + y);
        let new_pos = adjust_positions_clone(pos, offset);

        !self.cell_holder.intersects_any(&new_pos)
    }

    pub fn set_cell_at(&mut self, x: usize, y: usize, cell: CellType) {
        self.cell_holder.set_cell_at(x, y, cell);
    }

    pub fn send_garbage(&mut self, amount: u32, messiness: u32) {
        todo!();
    }

    fn try_apply_piece(&mut self, y: u32) -> bool {
        let piece = self.cur_piece.as_ref().unwrap();
        let points = piece.get_current_pos();
        let x = piece.get_x() as i32;
        let adjusted = adjust_positions_clone(points, Point::new(x, y as i32));

        let mut res = true;
        let piece = self.cur_piece.as_ref().unwrap();

        for point in adjusted {
            let cell = self.cell_holder.get_cell_at(point.x as usize, point.y as usize);
            if cell != CellType::None {
                res = false;
            }

            let cell_type = piece_type_to_cell_type(piece.get_type());
            self.cell_holder.set_cell_at(point.x as usize, point.y as usize, cell_type);
            self.cells_on_board += 1;
        }

        res
    }
}

