use std::rc::Rc;
use serde::{Deserialize, Serialize};
use crate::board::board_cell_holder::{BOARD_HEIGHT, BOARD_WIDTH, BoardCellHolder};
use crate::piece::Piece;
use crate::primitives::Point;

mod board_cell_holder;

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum CellType {
    None,
    I, O, T, L, J, S, Z,
    Garbage,
    Solid,
    Failing
}

pub struct Board {
    width: u32,
    height: u32,
    cell_holder: Box<BoardCellHolder>,
    cur_piece: Option<Rc<Piece>>,
    gravity: f32,
    lock: f32,
    intermediate_y: f32,
    y_needs_update: bool,
    y_to_check: u32
}

pub fn adjust_positions(data: &mut [Point], offset: Point) {
    for p in &mut *data {
        p.x += offset.x;
        p.y += offset.y;
    }
}

pub fn adjust_positions_clone(data: &[Point], offset: Point) -> Vec<Point> {

    data.into_iter()
        .map(|p| Point {
            x: p.x,
            y: p.y
        })
        .collect()
}

impl Default for Board {
    fn default() -> Self {
        Board {
            width: BOARD_WIDTH as u32,
            height: BOARD_HEIGHT as u32,
            cell_holder: Box::new(BoardCellHolder::default()),
            cur_piece: None,
            gravity: 0.0,
            lock: 0.0,
            intermediate_y: 0.0,
            y_needs_update: true,
            y_to_check: 0
        }
    }
}

impl Board {
    pub fn new(width: u32, height: u32) -> Self {
        Board {
            width,
            height,
            cell_holder: Box::new(BoardCellHolder::default()),
            cur_piece: None,
            gravity: 0.0,
            lock: 0.0,
            intermediate_y: 0.0,
            y_needs_update: true,
            y_to_check: 0
        }
    }

    pub fn set_piece(&mut self, piece: &Rc<Piece>) {
        let p = Rc::clone(piece);
        self.cur_piece = Some(p);
    }

    pub fn update(&mut self, dt: f32) {
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
            self.hard_drop();
        }

        // TODO: Change zero
        self.gravity += 0f32 * (dt * 10f32);
    }

    pub fn find_nearest_y(&self) -> u32 {
        todo!();
    }

    fn gravity_drop(&self) {
        todo!();
    }

    pub fn hard_drop(&self) {
        todo!();
    }
}