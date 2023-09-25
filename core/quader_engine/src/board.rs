use std::cmp::max;
use std::ops::{Add, AddAssign};
use std::rc::Rc;
use serde::{Deserialize, Serialize};
use crate::board_cell_holder::{BOARD_HEIGHT, BOARD_WIDTH, BoardCellHolder, Row};
use crate::piece::{Piece, PieceType};
use crate::primitives::Point;

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
    y_to_check: u32,
    cells_on_board: usize
}

pub fn adjust_positions<T: std::ops::AddAssign + Copy>(data: &mut [Point<T>], offset: Point<T>) {
    for p in &mut *data {
        p.x += offset.x;
        p.y += offset.y;
    }
}

pub fn adjust_positions_clone<T: std::ops::Add + Copy>(data: &[Point<T>], offset: Point<T>) -> Vec<Point<T::Output>> {

    data.iter()
        .map(|p| Point {
            x: p.x + offset.x,
            y: p.y + offset.y
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
            y_to_check: 0,
            cells_on_board: 0
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
            y_to_check: 0,
            cells_on_board: 0
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
        let piece = self.cur_piece.as_ref().unwrap();
        let mut y = piece.get_y();
        let points = piece.get_positions();


        for i in piece.get_y()..=self.height {
            let offset: Point<i32> = Point::new(piece.get_x() as i32, i as i32);
            let new_points = adjust_positions_clone(&points, offset);
            if self.cell_holder.intersects_any(&new_points) {
                break;
            }

            y = i;
        }

        y
    }

    pub fn get_layout(&self) -> &[Row; BOARD_HEIGHT] {
        self.cell_holder.get_layout()
    }

    fn gravity_drop(&self) {
        todo!();
    }

    pub fn hard_drop(&self) {
        todo!();
    }

    pub fn test_movement(&mut self, x: u32, y: u32) -> bool {
        let piece = self.cur_piece.as_mut().unwrap();
        let b = piece.get_bounds();

        if b.x as u32 + b.width + x > self.width {
            return false;
        }
        if b.y as u32 + b.height + y > self.height {
            return false;
        }

        let pos = piece.get_positions();
        let offset: Point<i32> = Point::new(piece.get_x() as i32 + x as i32, piece.get_y() as i32 + y as i32);
        let new_pos = adjust_positions_clone(pos, offset);

        !self.cell_holder.intersects_any(&new_pos)
    }

    fn try_apply_piece(&mut self, points: &[Point], x: i32, y: i32) -> bool {
        let adjusted = adjust_positions_clone(points, Point::new(x, y));

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

fn piece_type_to_cell_type(piece_type: &PieceType) -> CellType {
    match piece_type {
        PieceType::I => CellType::I,
        PieceType::O => CellType::O,
        PieceType::T => CellType::T,
        PieceType::L => CellType::L,
        PieceType::J => CellType::J,
        PieceType::S => CellType::S,
        PieceType::Z => CellType::Z,
        PieceType::Pixel => CellType::Garbage,
    }
}