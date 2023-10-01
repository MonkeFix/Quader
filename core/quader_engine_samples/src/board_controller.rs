use std::ops::{Add, Mul};
use macroquad::prelude::*;

use quader_engine::board::{adjust_point_clone, Board, CellType};
use quader_engine::board_cell_holder::{BOARD_VISIBLE_HEIGHT, cell_to_color};
use quader_engine::piece::{Piece, PieceType, RotationDirection};
use quader_engine::primitives::Point;

use crate::renderable::Renderable;
use crate::updatable::Updatable;

const DEFALUT_CELL_SIZE: f32 = 32.0;

pub struct BoardController {
    board: Box<Board>,
    x: f32,
    y: f32,
    cell_size: f32
}

impl Default for BoardController {
    fn default() -> Self {
        BoardController::new(0.0, 0.0)
    }
}

impl BoardController {
    pub fn new(x: f32, y: f32) -> Self {

        let mut board = Box::<Board>::default();
        board.create_piece(PieceType::J);

        BoardController {
            board,
            x, y,
            cell_size: DEFALUT_CELL_SIZE
        }
    }

    fn point_to_coords(&self, point: &Point) -> (f32, f32) {
        self.usize_to_coords(point.x as usize, point.y as usize)
    }

    fn usize_to_coords(&self, x: usize, y: usize) -> (f32, f32) {
        (
            self.x + x as f32 * self.cell_size,
            self.y + y as f32 * self.cell_size
        )
    }
    fn u32_to_coords(&self, x: u32, y: u32) -> (f32, f32) {
        (
            self.x + x as f32 * self.cell_size,
            self.y + y as f32 * self.cell_size
        )
    }
    fn i32_to_coords(&self, x: i32, y: i32) -> (f32, f32) {
        (
            self.x + x as f32 * self.cell_size,
            self.y + y as f32 * self.cell_size
        )
    }

    fn render_cell(&self, x: f32, y: f32, color: quader_engine::primitives::Color) {
        draw_rectangle(
            x, y,
            self.cell_size, self.cell_size,
            Color::from_rgba(color.r, color.g, color.b, 255)
        );

        draw_rectangle_lines(x, y, self.cell_size, self.cell_size, 1.0, Color::from_rgba(255, 255, 255, 50));
    }

    fn render_piece(&self, x: f32, y: f32, piece: &Piece) {
        let color = piece.get_color();
        draw_rectangle(
            x, y,
            self.cell_size, self.cell_size,
            Color::from_rgba(color.r, color.g, color.b, 255)
        );

        draw_rectangle_lines(x, y, self.cell_size, self.cell_size, 2.0, Color::from_rgba(255, 255, 255, 50));
    }
}

impl Renderable for BoardController {

    fn render(&self) {
        // render board layout
        let layout = self.board.as_ref().get_layout();

        for (y, row) in layout.iter().rev().take(BOARD_VISIBLE_HEIGHT / 2).enumerate() {
            for (x, cell) in row.into_iter().enumerate() {

                let color = cell_to_color(&cell);
                let pos = self.usize_to_coords(x, y);

                self.render_cell(pos.0, pos.1, color);
            }
        }

        // render current piece
        if let Some(piece) = self.board.get_piece() {
            let points = piece.get_current_pos();
            points
                .iter()
                .map(|p| adjust_point_clone(p, Point::new(piece.get_x() as i32, piece.get_y() as i32)))
                .for_each(|p| {
                    let pos = self.point_to_coords(&p);
                    self.render_piece(pos.0, pos.1, piece);
                });
        }
    }

    fn debug_render(&self) {
        // render piece bounds
        if let Some(piece) = self.board.get_piece() {
            let bounds = piece.get_bounds();
            let pos = self.i32_to_coords(bounds.x, bounds.y);

            draw_rectangle_lines(
                pos.0,
                pos.1,
                bounds.width as f32 * self.cell_size,
                bounds.height as f32 * self.cell_size,
                1.0,
                Color::from_rgba(255, 0, 0, 255)
            );

            let pos = self.u32_to_coords(piece.get_x(), piece.get_y());
            draw_rectangle(self.x + pos.0 * self.cell_size, self.y + pos.1 * self.cell_size, 3., 3., Color::from_rgba(255, 255, 255, 255));
        }
    }
}

impl Updatable for BoardController {
    fn update(&mut self, dt: f32) {
        if is_key_pressed(KeyCode::A) {
            self.board.set_cell_at(0, 0, CellType::J);
        }

        if is_key_pressed(KeyCode::Left) {
            self.board.move_left();
        }
        if is_key_pressed(KeyCode::Right) {
            self.board.move_right();
        }
        if is_key_pressed(KeyCode::Down) {
            self.board.soft_drop();
        }
        if is_key_pressed(KeyCode::Space) {
            self.board.hard_drop();
        }
        if is_key_pressed(KeyCode::Z) {
            self.board.rotate(RotationDirection::CounterClockwise);
        }
        if is_key_pressed(KeyCode::X) {
            self.board.rotate(RotationDirection::Clockwise);
        }
        if is_key_pressed(KeyCode::F) {
            self.board.rotate(RotationDirection::Deg180);
        }

        self.board.update(dt);
    }
}