use std::collections::HashMap;
use macroquad::prelude::*;
use quader_engine::board::Board;
use quader_engine::cell_holder::CellType;
use quader_engine::piece::{get_points_for_piece, OffsetType, RotationState, PieceType};
use quader_engine::primitives::Point;
use quader_engine::utils::{adjust_point_clone, piece_type_to_cell_type, piece_type_to_offset_type, piece_type_to_color};

pub const CELL_SIZE: f32 = 32.0;


fn create_cell_rects() -> HashMap<CellType, Rect> {
    let mut result = HashMap::new();
    result.insert(CellType::Z, Rect::new(32. * 0., 0., 32., 32.));
    result.insert(CellType::L, Rect::new(32. * 1., 0., 32., 32.));
    result.insert(CellType::O, Rect::new(32. * 2., 0., 32., 32.));
    result.insert(CellType::S, Rect::new(32. * 3., 0., 32., 32.));
    result.insert(CellType::I, Rect::new(32. * 4., 0., 32., 32.));
    result.insert(CellType::J, Rect::new(32. * 5., 0., 32., 32.));
    result.insert(CellType::T, Rect::new(32. * 6., 0., 32., 32.));
    result.insert(CellType::Ghost, Rect::new(32. * 7., 0., 32., 32.));
    result.insert(CellType::Solid, Rect::new(32. * 8., 0., 32., 32.));
    result.insert(CellType::Garbage, Rect::new(32. * 9., 0., 32., 32.));

    result
}

pub struct BoardRenderer {
    pub x: f32,
    pub y: f32,
    pub render_offset: f32,
    texture_atlas: Option<Texture2D>,
    cell_rects: HashMap<CellType, Rect>,
    board_tex: Option<Texture2D>,
}

impl BoardRenderer {
    pub fn new(x: f32, y: f32, board_height: usize) -> Self {
        Self {
            x, y,
            render_offset: board_height as f32 * CELL_SIZE,
            texture_atlas: None,
            cell_rects: create_cell_rects(),
            board_tex: None
        }
    }

    pub async fn load_content(&mut self) {
        self.texture_atlas = Some(load_texture("assets/skins/default_3.png").await.unwrap());
        self.board_tex = Some(load_texture("assets/skins/board_default.png").await.unwrap());
    }

    pub fn render(&self, board: &Board) {
        let board_tex = self.board_tex.as_ref().unwrap();
        draw_texture(board_tex, self.x - 188., self.y - 1., WHITE);

        // render board layout
        let layout = board.get_cell_holder();

        for (y, row) in layout.get_layout().iter().enumerate() {
            for (x, cell) in row.into_iter().enumerate() {
                let pos = self.i32_to_coords(x as i32, y as i32);

                if cell != CellType::None {
                    self.render_cell_type(pos.0, pos.1 - self.render_offset, &cell, 255);
                } else if y >= board.game_settings.board.height {
                    draw_rectangle(pos.0, pos.1 - self.render_offset, 32., 32., Color::from_rgba(20, 20, 20, 230));
                    draw_rectangle_lines(pos.0, pos.1 - self.render_offset, 32., 32., 1., Color::from_rgba(255, 255, 255, 25));
                }
            }
        }

        // render current piece
        let piece = board.get_piece_mgr().get_piece();

        let points = piece.get_current_pos();
        points
            .iter()
            .map(|p| adjust_point_clone(p, Point::new(piece.get_x() as i32, piece.get_y() as i32)))
            .for_each(|p| {
                let pos = self.point_to_coords(&p);
                self.render_cell_type(pos.0, pos.1 - self.render_offset, &piece.get_cell_type(), 255);
            });

        // render ghost piece
        let ghost_y = board.find_nearest_y();
        points
            .iter()
            .map(|p| adjust_point_clone(p, Point::new(piece.get_x() as i32, ghost_y as i32)))
            .for_each(|p| {
                let pos = self.point_to_coords(&p);
                //self.render_cell_type(pos.0, pos.1 - self.render_offset, &piece.get_cell_type(), 150);
                self.render_piece_ghost(pos.0, pos.1 - self.render_offset, piece.get_type(), 150);
            });

        // render hold piece
        if let Some(hold_piece) = board.get_hold_piece() {
            let points = get_points_for_piece(hold_piece, RotationState::Initial);

            points
                .iter()
                //.map(|p| adjust_point_clone(p, Point::new(p.x, p.y)))
                .for_each(|p| {
                    let pos = self.point_to_coords(&p);
                    self.render_cell_type(pos.0 - 110., pos.1 + 86., &piece_type_to_cell_type(hold_piece), 255);
                });
        }

        // render queue
        let queue = &board.get_piece_mgr().piece_queue.queue;
        for (y, piece_type) in queue.iter().enumerate() {
            let points = get_points_for_piece(*piece_type, RotationState::Initial);

            points
                .iter()
                //.map(|p| adjust_point_clone(p, Point::new(p.x, p.y)))
                .for_each(|p| {
                    let pos = self.point_to_coords(&p);
                    let offset_type = piece_type_to_offset_type(piece_type);

                    let pos = (
                        match offset_type {
                            OffsetType::Cell => pos.0 + 420.,
                            OffsetType::BetweenCells => pos.0 + 420. + 16.
                        },
                        88. + pos.1 + 96. * y as f32
                    );

                    self.render_cell_type(pos.0, pos.1, &piece_type_to_cell_type(*piece_type), 255)
                });
        }

        // render incoming damage
        let dmg_queue = &board.garbage_mgr.queue;
        let mut total_dmg = 0;
        for (_i, dmg) in dmg_queue.iter().enumerate() {
            let x = self.x + 322.;
            let y = 640. + self.y - total_dmg as f32 * 32.;
            let w = 14.;
            let h = -32. * dmg.amount as f32;
            draw_rectangle(x, y, w, h, RED);
            draw_rectangle_lines(x, y, w, h, 3., Color::from_rgba(20, 0, 0, 255));

            total_dmg += dmg.amount;
        }
    }

    pub fn point_to_coords(&self, point: &Point) -> (f32, f32) {
        self.i32_to_coords(point.x, point.y)
    }
    pub fn i32_to_coords(&self, x: i32, y: i32) -> (f32, f32) {
        (
            self.x + x as f32 * CELL_SIZE,
            self.y + y as f32 * CELL_SIZE
        )
    }

    fn render_cell_type(&self, x: f32, y: f32, cell_type: &CellType, alpha: u8) {

        let ta = &self.texture_atlas.as_ref().unwrap();

        draw_texture_ex(ta, x, y, Color::from_rgba(255, 255, 255, alpha), DrawTextureParams {
            source: Some(self.cell_rects[cell_type].clone()),
            ..Default::default()
        });
    }

    fn render_piece_ghost(&self, x: f32, y: f32, piece_type: PieceType, alpha: u8) {
        let ta = &self.texture_atlas.as_ref().unwrap();

        let col = piece_type_to_color(piece_type);

        draw_texture_ex(ta, x, y, Color::from_rgba(col.r, col.g, col.b, alpha), DrawTextureParams {
            source: Some(self.cell_rects[&CellType::Ghost].clone()),
            ..Default::default()
        });
    }
}

