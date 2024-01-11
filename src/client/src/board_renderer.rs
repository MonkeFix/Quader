/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENSE file in the repository root for full licence text.
 */

use std::collections::HashMap;
use macroquad::prelude::*;
use quader_engine::board::Board;
use quader_engine::cell_holder::CellType;
use quader_engine::piece::{get_points_for_piece, OffsetType, RotationState, PieceType};
use quader_engine::primitives::Point;
use quader_engine::utils::{adjust_point_clone, piece_type_to_cell_type, piece_type_to_offset_type, piece_type_to_color};
use crate::assets::{Assets, CELL_SIZE};


pub struct BoardRenderer {
    pub x: f32,
    pub y: f32,
    pub render_offset: f32
}

impl BoardRenderer {
    pub fn new(x: f32, y: f32, board_height: usize) -> Self {
        Self {
            x, y,
            render_offset: board_height as f32 * CELL_SIZE,
        }
    }

    pub fn render(&self, assets: &Assets, board: &Board) {
        draw_texture(&assets.board_tex, self.x - 188., self.y - 1., WHITE);

        // render board layout
        let layout = board.get_cell_holder();

        for (y, row) in layout.get_layout().iter().enumerate() {
            for (x, cell) in row.into_iter().enumerate() {
                let pos = self.i32_to_coords(x as i32, y as i32);

                if cell != CellType::None {
                    self.render_cell_type(assets, pos.0, pos.1 - self.render_offset, &cell, 255);
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
                self.render_cell_type(assets, pos.0, pos.1 - self.render_offset, &piece.get_cell_type(), 255);
            });

        // render ghost piece
        let ghost_y = board.piece_mgr.nearest_y; //board.find_nearest_y();
        points
            .iter()
            .map(|p| adjust_point_clone(p, Point::new(piece.get_x() as i32, ghost_y as i32)))
            .for_each(|p| {
                let pos = self.point_to_coords(&p);
                //self.render_cell_type(pos.0, pos.1 - self.render_offset, &piece.get_cell_type(), 150);
                self.render_piece_ghost(assets, pos.0, pos.1 - self.render_offset, piece.get_type(), 150);
            });

        // render hold piece
        if let Some(hold_piece) = board.get_hold_piece() {
            let points = get_points_for_piece(hold_piece, RotationState::Initial);

            points
                .iter()
                //.map(|p| adjust_point_clone(p, Point::new(p.x, p.y)))
                .for_each(|p| {
                    let pos = self.point_to_coords(&p);
                    self.render_cell_type(assets, pos.0 - 110., pos.1 + 86., &piece_type_to_cell_type(hold_piece), 255);
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

                    self.render_cell_type(assets, pos.0, pos.1, &piece_type_to_cell_type(*piece_type), 255)
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

        // draw stats
        let x_offset = -180.0;
        let y_offset = 200.0;
        draw_text(&format!("Time: {:.2}", board.board_stats.elapsed_seconds), x_offset + self.x, y_offset + self.y, 32., RED);
        draw_text(&format!("APM: {:.2}", board.board_stats.apm), x_offset + self.x, y_offset + self.y + 34., 32., RED);
        draw_text(&format!("PPS: {:.2}", board.board_stats.pps), x_offset + self.x, y_offset + self.y + 34. * 2., 32., RED);
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

    fn render_cell_type(&self, assets: &Assets, x: f32, y: f32, cell_type: &CellType, alpha: u8) {

        let ta = &assets.texture_atlas;

        draw_texture_ex(ta, x, y, Color::from_rgba(255, 255, 255, alpha), DrawTextureParams {
            source: Some(assets.source_rect(cell_type)),
            ..Default::default()
        });
    }

    fn render_piece_ghost(&self, assets: &Assets, x: f32, y: f32, piece_type: PieceType, alpha: u8) {
        let ta = &assets.texture_atlas;

        let col = piece_type_to_color(piece_type);

        draw_texture_ex(ta, x, y, Color::from_rgba(col.r, col.g, col.b, alpha), DrawTextureParams {
            source: Some(assets.source_rect(&CellType::Ghost)),
            ..Default::default()
        });
    }
}

