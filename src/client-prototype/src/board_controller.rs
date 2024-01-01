use std::collections::HashMap;
use std::sync::Arc;
use macroquad::hash;
use macroquad::prelude::*;
use macroquad::ui::root_ui;

use quader_engine::board::{Board};
use quader_engine::cell_holder::CellType;
use quader_engine::game_settings::{GameSettings};
use quader_engine::piece::{get_points_for_piece, OffsetType, Piece, PieceType, RotationDirection, RotationState};
use quader_engine::primitives::Point;
use quader_engine::rng_manager::RngManager;
use quader_engine::utils::{adjust_point_clone, cell_to_color, piece_type_to_cell_type, piece_type_to_color, piece_type_to_offset_type};
use quader_engine::wall_kick_data::WallKickData;

use crate::renderable::Renderable;
use crate::updatable::Updatable;

const DEFAULT_CELL_SIZE: f32 = 32.0;

struct PieceMover {
    elapsed: f32,
    #[allow(dead_code)]
    arr: f32,
    das: f32,
    sdf: u32,
    is_left_down: bool,
    is_right_down: bool
}

impl PieceMover {
    pub fn move_left(&self, board: &mut Board) {
        board.move_left(1);
    }

    pub fn move_right(&self, board: &mut Board) {
        board.move_right(1);
    }
}

fn create_cell_rects() -> HashMap<CellType, Rect> {
    let mut result = HashMap::new();
    result.insert(CellType::Z, Rect::new(32. * 0., 0., 32., 32.));
    result.insert(CellType::L, Rect::new(32. * 1., 0., 32., 32.));
    result.insert(CellType::O, Rect::new(32. * 2., 0., 32., 32.));
    result.insert(CellType::S, Rect::new(32. * 3., 0., 32., 32.));
    result.insert(CellType::I, Rect::new(32. * 4., 0., 32., 32.));
    result.insert(CellType::J, Rect::new(32. * 5., 0., 32., 32.));
    result.insert(CellType::T, Rect::new(32. * 6., 0., 32., 32.));
    result.insert(CellType::Solid, Rect::new(32. * 7., 0., 32., 32.));
    result.insert(CellType::Garbage, Rect::new(32. * 8., 0., 32., 32.));

    result
}

pub struct BoardController {
    x: f32,
    y: f32,
    cell_size: f32,
    render_offset: f32,
    board: Board,
    piece_mover: PieceMover,
    game_settings: GameSettings,
    texture_atlas: Option<Texture2D>,
    cell_rects: HashMap<CellType, Rect>,
    board_tex: Option<Texture2D>
    //wkd: Arc<WallKickData>
}

impl BoardController {
    pub fn new(x: f32, y: f32) -> Self {

        let mut rng_mgr = RngManager::from_entropy();
        let seed = rng_mgr.gen();

        let game_settings = GameSettings::default();
        let wkd = Arc::new(WallKickData::new(game_settings.wall_kick_data_mode));
        let board = Board::new(game_settings, wkd, seed);

        dbg!(&game_settings);



        BoardController {
            board,
            x, y,
            cell_size: DEFAULT_CELL_SIZE,
            render_offset: game_settings.board.height as f32 * DEFAULT_CELL_SIZE,
            piece_mover: PieceMover {
                elapsed: 0.0,
                arr: 0.0,
                das: 128.0,
                sdf: u32::MAX,
                is_left_down: false,
                is_right_down: false
            },
            game_settings,
            texture_atlas: None,
            cell_rects: create_cell_rects(),
            board_tex: None
            //wkd
        }
    }

    pub async fn load_content(&mut self) {
        self.texture_atlas = Some(load_texture("assets/skins/default_3.png").await.unwrap());
        self.board_tex = Some(load_texture("assets/skins/board_default.png").await.unwrap());
    }

    fn point_to_coords(&self, point: &Point) -> (f32, f32) {
        self.i32_to_coords(point.x, point.y)
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

    /*fn render_cell(&self, x: f32, y: f32, color: quader_engine::primitives::Color) {
        draw_rectangle(
            x, y,
            self.cell_size, self.cell_size,
            Color::from_rgba(color.r, color.g, color.b, 255)
        );

        draw_rectangle_lines(x, y, self.cell_size, self.cell_size, 1.0, Color::from_rgba(255, 255, 255, 50));
    }*/

    fn render_piece(&self, x: f32, y: f32, piece: &Piece, alpha: u8) {
        self.render_cell_type(x, y, &piece.get_cell_type(), alpha);
    }

    fn render_cell_type(&self, x: f32, y: f32, cell_type: &CellType, alpha: u8) {

        let ta = &self.texture_atlas.as_ref().unwrap();

        draw_texture_ex(ta, x, y, Color::from_rgba(255, 255, 255, alpha), DrawTextureParams {
            source: Some(self.cell_rects[cell_type].clone()),
            ..Default::default()
        });

        /*let color = piece_type_to_color(piece_type);

        draw_rectangle(
            x, y,
            self.cell_size, self.cell_size,
            Color::from_rgba(color.r, color.g, color.b, alpha)
        );


        draw_rectangle_lines(x, y, self.cell_size, self.cell_size, 2.0, Color::from_rgba(255, 255, 255, alpha / 5));*/
    }
}

impl Renderable for BoardController {

    fn render(&self) {

        let board_tex = self.board_tex.as_ref().unwrap();
        draw_texture(board_tex, self.x - 188., self.y - 1., WHITE);

        // render board layout
        let b = &self.board;
        let layout = b.get_cell_holder();

        for (y, row) in layout.get_layout().iter().enumerate() {
            for (x, cell) in row.into_iter().enumerate() {
                let color = cell_to_color(cell);
                let pos = self.usize_to_coords(x, y);

                if cell != CellType::None {
                    self.render_cell_type(pos.0, pos.1 - self.render_offset, &cell, 255);
                } else if y >= self.game_settings.board.height {
                    draw_rectangle(pos.0, pos.1 - self.render_offset, 32., 32., Color::from_rgba(20, 20, 20, 230));
                    draw_rectangle_lines(pos.0, pos.1 - self.render_offset, 32., 32., 1., Color::from_rgba(255, 255, 255, 25));
                }
            }
        }

        // render current piece
        let piece = b.get_piece_mgr().get_piece();

        let points = piece.get_current_pos();
        points
            .iter()
            .map(|p| adjust_point_clone(p, Point::new(piece.get_x() as i32, piece.get_y() as i32)))
            .for_each(|p| {
                let pos = self.point_to_coords(&p);
                self.render_piece(pos.0, pos.1 - self.render_offset, piece, 255);
            });

        // render ghost piece
        let ghost_y = self.board.find_nearest_y();
        points
            .iter()
            .map(|p| adjust_point_clone(p, Point::new(piece.get_x() as i32, ghost_y as i32)))
            .for_each(|p| {
                let pos = self.point_to_coords(&p);
                self.render_piece(pos.0, pos.1 - self.render_offset, piece, 150);
            });

        // render hold piece
        if let Some(hold_piece) = b.get_hold_piece() {
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
        let queue = &b.get_piece_mgr().piece_queue.queue;
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
    }

    fn debug_render(&mut self) {
        return;
        // render piece bounds
        //let board = &self.board;
        //let piece = board.get_piece_mgr().get_piece();

        //let bounds = piece.get_bounds();
        //let pos = self.i32_to_coords(bounds.x, bounds.y);

        /*draw_rectangle_lines(
            pos.0,
            pos.1 - self.render_offset,
            bounds.width as f32 * self.cell_size,
            bounds.height as f32 * self.cell_size,
            1.0,
            Color::from_rgba(255, 0, 0, 255)
        );

        let pos = self.u32_to_coords(piece.get_x(), piece.get_y());
        draw_rectangle(self.x + pos.0 * self.cell_size, self.y + pos.1 * self.cell_size, 3., 3., Color::from_rgba(255, 255, 255, 255));
*/
        /*match piece.get_offset_type() {
            OffsetType::Cell => draw_circle(pos.0, pos.1, 8.0, Color::from_rgba(255, 0, 0, 255)),
            OffsetType::BetweenCells => draw_circle(pos.0, pos.1 - 800.0, 8.0, Color::from_rgba(255, 0, 0, 255)),
        }*/

        // render debug ui
        root_ui().window(hash!(), Vec2::new(800., 20.), Vec2::new(450., 200.), |ui| {
            let board = &self.board;
            let piece_mgr = board.get_piece_mgr();
            let piece = piece_mgr.get_piece();
            ui.label(None, &format!("Piece position: {{{}, {}}}", piece.get_x(), piece.get_y()));
            ui.label(None, &format!("Nearest Y: {}", board.find_nearest_y()));

            /*if ui.button(None, "I") {
                self.board.create_piece(PieceType::I);
            }
            ui.same_line(0.0);
            if ui.button(None, "J") {
                self.board.create_piece(PieceType::J);
            }
            ui.same_line(0.0);
            if ui.button(None, "O") {
                self.board.create_piece(PieceType::O);
            }
            ui.same_line(0.0);
            if ui.button(None, "L") {
                self.board.create_piece(PieceType::L);
            }
            ui.same_line(0.0);
            if ui.button(None, "Z") {
                self.board.create_piece(PieceType::Z);
            }
            ui.same_line(0.0);
            if ui.button(None, "S") {
                self.board.create_piece(PieceType::S);
            }
            ui.same_line(0.0);
            if ui.button(None, "T") {
                self.board.create_piece(PieceType::T);
            }

            ui.label(None, "Piece Queue:");
            let queue = self.board.get_queue();
            for p in queue {
                ui.label(None, &format!("{:?}", p.get_type()));
                ui.same_line(0.0);
            }*/
            // skip the last same_line()
            ui.label(None, "");
        });
    }
}

impl Updatable for BoardController {
    fn update(&mut self, dt: f32) {
        let elapsed = dt * 1000.0; // convert to milliseconds

        if is_key_down(KeyCode::Q) {
            self.y -= 1.0;
        }
        if is_key_down(KeyCode::A) {
            self.y += 1.0;
        }

        if is_key_pressed(KeyCode::Left) {
            self.piece_mover.move_left(&mut self.board);
        }
        if is_key_down(KeyCode::Left) {
            self.piece_mover.is_left_down = true;
            self.piece_mover.elapsed += elapsed;
        }
        if is_key_released(KeyCode::Left) {
            self.piece_mover.is_left_down = false;
            self.piece_mover.elapsed = 0.0;
        }

        if is_key_pressed(KeyCode::Right) {
            self.piece_mover.move_right(&mut self.board);
        }
        if is_key_down(KeyCode::Right) {
            self.piece_mover.is_right_down = true;
            self.piece_mover.elapsed += elapsed;
        }
        if is_key_released(KeyCode::Right) {
            self.piece_mover.is_right_down = false;
            self.piece_mover.elapsed = 0.0;
        }

        if self.piece_mover.elapsed >= self.piece_mover.das {
            let moves = 10;

            for _ in 0..moves {
                if self.piece_mover.is_left_down {
                    self.piece_mover.move_left(&mut self.board);
                }
                if self.piece_mover.is_right_down {
                    self.piece_mover.move_right(&mut self.board);
                }
            }
        }

        if is_key_pressed(KeyCode::Down) {
            self.board.soft_drop(self.piece_mover.sdf);
        }
        if is_key_pressed(KeyCode::Space) {
            let result = self.board.hard_drop();
            match result {
                Ok(move_info) => {
                    dbg!(move_info);
                }
                Err(reason) => {
                    dbg!(reason);
                }
            }
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
        if is_key_pressed(KeyCode::C) {
            self.board.hold_piece();
        }

        if is_key_pressed(KeyCode::T) {
            self.board.attack(2);
        }

        if let Some(res) = self.board.update(dt) {
            match res {
                Ok(move_info) => {
                    dbg!(move_info);
                }
                Err(reason) => {
                    dbg!(reason);
                }
            }
        }
    }
}