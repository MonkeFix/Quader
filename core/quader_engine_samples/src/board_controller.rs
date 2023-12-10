use std::sync::mpsc::Receiver;
use macroquad::hash;
use macroquad::prelude::*;
use macroquad::ui::root_ui;

use quader_engine::board::{BoardOld};
use quader_engine::board_command::{BoardCommand, BoardCommandType, BoardMessage, BoardMoveDir};
use quader_engine::board_manager::BoardManager;
use quader_engine::cell_holder::CellType;
use quader_engine::game_settings::{BOARD_VISIBLE_HEIGHT, GameSettings};
use quader_engine::piece::{Piece, PieceType, RotationDirection};
use quader_engine::primitives::Point;
use quader_engine::utils::{adjust_point_clone, cell_to_color};

use crate::renderable::Renderable;
use crate::updatable::Updatable;

const DEFAULT_CELL_SIZE: f32 = 32.0;

pub struct BoardController {
    board: Box<BoardOld>,
    x: f32,
    y: f32,
    cell_size: f32,
    render_offset: f32,
    board_mgr: BoardManager,
    receiver: Receiver<BoardMessage>,
    uuid: String
}

impl BoardController {
    pub fn new(x: f32, y: f32) -> Self {

        let game_settings = GameSettings::default();

        let mut board = Box::<BoardOld>::default();
        board.create_piece(PieceType::I);

        let mut board_mgr = BoardManager::new(game_settings);

        let rcv = board_mgr.add_board();

        BoardController {
            board,
            x, y,
            cell_size: DEFAULT_CELL_SIZE,
            render_offset: BOARD_VISIBLE_HEIGHT as f32 * DEFAULT_CELL_SIZE,
            board_mgr,
            receiver: rcv.1,
            uuid: rcv.0
        }
    }

    /*pub fn init(&mut self) {
        let rcv = self.board_mgr.add_board();
        self.receiver = Some(rcv.1);
        self.uuid = Some(rcv.0);
    }*/

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

        for (y, row) in layout.iter().enumerate() {
            for (x, cell) in row.into_iter().enumerate() {
                let color = cell_to_color(&cell);
                let pos = self.usize_to_coords(x, y);

                if y >= BOARD_VISIBLE_HEIGHT {
                    self.render_cell(pos.0, pos.1 - self.render_offset, color);
                }
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
                    self.render_piece(pos.0, pos.1 - self.render_offset, piece);
                });
        }
    }

    fn debug_render(&mut self) {
        // render piece bounds
        if let Some(piece) = self.board.get_piece() {
            let bounds = piece.get_bounds();
            let pos = self.i32_to_coords(bounds.x, bounds.y);

            draw_rectangle_lines(
                pos.0,
                pos.1 - self.render_offset,
                bounds.width as f32 * self.cell_size,
                bounds.height as f32 * self.cell_size,
                1.0,
                Color::from_rgba(255, 0, 0, 255)
            );

            let pos = self.u32_to_coords(piece.get_x(), piece.get_y());
            draw_rectangle(self.x + pos.0 * self.cell_size, self.y + pos.1 * self.cell_size, 3., 3., Color::from_rgba(255, 255, 255, 255));


        }

        // render debug ui
        root_ui().window(hash!(), Vec2::new(800., 20.), Vec2::new(450., 200.), |ui| {
            let piece = self.board.get_piece().unwrap();
            ui.label(None, &format!("Piece position: {{{}, {}}}", piece.get_x(), piece.get_y()));
            ui.label(None, &format!("Nearest Y: {}", self.board.find_nearest_y()));

            if ui.button(None, "I") {
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
            }
            // skip the last same_line()
            ui.label(None, "");
        });
    }
}

impl Updatable for BoardController {
    fn update(&mut self, dt: f32) {
        if is_key_pressed(KeyCode::A) {
            self.board.set_cell_at(0, 0, CellType::J);
        }

        if is_key_pressed(KeyCode::Left) {
            self.board.move_left(1);

            self.board_mgr.send_command(
                &self.uuid,
                BoardCommand::new(BoardCommandType::Move(BoardMoveDir::Left, 1))
            );
        }
        if is_key_pressed(KeyCode::Right) {
            self.board.move_right(1);

            self.board_mgr.send_command(
                &self.uuid,
                BoardCommand::new(BoardCommandType::Move(BoardMoveDir::Right, 1))
            );
        }
        if is_key_pressed(KeyCode::Down) {
            self.board.soft_drop(1);

            self.board_mgr.send_command(
                &self.uuid,
                BoardCommand::new(BoardCommandType::SoftDrop(1))
            );
        }
        if is_key_pressed(KeyCode::Space) {
            self.board.hard_drop();

            self.board_mgr.send_command(
                &self.uuid,
                BoardCommand::new(BoardCommandType::HardDrop)
            );
        }
        if is_key_pressed(KeyCode::Z) {
            self.board.rotate(&RotationDirection::CounterClockwise);

            self.board_mgr.send_command(
                &self.uuid,
                BoardCommand::new(BoardCommandType::Rotate(RotationDirection::CounterClockwise))
            );
        }
        if is_key_pressed(KeyCode::X) {
            self.board.rotate(&RotationDirection::Clockwise);

            self.board_mgr.send_command(
                &self.uuid,
                BoardCommand::new(BoardCommandType::Rotate(RotationDirection::Clockwise))
            );
        }
        if is_key_pressed(KeyCode::F) {
            self.board.rotate(&RotationDirection::Deg180);

            self.board_mgr.send_command(
                &self.uuid,
                BoardCommand::new(BoardCommandType::Rotate(RotationDirection::Deg180))
            );
        }

        self.board.update(dt);
        self.board_mgr.update(dt);

        if let Ok(msg) = &self.receiver.try_recv() {
            match msg {
                BoardMessage::NewPieceInQueue(_) => {}
                BoardMessage::PieceUpdated => {}
                BoardMessage::GarbageReceived(_, _) => {}
                BoardMessage::GameStateChanged(_) => {}
                BoardMessage::PlayerRemoved => {}
                BoardMessage::BoardUpdated => {}
            }
            println!("msg: {:?}", msg);
        }

        /*for msg in self.receiver.iter() {
             println!("msg: {:?}", msg);
        }*/
        /*while let Some(i) = self.receiver.recv() {
            println!("got = {:?}", i);
        }*/
    }
}