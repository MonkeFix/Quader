use std::cell::RefCell;
use std::rc::Rc;
use std::sync::mpsc::Receiver;
use macroquad::hash;
use macroquad::prelude::*;
use macroquad::ui::root_ui;

use quader_engine::board::{Board};
use quader_engine::board_command::{BoardCommand, BoardMessage, BoardMoveDir};
use quader_engine::board_manager::BoardManager;
use quader_engine::game_settings::{BOARD_VISIBLE_HEIGHT, GameSettings};
use quader_engine::piece::{get_points_for_piece, Piece, PieceType, RotationDirection, RotationState};
use quader_engine::primitives::Point;
use quader_engine::replays::HardDropInfo;
use quader_engine::utils::{adjust_point_clone, cell_to_color, piece_type_to_color};

use crate::renderable::Renderable;
use crate::updatable::Updatable;

const DEFAULT_CELL_SIZE: f32 = 32.0;

struct PieceMover {
    elapsed: f32,
    arr: f32,
    das: f32,
    sdf: u32,
    is_left_down: bool,
    is_right_down: bool
}

impl PieceMover {
    pub fn move_left(&self, uuid: &str, bm: &mut BoardManager) {
        bm.send_command(uuid, BoardCommand::Move(BoardMoveDir::Left, 1));
    }

    pub fn move_right(&self, uuid: &str, bm: &mut BoardManager) {
        bm.send_command(uuid, BoardCommand::Move(BoardMoveDir::Right, 1));
    }
}

pub struct BoardController {
    x: f32,
    y: f32,
    cell_size: f32,
    render_offset: f32,
    board_mgr: BoardManager,
    receiver: Receiver<BoardMessage>,
    uuid: String,
    board: Rc<RefCell<Board>>,
    piece_mover: PieceMover
}

impl BoardController {
    pub fn new(x: f32, y: f32) -> Self {

        let game_settings = GameSettings::default();

        let mut board_mgr = BoardManager::new(game_settings);

        let rcv = board_mgr.add_board();

        //dbg!(&rcv.2);

        BoardController {
            board: rcv.2,
            x, y,
            cell_size: DEFAULT_CELL_SIZE,
            render_offset: BOARD_VISIBLE_HEIGHT as f32 * DEFAULT_CELL_SIZE,
            board_mgr,
            receiver: rcv.1,
            uuid: rcv.0,
            piece_mover: PieceMover {
                elapsed: 0.0,
                arr: 0.0,
                das: 128.0,
                sdf: u32::MAX,
                is_left_down: false,
                is_right_down: false
            }
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

    fn render_piece(&self, x: f32, y: f32, piece: &Piece, alpha: u8) {
        self.render_piece_type(x, y, piece.get_type(), alpha);
    }

    fn render_piece_type(&self, x: f32, y: f32, piece_type: PieceType, alpha: u8) {
        let color = piece_type_to_color(piece_type);

        draw_rectangle(
            x, y,
            self.cell_size, self.cell_size,
            Color::from_rgba(color.r, color.g, color.b, alpha)
        );

        draw_rectangle_lines(x, y, self.cell_size, self.cell_size, 2.0, Color::from_rgba(255, 255, 255, alpha / 5));
    }
}

impl Renderable for BoardController {

    fn render(&self) {
        // render board layout
        let b = self.board.as_ref().borrow();
        let layout = b.get_cell_holder();

        for (y, row) in layout.get_layout().iter().enumerate() {
            for (x, cell) in row.into_iter().enumerate() {
                let color = cell_to_color(cell);
                let pos = self.usize_to_coords(x, y);

                if y >= BOARD_VISIBLE_HEIGHT {
                    self.render_cell(pos.0, pos.1 - self.render_offset, color);
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
        let ghost_y = self.board.borrow().find_nearest_y();
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
                .map(|p| adjust_point_clone(p, Point::new(64 + p.x * self.cell_size as i32, 160 + p.y * self.cell_size as i32)))
                .for_each(|p| {
                    self.render_piece_type(p.x as f32, p.y as f32, hold_piece, 255);
                });
        }

        // render queue
        let queue = &b.get_piece_mgr().piece_queue.queue;
        for (y, piece_type) in queue.iter().enumerate() {
            let points = get_points_for_piece(*piece_type, RotationState::Initial);

            points
                .iter()
                .map(|p| adjust_point_clone(p, Point::new(540 + p.x * self.cell_size as i32, 160 + (100 * y as i32) + p.y * self.cell_size as i32)))
                .for_each(|p| {
                    self.render_piece_type(p.x as f32, p.y as f32, *piece_type, 255)
                });
        }
    }

    fn debug_render(&mut self) {

        // render piece bounds
        let board = self.board.as_ref().borrow();
        let piece = board.get_piece_mgr().get_piece();

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

        /*match piece.get_offset_type() {
            OffsetType::Cell => draw_circle(pos.0, pos.1, 8.0, Color::from_rgba(255, 0, 0, 255)),
            OffsetType::BetweenCells => draw_circle(pos.0, pos.1 - 800.0, 8.0, Color::from_rgba(255, 0, 0, 255)),
        }*/

        // render debug ui
        root_ui().window(hash!(), Vec2::new(800., 20.), Vec2::new(450., 200.), |ui| {
            let board = self.board.borrow();
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
        if is_key_pressed(KeyCode::A) {

        }

        let elapsed = dt * 1000.0; // convert to milliseconds

        if is_key_pressed(KeyCode::Left) {
            self.piece_mover.move_left(&self.uuid, &mut self.board_mgr);
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
            self.piece_mover.move_right(&self.uuid, &mut self.board_mgr);
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
                    self.piece_mover.move_left(&self.uuid, &mut self.board_mgr);
                }
                if self.piece_mover.is_right_down {
                    self.piece_mover.move_right(&self.uuid, &mut self.board_mgr);
                }
            }
        }

        if is_key_pressed(KeyCode::Down) {
            self.board_mgr.send_command(
                &self.uuid,
                BoardCommand::SoftDrop(self.piece_mover.sdf)
            );
        }
        if is_key_pressed(KeyCode::Space) {
            self.board_mgr.send_command(
                &self.uuid,
                BoardCommand::HardDrop
            );
        }
        if is_key_pressed(KeyCode::Z) {
            self.board_mgr.send_command(
                &self.uuid,
                BoardCommand::Rotate(RotationDirection::CounterClockwise)
            );
        }
        if is_key_pressed(KeyCode::X) {
            self.board_mgr.send_command(
                &self.uuid,
                BoardCommand::Rotate(RotationDirection::Clockwise)
            );
        }
        if is_key_pressed(KeyCode::F) {
            self.board_mgr.send_command(
                &self.uuid,
                BoardCommand::Rotate(RotationDirection::Deg180)
            );
        }
        if is_key_pressed(KeyCode::C) {
            self.board_mgr.send_command(
                &self.uuid,
                BoardCommand::HoldPiece
            )
        }

        if is_key_pressed(KeyCode::T) {
            let mut board = self.board.borrow_mut();
            board.push_garbage(1, 0);
            println!("{}", board.get_cell_holder().get_occupied_cell_count())
        }

        self.board_mgr.send_command(&self.uuid, BoardCommand::Update(dt));

        if let Ok(msg) = self.board_mgr.bi.poll_recv_hard_drop() {
            if msg.lines_cleared > 0 {
                dbg!(&msg);
            }
        }

        if let Ok(msg) = &self.receiver.try_recv() {
            match msg {
                BoardMessage::NewPieceInQueue(_) => {}
                BoardMessage::PieceUpdated => {}
                BoardMessage::GarbageReceived(_, _) => {}
                BoardMessage::GameStateChanged(_) => {}
                BoardMessage::PlayerRemoved => {}
                BoardMessage::BoardUpdated => {}
            }
            //println!("msg: {:?}", msg);
        }

        /*for msg in self.receiver.iter() {
             println!("msg: {:?}", msg);
        }*/
        /*while let Some(i) = self.receiver.recv() {
            println!("got = {:?}", i);
        }*/
    }
}