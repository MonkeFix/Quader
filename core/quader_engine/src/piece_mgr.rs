use std::rc::Rc;
use std::sync::{Arc, Weak};
use crate::board::{Board, BoardComponent, BoardEntity};
use crate::cell_holder::CellHolder;
use crate::game_settings::{BoardSettings, GameSettings};
use crate::piece::{OffsetType, Piece, PieceType, RotationDirection, WallKickCheckParams};
use crate::primitives::Point;
use crate::utils::adjust_positions_clone;
use crate::wall_kick_data::WallKickData;

pub struct PieceMgr {
    curr_piece: Option<Piece>,
    board: Arc<Board>,
    board_width: usize,
    board_height: usize
}

impl PieceMgr {
    pub fn new(game_settings: &GameSettings, board: &Arc<Board>) -> Self {
        Self {
            curr_piece: None,
            board: Arc::clone(board),
            board_width: game_settings.get_board().width,
            board_height: game_settings.get_board().height
        }
    }

    pub fn set_piece(&mut self, piece_type: PieceType) {
        let piece = Piece::new(piece_type);

        self.curr_piece = Some(piece);
        self.reset();
    }

    pub fn get_piece(&self) -> Option<&Piece> {
        match &self.curr_piece {
            None => None,
            Some(piece) => Some(piece)
        }
    }

    pub fn move_left(&mut self, delta: i32) {
        if self.test_movement(-1, 0) {
            self.curr_piece.as_mut().expect("Piece must be set").move_left();
        }
    }

    pub fn move_right(&mut self, delta: i32) {
        if self.test_movement(1, 0) {
            self.curr_piece.as_mut().expect("Piece must be set").move_left();
        }
    }

    pub fn rotate(&mut self, wkd: &WallKickData, rotation: &RotationDirection) {
        let piece = self.curr_piece.as_ref().expect("Piece must be set");
        let rot_type = piece.get_rotation_type(&rotation);
        let tests = &wkd.get(piece.get_wall_kick_type())[&rot_type.0];

        let test = self.test_rotation(WallKickCheckParams {
            tests,
            expected_pos: rot_type.1
        });

        if let Some(point) = test {
            self.curr_piece
                .as_mut()
                .unwrap()
                .rotate(rotation, point.x, point.y);
        }
    }

    pub fn find_nearest_y(&self) -> u32 {
        if let Some(piece) = self.curr_piece.as_ref() {
            let mut y = piece.get_y();
            let points = piece.get_positions();

            let cell_holder = self.board.get_component::<CellHolder>("cell_holder").unwrap();

            for i in piece.get_y()..=(self.board_height as u32) {
                let offset: Point<i32> = Point::new(piece.get_x() as i32, i as i32);
                let new_points = adjust_positions_clone(points, offset);
                if cell_holder.intersects_any(&new_points) {
                    break;
                }

                y = i;
            }

            return y;
        }

        0
    }

    fn test_movement(&self, x: i32, y: i32) -> bool {

        let cell_holder = self.board.get_component::<CellHolder>("cell_holder").unwrap();

        let piece = self.curr_piece.as_ref().unwrap();
        let b = piece.get_bounds();

        if b.x + x < 0 || b.x + b.width as i32 + x > self.board_width as i32 {
            return false;
        }
        if b.y + b.height as i32 + y > self.board_height as i32 {
            return false;
        }

        let pos = piece.get_positions();
        let offset: Point<i32> = Point::new(
            piece.get_x() as i32 + x,
            piece.get_y() as i32 + y
        );
        let new_pos = adjust_positions_clone(pos, offset);

        !cell_holder.intersects_any(&new_pos)
    }

    fn test_rotation(&self, kick_params: WallKickCheckParams) -> Option<Point> {
        let tests = kick_params.tests;
        let expected_pos = kick_params.expected_pos;
        let piece = self.curr_piece.as_ref().unwrap();
        let cell_holder = self.board.get_component::<CellHolder>("cell_holder").unwrap();

        for t in tests {
            let test = Point::new(t.x, -t.y);

            let adjusted = adjust_positions_clone(
                expected_pos,
                Point::new(piece.get_x() as i32 + test.x, piece.get_y() as i32 + test.y)
            );

            if !cell_holder.intersects_any(&adjusted) {
                return Some(test);
            }
        }

        None
    }
}

impl BoardComponent for PieceMgr {
    fn get_name(&self) -> &'static str {
        "piece_mgr"
    }

    fn reset(&mut self) {
        let piece = self.curr_piece.as_mut().unwrap();

        match piece.get_offset_type() {
            OffsetType::Cell => piece
                .set_x(self.board_width as u32 / 2 - 1),
            OffsetType::BetweenCells => piece
                .set_x(((self.board_width as f32) / 2.0).round() as u32)
        }

        match piece.get_type() {
            PieceType::I => piece.set_y(self.board_height as u32 / 2 + 1),
            _ => piece.set_y(self.board_height as u32 / 2)
        };

        piece.reset();
    }
}