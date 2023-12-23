use std::cell::RefCell;
use std::rc::Rc;
use crate::board::{BoardComponent, UpdateErrorReason};
use crate::cell_holder::{CellHolder, CellType};
use crate::game_settings::{BOARD_VISIBLE_HEIGHT, GameSettings};
use crate::piece::{OffsetType, Piece, PieceType, RotationDirection, WallKickCheckParams};
use crate::primitives::Point;
use crate::utils::{adjust_positions_clone, piece_type_to_cell_type};
use crate::wall_kick_data::WallKickData;

/*#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct HardDropResult {
    is_success: bool,
    lines_cleared: u32
}

const HARD_DROP_FAIL: HardDropResult = HardDropResult {
    is_success: false,
    lines_cleared: 0
};*/

pub struct PieceMgr {
    curr_piece: Option<Piece>,
    cell_holder: Rc<RefCell<CellHolder>>,
    board_width: usize,
    board_height: usize,
    hold_piece: Option<PieceType>,
    is_hold_used: bool
}

impl PieceMgr {
    pub fn new(game_settings: &GameSettings, cell_holder: Rc<RefCell<CellHolder>>) -> Self {
        Self {
            curr_piece: None,
            cell_holder,
            board_width: game_settings.get_board().width,
            board_height: game_settings.get_board().height,
            hold_piece: None,
            is_hold_used: false
        }
    }

    pub fn create_piece(&mut self, piece_type: PieceType) -> Option<&Piece> {
        let piece = Piece::new(piece_type);

        self.curr_piece = Some(piece);
        self.reset_piece();

        self.get_piece()
    }

    pub fn set_piece(&mut self, piece: Piece) {
        self.curr_piece = Some(piece);
        self.reset_piece();
    }

    pub fn get_piece(&self) -> Option<&Piece> {
        match &self.curr_piece {
            None => None,
            Some(piece) => Some(piece)
        }
    }

    pub fn get_hold_piece(&self) -> Option<PieceType> {
        self.hold_piece
    }

    /// Holds current piece if possible. If success, returns `Some(&Piece)`, otherwise `None`.
    pub fn hold_piece<F: FnMut() -> PieceType>(&mut self, mut get_piece_func: F) -> Option<&Piece> {
        // we can hold piece once per turn
        if self.is_hold_used {
            return None;
        }

        self.is_hold_used = true;

        // if we have hold piece, then replace the current piece with the hold one
        // and put the new piece to hold
        if let Some(piece) = self.hold_piece {
            let curr_piece = self.get_piece().unwrap();
            self.hold_piece = Some(curr_piece.get_type());

            return Some(self.create_piece(piece).unwrap());

        } else {
            // otherwise put current piece to hold and set a new piece
            self.hold_piece = Some(self.get_piece().unwrap().get_type());

            let new_piece = get_piece_func();

            return Some(self.create_piece(new_piece).unwrap());
        }
    }

    /// Tries to move the current piece one cell to the left `delta` times.
    /// If it fails, nothing happens, as the piece collides with either board's bounds
    /// or occupied cells.
    pub fn move_left(&mut self, _delta: i32) {
        //for _ in 0..=delta {
            if self.test_movement(-1, 0) {
                self.curr_piece.as_mut().expect("Piece must be set").move_left();
            }
        //}
    }

    /// Tries to move the current piece one cell to the right `delta` times.
    /// If it fails, nothing happens, as the piece collides with either board's bounds
    /// or occupied cells.
    pub fn move_right(&mut self, _delta: i32) {
        //for _ in 0..=delta {
            if self.test_movement(1, 0) {
                self.curr_piece.as_mut().expect("Piece must be set").move_right();
            }
        //}
    }

    pub fn rotate(&mut self, wkd: &WallKickData, rotation: RotationDirection) {
        let piece = self.curr_piece.as_ref().expect("Piece must be set");
        let rot_type = piece.get_rotation_type(rotation);
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

    /// Returns nearest Y coordinate the piece fits at.
    /// May be useful for rendering ghost piece.
    pub fn find_nearest_y(&self) -> u32 {
        if let Some(piece) = self.curr_piece.as_ref() {
            let mut y = piece.get_y();
            let points = piece.get_positions();

            for i in piece.get_y()..=(self.board_height as u32) {
                let offset: Point<i32> = Point::new(piece.get_x() as i32, i as i32);
                let new_points = adjust_positions_clone(points, offset);
                if self.cell_holder.borrow().intersects_any(&new_points) {
                    break;
                }

                y = i;
            }

            return y;
        }

        0
    }

    /// Tries to move the current piece one cell down `dt` times.
    /// If it fails, then nothing happens as the piece collides with other cells.
    pub fn soft_drop(&mut self, _dt: u32) {
        //for _ in 0..=dt {
            if self.test_movement(0, 1) {
                self.curr_piece.as_mut().unwrap().move_down();
            }
        //}
    }

    /// Tries to hard drop the current piece.
    /// The method checks if the piece could fit in the desired cells.
    /// If it couldn't, returns Err, otherwise returns cleared lines count.
    pub fn hard_drop(&mut self) -> Result<u32, UpdateErrorReason> {
        let nearest_y = self.find_nearest_y();

        if !self.try_apply_piece(nearest_y) {
            return Err(UpdateErrorReason::CannotApplyPiece);
        }

        let lines_cleared = self.cell_holder.borrow().check_row_clears(None);

        if nearest_y <= BOARD_VISIBLE_HEIGHT as u32 && lines_cleared.is_empty() {
            return Err(UpdateErrorReason::CannotApplyPiece);
        }

        self.cell_holder.borrow_mut().clear_rows(&lines_cleared);

        let lines_cleared = lines_cleared.len() as u32;

        self.reset();

        Ok(lines_cleared)
    }

    fn test_movement(&self, x: i32, y: i32) -> bool {

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

        !self.cell_holder.borrow().intersects_any(&new_pos)
    }

    fn test_rotation(&self, kick_params: WallKickCheckParams) -> Option<Point> {
        let tests = kick_params.tests;
        let expected_pos = kick_params.expected_pos;
        let piece = self.curr_piece.as_ref().unwrap();

        for t in tests {
            let test = Point::new(t.x, -t.y);

            let adjusted = adjust_positions_clone(
                expected_pos,
                Point::new(piece.get_x() as i32 + test.x, piece.get_y() as i32 + test.y)
            );

            if !self.cell_holder.borrow().intersects_any(&adjusted) {
                return Some(test);
            }
        }

        None
    }

    /// Returns false if the piece couldn't be fit using its current points.
    /// It usually means that the player just lost.
    fn try_apply_piece(&mut self, y: u32) -> bool {
        let piece = self.curr_piece.as_ref().unwrap();
        let points = piece.get_current_pos();
        let x = piece.get_x() as i32;
        let adjusted = adjust_positions_clone(points, Point::new(x, y as i32));

        let mut res = true;

        for point in adjusted {
            let cell = self.cell_holder.borrow().get_cell_at(point.x as usize, point.y as usize);
            if cell != CellType::None {
                res = false;
            }

            let cell_type = piece_type_to_cell_type(piece.get_type());
            self.cell_holder.borrow_mut().set_cell_at(point.x as usize, point.y as usize, cell_type);
        }

        res
    }

    fn reset_piece(&mut self) {
        let piece = self.curr_piece.as_mut().unwrap();

        // Pieces O and I are fit between cells.
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

impl BoardComponent for PieceMgr {
    fn get_name(&self) -> &'static str {
        "piece_mgr"
    }

    fn reset(&mut self) {
        self.is_hold_used = false;

        self.reset_piece();
    }
}