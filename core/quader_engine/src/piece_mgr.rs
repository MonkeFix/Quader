use crate::board::{BoardComponent, UpdateErrorReason};
use crate::cell_holder::{CellHolder, CellType};
use crate::damage_calculation::create_board_move_bits;
use crate::game_settings::{BOARD_VISIBLE_HEIGHT, GameSettings};
use crate::piece::{OffsetType, Piece, PieceType, RotationDirection, WallKickCheckParams};
use crate::piece_queue::PieceQueue;
use crate::primitives::Point;
use crate::replays::MoveInfo;
use crate::utils::{adjust_positions_clone, piece_type_to_cell_type};
use crate::wall_kick_data::WallKickData;

fn reset_piece(piece: &mut Piece, board_width: usize, board_height: usize) {
    // Pieces O and I are fit between cells.
    match piece.get_offset_type() {
        OffsetType::Cell => piece
            .set_x(board_width as u32 / 2 - 1),
        OffsetType::BetweenCells => piece
            .set_x(((board_width as f32) / 2.0).round() as u32)
    }

    match piece.get_type() {
        PieceType::I => piece.set_y(board_height as u32 / 2 + 1),
        _ => piece.set_y(board_height as u32 / 2)
    };

    piece.reset();
}

pub struct PieceMgr {
    curr_piece: Piece,
    pub cell_holder: Box<CellHolder>,
    board_width: usize,
    board_height: usize,
    hold_piece: Option<PieceType>,
    is_hold_used: bool,
    pub piece_queue: PieceQueue
}

impl PieceMgr {
    pub fn new(game_settings: &GameSettings, seed: u64) -> Self {

        let mut piece_queue = PieceQueue::new(seed);
        let next_piece = piece_queue.next();
        let mut piece = Piece::new(next_piece);
        reset_piece(&mut piece, game_settings.get_board().width, game_settings.get_board().height);

        println!("cur piece: {:?}, queue: {:?}", piece.get_type(), piece_queue.queue);

        Self {
            curr_piece: piece,
            cell_holder: Box::new(CellHolder::new(game_settings.get_board())),
            board_width: game_settings.get_board().width,
            board_height: game_settings.get_board().height,
            hold_piece: None,
            is_hold_used: false,
            piece_queue
        }
    }

    pub fn create_piece(&mut self, piece_type: PieceType) -> &Piece {
        let piece = Piece::new(piece_type);

        self.curr_piece = piece;
        self.reset_cur_piece();

        self.get_piece()
    }

    pub fn set_piece(&mut self, piece: Piece) {
        self.curr_piece = piece;
        self.reset_cur_piece();
    }

    pub fn get_piece(&self) -> &Piece {
        &self.curr_piece
    }

    pub fn get_hold_piece(&self) -> Option<PieceType> {
        self.hold_piece
    }

    /// Holds current piece if possible. If success, returns `Some(&Piece)`, otherwise `None`.
    pub fn hold_piece(&mut self) -> Option<&Piece> {
        // we can hold piece once per turn
        if self.is_hold_used {
            return None;
        }

        self.is_hold_used = true;

        // if we have hold piece, then replace the current piece with the hold one
        // and put the new piece to hold
        return if let Some(piece) = self.hold_piece {
            let curr_piece = self.get_piece();
            self.hold_piece = Some(curr_piece.get_type());

            Some(self.create_piece(piece))
        } else {
            // otherwise put current piece to hold and set a new piece
            self.hold_piece = Some(self.get_piece().get_type());

            let new_piece = self.piece_queue.next();

            Some(self.create_piece(new_piece))
        }
    }

    /// Tries to move the current piece one cell to the left `delta` times.
    /// If it fails, nothing happens, as the piece collides with either board's bounds
    /// or occupied cells.
    pub fn move_left(&mut self, _delta: i32) {
        //for _ in 0..=delta {
            if self.test_movement(-1, 0) {
                self.curr_piece.move_left();
            }
        //}
    }

    /// Tries to move the current piece one cell to the right `delta` times.
    /// If it fails, nothing happens, as the piece collides with either board's bounds
    /// or occupied cells.
    pub fn move_right(&mut self, _delta: i32) {
        //for _ in 0..=delta {
            if self.test_movement(1, 0) {
                self.curr_piece.move_right();
            }
        //}
    }

    /// Rotates the current piece using specified `WallKickData` and specified `RotationDirection`.
    pub fn rotate(&mut self, wkd: &WallKickData, rotation: RotationDirection) {
        let piece = &self.curr_piece;
        let rot_type = piece.get_rotation_type(rotation);
        let tests = &wkd.get(piece.get_wall_kick_type())[&rot_type.0];

        let test = self.test_rotation(WallKickCheckParams {
            tests,
            expected_pos: rot_type.1
        });

        if let Some(point) = test {
            self.curr_piece.rotate(rotation, point.x, point.y);
        }
    }

    /// Returns nearest Y coordinate the piece fits at.
    /// May be useful for rendering ghost piece.
    pub fn find_nearest_y(&self) -> u32 {
        let piece = &self.curr_piece;
        let mut y = piece.get_y();
        let points = piece.get_positions();

        for i in piece.get_y()..=(self.board_height as u32) {
            let offset: Point<i32> = Point::new(piece.get_x() as i32, i as i32);
            let new_points = adjust_positions_clone(points, offset);
            if self.cell_holder.intersects_any(&new_points) {
                break;
            }

            y = i;
        }

        y
    }

    /// Tries to move the current piece one cell down `dt` times.
    /// If it fails, then nothing happens as the piece collides with other cells.
    pub fn soft_drop(&mut self, _dt: u32) {
        //for _ in 0..=dt {
            if self.test_movement(0, 1) {
                self.curr_piece.move_down();
            }
        //}
    }

    /// Tries to hard drop the current piece.
    /// The method checks if the piece could fit in the desired cells.
    /// If it fails, returns Err.
    pub fn hard_drop(&mut self) -> Result<MoveInfo, UpdateErrorReason> {
        let nearest_y = self.find_nearest_y();

        // failed to apply piece as the cells are occupied
        if !self.try_apply_piece(nearest_y) {
            return Err(UpdateErrorReason::CannotApplyPiece);
        }

        let lines_cleared = self.cell_holder.check_row_clears(None);

        // failed to apply piece as it is out of board's bounds
        if nearest_y <= BOARD_VISIBLE_HEIGHT as u32 && lines_cleared.is_empty() {
            return Err(UpdateErrorReason::CannotApplyPiece);
        }

        self.cell_holder.clear_rows(&lines_cleared);

        let lines_cleared = lines_cleared.len() as u32;

        self.reset();

        let next_piece = self.piece_queue.next();
        self.create_piece(next_piece);

        println!("cur piece: {:?}, queue: {:?}", next_piece, self.piece_queue.queue);

        let result = MoveInfo {
            lines_cleared,
            is_success: true,
            ..Default::default()
        };

        Ok(result)
    }

    fn test_movement(&self, x: i32, y: i32) -> bool {

        let piece = &self.curr_piece;
        let b = piece.get_bounds();

        if b.x + x < 0 || b.x + b.width as i32 + x > self.board_width as i32 {
            return false;
        }
        if b.y + b.height as i32 + y > self.board_height as i32 {
            return false;
        }

        let pos = piece.get_positions();
        // casting to a signed integer here as a point could be to the left (-x) or to the top (-y)
        let offset: Point<i32> = Point::new(
            piece.get_x() as i32 + x,
            piece.get_y() as i32 + y
        );
        let new_pos = adjust_positions_clone(pos, offset);

        !self.cell_holder.intersects_any(&new_pos)
    }

    fn test_rotation(&self, kick_params: WallKickCheckParams) -> Option<Point> {
        let tests = kick_params.tests;
        let expected_pos = kick_params.expected_pos;
        let piece = &self.curr_piece;

        for t in tests {
            let test = Point::new(t.x, -t.y);

            let adjusted = adjust_positions_clone(
                expected_pos,
                Point::new(piece.get_x() as i32 + test.x, piece.get_y() as i32 + test.y)
            );

            if !self.cell_holder.intersects_any(&adjusted) {
                return Some(test);
            }
        }

        None
    }

    /// Returns false if the piece couldn't be fit using its current points.
    /// It usually means that the player just lost.
    fn try_apply_piece(&mut self, y: u32) -> bool {
        let piece = &self.curr_piece;
        let points = piece.get_current_pos();
        let x = piece.get_x() as i32;
        let adjusted = adjust_positions_clone(points, Point::new(x, y as i32));

        let mut res = true;

        for point in adjusted {
            let cell = self.cell_holder.get_cell_at(point.x as usize, point.y as usize);
            if cell != CellType::None {
                res = false;
            }

            let cell_type = piece_type_to_cell_type(piece.get_type());
            self.cell_holder.set_cell_at(point.x as usize, point.y as usize, cell_type);
        }

        res
    }

    fn reset_cur_piece(&mut self) {
        reset_piece(&mut self.curr_piece, self.board_width, self.board_height);
    }
}

impl BoardComponent for PieceMgr {
    fn get_name(&self) -> &'static str {
        "piece_mgr"
    }

    fn reset(&mut self) {
        self.is_hold_used = false;

        self.reset_cur_piece();
    }
}