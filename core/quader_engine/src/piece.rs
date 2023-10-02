use std::fmt::{Display, Formatter};
use std::rc::{Rc, Weak};
use serde::{Deserialize, Serialize};
use crate::primitives::{Point, Rect, Color};
use crate::board::{CellType};
use crate::wall_kick_data::{WallKickData, WallKickType};

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum PieceType {
    I, O, T, L, J, S, Z, Pixel
}

impl Display for PieceType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            PieceType::I => "I",
            PieceType::O => "O",
            PieceType::T => "T",
            PieceType::L => "L",
            PieceType::J => "J",
            PieceType::S => "S",
            PieceType::Z => "Z",
            PieceType::Pixel => "Pixel",
        };

        write!(f, "{}", str)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum RotationState {
    Initial, Clockwise, CounterClockwise, Deg180
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum RotationDirection {
    Clockwise, CounterClockwise, Deg180
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum RotationMove {
    InitToRight,
    RightToInit,

    RightToDeg180,
    Deg180ToRight,

    Deg180ToLeft,
    LeftToDeg180,

    LeftToInit,
    InitToLeft,

    InitToDeg180,
    Deg180ToInit
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum OffsetType {
    Cell,
    BetweenCells
}

#[derive(Debug)]
pub struct WallKickCheckParams<'a> {
    pub tests: &'a [Point],
    pub expected_pos: &'a [Point]
}

#[derive(Debug)]
pub struct WallKickCheckResult {
    pub is_success: bool,
    pub wall_kick_pos: Option<Point<i32>>
}

#[derive(Debug)]
pub struct Piece {
    piece_type: PieceType,
    board_cell_type: CellType,
    init_pos: Vec<Point>,
    right_pos: Vec<Point>,
    deg180_pos: Vec<Point>,
    left_pos: Vec<Point>,
    offset_type: OffsetType,
    bounds: Rect,
    x: u32,
    y: u32,
    current_rotation: RotationState,
    wall_kick_data: Weak<WallKickData>,
    wall_kick_type: WallKickType
}

pub fn calc_bounds(positions: &[Point], x: i32, y: i32) -> Rect {
    let mut min_x = i32::MAX;
    let mut min_y = i32::MAX;
    let mut max_x = i32::MIN;
    let mut max_y = i32::MIN;

    for &p in positions {
        if p.x < min_x { min_x = p.x; }
        else if p.x > max_x { max_x = p.x; }

        if p.y < min_y { min_y = p.y; }
        else if p.y > max_y { max_y = p.y; }
    }

    let w = 1 + (if min_x == max_x { 0 } else { min_x.abs() + max_x.abs() } );
    let h = 1 + (if min_y == max_y { 0 } else { min_y.abs() + max_y.abs() } );

    assert!(w > 0);
    assert!(h > 0);

    Rect {
        x: x + min_x, y: y + min_y,
        width: w as u32, height: h as u32
    }
}

/// Rotates a 3x3 array counter-clockwise
/*pub fn rotate_array3x3(a: &mut [[u8; 3]]) {
    let n = a.len();
    let mut tmp: u8;

    for i in 0..n/2 {
        for j in i..n - i - 1 {
            tmp = a[i][j];
            a[i][j] = a[j][n - i - 1];
            a[j][n - i - 1] = a[n - i - 1][n - j - 1];
            a[n - i - 1][n - j - 1] = a[n - j - 1][i];
            a[n - j - 1][i] = tmp;
        }
    }
}

pub fn rotate_array<T: Copy>(a: &mut Vec<&mut Vec<T>>) {
    let n = a.len();
    let mut tmp: T;

    for i in 0..n/2 {
        for j in i..n - i - 1 {
            tmp = a[i][j];
            a[i][j] = a[j][n - i - 1];
            a[j][n - i - 1] = a[n - i - 1][n - j - 1];
            a[n - i - 1][n - j - 1] = a[n - j - 1][i];
            a[n - j - 1][i] = tmp;
        }
    }
}*/

impl Piece {
    pub fn new(wall_kick_data: Weak<WallKickData>, piece_type: PieceType) -> Self {
        let wall_kick_type = match piece_type {
            PieceType::I => WallKickType::PieceI,
            PieceType::O => WallKickType::PieceO,
            _ => WallKickType::Default
        };

        let offset_type = match piece_type {
            PieceType::I | PieceType::O => OffsetType::BetweenCells,
            _ => OffsetType::Cell
        };

        let board_cell_type;
        let init_pos;
        let right_pos;
        let deg180_pos;
        let left_pos;

        match piece_type {
            PieceType::I => {
                board_cell_type = CellType::I;
                init_pos =   vec![ Point::new(-1, -1), Point::new(-2, -1), Point::new(1, -1), Point::new(0, -1) ];
                right_pos =  vec![ Point::new(0, -1),  Point::new(0, 0),   Point::new(0, 1),  Point::new(0, -2) ];
                deg180_pos = vec![ Point::new(-1, 0),  Point::new(0, 0),   Point::new(1, 0),  Point::new(-2, 0) ];
                left_pos =   vec![ Point::new(-1, -1), Point::new(-1, -2), Point::new(-1, 1), Point::new(-1, 0) ];
            },
            PieceType::O => {
                board_cell_type = CellType::O;
                init_pos = vec![ Point::new(0, 0), Point::new(-1, 0), Point::new(0, -1), Point::new(-1, -1) ];
                right_pos = vec![ Point::new(0, 0), Point::new(-1, 0), Point::new(0, -1), Point::new(-1, -1) ];
                deg180_pos = vec![ Point::new(0, 0), Point::new(-1, 0), Point::new(0, -1), Point::new(-1, -1) ];
                left_pos = vec![ Point::new(0, 0), Point::new(-1, 0), Point::new(0, -1), Point::new(-1, -1) ];
            },
            PieceType::T => {
                board_cell_type = CellType::T;
                init_pos = vec![Point::new(0, 0),Point::new(-1, 0),Point::new(1, 0),Point::new(0, -1)];
                right_pos = vec![Point::new(0, 0),Point::new(1, 0),Point::new(0, -1),Point::new(0, 1)];
                deg180_pos = vec![Point::new(0, 0),Point::new(-1, 0),Point::new(1, 0),Point::new(0, 1)];
                left_pos = vec![Point::new(0, 0),Point::new(-1, 0),Point::new(0, -1),Point::new(0, 1)];
            },
            PieceType::L => {
                board_cell_type = CellType::L;
                init_pos = vec![ Point::new(0, 0), Point::new(-1, 0), Point::new(1, 0), Point::new(1, -1) ];
                right_pos = vec![ Point::new(0, 0), Point::new(0, -1), Point::new(0, 1), Point::new(1, 1) ];
                deg180_pos = vec![ Point::new(0, 0), Point::new(-1, 0), Point::new(-1, 1), Point::new(1, 0) ];
                left_pos = vec![ Point::new(0, 0), Point::new(0, -1), Point::new(-1, -1), Point::new(0, 1) ];
            },
            PieceType::J => {
                board_cell_type = CellType::J;
                init_pos = vec![ Point::new(0, 0), Point::new(1, 0), Point::new(-1, 0), Point::new(-1, -1) ];
                right_pos = vec![ Point::new(0, 0), Point::new(0, -1), Point::new(1, -1), Point::new(0, 1) ];
                deg180_pos = vec![ Point::new(0, 0), Point::new(-1, 0), Point::new(1, 0), Point::new(1, 1) ];
                left_pos = vec![ Point::new(0, 0), Point::new(0, -1), Point::new(0, 1), Point::new(-1, 1) ];
            },
            PieceType::S => {
                board_cell_type = CellType::S;
                init_pos = vec![ Point::new(0, 0), Point::new(-1, 0), Point::new(0, -1), Point::new(1, -1) ];
                right_pos = vec![ Point::new(0, 0), Point::new(0, -1), Point::new(1, 0), Point::new(1, 1) ];
                deg180_pos = vec![ Point::new(0, 0), Point::new(1, 0), Point::new(0, 1), Point::new(-1, 1) ];
                left_pos = vec![ Point::new(0, 0), Point::new(-1, 0), Point::new(-1, -1), Point::new(0, 1) ];
            },
            PieceType::Z => {
                board_cell_type = CellType::Z;
                init_pos = vec![ Point::new(0,0), Point::new(-1, -1), Point::new(0, -1), Point::new(1, 0)];
                right_pos = vec![ Point::new(0, 0), Point::new(0, 1), Point::new(1, 0), Point::new(1, -1) ];
                deg180_pos = vec![ Point::new(0, 0), Point::new(-1, 0), Point::new(0, 1), Point::new(1, 1) ];
                left_pos = vec![ Point::new(0,0), Point::new(0, -1), Point::new(-1, 0), Point::new(-1, 1) ];
            },
            PieceType::Pixel => {
                board_cell_type = CellType::Garbage;
                init_pos = vec![ Point::new(0, 0) ];
                right_pos = vec![ Point::new(0, 0) ];
                deg180_pos = vec![ Point::new(0, 0) ];
                left_pos = vec![ Point::new(0, 0) ];
            }
        };
        Piece {
            bounds: calc_bounds(&init_pos, 0, 0),
            piece_type,
            board_cell_type,
            init_pos,
            right_pos,
            deg180_pos,
            left_pos,
            offset_type,
            x: 0,
            y: 0,
            current_rotation: RotationState::Initial,
            wall_kick_data,
            wall_kick_type,
        }
    }

    pub fn reset(&mut self) {
        self.current_rotation = RotationState::Initial;
        self.bounds = self.calc_bounds();
    }

    pub fn get_positions(&self) -> &[Point] {
        match self.current_rotation {
            RotationState::Initial => &self.init_pos,
            RotationState::Clockwise => &self.right_pos,
            RotationState::CounterClockwise => &self.left_pos,
            RotationState::Deg180 => &self.deg180_pos
        }
    }

    pub fn get_type(&self) -> &PieceType {
        &self.piece_type
    }

    pub fn set_x(&mut self, x: u32) {
        self.x = x;
        self.bounds = self.calc_bounds();
    }

    pub fn set_y(&mut self, y: u32) {
        self.y = y;
        self.bounds = self.calc_bounds();
    }

    pub fn get_x(&self) -> u32 {
        self.x
    }

    pub fn get_y(&self) -> u32 {
        self.y
    }

    pub fn move_left(&mut self) {
        self.set_x(self.x - 1);
    }

    pub fn move_right(&mut self) {
        self.set_x(self.x + 1);
    }

    pub fn move_down(&mut self) {
        self.set_y(self.y + 1);
    }

    pub fn get_offset_type(&self) -> &OffsetType {
        &self.offset_type
    }

    pub fn get_current_pos(&self) -> &[Point] {
        match self.current_rotation {
            RotationState::Initial => &self.init_pos,
            RotationState::Clockwise => &self.right_pos,
            RotationState::CounterClockwise => &self.left_pos,
            RotationState::Deg180 => &self.deg180_pos
        }
    }

    pub fn get_wall_kick_type(&self) -> &WallKickType {
        &self.wall_kick_type
    }

    pub fn rotate(&mut self, rotation: RotationDirection, x_offset: i32, y_offset: i32) {
        self.rotate_simple(rotation);

        let ix = self.x as i32 + x_offset;
        let iy = self.y as i32 + y_offset;

        self.x = ix as u32;
        self.y = iy as u32;

        self.bounds = self.calc_bounds();
    }

    pub fn get_bounds(&self) -> &Rect {
        &self.bounds
    }

    pub fn get_color(&self) -> Color {
        match self.piece_type {
            PieceType::I => *Color::PIECE_I,
            PieceType::O => *Color::PIECE_O,
            PieceType::T => *Color::PIECE_T,
            PieceType::L => *Color::PIECE_L,
            PieceType::J => *Color::PIECE_J,
            PieceType::S => *Color::PIECE_S,
            PieceType::Z => *Color::PIECE_Z,
            PieceType::Pixel => *Color::PIECE_GARBAGE,
        }
    }

    fn calc_bounds(&self) -> Rect {
        calc_bounds(self.get_current_pos(), self.x as i32, self.y as i32)
    }

    fn rotate_simple(&mut self, rotation: RotationDirection) {
        match rotation {
            RotationDirection::Clockwise => self.rotate_right(),
            RotationDirection::CounterClockwise => self.rotate_left(),
            RotationDirection::Deg180 => { self.rotate_right(); self.rotate_right(); }
        }

        self.bounds = self.calc_bounds();
    }

    fn rotate_right(&mut self) {
        self.current_rotation = match self.current_rotation {
            RotationState::Initial => RotationState::Clockwise,
            RotationState::Clockwise => RotationState::Deg180,
            RotationState::Deg180 => RotationState::CounterClockwise,
            RotationState::CounterClockwise => RotationState::Initial
        }
    }

    fn rotate_left(&mut self) {
        self.current_rotation = match self.current_rotation {
            RotationState::Initial => RotationState::CounterClockwise,
            RotationState::Clockwise => RotationState::Initial,
            RotationState::CounterClockwise => RotationState::Deg180,
            RotationState::Deg180 => RotationState::Clockwise
        }
    }

    pub fn get_rotation_type(&self, rotation: &RotationDirection) -> (RotationMove, &Vec<Point>) {
        match self.current_rotation {
            RotationState::Initial => match rotation {
                RotationDirection::Clockwise => (RotationMove::InitToRight, &self.right_pos),
                RotationDirection::CounterClockwise => (RotationMove::InitToLeft, &self.left_pos),
                RotationDirection::Deg180 => (RotationMove::InitToDeg180, &self.deg180_pos),
            }
            RotationState::Clockwise => match rotation {
                RotationDirection::Clockwise => (RotationMove::RightToDeg180, &self.deg180_pos),
                RotationDirection::CounterClockwise => (RotationMove::RightToInit, &self.init_pos),
                RotationDirection::Deg180 => (RotationMove::Deg180ToLeft, &self.left_pos),
            }
            RotationState::Deg180 => match rotation {
                RotationDirection::Clockwise => (RotationMove::Deg180ToLeft, &self.left_pos),
                RotationDirection::CounterClockwise => (RotationMove::Deg180ToRight, &self.right_pos),
                RotationDirection::Deg180 => (RotationMove::Deg180ToInit, &self.init_pos),
            }
            RotationState::CounterClockwise => match rotation {
                RotationDirection::Clockwise => (RotationMove::LeftToInit, &self.init_pos),
                RotationDirection::CounterClockwise => (RotationMove::LeftToDeg180, &self.deg180_pos),
                RotationDirection::Deg180 => (RotationMove::InitToRight, &self.right_pos),
            }
        }
    }
}

#[cfg(test)]
mod tests {
/*    use crate::piece::rotate_array;

    #[test]
    fn rotate_matrix() {
        let mut r1 = vec![0, 0, 1];
        let mut r2 = vec![1, 1, 1];
        let mut r3 = vec![0, 0, 0];

        let mut piece_j = vec![
            &mut r1, &mut r2, &mut r3
        ];

        let mut r4 = vec![1, 1, 0];
        let mut r5 = vec![0, 1, 0];
        let mut r6 = vec![0, 1, 0];

        let rotated_piece_j = vec![
            &mut r4, &mut r5, &mut r6
        ];

        rotate_array(&mut piece_j);
        assert_eq!(piece_j, rotated_piece_j);
    }*/
}