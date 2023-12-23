use std::fmt::{Display, Formatter};
use serde::{Deserialize, Serialize};
use crate::cell_holder::CellType;
use crate::piece_points;
use crate::primitives::{Point, Rect, Color};
use crate::utils::calc_bounds;
use crate::wall_kick_data::{WallKickType};

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

#[derive(Debug, Clone, Copy, Eq, PartialEq, Serialize, Deserialize)]
pub enum RotationState {
    Initial = 0, Clockwise = 1, Deg180 = 2, CounterClockwise = 3
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Serialize, Deserialize)]
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

fn get_points_for_piece(piece_type: PieceType, state: RotationState) -> &'static [Point] {
    match piece_type {
        PieceType::I => {
            match state {
                RotationState::Initial => &piece_points::piece_i::INIT_POS,
                RotationState::Clockwise => &piece_points::piece_i::RIGHT_POS,
                RotationState::CounterClockwise => &piece_points::piece_i::LEFT_POS,
                RotationState::Deg180 => &piece_points::piece_i::DEG180_POS,
            }
        },
        PieceType::O => {
            match state {
                RotationState::Initial => &piece_points::piece_o::INIT_POS,
                RotationState::Clockwise => &piece_points::piece_o::RIGHT_POS,
                RotationState::CounterClockwise => &piece_points::piece_o::LEFT_POS,
                RotationState::Deg180 => &piece_points::piece_o::DEG180_POS,
            }
        },
        PieceType::T =>
            match state {
                RotationState::Initial => &piece_points::piece_t::INIT_POS,
                RotationState::Clockwise => &piece_points::piece_t::RIGHT_POS,
                RotationState::CounterClockwise => &piece_points::piece_t::LEFT_POS,
                RotationState::Deg180 => &piece_points::piece_t::DEG180_POS,
            },
        PieceType::L => {
            match state {
                RotationState::Initial => &piece_points::piece_l::INIT_POS,
                RotationState::Clockwise => &piece_points::piece_l::RIGHT_POS,
                RotationState::CounterClockwise => &piece_points::piece_l::LEFT_POS,
                RotationState::Deg180 => &piece_points::piece_l::DEG180_POS,
            }
        },
        PieceType::J => {
            match state {
                RotationState::Initial => &piece_points::piece_j::INIT_POS,
                RotationState::Clockwise => &piece_points::piece_j::RIGHT_POS,
                RotationState::CounterClockwise => &piece_points::piece_j::LEFT_POS,
                RotationState::Deg180 => &piece_points::piece_j::DEG180_POS,
            }
        },
        PieceType::S => {
            match state {
                RotationState::Initial => &piece_points::piece_s::INIT_POS,
                RotationState::Clockwise => &piece_points::piece_s::RIGHT_POS,
                RotationState::CounterClockwise => &piece_points::piece_s::LEFT_POS,
                RotationState::Deg180 => &piece_points::piece_s::DEG180_POS,
            }
        },
        PieceType::Z => {
            match state {
                RotationState::Initial => &piece_points::piece_z::INIT_POS,
                RotationState::Clockwise => &piece_points::piece_z::RIGHT_POS,
                RotationState::CounterClockwise => &piece_points::piece_z::LEFT_POS,
                RotationState::Deg180 => &piece_points::piece_z::DEG180_POS,
            }
        },
        _ => panic!("invalid piece type")
    }
}

#[derive(Debug)]
pub struct WallKickCheckParams<'a> {
    pub tests: &'a [Point],
    pub expected_pos: &'a [Point]
}

#[derive(Debug)]
pub struct Piece {
    piece_type: PieceType,
    board_cell_type: CellType,
    offset_type: OffsetType,
    bounds: Rect,
    x: u32,
    y: u32,
    current_rotation: RotationState,
    wall_kick_type: WallKickType
}

impl Piece {
    pub fn new(piece_type: PieceType) -> Self {
        let wall_kick_type = match piece_type {
            PieceType::I => WallKickType::PieceI,
            PieceType::O => WallKickType::PieceO,
            _ => WallKickType::Default
        };

        let offset_type = match piece_type {
            PieceType::I | PieceType::O => OffsetType::BetweenCells,
            _ => OffsetType::Cell
        };

        let board_cell_type = crate::utils::piece_type_to_cell_type(piece_type);
        let init_pos = get_points_for_piece(piece_type, RotationState::Initial);

        Piece {
            bounds: calc_bounds(init_pos, 0, 0),
            piece_type,
            board_cell_type,
            offset_type,
            x: 0,
            y: 0,
            current_rotation: RotationState::Initial,
            wall_kick_type,
        }
    }

    pub fn reset(&mut self) {
        self.current_rotation = RotationState::Initial;
        self.bounds = self.calc_bounds();
    }

    pub fn get_positions(&self) -> &[Point] {
        get_points_for_piece(self.piece_type, self.current_rotation)
    }

    pub fn get_type(&self) -> PieceType {
        self.piece_type
    }

    pub fn get_cell_type(&self) -> CellType { self.board_cell_type }

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

    pub fn get_offset_type(&self) -> OffsetType {
        self.offset_type
    }

    pub fn get_current_pos(&self) -> &[Point] {
        get_points_for_piece(self.piece_type, self.current_rotation)
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

    pub fn get_bounds(&self) -> Rect {
        self.bounds
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

    pub fn get_rotation_type(&self, rotation: RotationDirection) -> (RotationMove, &[Point]) {
        match self.current_rotation {
            RotationState::Initial => match rotation {
                RotationDirection::Clockwise => (RotationMove::InitToRight, get_points_for_piece(self.piece_type, RotationState::Clockwise)),
                RotationDirection::CounterClockwise => (RotationMove::InitToLeft, get_points_for_piece(self.piece_type, RotationState::CounterClockwise)),
                RotationDirection::Deg180 => (RotationMove::InitToDeg180, get_points_for_piece(self.piece_type, RotationState::Deg180)),
            }
            RotationState::Clockwise => match rotation {
                RotationDirection::Clockwise => (RotationMove::RightToDeg180, get_points_for_piece(self.piece_type, RotationState::Deg180)),
                RotationDirection::CounterClockwise => (RotationMove::RightToInit, get_points_for_piece(self.piece_type, RotationState::Initial)),
                RotationDirection::Deg180 => (RotationMove::Deg180ToLeft, get_points_for_piece(self.piece_type, RotationState::CounterClockwise)),
            }
            RotationState::Deg180 => match rotation {
                RotationDirection::Clockwise => (RotationMove::Deg180ToLeft, get_points_for_piece(self.piece_type, RotationState::CounterClockwise)),
                RotationDirection::CounterClockwise => (RotationMove::Deg180ToRight, get_points_for_piece(self.piece_type, RotationState::Clockwise)),
                RotationDirection::Deg180 => (RotationMove::Deg180ToInit, get_points_for_piece(self.piece_type, RotationState::Initial)),
            }
            RotationState::CounterClockwise => match rotation {
                RotationDirection::Clockwise => (RotationMove::LeftToInit, get_points_for_piece(self.piece_type, RotationState::Initial)),
                RotationDirection::CounterClockwise => (RotationMove::LeftToDeg180, get_points_for_piece(self.piece_type, RotationState::Deg180)),
                RotationDirection::Deg180 => (RotationMove::InitToRight, get_points_for_piece(self.piece_type, RotationState::Clockwise)),
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