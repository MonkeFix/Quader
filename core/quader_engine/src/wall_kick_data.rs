use std::collections::HashMap;
use serde::{Deserialize, Serialize};
use crate::piece::RotationMove;
use crate::primitives::Point;

type WK = HashMap<RotationMove, Vec<Point>>;

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum WallKickType {
    Default, PieceI, PieceO
}

#[derive(Debug)]
pub struct WallKickData {
    default: WK,
    piece_i: WK,
    piece_o: WK,
}

impl WallKickData {
    fn new() -> Self {

        let default = HashMap::from([
            ( RotationMove::InitToRight,   vec![Point::new(0, 0), Point::new(-1, 0), Point::new(-1, 1),  Point::new(0, -2), Point::new(-1, -2) ] ),
            ( RotationMove::RightToInit,   vec![Point::new(0, 0), Point::new(1 , 0), Point::new(1, -1),  Point::new(0, 2),  Point::new(1, 2)] ),
            ( RotationMove::RightToDeg180, vec![Point::new(0, 0), Point::new(1 , 0), Point::new(1, -1),  Point::new(0, 1),  Point::new(0, 2)] ),
            ( RotationMove::Deg180ToRight, vec![Point::new(0, 0), Point::new(-1, 0), Point::new(-1, 1),  Point::new(0, -2), Point::new(-1, -2)] ),
            ( RotationMove::Deg180ToLeft,  vec![Point::new(0, 0), Point::new(1 , 0), Point::new(1, 1),   Point::new(0, -2), Point::new(1, -2)] ),
            ( RotationMove::LeftToDeg180,  vec![Point::new(0, 0), Point::new(-1, 0), Point::new(-1, -1), Point::new(0, 2),  Point::new(-1, 2)] ),
            ( RotationMove::LeftToInit,    vec![Point::new(0, 0), Point::new(-1, 0), Point::new(-1, -1), Point::new(0, 2),  Point::new(-1, 2)] ),
            ( RotationMove::InitToLeft,    vec![Point::new(0, 0), Point::new(1 , 0), Point::new(1, 1),   Point::new(0, -2), Point::new(1, -2)] ),
            ( RotationMove::InitToDeg180,  vec![Point::new(0, 0), Point::new(0 , 1), Point::new(0, 2)] ),
            ( RotationMove::Deg180ToInit,  vec![Point::new(0, 0), Point::new(0 ,-1), Point::new(0, -2)] ),
        ]);

        let piece_i = HashMap::from([
            ( RotationMove::InitToRight,  vec![ Point::new(0, 0), Point::new(-2, 0), Point::new(1, 0),  Point::new(-2, -1), Point::new(1, 2) ] ),
            ( RotationMove::RightToInit,  vec![ Point::new(0, 0), Point::new(2, 0),  Point::new(-1, 0), Point::new(2, 1),   Point::new(-1, -2) ] ),
            ( RotationMove::RightToDeg180, vec![ Point::new(0, 0), Point::new(-1, 0), Point::new(2, 0),  Point::new(-1, 2),  Point::new(2, -1) ] ),
            ( RotationMove::Deg180ToRight, vec![ Point::new(0, 0), Point::new(1, 0),  Point::new(-2, 0), Point::new(1, -2),  Point::new(-2, 1) ] ),
            ( RotationMove::Deg180ToLeft,  vec![ Point::new(0, 0), Point::new(2, 0),  Point::new(-1, 0), Point::new(2, 1),   Point::new(-1, -2) ] ),
            ( RotationMove::LeftToDeg180,  vec![ Point::new(0, 0), Point::new(-2, 0), Point::new(1, 0),  Point::new(-2, -1), Point::new(1, 2) ] ),
            ( RotationMove::LeftToInit,   vec![ Point::new(0, 0), Point::new(1, 0),  Point::new(-2, 0), Point::new(1, -2),  Point::new(-2, 1) ] ),
            ( RotationMove::InitToLeft,   vec![ Point::new(0, 0), Point::new(-1, 0), Point::new(2, 0),  Point::new(-1, 2),  Point::new(2, -1) ] ),
            ( RotationMove::InitToDeg180, vec![ Point::new(0, 0), Point::new(0, 1),  Point::new(0, 2) ] ),
            ( RotationMove::Deg180ToInit, vec![ Point::new(0, 0), Point::new(0, -1), Point::new(0, -2) ] ),
        ]);

        let piece_o = HashMap::from([
            ( RotationMove::InitToRight,  vec![ Point::new(0, 0) ] ),
            ( RotationMove::RightToInit,  vec![ Point::new(0, 0) ] ),
            ( RotationMove::RightToDeg180, vec![ Point::new(0, 0) ] ),
            ( RotationMove::Deg180ToRight, vec![ Point::new(0, 0) ] ),
            ( RotationMove::Deg180ToLeft,  vec![ Point::new(0, 0) ] ),
            ( RotationMove::LeftToDeg180,  vec![ Point::new(0, 0) ] ),
            ( RotationMove::LeftToInit,   vec![ Point::new(0, 0) ] ),
            ( RotationMove::InitToLeft,   vec![ Point::new(0, 0) ] ),
            ( RotationMove::InitToDeg180, vec![ Point::new(0, 0) ] ),
            ( RotationMove::Deg180ToInit, vec![ Point::new(0, 0) ] ),
        ]);

        WallKickData { default, piece_i, piece_o }
    }

    pub fn get(&self, t: &WallKickType) -> &WK {
        match t {
            WallKickType::Default => self.get_default(),
            WallKickType::PieceI => self.get_piece_i(),
            WallKickType::PieceO => self.get_piece_o()
        }
    }

    pub fn get_default(&self) -> &WK {
        &self.default
    }

    pub fn get_piece_i(&self) -> &WK {
        &self.piece_i
    }

    pub fn get_piece_o(&self) -> &WK {
        &self.piece_o
    }
}
