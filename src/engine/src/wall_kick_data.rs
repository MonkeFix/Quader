/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENCE file in the repository root for full licence text.
 */

use std::collections::HashMap;
use std::sync::Mutex;
use serde::{Deserialize, Serialize};
use once_cell::sync::Lazy;
use crate::piece::RotationMove;
use crate::primitives::Point;

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum WallKickDataMode {
    Standard
}

type WK = HashMap<RotationMove, Vec<Point>>;

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum WallKickType {
    Default, PieceI, PieceO
}

#[derive(Debug, Serialize, Deserialize)]
pub struct WallKickData {
    default: WK,
    piece_i: WK,
    piece_o: WK,
}

pub static WALL_KICK_DATA: Lazy<Mutex<WallKickData>> = Lazy::new(|| {
    let wkd = WallKickData::new(WallKickDataMode::Standard);
    Mutex::new(wkd)
});

const WKD_TO_RIGHT: [Point; 5] = [
    Point { x: 0, y: 0 },
    Point { x: -1, y: 0 },
    Point { x: -1, y: 1 },
    Point { x: 0, y: -2 },
    Point { x: -1, y: -2 },
];

const WKD_TO_LEFT: [Point; 5] = [
    Point { x: 0, y: 0 },
    Point { x: 1, y: 0 },
    Point { x: 1, y: 1 },
    Point { x: 0, y: -2 },
    Point { x: 1, y: -2 },
];

const WKD_LEFT_TO_INIT_DEG180: [Point; 5] = [
    Point { x: 0, y: 0 },
    Point { x: -1, y: 0 },
    Point { x: -1, y: -1 },
    Point { x: 0, y: 2 },
    Point { x: -1, y: 2 },
];

impl WallKickData {
    pub fn new(mode: WallKickDataMode) -> Self {

        match mode {
            WallKickDataMode::Standard => {
                let default = HashMap::from([
                    ( RotationMove::InitToRight,   Vec::from(WKD_TO_RIGHT)),
                    ( RotationMove::RightToInit,   vec![Point::new(0, 0), Point::new(1 , 0), Point::new(1, -1),  Point::new(0, 2),  Point::new(1, 2)] ),
                    ( RotationMove::RightToDeg180, vec![Point::new(0, 0), Point::new(1 , 0), Point::new(1, -1),  Point::new(0, 1),  Point::new(0, 2)] ),
                    ( RotationMove::Deg180ToRight, Vec::from(WKD_TO_RIGHT)),
                    ( RotationMove::Deg180ToLeft,  Vec::from(WKD_TO_LEFT)),
                    ( RotationMove::LeftToDeg180,  Vec::from(WKD_LEFT_TO_INIT_DEG180)),
                    ( RotationMove::LeftToInit,    Vec::from(WKD_LEFT_TO_INIT_DEG180)),
                    ( RotationMove::InitToLeft,    Vec::from(WKD_TO_LEFT)),
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
        }
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

impl Default for WallKickData {
    fn default() -> Self {
        WallKickData::new(WallKickDataMode::Standard)
    }
}