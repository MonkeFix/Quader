use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Rect {
    x: i32,
    y: i32,
    width: u32,
    height: u32
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Point<T = u32> {
    pub x: T,
    pub y: T
}

impl Rect {
    pub fn top(&self) -> i32 {
        todo!()
    }

    pub fn bottom(&self) -> i32 {
        todo!()
    }

    pub fn left(&self) -> i32 {
        todo!()
    }

    pub fn right(&self) -> i32 {
        todo!()
    }
}

