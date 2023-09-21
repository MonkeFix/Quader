use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Rect {
    pub x: i32,
    pub y: i32,
    pub width: u32,
    pub height: u32
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Point<T = u32> {
    pub x: T,
    pub y: T
}

impl Rect {
    pub fn top(&self) -> i32 {
        self.y + (self.height as i32)
    }

    pub fn bottom(&self) -> i32 {
        self.y - (self.height as i32)
    }

    pub fn left(&self) -> i32 {
        self.x - (self.width as i32)
    }

    pub fn right(&self) -> i32 {
        self.y + (self.width as i32)
    }
}

