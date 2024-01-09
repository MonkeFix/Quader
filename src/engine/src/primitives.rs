/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENCE file in the repository root for full licence text.
 */

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Rect {
    pub x: i32,
    pub y: i32,
    pub width: u32,
    pub height: u32
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Point<T = i32> {
    pub x: T,
    pub y: T
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8
}

impl Rect {
    pub fn new(x: i32, y: i32, width: u32, height: u32) -> Self {
        Rect { x, y, width, height }
    }

    pub fn top(&self) -> i32 {
        self.y
    }

    pub fn bottom(&self) -> i32 {
        self.y + (self.height as i32)
    }

    pub fn left(&self) -> i32 {
        self.x
    }

    pub fn right(&self) -> i32 {
        self.x + (self.width as i32)
    }

    pub fn contains(&self, p: &Point) -> bool {
        p.x >= self.left()
            && p.x <= self.right()
            && p.y >= self.top()
            && p.y <= self.bottom()
    }

    pub fn intersects(&self, other: &Rect) -> bool {
        !(other.left() > self.right()
            || other.right() < self.left()
            || other.top() > self.bottom()
            || other.bottom() < self.top())
    }
}

impl<T> Point<T> {
    pub fn new(x: T, y: T) -> Point<T> {
        Point {
            x, y
        }
    }
}

impl Color {
    pub fn new(r: u8, g: u8, b: u8) -> Self {
        Color { r, g, b }
    }

    pub const WHITE: &'static Self = &Color { r: 255, g: 255, b: 255 };
    pub const BLACK: &'static Self = &Color { r: 0, g: 0, b: 0 };
    pub const PIECE_I: &'static Self = &Color { r: 49, g: 178, b: 131 };
    pub const PIECE_Z: &'static Self = &Color { r: 179, g: 51, b: 58 };
    pub const PIECE_S: &'static Self = &Color { r: 129, g: 177, b: 48 };
    pub const PIECE_L: &'static Self = &Color { r: 178, g: 98, b: 49 };
    pub const PIECE_J: &'static Self = &Color { r: 82, g: 57, b: 206 };
    pub const PIECE_T: &'static Self = &Color { r: 165, g: 62, b: 155 };
    pub const PIECE_O: &'static Self = &Color { r: 178, g: 153, b: 49 };
    pub const PIECE_GARBAGE: &'static Self = &Color { r: 102, g: 102, b: 102 };
}

#[cfg(test)]
mod tests {
    use crate::primitives::{Point, Rect};

    #[test]
    fn rectangle_bounds() {
        let rect = Rect {
            x: 10, y: 15,
            width: 16, height: 9
        };

        assert_eq!(rect.left(), 10);
        assert_eq!(rect.top(), 15);
        assert_eq!(rect.right(), 26);
        assert_eq!(rect.bottom(), 24);
    }

    #[test]
    fn rectangle_contains_point() {
        let rect = Rect {
            x: 10, y: 15,
            width: 10, height: 10
        };

        let p1 = Point { x: 10, y: 15 };
        let p2 = Point { x: 15, y: 20 };
        let p3 = Point { x: 5, y: 5 };

        assert!(rect.contains(&p1));
        assert!(rect.contains(&p2));
        assert!(!rect.contains(&p3));
    }

    #[test]
    fn rectangle_intersection() {
        let a = Rect {
            x: 10, y: 10,
            width: 20, height: 20
        };

        let b = Rect {
            x: 20, y: 20,
            width: 30, height: 30
        };

        let c = Rect {
            x: 70, y: 70,
            width: 20, height: 20
        };

        assert!(a.intersects(&b));
        assert!(!a.intersects(&c));
    }
}