use serde::{Deserialize, Serialize, Serializer};
use crate::primitives::{Point, Rect};

pub const BOARD_WIDTH: usize = 10;
pub const BOARD_HEIGHT: usize = 80;

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum BlockType {
    None,
    I, O, T, L, J, S, Z,
    Garbage,
    Solid,
    Failing
}

// TODO: Fix "Serialize, Deserialize" traits
#[derive(Debug)]
pub struct BoardBlockHolder {
    layout: [[BlockType; BOARD_WIDTH]; BOARD_HEIGHT],
    width: u32,
    height: u32
}

pub trait BoolArray {
    fn to_bool_array(&self) -> [[bool; BOARD_WIDTH]; BOARD_HEIGHT];
}

impl BoardBlockHolder {
    pub fn get_width(&self) -> u32 {
        self.width
    }
    pub fn get_height(&self) -> u32 {
        self.height
    }

    pub fn new(width: u32, height: u32) -> Self {
        let layout: [[BlockType; BOARD_WIDTH]; BOARD_HEIGHT] = [[BlockType::None; BOARD_WIDTH]; BOARD_HEIGHT];

        BoardBlockHolder {
            width, height, layout
        }
    }

    pub fn reset(&mut self) {
        for row in self.layout.iter_mut() {
            for x in row.iter_mut() {
                *x = BlockType::None;
            }
        }
    }

    pub fn check_row_clears(&self, bounds: Option<&Rect>) -> Vec<u32> {
        let mut rows_cleared = vec![];

        let max = match bounds {
            Some(b) => b.top() as u32,
            None => self.height
        };

        for y in std::cmp::max(max, 0)..self.height {
            let y = y as u32;
            if self.is_row_full(y) {
                rows_cleared.push(y);
            }
        }

        return rows_cleared;
    }

    pub fn is_row_full(&self, y: u32) -> bool {
        for x in 0..self.width {
            let b = self.get_block_at(x, y);
            if b == BlockType::None || b == BlockType::Solid {
                return false;
            }
        }

        true
    }

    pub fn get_block_at(&self, x: u32, y: u32) -> BlockType {
        self.layout[y as usize][x as usize]
    }

    pub fn set_block_at(&mut self, x: u32, y: u32, block: BlockType) {
        self.layout[y as usize][x as usize] = block;
    }

    pub fn is_out_of_bounds(&self, x: u32, y: u32) -> bool {
        x < 0 || x >= self.width || y >= self.height || y < 0
    }

    pub fn intersects_any(&self, points: &[Point]) -> bool {
        points
            .iter()
            .any(|p| {
                self.is_out_of_bounds(p.x, p.y) || self.get_block_at(p.x, p.y) != BlockType::None
            })
    }

    pub fn move_up(&mut self) {
        for y in 1..self.height {
            let empty = empty_row();
            let cur = self.layout[y as usize];

            self.layout[y as usize] = empty;
            self.layout[(y - 1) as usize] = cur;
        }
    }

    pub fn move_down(&mut self, from_y: u32) {
        for y in (from_y - 1)..=0 {
            let empty = empty_row();
            let cur = self.layout[y as usize].clone();

            self.layout[y as usize] = empty;
            self.layout[(y + 1) as usize] = cur;
        }
    }

    pub fn clear_rows(&mut self, ys: &[u32]) {
        for &y in ys {
            self.move_down(y);
        }
    }
}

impl BoolArray for BoardBlockHolder {
    fn to_bool_array(&self) -> [[bool; BOARD_WIDTH]; BOARD_HEIGHT] {
        let mut result = [[false; BOARD_WIDTH]; BOARD_HEIGHT];

        for (y, row) in self.layout.iter().enumerate() {
            for (x, bt) in row.iter().enumerate() {
                result[y][x] = *bt != BlockType::None;
            }
        }

        result
    }
}

fn empty_row() -> [BlockType; BOARD_WIDTH] {
    [BlockType::None; BOARD_WIDTH]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dimensions_are_correct() {
        let holder = BoardBlockHolder::new(BOARD_WIDTH as u32, BOARD_HEIGHT as u32);

        assert_eq!(BOARD_WIDTH as u32, holder.get_width());
        assert_eq!(BOARD_HEIGHT as u32, holder.get_height());
    }

    #[test]
    fn layout_is_empty_on_creation() {
        let holder = BoardBlockHolder::new(BOARD_WIDTH as u32, BOARD_HEIGHT as u32);

        for row in holder.layout {
            for x in row {
                assert_eq!(BlockType::None, x);
            }
        }
    }
}
