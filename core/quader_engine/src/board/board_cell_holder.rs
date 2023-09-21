use std::ops::Deref;
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

pub trait BoolArray {
    fn to_bool_array(&self) -> [[bool; BOARD_WIDTH]; BOARD_HEIGHT];
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Row([BlockType; BOARD_WIDTH]);

impl Row {
    pub fn new() -> Self {
        Row([BlockType::None; BOARD_WIDTH])
    }

    pub fn set(&mut self, x: u32, block_type: BlockType) {
        self.0[x as usize] = block_type;
    }

    pub fn get(&self, x: u32) -> BlockType {
        self.0[x as usize]
    }

    pub fn is_full(&self) -> bool {
        self.0.iter().all(|&b| b != BlockType::None && b != BlockType::Solid)
    }

    pub fn is_empty(&self) -> bool {
        self.0.iter().all(|&b| b == BlockType::None)
    }

    pub const EMPTY: &'static Self = &Row([BlockType::None; BOARD_WIDTH]);
    pub const SOLID: &'static Self = &Row([BlockType::Solid; BOARD_WIDTH]);
}

// TODO: Fix "Serialize, Deserialize" traits
#[derive(Debug, Copy, Clone)]
pub struct BoardBlockHolder {
    layout: [Row; BOARD_HEIGHT],
    width: u32,
    height: u32
}

impl BoardBlockHolder {
    pub fn get_width(&self) -> u32 {
        self.width
    }
    pub fn get_height(&self) -> u32 {
        self.height
    }

    pub fn new(width: u32, height: u32) -> Self {
        BoardBlockHolder {
            width, height,
            layout: [*Row::EMPTY; BOARD_HEIGHT].into()
        }
    }

    pub fn default() -> Self {
        BoardBlockHolder {
            width: BOARD_WIDTH as u32, height: BOARD_HEIGHT as u32,
            layout: [*Row::EMPTY; BOARD_HEIGHT].into()
        }
    }

    pub fn reset(&mut self) {
        for row in self.layout.iter_mut() {
            *row = *Row::EMPTY;
        }
    }

    pub fn check_row_clears(&self, bounds: Option<&Rect>) -> Vec<u32> {
        let mut rows_cleared = vec![];

        let max = match bounds {
            Some(b) => b.top() as u32,
            None => self.height
        };

        for y in std::cmp::max(max, 0)..self.height {
            if self.is_row_full(y) {
                rows_cleared.push(y);
            }
        }

        return rows_cleared;
    }

    pub fn is_row_full(&self, y: u32) -> bool {
        self.layout[y as usize].is_full()
    }

    pub fn get_block_at(&self, x: u32, y: u32) -> BlockType {
        self.layout[y as usize].get(x)
    }

    pub fn set_block_at(&mut self, x: u32, y: u32, block: BlockType) {
        self.layout[y as usize].set(x, block);
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
            let cur = self.layout[y as usize];

            self.layout[y as usize] = *Row::EMPTY;
            self.layout[(y - 1) as usize] = cur;
        }
    }

    pub fn move_down(&mut self, from_y: u32) {
        for y in (from_y - 1)..=0 {
            let cur = self.layout[y as usize].clone();

            self.layout[y as usize] = *Row::EMPTY;
            self.layout[(y + 1) as usize] = cur;
        }
    }

    pub fn clear_rows(&mut self, ys: &[u32]) {
        for &y in ys {
            self.move_down(y);
        }
    }

    pub fn get_row(&self, y: u32) -> &Row {
        &self.layout[y as usize]
    }

    pub fn get_row_mut(&mut self, y: u32) -> &mut Row {
        &mut self.layout[y as usize]
    }
}

impl BoolArray for BoardBlockHolder {
    fn to_bool_array(&self) -> [[bool; BOARD_WIDTH]; BOARD_HEIGHT] {
        let mut result = [[false; BOARD_WIDTH]; BOARD_HEIGHT];

        for (y, row) in self.layout.iter().enumerate() {
            for (x, bt) in row.0.iter().enumerate() {
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

    fn create_empty_holder() -> BoardBlockHolder {
        BoardBlockHolder::new(BOARD_WIDTH as u32, BOARD_HEIGHT as u32)
    }

    #[test]
    fn row_consts_are_correct() {
        let a = Row::EMPTY;
        format!("{:?}", a);
        format!("{:?}", a);
    }

    #[test]
    fn dimensions_are_correct() {
        let holder = create_empty_holder();

        assert_eq!(BOARD_WIDTH as u32, holder.get_width());
        assert_eq!(BOARD_HEIGHT as u32, holder.get_height());
    }

    #[test]
    fn layout_is_empty_on_creation() {
        let holder = create_empty_holder();

        for row in holder.layout {
            assert_eq!(*Row::EMPTY, row);
        }
    }

    #[test]
    fn resets_correctly() {
        let mut holder = create_empty_holder();

        holder.set_block_at(0, 0, BlockType::I);

        assert_eq!(BlockType::I, holder.get_block_at(0, 0));

        holder.reset();

        assert_eq!(*Row::EMPTY, *holder.get_row(0));
    }
}
