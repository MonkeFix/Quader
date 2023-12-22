use std::slice::Iter;
use serde::{Deserialize, Serialize};
use crate::board::{BoardComponent};
use crate::game_settings::{BOARD_HEIGHT, BOARD_WIDTH, BoardSettings};
use crate::primitives::{Point, Rect, Color};
use crate::utils::adjust_positions_clone;

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum CellType {
    None,
    I,
    O,
    T,
    L,
    J,
    S,
    Z,
    Garbage,
    Solid
}

pub trait BoolArray {
    fn to_bool_array(&self) -> [[bool; BOARD_WIDTH]; BOARD_HEIGHT];
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Row([CellType; BOARD_WIDTH]);

impl Row {
    pub fn set(&mut self, x: usize, cell_type: CellType) -> CellType {
        let tmp = self.0[x];
        self.0[x] = cell_type;

        tmp
    }

    pub fn get(&self, x: usize) -> CellType {
        self.0[x]
    }

    pub fn is_full(&self) -> bool {
        self.0.iter().all(|&b| b != CellType::None && b != CellType::Solid)
    }

    pub fn is_empty(&self) -> bool {
        self.0.iter().all(|&b| b == CellType::None)
    }

    pub fn get_occupied_cell_count(&self) -> usize {
        self.0.iter().filter(|c| increases_cells(**c)).count()
    }

    pub fn iter(&self) -> Iter<'_, CellType> {
        self.0.iter()
    }

    pub const EMPTY: &'static Self = &Row([CellType::None; BOARD_WIDTH]);
    pub const SOLID: &'static Self = &Row([CellType::Solid; BOARD_WIDTH]);
    pub const GARBAGE: &'static Self = &Row([CellType::Garbage; BOARD_WIDTH]);
}

impl Default for Row {
    fn default() -> Self {
        Row([CellType::None; BOARD_WIDTH])
    }
}

impl<'a> IntoIterator for &'a Row {
    type Item = CellType;
    type IntoIter = RowIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        RowIterator {
            row: self,
            index: 0
        }
    }
}

pub struct RowIterator<'a> {
    row: &'a Row,
    index: usize
}

impl<'a> Iterator for RowIterator<'a> {
    type Item = CellType;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.row.0.len() {
            self.index = 0;
            return None;
        }

        let result = self.row.0[self.index];
        self.index += 1;
        Some(result)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CellHolder {
    layout: Vec<Row>,
    width: usize,
    height: usize,
    occupied_cells: usize
}

pub fn increases_cells(cell_type: CellType) -> bool {
    cell_type != CellType::None && cell_type != CellType::Solid
}

impl Default for CellHolder {
    fn default() -> Self {
        CellHolder {
            width: BOARD_WIDTH, height: BOARD_HEIGHT,
            layout: [*Row::EMPTY; BOARD_HEIGHT].to_vec(),
            occupied_cells: 0
        }
    }
}

impl BoardComponent for CellHolder {
    fn get_name(&self) -> &'static str {
        "cell_holder"
    }
    fn reset(&mut self) {
        self.reset();
    }
}

impl CellHolder {
    pub fn get_width(&self) -> usize {
        self.width
    }
    pub fn get_height(&self) -> usize {
        self.height
    }

    pub fn new(board_settings: &BoardSettings) -> Self {
        // TODO: Add full height (height * 2)
        CellHolder {
            width: board_settings.width, height: board_settings.height,
            layout: [*Row::EMPTY; BOARD_HEIGHT].to_vec(),
            occupied_cells: 0
        }
    }

    pub fn reset(&mut self) {
        for row in self.layout.iter_mut() {
            *row = *Row::EMPTY;
        }

        self.occupied_cells = 0;
    }

    pub fn check_row_clears(&self, bounds: Option<&Rect>) -> Vec<usize> {

        let max = match bounds {
            Some(b) => b.top() as usize,
            None => 0
        };

        (std::cmp::max(max, 0)..self.height)
            .filter(|&y| self.is_row_full(y))
            .collect()
    }

    pub fn is_row_full(&self, y: usize) -> bool {
        self.layout[y].is_full()
    }

    pub fn get_cell_at(&self, x: usize, y: usize) -> CellType {
        self.layout[y].get(x)
    }

    pub fn set_cell_at(&mut self, x: usize, y: usize, cell: CellType) {
        let old = self.layout[y].set(x, cell);

        if increases_cells(cell) && !increases_cells(old) {
            self.occupied_cells += 1;
        }
    }

    pub fn is_out_of_bounds(&self, x: i32, y: i32) -> bool {
        x < 0 || x >= self.width as i32 || y >= self.height as i32 || y < 0
    }

    pub fn intersects(&self, point: &Point) -> bool {
        self.is_out_of_bounds(point.x, point.y)
            || self.get_cell_at(point.x as usize, point.y as usize) != CellType::None
    }

    pub fn intersects_any(&self, points: &[Point]) -> bool {
        points
            .iter()
            .any(|p| {
                self.intersects(p)
            })
    }

    pub fn move_up(&mut self, update_cell_count: bool) {
        (1..self.height)
            .for_each(|y| {
                let cur = self.layout[y];

                self.layout[y] = *Row::EMPTY;
                self.layout[y - 1] = cur;
            });

        if update_cell_count {
            self.occupied_cells += self.width;
        }
    }

    pub fn push_garbage(&mut self, hole_x: u32) {
        self.move_up(false);
        self.set_row(self.height - 1, self.create_garbage_row(hole_x));

        self.occupied_cells += self.width - 1;
    }

    pub fn create_garbage_row(&self, hole_x: u32) -> Row {
        let mut res = *Row::GARBAGE;
        res.set(hole_x as usize, CellType::None);

        res
    }

    pub fn move_down(&mut self, from_y: usize, update_cell_count: bool) {
        (0..=(from_y - 1))
            .rev()
            .for_each(|y| {
                let cur = self.layout[y];

                self.layout[y] = *Row::EMPTY;
                self.layout[y + 1] = cur;
            });
        if update_cell_count {
            self.occupied_cells -= self.width;
        }
    }

    pub fn clear_rows(&mut self, ys: &[usize]) {
        for &y in ys {
            self.move_down(y, true);
        }
    }

    pub fn get_row(&self, y: usize) -> &Row {
        &self.layout[y]
    }

    pub fn get_row_mut(&mut self, y: usize) -> &mut Row {
        &mut self.layout[y]
    }

    pub fn set_row(&mut self, y: usize, row: Row) {
        let old_row = self.layout[y];
        let update_occupied_cells = false;

        self.layout[y] = row;

        if update_occupied_cells {
            let c1 = old_row.get_occupied_cell_count();
            let c2 = row.get_occupied_cell_count();

            let min = std::cmp::min(c1, c2);
            let max = std::cmp::max(c1, c2);

            self.occupied_cells -= max - min;
        }
    }

    pub fn get_layout(&self) -> &[Row] {
        &self.layout
    }

    pub fn get_occupied_cell_count(&self) -> usize {
        self.occupied_cells
    }

    pub fn calc_nearest_y(&self, cur_x: u32, cur_y: u32, points: &[Point]) -> u32 {
        let mut y = cur_y;
        
        for i in cur_y..=(self.height as u32) {
            let offset: Point<i32> = Point::new(cur_x as i32, cur_y as i32);
            let new_points = adjust_positions_clone(points, offset);
            if self.intersects_any(&new_points) {
                break;
            }
            y = i;
        }
        
        y
    }
}

impl BoolArray for CellHolder {
    fn to_bool_array(&self) -> [[bool; BOARD_WIDTH]; BOARD_HEIGHT] {
        let mut result = [[false; BOARD_WIDTH]; BOARD_HEIGHT];

        for (y, row) in self.layout.iter().enumerate() {
            for (x, &bt) in row.0.iter().enumerate() {
                result[y][x] = bt != CellType::None;
            }
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const BOARD_SETTINGS: BoardSettings = BoardSettings {
        width: BOARD_WIDTH,
        height: BOARD_HEIGHT
    };

    fn create_empty_holder() -> CellHolder {
        CellHolder::new(&BOARD_SETTINGS)
    }

    fn decode(ch: &u8) -> CellType {
        match ch {
            b'X' => CellType::Garbage,
            b'I' => CellType::I,
            b'O' => CellType::O,
            b'T' => CellType::T,
            b'L' => CellType::L,
            b'J' => CellType::J,
            b'S' => CellType::S,
            b'Z' => CellType::Z,
            _ => CellType::Solid
        }
    }

    fn str_to_row(s: &str) -> Row {
        assert!(s.len() <= 10);

        let mut row = Row::default();

        for (i, c) in s.as_bytes().iter().enumerate() {
            let _ = row.set(i, decode(c));
        }

        row
    }

    #[test]
    fn row_consts_are_correct() {
        let a = Row::EMPTY;
        format!("{:?}", a);
        format!("{:?}", a);
    }

    #[test]
    fn solid_row_is_solid() {
        let solid1 = *Row::SOLID;
        let mut solid2 = Row::default();
        for x in 0..BOARD_WIDTH {
            solid2.set(x, CellType::Solid);
        }

        assert_eq!(solid1, solid2);

        let mut solid3 = Row::default();
        for x in 0..BOARD_WIDTH {
            solid3.set(x, CellType::Garbage);
        }

        assert!(!solid1.is_full());
        assert!(!solid2.is_full());
        assert!(solid3.is_full());
    }

    #[test]
    fn empty_row_is_empty() {
        let empty1 = *Row::EMPTY;
        let empty2 = Row::default();

        assert!(empty1.is_empty());
        assert!(empty2.is_empty());
        assert_eq!(empty1, empty2);

        let mut empty2 = Row::default();
        empty2.set(0, CellType::Garbage);
        assert_ne!(empty1, empty2);
        assert!(!empty2.is_empty());
    }

    #[test]
    fn row_updates_correctly() {
        let mut row = Row::default();

        row.set(0, CellType::Garbage);
        row.set(1, CellType::I);
        row.set(2, CellType::J);

        assert_eq!(str_to_row("XIJ"),
                   row);

        assert_eq!(row.get(0), CellType::Garbage);
        assert_eq!(row.get(1), CellType::I);
        assert_eq!(row.get(2), CellType::J);
    }

    #[test]
    fn dimensions_are_correct() {
        let holder = create_empty_holder();

        assert_eq!(BOARD_WIDTH, holder.get_width());
        assert_eq!(BOARD_HEIGHT, holder.get_height());
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

        holder.set_cell_at(0, 0, CellType::I);

        assert_eq!(CellType::I, holder.get_cell_at(0, 0));

        holder.reset();

        assert_eq!(*Row::EMPTY, *holder.get_row(0));
    }

    #[test]
    fn checks_row_clears() {
        let mut holder = create_empty_holder();
        let cleared_rows = holder.check_row_clears(None);
        assert_eq!(0, cleared_rows.len());

        let mut full_row = Row::default();
        for c in 0..10 {
            full_row.set(c, CellType::I);
        }
        holder.set_row(1, full_row);

        let cleared_rows = holder.check_row_clears(None);
        assert_eq!(1, cleared_rows.len());
        assert_eq!(1, cleared_rows[0]);

        holder.set_row(79, full_row);

        let cleared_rows = holder.check_row_clears(None);
        assert_eq!(2, cleared_rows.len());
        assert_eq!(1, cleared_rows[0]);
        assert_eq!(79, cleared_rows[1]);
    }
}
