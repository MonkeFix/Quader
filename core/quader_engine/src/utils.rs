use std::ops::{Add, AddAssign};
use crate::cell_holder::CellType;
use crate::piece::PieceType;
use crate::primitives::{Color, Point, Rect};

pub fn adjust_positions<T: AddAssign + Copy>(data: &mut [Point<T>], offset: Point<T>) {
    for p in &mut *data {
        p.x += offset.x;
        p.y += offset.y;
    }
}

pub fn adjust_positions_clone<T: Add + Copy>(data: &[Point<T>], offset: Point<T>) -> Vec<Point<T::Output>> {
    data.iter()
        .map(|p| Point {
            x: p.x + offset.x,
            y: p.y + offset.y
        })
        .collect()
}

pub fn adjust_point<T: AddAssign + Copy>(point: &mut Point<T>, offset: Point<T>) {
    point.x += offset.x;
    point.y += offset.y;
}

pub fn adjust_point_clone<T: Add<Output = T> + Copy>(point: &Point<T>, offset: Point<T>) -> Point<T> {
    Point::new(
        point.x + offset.x,
        point.y + offset.y
    )
}

pub fn piece_type_to_cell_type(piece_type: PieceType) -> CellType {
    match piece_type {
        PieceType::I => CellType::I,
        PieceType::O => CellType::O,
        PieceType::T => CellType::T,
        PieceType::L => CellType::L,
        PieceType::J => CellType::J,
        PieceType::S => CellType::S,
        PieceType::Z => CellType::Z,
        PieceType::Pixel => CellType::Garbage,
    }
}

pub fn cell_to_color(cell: CellType) -> Color {
    match cell {
        CellType::None => *Color::BLACK,
        CellType::I => *Color::PIECE_I,
        CellType::O => *Color::PIECE_O,
        CellType::T => *Color::PIECE_T,
        CellType::L => *Color::PIECE_L,
        CellType::J => *Color::PIECE_J,
        CellType::S => *Color::PIECE_S,
        CellType::Z => *Color::PIECE_Z,
        CellType::Garbage => *Color::PIECE_GARBAGE,
        CellType::Solid => *Color::PIECE_GARBAGE
    }
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

pub fn is_oob<T: PartialOrd<i32> + PartialOrd<T>>(x: T, y: T, width: T, height: T) -> bool {
    x < 0 || x >= width || y >= height || y < 0
}

// Rotates a 3x3 array counter-clockwise
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