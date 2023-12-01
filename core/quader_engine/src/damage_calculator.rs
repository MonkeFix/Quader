use crate::primitives::Point;

enum TSpinType {
    None, Full, Mini
}

pub const COMBO_1_THRESHOLD: u32 = 1;
pub const COMBO_2_THRESHOLD: u32 = 6;
pub const COMBO_3_THRESHOLD: u32 = 10;
pub const COMBO_4_THRESHOLD: u32 = 15;
pub const COMBO_5_THRESHOLD: u32 = 18;


pub const B2B_1_THRESHOLD: u32 = 0;
pub const B2B_2_THRESHOLD: u32 = 5;
pub const B2B_3_THRESHOLD: u32 = 10;
pub const B2B_4_THRESHOLD: u32 = 30;
pub const B2B_5_THRESHOLD: u32 = 60;

pub mod damage_mods {
    pub const NONE: u32 = 0;

    pub const T_SPIN_MINI: u32 = 1 << 1;
    pub const T_SPIN_FULL: u32 = 1 << 2;

    pub const SINGLE: u32 = 1 << 3;
    pub const DOUBLE: u32 = 1 << 4;
    pub const TRIPLE: u32 = 1 << 5;
    pub const QUAD: u32 = 1 << 6;

    pub const ALL_CLEAR: u32 = 1 << 7;

    pub const B2B_1: u32 = 1 << 8;
    pub const B2B_2: u32 = 1 << 9;
    pub const B2B_3: u32 = 1 << 10;
    pub const B2B_4: u32 = 1 << 11;
    pub const B2B_5: u32 = 1 << 12;

    pub const COMBO_1: u32 = 1 << 13;
    pub const COMBO_2: u32 = 1 << 14;
    pub const COMBO_3: u32 = 1 << 15;
    pub const COMBO_4: u32 = 1 << 16;
    pub const COMBO_5: u32 = 1 << 17;
}

fn has_flag(value: u32, flag: u32) -> bool {
    value & flag != 0
}

pub struct DamageCalculator {

}

impl DamageCalculator {
    pub fn new() -> Self { Self {} }

    pub fn calculate(&self) {

    }

    fn create_board_move<F>(
        &self,
        total_cells: u32,
        lines_cleared: u32,
        b2b: &mut u32,
        combo: &mut u32,
        piece_x: i32,
        piece_y: i32,
        width: usize,
        height: usize,
        not_empty_func: F
    ) -> u32 where F: Fn(i32, i32) -> bool {

        let mut res = 0;
        let mut break_b2b = true;

        if total_cells == 0 {
            res |= damage_mods::ALL_CLEAR;
        }

        if lines_cleared == 1 {
            res |= damage_mods::SINGLE;
        } else if lines_cleared == 2 {
            res |= damage_mods::DOUBLE;
        } else if lines_cleared == 3 {
            res |= damage_mods::TRIPLE;
        } else if lines_cleared >= 4 {
            res |= damage_mods::QUAD;
            *b2b += 1;
            break_b2b = false;
        }

        if *combo > COMBO_1_THRESHOLD {
            res |= damage_mods::COMBO_1;
        } else if *combo >= COMBO_2_THRESHOLD {
            res |= damage_mods::COMBO_2;
        } else if *combo >= COMBO_3_THRESHOLD {
            res |= damage_mods::COMBO_3;
        } else if *combo >= COMBO_4_THRESHOLD {
            res |= damage_mods::COMBO_4;
        } else if *combo >= COMBO_5_THRESHOLD {
            res |= damage_mods::COMBO_5;
        }

        match self.check_t_overhang(piece_x, piece_y, width, height, not_empty_func) {
            TSpinType::None => {},
            TSpinType::Full => {
                res |= damage_mods::T_SPIN_FULL;
                break_b2b = false;
                *b2b += 1;
            },
            TSpinType::Mini => {
                res |= damage_mods::T_SPIN_MINI;
                break_b2b = false;
                *b2b += 1;
            }
        }

        if lines_cleared > 0 && break_b2b {
            *b2b = 0;
        }

        if *b2b > B2B_1_THRESHOLD {
            res |= damage_mods::B2B_1;
        } else if *b2b >= B2B_2_THRESHOLD {
            res |= damage_mods::B2B_2;
        } else if *b2b >= B2B_3_THRESHOLD {
            res |= damage_mods::B2B_3;
        } else if *b2b >= B2B_4_THRESHOLD {
            res |= damage_mods::B2B_4;
        } else if *b2b >= B2B_5_THRESHOLD {
            res |= damage_mods::B2B_5;
        }

        if lines_cleared == 0 {
            *combo = 0;
        }

        res
    }

    fn check_t_overhang<F>(&self, piece_x: i32, piece_y: i32, width: usize, height: usize, not_empty_func: F) -> TSpinType
        where F: Fn(i32, i32) -> bool {

        let top_left = Point::new(piece_x - 1, piece_y - 1);
        let top_right = Point::new(piece_x + 1, piece_y - 1);
        let bottom_left = Point::new(piece_x - 1, piece_y + 1);
        let bottom_right = Point::new(piece_x + 1, piece_y + 1);

        let point_arr = [top_left, top_right, bottom_left, bottom_right];

        let mut oob_overhangs = 0;
        let mut non_oob_overhangs = 0;

        for p in point_arr {
            if self.is_oob(p, width, height) {
                oob_overhangs += 1;
            } else if not_empty_func(p.x, p.y) {
                non_oob_overhangs += 1;
            }
        }

        if oob_overhangs > 0 && non_oob_overhangs > 0 {
            return TSpinType::Mini;
        }

        if oob_overhangs == 0 && non_oob_overhangs >= 3 {
            return TSpinType::Full;
        }

        TSpinType::None
    }

    fn is_oob(&self, p: Point<i32>, width: usize, height: usize) -> bool {
        p.x < 0 || p.x >= width as i32 || p.y >= height as i32 || p.y < 0
    }

}
