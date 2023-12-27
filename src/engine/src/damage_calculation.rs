use crate::game_settings::{AttackSettings, BoardSettings};
use crate::primitives::Point;
use crate::replays::MoveResult;
use crate::scoring::{damage_mods, has_flag, thresholds, TSpinStatus};


pub fn calculate_damage(attack_settings: &AttackSettings, mv: &MoveResult) -> u32 {
    let mut attack = attack_settings.lines_0;

    if mv.hard_drop_info.lines_cleared == 0 {
        return attack;
    }

    let mods = mv.mod_bits;

    // COMBOS
    if has_flag(mods, damage_mods::COMBO_1) { attack += attack_settings.combos[0]; }
    if has_flag(mods, damage_mods::COMBO_2) { attack += attack_settings.combos[1]; }
    if has_flag(mods, damage_mods::COMBO_3) { attack += attack_settings.combos[2]; }
    if has_flag(mods, damage_mods::COMBO_4) { attack += attack_settings.combos[3]; }
    if has_flag(mods, damage_mods::COMBO_5) { attack += attack_settings.combos[4]; }

    if has_flag(mods, damage_mods::ALL_CLEAR) { attack += attack_settings.all_clear; }

    // B2Bs
    if has_flag(mods, damage_mods::B2B_1) { attack += attack_settings.b2bs[0]; }
    if has_flag(mods, damage_mods::B2B_2) { attack += attack_settings.b2bs[1]; }
    if has_flag(mods, damage_mods::B2B_3) { attack += attack_settings.b2bs[2]; }
    if has_flag(mods, damage_mods::B2B_4) { attack += attack_settings.b2bs[3]; }
    if has_flag(mods, damage_mods::B2B_5) { attack += attack_settings.b2bs[4]; }

    // T-SPINS
    if has_flag(mods, damage_mods::T_SPIN_FULL) {
        // FULL
        if has_flag(mods, damage_mods::SINGLE) {
            attack += attack_settings.t_spin_single;
        }
        if has_flag(mods, damage_mods::DOUBLE) {
            attack += attack_settings.t_spin_double;
        }
        if has_flag(mods, damage_mods::TRIPLE) {
            attack += attack_settings.t_spin_triple;
        }
    } else if has_flag(mods, damage_mods::T_SPIN_MINI) {
        // MINI
        if has_flag(mods, damage_mods::SINGLE) {
            attack += attack_settings.t_spin_single_mini;
        }
    } else {
        // REGULAR ATTACKS
        if has_flag(mods, damage_mods::SINGLE) {
            attack += attack_settings.lines_1;
        }
        if has_flag(mods, damage_mods::DOUBLE) {
            attack += attack_settings.lines_2;
        }
        if has_flag(mods, damage_mods::TRIPLE) {
            attack += attack_settings.lines_3;
        }
        if has_flag(mods, damage_mods::QUAD) {
            attack += attack_settings.lines_4;
        }
    }

    attack
}

pub fn create_board_move_bits(
    total_cells: u32,
    mv: &MoveResult,
    t_spin_status: TSpinStatus
) -> u32 {

    let mut res = 0;

    if total_cells == 0 {
        res |= damage_mods::ALL_CLEAR;
    }

    if mv.hard_drop_info.lines_cleared == 1 {
        res |= damage_mods::SINGLE;
    } else if mv.hard_drop_info.lines_cleared == 2 {
        res |= damage_mods::DOUBLE;
    } else if mv.hard_drop_info.lines_cleared == 3 {
        res |= damage_mods::TRIPLE;
    } else if mv.hard_drop_info.lines_cleared >= 4 {
        res |= damage_mods::QUAD;
    }

    if mv.combo > thresholds::COMBO_1_THRESHOLD {
        res |= damage_mods::COMBO_1;
    } else if mv.combo >= thresholds::COMBO_2_THRESHOLD {
        res |= damage_mods::COMBO_2;
    } else if mv.combo >= thresholds::COMBO_3_THRESHOLD {
        res |= damage_mods::COMBO_3;
    } else if mv.combo >= thresholds::COMBO_4_THRESHOLD {
        res |= damage_mods::COMBO_4;
    } else if mv.combo >= thresholds::COMBO_5_THRESHOLD {
        res |= damage_mods::COMBO_5;
    }

    match t_spin_status {
        TSpinStatus::None => {},
        TSpinStatus::Full => {
            res |= damage_mods::T_SPIN_FULL;
        },
        TSpinStatus::Mini => {
            res |= damage_mods::T_SPIN_MINI;
        }
    }

    if mv.b2b > thresholds::B2B_1_THRESHOLD {
        res |= damage_mods::B2B_1;
    } else if mv.b2b >= thresholds::B2B_2_THRESHOLD {
        res |= damage_mods::B2B_2;
    } else if mv.b2b >= thresholds::B2B_3_THRESHOLD {
        res |= damage_mods::B2B_3;
    } else if mv.b2b >= thresholds::B2B_4_THRESHOLD {
        res |= damage_mods::B2B_4;
    } else if mv.b2b >= thresholds::B2B_5_THRESHOLD {
        res |= damage_mods::B2B_5;
    }

    res
}

pub fn check_t_overhang<F>(board_settings: &BoardSettings, piece_x: i32, piece_y: i32, not_empty_func: F) -> TSpinStatus
    where F: Fn(Point) -> bool {

    let point_arr = [
        // TOP LEFT
        Point::new(piece_x - 1, piece_y - 1),
        // TOP RIGHT
        Point::new(piece_x + 1, piece_y - 1),
        // BOTTOM LEFT
        Point::new(piece_x - 1, piece_y + 1),
        // BOTTOM RIGHT
        Point::new(piece_x + 1, piece_y + 1)
    ];

    let mut oob_overhangs = 0;
    let mut non_oob_overhangs = 0;

    for p in point_arr {
        if crate::utils::is_oob(p.x, p.y, board_settings.width as i32, board_settings.full_height() as i32) {
            oob_overhangs += 1;
        } else if not_empty_func(p) {
            non_oob_overhangs += 1;
        }
    }

    if oob_overhangs > 0 && non_oob_overhangs > 0 {
        return TSpinStatus::Mini;
    }

    if oob_overhangs == 0 && non_oob_overhangs >= 3 {
        return TSpinStatus::Full;
    }

    TSpinStatus::None
}
