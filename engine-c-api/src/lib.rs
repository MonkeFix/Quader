#![allow(dead_code)]
use std::sync::Arc;

use crate::converters::*;
use quader_engine::{
    board::Board,
    prelude::{GameSettings, MoveResult, WallKickData},
    time::TimeMgr,
};

mod converters;

#[repr(C)]
struct QGravitySettings {
    grav_const: f32,
    grav_base: f32,
    grav_incr: f32,
    lock_delay: f32,
    lock_prolong_amount: f32,
}

#[repr(C)]
struct QBoardSettings {
    width: u32,
    height: u32,
}

#[repr(C)]
struct QAttackSettings {
    lines_0: u32,
    lines_1: u32,
    lines_2: u32,
    lines_3: u32,
    lines_4: u32,
    t_spin_single: u32,
    t_spin_double: u32,
    t_spin_triple: u32,
    t_spin_single_mini: u32,
    all_clear: u32,
    b2bs: [u32; 5],
    combos: [u32; 5],
    garbage_delay_ms: u32,
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
#[allow(non_camel_case_types)]
enum QWallKickMode {
    Q_STANDARD = 0,
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
#[allow(non_camel_case_types)]
enum QRotationState {
    Q_INITIAL = 0,
    Q_CLOCKWISE = 1,
    Q_DEG180 = 2,
    Q_COUNTER_CLOCKWISE = 3,
}

#[repr(C)]
struct QRotationResult {
    rotated_successfully: bool,
    current_state: QRotationState,
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
#[allow(non_camel_case_types)]
enum QRotationDirection {
    Q_ROT_CLOCKWISE = 0,
    Q_ROT_COUNTER_CLOCKWISE = 1,
    Q_ROT_DEG180 = 2,
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
#[allow(non_camel_case_types)]
enum QPieceType {
    Q_NONE,
    Q_I,
    Q_O,
    Q_T,
    Q_L,
    Q_J,
    Q_S,
    Q_Z,
    Q_PIXEL,
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
#[allow(non_camel_case_types)]
enum QBoardError {
    Q_NONE = 0,
    Q_CANNOT_APPLY_PIECE = 1,
    Q_BOARD_DEAD = 2,
    Q_BOARD_DISABLED = 3,
    Q_CANNOT_SPAWN_PIECE = 4,
}

#[repr(C)]
struct QHoldResult {
    current_piece_type: QPieceType,
    held_successfully: bool,
    board_error: QBoardError,
}

#[repr(C)]
struct QIncomingDamage {
    amount: i32,
    delay: u32,
    hole_x: u32,
}

#[repr(C)]
struct QGarbageHardDropResult {
    damage_queue_len: usize,
    damage_queue: *const QIncomingDamage,
    out_damage: i32,
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
#[allow(non_camel_case_types)]
enum QMoveAction {
    Q_MOVE_LEFT,
    Q_MOVE_RIGHT,
    Q_ROTATE_CW,
    Q_ROTATE_CCW,
    Q_ROTATE_DEG180,
    Q_SOFT_DROP,
    Q_HARD_DROP,
    Q_HOLD_PIECE,
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
#[allow(non_camel_case_types)]
enum QTSpinStatus {
    Q_TSPIN_NONE,
    Q_TSPIN_FULL,
    Q_TSPIN_MINI,
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
#[allow(non_camel_case_types)]
enum QLastMoveType {
    Q_MOVE_TYPE_NONE,
    Q_MOVE_TYPE_ROTATION,
    Q_MOVE_TYPE_MOVEMENT,
}

#[repr(C)]
struct QHardDropInfo {
    lines_cleared: u32,
    tspin_status: QTSpinStatus,
    last_move_type: QLastMoveType,
    occupied_cells_left: u32,
}

#[repr(C)]
struct QMoveQueueElem {
    timestamp: f32,
    move_action: QMoveAction,
}

#[repr(C)]
struct QMoveResult {
    timestamp: f32,
    mod_bits: u32,
    b2b: u32,
    combo: u32,
    is_success: bool,
    attack: QGarbageHardDropResult,
    hard_drop_info: QHardDropInfo,
    move_queue_len: usize,
    move_queue: *const QMoveQueueElem,
    error: QBoardError,
}

#[unsafe(no_mangle)]
unsafe extern "C" fn q_board_create(
    gravity_settings: &QGravitySettings,
    board_settings: &QBoardSettings,
    attack_settings: &QAttackSettings,
    wall_kick_mode: &QWallKickMode,
    seed: u64,
) -> *mut Board {
    let wkd = Arc::new(WallKickData::new(convert_wall_kick_mode(wall_kick_mode)));
    let board = Board::new(
        GameSettings {
            gravity: convert_gravity_settings(gravity_settings),
            board: convert_board_settings(board_settings),
            attack: convert_attack_settings(attack_settings),
            wall_kick_mode: convert_wall_kick_mode(wall_kick_mode),
        },
        wkd,
        seed,
    );

    Box::into_raw(Box::new(board))
}

#[unsafe(no_mangle)]
unsafe extern "C" fn q_board_create_default(seed: u64) -> *mut Board {
    let board = Board::new(
        GameSettings::default(),
        Arc::new(WallKickData::default()),
        seed,
    );

    Box::into_raw(Box::new(board))
}

#[unsafe(no_mangle)]
extern "C" fn q_board_destroy(board: *mut Board) {
    unsafe {
        drop(Box::from_raw(board));
    }
}

#[unsafe(no_mangle)]
unsafe extern "C" fn q_board_update(board: &mut Board, time_mgr: &TimeMgr) {
    board.update(time_mgr);
}

#[unsafe(no_mangle)]
unsafe extern "C" fn q_board_move_left(board: &mut Board, delta: u32) -> u32 {
    board.move_left(delta)
}

#[unsafe(no_mangle)]
unsafe extern "C" fn q_board_move_right(board: &mut Board, delta: u32) -> u32 {
    board.move_right(delta)
}

#[unsafe(no_mangle)]
unsafe extern "C" fn q_board_rotate(
    board: &mut Board,
    direction: &QRotationDirection,
) -> QRotationResult {
    let res = board.rotate(convert_rotation_direction(direction));

    QRotationResult {
        rotated_successfully: res.is_some(),
        current_state: convert_rotation_state(
            res.as_ref().unwrap_or(&board.cur_piece().current_rotation),
        ),
    }
}

#[unsafe(no_mangle)]
unsafe extern "C" fn q_board_try_hold(board: &mut Board) -> QHoldResult {
    let mut current_piece_type = convert_piece_type(&board.cur_piece().get_type());
    let res = board.try_hold_piece();

    let board_error = if let Some(r) = res {
        match r {
            Ok(data) => {
                current_piece_type = convert_piece_type(&data.get_type());
                QBoardError::Q_NONE
            }
            Err(err) => convert_board_result(err),
        }
    } else {
        QBoardError::Q_NONE
    };

    QHoldResult {
        current_piece_type,
        held_successfully: res.is_some(),
        board_error,
    }
}

#[unsafe(no_mangle)]
unsafe extern "C" fn q_board_get_hold_piece(board: &mut Board) -> QHoldResult {
    let res = board.get_hold_piece();

    let current_piece_type = if let Some(p) = res {
        convert_piece_type(&p)
    } else {
        QPieceType::Q_NONE
    };

    QHoldResult {
        current_piece_type,
        held_successfully: res.is_some(),
        board_error: QBoardError::Q_NONE,
    }
}

#[unsafe(no_mangle)]
unsafe extern "C" fn q_board_hard_drop(board: &mut Board) -> QMoveResult {
    let hd = board.hard_drop();
    let mut q_move_result;

    match hd {
        Ok(move_res) => {
            q_move_result = unsafe { convert_move_result(&move_res) };
        }
        Err(err) => {
            q_move_result = unsafe { convert_move_result(&MoveResult::default()) };
            q_move_result.is_success = false;
            q_move_result.error = convert_board_result(err);
        }
    };

    q_move_result
}

#[unsafe(no_mangle)]
unsafe extern "C" fn q_board_soft_drop(board: &mut Board, delta: u32) -> u32 {
    board.soft_drop(delta)
}

#[unsafe(no_mangle)]
unsafe extern "C" fn q_board_push_garbage(board: &mut Board, amount: u32, messiness: u32) {
    board.push_garbage(amount, messiness);
}

#[unsafe(no_mangle)]
unsafe extern "C" fn q_board_attack(board: &mut Board, damage: i32) {
    board.attack(damage);
}

#[unsafe(no_mangle)]
unsafe extern "C" fn q_board_find_nearest_y(board: &mut Board) -> u32 {
    board.find_nearest_y()
}

#[unsafe(no_mangle)]
unsafe extern "C" fn q_board_reset(board: &mut Board) {
    board.reset(None);
}

#[unsafe(no_mangle)]
unsafe extern "C" fn q_board_reset_with_seed(board: &mut Board, seed: u64) {
    board.reset(Some(seed));
}

#[unsafe(no_mangle)]
unsafe extern "C" fn q_board_enable(board: &mut Board) {
    board.enable();
}

#[unsafe(no_mangle)]
unsafe extern "C" fn q_board_disable(board: &mut Board) {
    board.disable();
}

#[unsafe(no_mangle)]
unsafe extern "C" fn q_board_is_enabled(board: &mut Board) -> bool {
    board.is_enabled()
}

#[unsafe(no_mangle)]
unsafe extern "C" fn q_board_is_dead(board: &mut Board) -> bool {
    board.is_dead()
}
