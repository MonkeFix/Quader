use quader_engine::prelude::*;

use crate::{
    QAttackSettings, QBoardError, QBoardSettings, QGarbageHardDropResult, QGravitySettings, QHardDropInfo, QIncomingDamage, QLastMoveType, QMoveAction, QMoveQueueElem, QMoveResult, QPieceType, QRotationDirection, QRotationState, QTSpinStatus, QWallKickMode
};

pub fn convert_gravity_settings(gravity_settings: &QGravitySettings) -> GravitySettings {
    GravitySettings {
        grav_const: gravity_settings.grav_const,
        grav_base: gravity_settings.grav_base,
        grav_incr: gravity_settings.grav_incr,
        lock_delay: gravity_settings.lock_delay,
        lock_prolong_amount: gravity_settings.lock_prolong_amount,
    }
}

pub fn convert_board_settings(board_settings: &QBoardSettings) -> BoardSettings {
    BoardSettings {
        width: board_settings.width as usize,
        height: board_settings.height as usize,
    }
}

pub fn convert_attack_settings(attack_settings: &QAttackSettings) -> AttackSettings {
    AttackSettings {
        lines_0: attack_settings.lines_0,
        lines_1: attack_settings.lines_1,
        lines_2: attack_settings.lines_2,
        lines_3: attack_settings.lines_3,
        lines_4: attack_settings.lines_4,
        t_spin_single: attack_settings.t_spin_single,
        t_spin_double: attack_settings.t_spin_double,
        t_spin_triple: attack_settings.t_spin_triple,
        t_spin_single_mini: attack_settings.t_spin_single_mini,
        all_clear: attack_settings.all_clear,
        b2bs: attack_settings.b2bs,
        combos: attack_settings.combos,
        garbage_delay_ms: attack_settings.garbage_delay_ms,
    }
}

pub fn convert_wall_kick_mode(wall_kick_mode: &QWallKickMode) -> WallKickMode {
    match wall_kick_mode {
        QWallKickMode::Q_STANDARD => WallKickMode::Standard,
    }
}

pub fn convert_rotation_state(state: &RotationState) -> QRotationState {
    match state {
        RotationState::Initial => QRotationState::Q_INITIAL,
        RotationState::Clockwise => QRotationState::Q_CLOCKWISE,
        RotationState::Deg180 => QRotationState::Q_DEG180,
        RotationState::CounterClockwise => QRotationState::Q_COUNTER_CLOCKWISE,
    }
}

pub fn convert_rotation_direction(direction: &QRotationDirection) -> RotationDirection {
    match direction {
        QRotationDirection::Q_ROT_CLOCKWISE => RotationDirection::Clockwise,
        QRotationDirection::Q_ROT_COUNTER_CLOCKWISE => RotationDirection::CounterClockwise,
        QRotationDirection::Q_ROT_DEG180 => RotationDirection::Deg180,
    }
}

pub fn convert_piece_type(piece_type: &PieceType) -> QPieceType {
    match piece_type {
        PieceType::I => QPieceType::Q_I,
        PieceType::O => QPieceType::Q_O,
        PieceType::T => QPieceType::Q_T,
        PieceType::L => QPieceType::Q_L,
        PieceType::J => QPieceType::Q_J,
        PieceType::S => QPieceType::Q_S,
        PieceType::Z => QPieceType::Q_Z,
        PieceType::Pixel => QPieceType::Q_Z,
    }
}

pub fn convert_board_result(err: BoardErrorReason) -> QBoardError {
    match err {
        BoardErrorReason::CannotApplyPiece => QBoardError::Q_CANNOT_APPLY_PIECE,
        BoardErrorReason::BoardDead => QBoardError::Q_BOARD_DEAD,
        BoardErrorReason::BoardDisabled => QBoardError::Q_BOARD_DISABLED,
        BoardErrorReason::CannotSpawnPiece => QBoardError::Q_CANNOT_SPAWN_PIECE,
    }
}

pub fn convert_move_action(act: &MoveAction) -> QMoveAction {
    match act {
        MoveAction::MoveLeft => QMoveAction::Q_MOVE_LEFT,
        MoveAction::MoveRight => QMoveAction::Q_MOVE_RIGHT,
        MoveAction::RotateCW => QMoveAction::Q_ROTATE_CW,
        MoveAction::RotateCCW => QMoveAction::Q_ROTATE_CCW,
        MoveAction::RotateDeg180 => QMoveAction::Q_ROTATE_DEG180,
        MoveAction::SoftDrop => QMoveAction::Q_SOFT_DROP,
        MoveAction::HardDrop => QMoveAction::Q_HARD_DROP,
        MoveAction::HoldPiece => QMoveAction::Q_HOLD_PIECE,
    }
}

pub unsafe fn convert_garbage_result(res: &GarbageHardDropResult) -> QGarbageHardDropResult {
    let converted = res
        .in_damage_queue
        .iter()
        .map(|&d| QIncomingDamage {
            amount: d.amount,
            delay: d.delay,
            hole_x: d.hole_x,
        })
        .collect::<Vec<QIncomingDamage>>();

    let len = converted.len();
    let raw = converted.as_ptr();

    QGarbageHardDropResult {
        damage_queue_len: len,
        damage_queue: raw,
        out_damage: res.out_damage,
    }
}

pub fn convert_tspin_status(tspin: &TSpinStatus) -> QTSpinStatus {
    match tspin {
        TSpinStatus::None => QTSpinStatus::Q_TSPIN_NONE,
        TSpinStatus::Full => QTSpinStatus::Q_TSPIN_FULL,
        TSpinStatus::Mini => QTSpinStatus::Q_TSPIN_MINI,
    }
}

pub fn convert_move_type(t: &LastMoveType) -> QLastMoveType {
    match t {
        LastMoveType::None => QLastMoveType::Q_MOVE_TYPE_NONE,
        LastMoveType::Rotation => QLastMoveType::Q_MOVE_TYPE_ROTATION,
        LastMoveType::Movement => QLastMoveType::Q_MOVE_TYPE_MOVEMENT,
    }
}

pub fn convert_hard_drop_info(hdi: &HardDropInfo) -> QHardDropInfo {
    QHardDropInfo {
        lines_cleared: hdi.lines_cleared,
        tspin_status: convert_tspin_status(&hdi.tspin_status),
        last_move_type: convert_move_type(&hdi.last_move_type),
        occupied_cells_left: hdi.occupied_cells_left,
    }
}

pub unsafe fn convert_move_result(move_result: &MoveResult) -> QMoveResult {
    let converted = move_result
        .move_queue
        .iter()
        .map(|&d| QMoveQueueElem {
            timestamp: d.0,
            move_action: convert_move_action(&d.1),
        })
        .collect::<Vec<QMoveQueueElem>>();

    let move_queue_len = converted.len();
    let move_queue_ptr = converted.as_ptr();

    QMoveResult {
        timestamp: move_result.timestamp,
        mod_bits: move_result.mod_bits,
        b2b: move_result.b2b,
        combo: move_result.combo,
        is_success: move_result.is_success,
        attack: unsafe { convert_garbage_result(&move_result.attack) },
        hard_drop_info: convert_hard_drop_info(&move_result.hard_drop_info),
        move_queue_len,
        move_queue: move_queue_ptr,
        error: QBoardError::Q_NONE,
    }
}
