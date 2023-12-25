use crate::game_settings::{GravitySettings};
use crate::piece_mgr::PieceMgr;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum GravityUpdateResult {
    None,
    /// Amount
    SoftDrop(u32),
    HardDrop
}

#[derive(Debug)]
pub struct GravityMgr {
    pub(crate) cur_gravity: f32,
    pub(crate) cur_lock: f32,
    /// Used for handling subcell movement of the current piece.
    pub(crate) intermediate_y: f32,
    pub(crate) y_needs_update: bool,
    pub(crate) y_to_check: u32,

    gravity_settings: GravitySettings,
    pub is_enabled: bool
}

impl GravityMgr {
    pub fn new(gravity_settings: &GravitySettings) -> Self {
        let cur_gravity = gravity_settings.grav_base;
        let cur_lock = gravity_settings.lock_delay;

        Self {
            cur_gravity,
            cur_lock,
            intermediate_y: 0.0,
            y_needs_update: true,
            y_to_check: 0,
            gravity_settings: *gravity_settings,
            is_enabled: true
        }
    }

    pub fn prolong_lock(&mut self) {
        self.cur_lock = (self.cur_lock + self.gravity_settings.lock_prolong_amount)
            .min(self.gravity_settings.lock_delay);
    }

    pub fn reset_lock(&mut self) {
        self.cur_lock = self.gravity_settings.lock_delay;
    }

    pub fn reset(&mut self) {
        self.intermediate_y = 0.0;
        self.y_needs_update = true;
        self.y_to_check = 0;

        self.cur_gravity = self.gravity_settings.grav_base;
        self.cur_lock = self.gravity_settings.lock_delay;
    }

    pub fn update(&mut self, piece_mgr: &PieceMgr, dt: f32) -> GravityUpdateResult {
        let mut res = GravityUpdateResult::None;

        self.intermediate_y += self.cur_gravity * dt;

        if self.y_needs_update {
            self.y_to_check = piece_mgr.find_nearest_y();
            self.y_needs_update = false;
        }

        if self.intermediate_y >= 1.0 {
            // gravity may drop pieces faster than 1 cell per tick, so we're handling that here
            let diff = (self.intermediate_y - 1.0).max(1.0) as u32;
            res = GravityUpdateResult::SoftDrop(diff);

            self.y_needs_update = true;
            self.intermediate_y = 0.0;
        }

        // If current piece "touches" any occupied cell, we decrease the lock
        if self.y_to_check == piece_mgr.get_piece().get_y() {
            self.cur_lock -= 1.0 * dt;
        }

        // If lock is zero we force hard drop the piece
        if self.cur_lock <= 0.0 {
            res = GravityUpdateResult::HardDrop;
            self.cur_lock = self.gravity_settings.lock_delay;
            self.y_needs_update = true;
            self.intermediate_y = 0.0;
        }

        self.cur_gravity += self.gravity_settings.grav_incr * dt;

        res
    }

    pub fn enable(&mut self) {
        self.is_enabled = true;
    }

    pub fn disable(&mut self) {
        self.is_enabled = false;
    }
}