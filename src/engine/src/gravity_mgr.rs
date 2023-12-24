use crate::game_settings::{GameSettings, GravitySettings};
use crate::piece_mgr::PieceMgr;

#[derive(Debug)]
pub struct GravityMgr {
    pub(crate) cur_gravity: f32,
    pub(crate) cur_lock: f32,
    /// Used for handling subcell movement of the current piece.
    pub(crate) intermediate_y: f32,
    pub(crate) y_needs_update: bool,
    pub(crate) y_to_check: u32,

    gravity_settings: GravitySettings,
    pub piece_mgr: Box<PieceMgr>,
    pub is_enabled: bool
}

impl GravityMgr {
    pub fn new(game_settings: &GameSettings, seed: u64) -> Self {
        let gravity_settings = game_settings.gravity;
        let cur_gravity = gravity_settings.grav_base;
        let cur_lock = gravity_settings.lock_delay;

        Self {
            cur_gravity,
            cur_lock,
            intermediate_y: 0.0,
            y_needs_update: true,
            y_to_check: 0,
            gravity_settings,
            piece_mgr: Box::new(PieceMgr::new(game_settings, seed)),
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

    pub fn update(&mut self, dt: f32) -> bool {
        let mut res = false;

        self.intermediate_y += self.cur_gravity * dt;

        if self.y_needs_update {
            self.y_to_check = self.piece_mgr.find_nearest_y();
            self.y_needs_update = false;
        }

        if self.intermediate_y >= 1.0 {
            // gravity may drop pieces faster than 1 cell per tick, so we're handling that here
            let diff = (self.intermediate_y - 1.0).max(1.0) as u32;
            for _ in 0..diff {
                self.piece_mgr.soft_drop(1);
            }

            self.y_needs_update = true;
            self.intermediate_y = 0.0;
        }

        // If current piece "touches" any occupied cell, we decrease the lock
        if self.y_to_check == self.piece_mgr.get_piece().get_y() {
            self.cur_lock -= 1.0 * dt;
        }

        // If lock is zero we force hard drop the piece
        if self.cur_lock <= 0.0 {
            res = true;
            self.cur_lock = self.gravity_settings.lock_delay;
            self.y_needs_update = true;
            self.intermediate_y = 0.0;
        }

        self.cur_gravity += self.gravity_settings.grav_incr * dt;

        res
    }

    pub fn enable(&mut self) {
        if self.is_enabled {
            return;
        }

        self.is_enabled = true;
        self.piece_mgr.enable();
    }

    pub fn disable(&mut self) {
        if !self.is_enabled {
            return;
        }

        self.is_enabled = false;
        self.piece_mgr.disable();
    }
}
