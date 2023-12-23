use std::cell::RefCell;
use std::rc::Rc;
use crate::board::{BoardComponent, UpdateErrorReason};
use crate::game_settings::GravitySettings;
use crate::piece_mgr::PieceMgr;

pub struct GravityMgr {
    pub(crate) cur_gravity: f32,
    pub(crate) cur_lock: f32,
    /// Used for handling subcell movement of the current piece.
    pub(crate) intermediate_y: f32,
    pub(crate) y_needs_update: bool,
    pub(crate) y_to_check: u32,

    gravity_settings: GravitySettings,
    piece_mgr: Rc<RefCell<PieceMgr>>
}

impl GravityMgr {
    pub fn new(gravity_settings: &GravitySettings,
               piece_mgr: Rc<RefCell<PieceMgr>>) -> Self {
        let cur_gravity = gravity_settings.grav_base;
        let cur_lock = gravity_settings.lock_delay;

        Self {
            cur_gravity,
            cur_lock,
            intermediate_y: 0.0,
            y_needs_update: true,
            y_to_check: 0,
            gravity_settings: *gravity_settings,
            piece_mgr
        }
    }

    pub fn prolong_lock(&mut self) {
        self.cur_lock = (self.cur_lock + self.gravity_settings.lock_prolong_amount)
            .min(self.gravity_settings.lock_delay);
    }
}

impl BoardComponent for GravityMgr {
    fn get_name(&self) -> &'static str {
        "gravity_mgr"
    }

    fn reset(&mut self) {
        self.intermediate_y = 0.0;
        self.y_needs_update = true;
        self.y_to_check = 0;

        self.cur_gravity = self.gravity_settings.grav_base;
        self.cur_lock = self.gravity_settings.lock_delay;
    }

    fn update(&mut self, dt: f32) -> Option<Result<u32, UpdateErrorReason>> {
        let mut res = None;

        self.intermediate_y += self.cur_gravity * dt;

        let mut piece_mgr = self.piece_mgr.borrow_mut();
        if self.y_needs_update {
            self.y_to_check = piece_mgr.find_nearest_y();
            self.y_needs_update = false;
        }

        if self.intermediate_y >= 1.0 {
            //let diff = (self.intermediate_y - 1.0).max(1.0) as u32;
            //for _ in 0..=diff {
                piece_mgr.soft_drop(1);
            //}

            self.y_needs_update = true;
            self.intermediate_y = 0.0;
        }

        if let Some(p) = &piece_mgr.get_piece() {
            if self.y_to_check == p.get_y() {
                self.cur_lock -= 1.0 * dt;
            }
        }

        if self.cur_lock <= 0.0 {
            res = Some(piece_mgr.hard_drop());
            self.cur_lock = self.gravity_settings.lock_delay;
            self.y_needs_update = true;
            self.intermediate_y = 0.0;
        }

        self.cur_gravity += self.gravity_settings.grav_incr * dt;

        res
    }
}