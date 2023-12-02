use crate::game_settings::GravitySettings;
use crate::piece::Piece;

pub struct GravityMgr<'a, F> where F: FnOnce() -> u32 + Copy {
    pub(crate) gravity_settings: &'a GravitySettings,
    pub(crate) piece: Option<&'a Piece>,
    pub(crate) cur_gravity: f32,
    pub(crate) cur_lock: f32,
    pub(crate) intermediate_y: f32,
    pub(crate) y_needs_update: bool,
    pub(crate) calc_y_func: F,
    pub(crate) y_to_check: u32
}

impl<'a, F> GravityMgr<'a, F> where F: FnOnce() -> u32 + Copy {
    pub fn new(gravity_settings: &'a GravitySettings, calc_y_func: F) -> Self {
        let cur_gravity = gravity_settings.grav_base;
        let cur_lock = gravity_settings.lock_delay;

        Self {
            gravity_settings,
            cur_gravity,
            cur_lock,
            piece: None,
            intermediate_y: 0.0,
            y_needs_update: true,
            calc_y_func,
            y_to_check: 0
        }
    }

    pub(crate) fn set_piece(&mut self, piece: &'a Piece) {
        self.piece = Some(piece);
    }

    pub fn update(&mut self, dt: f32) {
        self.intermediate_y += self.cur_gravity * dt;

        if self.y_needs_update {
            // TODO: Maybe replace the F()
            self.y_to_check = (self.calc_y_func)();
            self.y_needs_update = false;
        }

        if self.intermediate_y > 1.0 {
            let diff = (self.intermediate_y - 1.0).max(1.0) as u32;
            for i in 0..diff {
                todo!("soft drop by gravity");
            }

            self.y_needs_update = true;
            self.intermediate_y = 0.0;
        }

        if let Some(p) = &self.piece {
            if self.y_to_check == p.get_y() {
                self.cur_lock -= 1.0 * dt;
            }
        }

        if self.cur_lock <= 0.0 {
            todo!("hard drop by gravity");
        }

        self.cur_gravity += self.gravity_settings.grav_incr * (dt * 10.0);
    }

    pub fn prolong_lock(&mut self) {
        self.cur_lock = (self.cur_lock + self.gravity_settings.lock_prolong_amount)
            .min(self.gravity_settings.lock_delay);
    }
}