use serde::{Deserialize, Serialize};

#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
pub struct TimeMgr {
    pub cur_tick: f64,
    pub cur_sec: f32,
    pub is_enabled: bool
}

impl Default for TimeMgr {
    fn default() -> Self {
        Self {
            cur_tick: 0.0,
            cur_sec: 0.0,
            is_enabled: true
        }
    }
}

impl TimeMgr {
    pub fn new() -> Self {
        Self::default()
    }
    
    pub fn update(&mut self, dt: f32) {
        if !self.is_enabled {
            return;
        }

        self.cur_tick += 1.0;
        self.cur_sec += dt;
    }
    
    pub fn reset(&mut self) {
        self.cur_tick = 0.0;
        self.cur_sec = 0.0;
    }

    pub fn enable(&mut self) {
        self.is_enabled = true;
    }

    pub fn disable(&mut self) {
        self.is_enabled = false;
    }
}