use serde::{Deserialize, Serialize};

#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
pub struct TimeMgr {
    pub elapsed_tick: u64,
    pub elapsed_sec: f32,
    pub is_enabled: bool,
    pub last_dt: f32
}

impl Default for TimeMgr {
    fn default() -> Self {
        Self {
            elapsed_tick: 0,
            elapsed_sec: 0.0,
            last_dt: 0.0,
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

        self.last_dt = dt;
        self.elapsed_tick += 1;
        self.elapsed_sec += dt;
    }
    
    pub fn reset(&mut self) {
        self.elapsed_tick = 0;
        self.elapsed_sec = 0.0;
        self.last_dt = 0.0;
    }

    pub fn enable(&mut self) {
        self.is_enabled = true;
    }

    pub fn disable(&mut self) {
        self.is_enabled = false;
    }
}