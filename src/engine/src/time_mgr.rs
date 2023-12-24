pub struct TimeMgr {
    cur_tick: f64,
    cur_sec: f32
}

impl TimeMgr {
    pub fn new() -> Self {
        Self {
            cur_tick: 0.0,
            cur_sec: 0.0
        }
    }
    
    pub fn update(&mut self, dt: f32) {
        self.cur_tick += 1.0;
        self.cur_sec += dt;
    }
    
    pub fn reset(&mut self) {
        self.cur_tick = 0.0;
        self.cur_sec = 0.0;
    }
}