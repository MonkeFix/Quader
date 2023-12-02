pub struct TimeMgr {
    cur_tick: f64 
}

impl TimeMgr {
    pub fn new() -> Self {
        Self {
            cur_tick: 0.0
        }
    }
    
    pub fn update(&mut self) {
        self.cur_tick += 1.0;
    }
}