use std::{collections::HashMap, sync::Arc, time::Instant};

use futures_util::Future;
use quader_engine::{time_mgr::TimeMgr, wall_kick_data::WallKickData, board::Board};
use rand::{thread_rng, RngCore};
use tokio::select;

use crate::ConnId;

#[derive(Debug)]
pub struct BoardManager {
    pub is_started: bool,
    time_mgr: TimeMgr,
    wkd: Arc<WallKickData>,
    seed: u64,
    pub boards: HashMap<ConnId, Board>
}

impl BoardManager {
    pub fn new() -> Self {
        Self {
            is_started: false,
            time_mgr: TimeMgr::new(),
            wkd: Arc::new(WallKickData::default()),
            seed: thread_rng().next_u64(),
            boards: HashMap::new()
        }
    }

    pub async fn run(&mut self) {
        self.is_started = true;

        select! {
            dt = game_tick() => {
                self.time_mgr.update(0.0);
            }
        }
    }

    
}

const MS_PER_UPDATE: f32 = 1.0 / 30.0;

async fn game_tick() -> impl Future {
    
    let update = async move {
        let mut prev = Instant::now();
        let mut dt;
        let mut lag = 0.0;

        loop {
            let curr = Instant::now();
            let elapsed = curr - prev;
            prev = curr;
            lag += elapsed.as_secs_f32();

            while lag >= MS_PER_UPDATE {
                dt = elapsed.as_secs_f32();
                lag -= MS_PER_UPDATE;
            }
        }
    };

    update
}