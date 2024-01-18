use std::{collections::HashMap, sync::Arc, time::Instant};

use futures_util::Future;
use quader_engine::{time_mgr::TimeMgr, wall_kick_data::WallKickData, board::Board};
use rand::{thread_rng, RngCore};
use tokio::{select, sync::oneshot};

use crate::ConnId;

#[derive(Debug)]
pub struct BoardManager {
    pub is_started: bool,
    time_mgr: TimeMgr,
    wkd: Arc<WallKickData>,
    seed: u64,
    pub boards: HashMap<ConnId, Board>,
    terminate_tx: Option<oneshot::Sender<()>>,
}

impl BoardManager {
    pub fn new() -> Self {
    
        Self {
            is_started: false,
            time_mgr: TimeMgr::new(),
            wkd: Arc::new(WallKickData::default()),
            seed: thread_rng().next_u64(),
            boards: HashMap::new(),
            terminate_tx: None
        }
    }

    pub async fn run(mut self) {
        let (terminate_tx, terminate_rx) = oneshot::channel();
        self.terminate_tx = Some(terminate_tx);

        self.is_started = true;

        let mut dt = 0.0;
        let update = async {
            let mut prev = Instant::now();
            let mut lag = 0.0;
    
            loop {
                let curr = Instant::now();
                let elapsed = curr - prev;
                prev = curr;
                lag += elapsed.as_secs_f32();
    
                while lag >= MS_PER_UPDATE {
                    dt = lag / MS_PER_UPDATE;
                    lag -= MS_PER_UPDATE;
                }
            }
        };

        select! {
            _ = update => {
                log::info!("update {}", dt);
                self.time_mgr.update(dt);
            }
            _ = terminate_rx => {
                log::info!("terminating board manager");
            }
        }
    }

    pub fn stop(&self) {
        
    }
}

const MS_PER_UPDATE: f32 = 1.0 / 30.0;
