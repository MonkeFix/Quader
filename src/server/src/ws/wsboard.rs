use std::{collections::HashMap, sync::Arc, time::Instant};

use quader_engine::{time_mgr::TimeMgr, wall_kick_data::WallKickData, board::Board};
use rand::{thread_rng, RngCore};
use tokio::{select, sync::{oneshot, mpsc}};

use crate::ConnId;


const MS_PER_UPDATE: f32 = 1.0 / 30.0;

#[derive(Debug)]
pub struct WsBoardMgr {
    pub is_started: bool,
    time_mgr: TimeMgr,
    wkd: Arc<WallKickData>,
    seed: u64,
    pub boards: HashMap<ConnId, Board>,
    terminate_tx: Option<oneshot::Sender<()>>,
    cmd_rx: mpsc::UnboundedReceiver<()>
}

impl WsBoardMgr {
    pub fn new() -> (Self, WsBoardMgrHandle) {
    
        let (cmd_tx, cmd_rx) = mpsc::unbounded_channel();

        (Self {
            is_started: false,
            time_mgr: TimeMgr::new(),
            wkd: Arc::new(WallKickData::default()),
            seed: thread_rng().next_u64(),
            boards: HashMap::new(),
            terminate_tx: None,
            cmd_rx
        }, WsBoardMgrHandle { cmd_tx })
    }

    pub async fn run(mut self) {
        let (terminate_tx, terminate_rx) = oneshot::channel();
        self.terminate_tx = Some(terminate_tx);

        self.is_started = true;
        

        let update = async {
            let mut prev = Instant::now();
            let mut lag = 0.0;
    
            loop {
                let curr = Instant::now();
                let elapsed = curr - prev;
                prev = curr;
                lag += elapsed.as_secs_f32();
    
                // TODO: process input
    
                while lag >= MS_PER_UPDATE {
                    self.time_mgr.update(lag);
                    //TODO: 
                    self.update_boards();
                    //log::info!("update, lag: {lag}, elapsed: {}", elapsed_sec);
                    lag -= MS_PER_UPDATE;
                }
            }
        };

        select! {
            _ = update => {

            }
            _ = terminate_rx => {
                log::info!("terminating board manager");
            }
        }
    }

    fn update_boards(&mut self) {
        for (conn, board) in &mut self.boards {

            match board.update(&self.time_mgr) {
                Some(Err(err)) => {
                    log::info!("conn {conn}: board is dead. reason: {err:?}");
                },
                _ => {},
            }
        }
    }
}

pub struct WsBoardMgrHandle {
    cmd_tx: mpsc::UnboundedSender<()>
}

impl WsBoardMgrHandle {
    pub async fn start(&self) {
        let (res_tx, res_rx) = oneshot::channel();

        self.cmd_tx
            .send(())
            .unwrap();

        res_rx.await.unwrap()
    }
}