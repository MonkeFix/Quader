use std::{
    collections::HashMap,
    sync::Arc,
    time::{Duration, Instant},
};

use quader_engine::{board::Board, time_mgr::TimeMgr, wall_kick_data::WallKickData};
use rand::{thread_rng, RngCore};
use tokio::{
    pin, select,
    sync::{mpsc, oneshot},
};

use crate::ConnId;

const MS_PER_UPDATE: f32 = 1.0 / 30.0;

#[derive(Debug)]
pub struct WsBoardMgr {
    pub is_started: bool,
    time_mgr: TimeMgr,
    wkd: Arc<WallKickData>,
    seed: u64,
    boards: HashMap<ConnId, Board>,
    terminate_tx: Option<oneshot::Sender<()>>,
    cmd_rx: mpsc::UnboundedReceiver<()>,
}

impl WsBoardMgr {
    pub fn new() -> (Self, WsBoardMgrHandle) {
        let (cmd_tx, cmd_rx) = mpsc::unbounded_channel();

        (
            Self {
                is_started: false,
                time_mgr: TimeMgr::new(),
                wkd: Arc::new(WallKickData::default()),
                seed: thread_rng().next_u64(),
                boards: HashMap::new(),
                terminate_tx: None,
                cmd_rx,
            },
            WsBoardMgrHandle { cmd_tx },
        )
    }

    pub async fn run(mut self) {
        let (terminate_tx, terminate_rx) = oneshot::channel();
        self.terminate_tx = Some(terminate_tx);
        let mut interval = tokio::time::interval(Duration::from_millis(333));

        self.is_started = true;

        let mut prev = Instant::now();

        loop {
            let tick = interval.tick();
            pin!(tick);

            let now = Instant::now();
            let dt = now - prev;
            prev = now;

            select! {
                /* _ = update => {

                }
                _ = terminate_rx => {
                    log::info!("terminating board manager");
                } */
                _ = tick => {
                    //log::debug!("TICK, dt: {:?}", dt.as_secs_f32());
                    self.time_mgr.update(dt.as_secs_f32());
                }
            }
        }
    }

    fn update_boards(&mut self) {
        for (conn, board) in &mut self.boards {
            match board.update(&self.time_mgr) {
                Some(Err(err)) => {
                    log::info!("conn {conn}: board is dead. reason: {err:?}");
                }
                _ => {}
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct WsBoardMgrHandle {
    cmd_tx: mpsc::UnboundedSender<()>,
}

impl WsBoardMgrHandle {
    pub async fn start(&self) {
        let (res_tx, res_rx) = oneshot::channel();

        self.cmd_tx.send(()).unwrap();

        res_rx.await.unwrap()
    }
}
