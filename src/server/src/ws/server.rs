/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENSE file in the repository root for full license text.
 */

use crate::{ConnId, Msg, RoomId};
use quader_engine::{board::{Board, self}, wall_kick_data::WallKickData, time_mgr::TimeMgr};
use rand::{thread_rng, Rng as _, RngCore};
use std::{
    collections::{HashMap, HashSet},
    io,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
};
use tokio::sync::{mpsc, oneshot};

use super::handler::WsBoardCommand;

enum Command {
    Connect {
        conn_tx: mpsc::UnboundedSender<Msg>,
        res_tx: oneshot::Sender<ConnId>,
    },
    Disconnect {
        conn: ConnId,
    },
    List {
        res_tx: oneshot::Sender<Vec<RoomId>>,
    },
    Join {
        conn: ConnId,
        room: RoomId,
        res_tx: oneshot::Sender<()>,
    },
    Message {
        msg: Msg,
        conn: ConnId,
        res_tx: oneshot::Sender<()>,
    },
    BoardCommand {
        cmd: WsBoardCommand,
        conn: ConnId,
        res_tx: oneshot::Sender<String>
    }
}

#[derive(Debug)]
pub struct ChatServer {
    sessions: HashMap<ConnId, mpsc::UnboundedSender<Msg>>,
    rooms: HashMap<RoomId, HashSet<ConnId>>,
    visitor_count: Arc<AtomicUsize>,
    cmd_rx: mpsc::UnboundedReceiver<Command>,
    boards: HashMap<ConnId, Board>,
    time_mgr: TimeMgr,
    wkd: Arc<WallKickData>,
    seed: u64
}

impl ChatServer {
    pub fn new() -> (Self, ChatServerHandle) {
        let mut rooms = HashMap::with_capacity(4);

        rooms.insert("main".to_owned(), HashSet::new());

        let (cmd_tx, cmd_rx) = mpsc::unbounded_channel();

        (
            Self {
                sessions: HashMap::new(),
                rooms,
                visitor_count: Arc::new(AtomicUsize::new(0)),
                cmd_rx,
                boards: HashMap::new(),
                time_mgr: TimeMgr::new(),
                wkd: Arc::new(WallKickData::new(quader_engine::wall_kick_data::WallKickDataMode::Standard)),
                seed: thread_rng().next_u64()
            },
            ChatServerHandle { cmd_tx },
        )
    }

    async fn send_system_message(&self, room: &str, skip: ConnId, msg: impl Into<String>) {
        if let Some(sessions) = self.rooms.get(room) {
            let msg = msg.into();

            for conn_id in sessions {
                if *conn_id != skip {
                    if let Some(tx) = self.sessions.get(conn_id) {
                        let _ = tx.send(msg.clone());
                    }
                }
            }
        }
    }

    async fn send_message(&self, conn: ConnId, msg: impl Into<String>) {
        if let Some(room) = self
            .rooms
            .iter()
            .find_map(|(room, participants)| participants.contains(&conn).then_some(room))
        {
            self.send_system_message(room, conn, msg).await;
        };
    }

    async fn connect(&mut self, tx: mpsc::UnboundedSender<Msg>) -> ConnId {
        log::info!("Someone joined");

        self.send_system_message("main", 0, "Someone joined").await;

        let id = thread_rng().gen::<usize>();
        self.sessions.insert(id, tx);

        self.rooms.entry("main".to_owned()).or_default().insert(id);

        let count = self.visitor_count.fetch_add(1, Ordering::SeqCst);
        self.send_system_message("main", 0, format!("Total visitors {count}"))
            .await;

        id
    }

    async fn disconnect(&mut self, conn_id: ConnId) {
        log::info!("Someone disconnected");

        let mut rooms: Vec<String> = vec![];

        if self.sessions.remove(&conn_id).is_some() {
            for (name, sessions) in &mut self.rooms {
                if sessions.remove(&conn_id) {
                    rooms.push(name.to_owned());
                }
            }
        }

        for room in rooms {
            self.send_system_message(&room, 0, "Someone disconnected")
                .await;
        }
    }

    fn list_rooms(&mut self) -> Vec<String> {
        self.rooms.keys().cloned().collect()
    }

    async fn join_room(&mut self, conn_id: ConnId, room: String) {
        let mut rooms = Vec::new();

        for (n, sessions) in &mut self.rooms {
            if sessions.remove(&conn_id) {
                rooms.push(n.to_owned());
            }
        }

        for room in rooms {
            self.send_system_message(&room, 0, "Someone disconnected")
                .await;
        }

        self.rooms.entry(room.clone()).or_default().insert(conn_id);

        self.send_system_message(&room, conn_id, "Someone connected")
            .await;
    }

    async fn exec_board_cmd(&mut self, conn: ConnId, cmd: WsBoardCommand) -> String {
        match cmd {
            WsBoardCommand::Create => {
                let board = Board::new(
                    quader_engine::game_settings::GameSettings::default(), 
                    self.wkd.clone(), 
                    self.seed
                );
                self.boards.insert(conn.clone(), board);
            },
            WsBoardCommand::Destroy(id) => {
                self.boards.remove(&id);
            },
            WsBoardCommand::Move(dir, amount) => {
                let board = self.boards.get_mut(&conn).unwrap();
                board.move_to(dir, amount);
            },
            WsBoardCommand::Rotate(dir) => {
                let board = self.boards.get_mut(&conn).unwrap();
                board.rotate(dir);
            },
            WsBoardCommand::HardDrop => {
                let board = self.boards.get_mut(&conn).unwrap();
                let _res = board.hard_drop();
            },
            WsBoardCommand::SoftDrop(amount) => {
                let board = self.boards.get_mut(&conn).unwrap();
                board.soft_drop(amount);
            },
            WsBoardCommand::SendGarbage(amount, messiness) => {
                let board = self.boards.get_mut(&conn).unwrap();
                board.push_garbage(amount, messiness);
            },
            WsBoardCommand::Attack(amount) => {
                let board = self.boards.get_mut(&conn).unwrap();
                board.attack(amount);
            },
            WsBoardCommand::Update(dt) => {
                let board = self.boards.get_mut(&conn).unwrap();
                self.time_mgr.update(dt);
                board.update(&self.time_mgr);
            },
            WsBoardCommand::HoldPiece => {
                let board = self.boards.get_mut(&conn).unwrap();
                board.try_hold_piece();
            },
        }

        "ok".to_string()
    }

    pub async fn run(mut self) -> io::Result<()> {
        while let Some(cmd) = self.cmd_rx.recv().await {
            match cmd {
                Command::Connect { conn_tx, res_tx } => {
                    let conn_id = self.connect(conn_tx).await;
                    let _ = res_tx.send(conn_id);
                }
                Command::Disconnect { conn } => {
                    self.disconnect(conn).await;
                }
                Command::List { res_tx } => {
                    let _ = res_tx.send(self.list_rooms());
                }
                Command::Join { conn, room, res_tx } => {
                    self.join_room(conn, room).await;
                    let _ = res_tx.send(());
                }
                Command::Message { conn, msg, res_tx } => {
                    self.send_message(conn, msg).await;
                    let _ = res_tx.send(());
                }
                Command::BoardCommand { cmd, conn, res_tx } => {
                    let res = self.exec_board_cmd(conn, cmd).await;
                    let _ = res_tx.send(res);
                },
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ChatServerHandle {
    cmd_tx: mpsc::UnboundedSender<Command>,
}

impl ChatServerHandle {
    pub async fn connect(&self, conn_tx: mpsc::UnboundedSender<String>) -> ConnId {
        let (res_tx, res_rx) = oneshot::channel();

        self.cmd_tx
            .send(Command::Connect { conn_tx, res_tx })
            .unwrap();

        res_rx.await.unwrap()
    }

    pub async fn list_rooms(&self) -> Vec<String> {
        let (res_tx, res_rx) = oneshot::channel();

        self.cmd_tx.send(Command::List { res_tx }).unwrap();

        res_rx.await.unwrap()
    }

    pub async fn join_room(&self, conn: ConnId, room: impl Into<String>) {
        let (res_tx, res_rx) = oneshot::channel();

        self.cmd_tx
            .send(Command::Join {
                conn,
                room: room.into(),
                res_tx,
            })
            .unwrap();

        res_rx.await.unwrap()
    }

    pub async fn send_message(&self, conn: ConnId, msg: impl Into<String>) {
        let (res_tx, res_rx) = oneshot::channel();

        self.cmd_tx
            .send(Command::Message {
                msg: msg.into(),
                conn,
                res_tx,
            })
            .unwrap();

        res_rx.await.unwrap();
    }

    pub fn disconnect(&self, conn: ConnId) {
        self.cmd_tx.send(Command::Disconnect { conn }).unwrap();
    }

    pub async fn on_board_cmd(&self, conn: ConnId, cmd: WsBoardCommand) -> String {
        let (res_tx, res_rx) = oneshot::channel();

        self.cmd_tx
            .send(Command::BoardCommand { 
                cmd, conn, res_tx 
            })
            .unwrap();

        let res = res_rx.await.unwrap();

        res
    }
}
