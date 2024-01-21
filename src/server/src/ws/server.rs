/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENSE file in the repository root for full license text.
 */

use crate::{
    lobbies::{Lobby, LobbyContainer, LobbySettings, LobbyListing},
    ConnId, Msg, RoomId, LobbyUuid, LobbyName, auth::UserInfo,
};
use quader_engine::{
    board::{self, Board},
    time_mgr::TimeMgr,
    wall_kick_data::WallKickData,
};
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

use super::{handler::WsBoardCommand, wsboard::WsBoardMgr};

#[derive(Debug)]
struct Session {
    pub conn: ConnId,
    pub user_info: UserInfo,
    pub conn_tx: mpsc::UnboundedSender<Msg>
}

enum Command {
    Connect {
        conn_tx: mpsc::UnboundedSender<Msg>,
        user_info: UserInfo,
        res_tx: oneshot::Sender<ConnId>,
    },
    Disconnect {
        conn: ConnId,
    },
    List {
        res_tx: oneshot::Sender<Vec<RoomId>>,
    },
    ListLobbies {
        res_tx: oneshot::Sender<Vec<LobbyListing>>,
    },
    Join {
        conn: ConnId,
        room: RoomId,
        res_tx: oneshot::Sender<()>,
    },
    JoinLobby {
        conn: ConnId,
        lobby_id: String,
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
        res_tx: oneshot::Sender<String>,
    },
    StartMatch {
        conn: ConnId,
        res_tx: oneshot::Sender<()>,
    },
}

/// Basic architecture:
/// ```
/// ┌─────────────────────────────────┐
/// │             SERVER              │
/// └─────────────────────────────────┘
///                  ▲
///                  │
/// ┌─────────────────────────────────┐
/// │         LobbyContainer          │
/// └─────────────────────────────────┘
///      ▲           ▲           ▲
///      │           │           │
/// ┌────┴────┐ ┌────┴────┐ ┌────┴────┐
/// │ Lobby1  │ │ Lobby2  │ │ LobbyN  │
/// └─────────┘ └─────────┘ └─────────┘
///      ▲           ▲           ▲
///      │           │           │
/// ┌────┴────┐ ┌────┴────┐ ┌────┴────┐
/// │BoardMgr1│ │BoardMgr2│ │BoardMgrN│
/// └─────────┘ └─────────┘ └─────────┘
///      ▲
///      │◄────────┐◄───────┐
///      │         │        │
///  ┌───┴───┐ ┌───┴───┐ ┌──┴────┐
///  │Board_1│ │Board_2│ │Board_N│
///  └───────┘ └───────┘ └───────┘
/// ```
#[derive(Debug)]
pub struct ChatServer {
    sessions: HashMap<ConnId, Session>,
    rooms: HashMap<RoomId, HashSet<ConnId>>,
    lobby_container: LobbyContainer,
    visitor_count: Arc<AtomicUsize>,
    cmd_rx: mpsc::UnboundedReceiver<Command>,
}

impl ChatServer {
    pub fn new() -> (Self, ChatServerHandle) {
        let mut rooms = HashMap::with_capacity(4);

        rooms.insert("main".to_owned(), HashSet::new());

        let mut lobby_container = LobbyContainer::new();
        seed_lobbies(&mut lobby_container);

        let (cmd_tx, cmd_rx) = mpsc::unbounded_channel();

        (
            Self {
                sessions: HashMap::new(),
                rooms,
                lobby_container,
                visitor_count: Arc::new(AtomicUsize::new(0)),
                cmd_rx,
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
                        let _ = tx.conn_tx.send(msg.clone());
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

    async fn connect(&mut self, tx: mpsc::UnboundedSender<Msg>, user_info: UserInfo) -> ConnId {
        log::info!("Someone joined");

        self.send_system_message("main", 0, "Someone joined").await;

        let id = thread_rng().gen::<usize>();
        let session = Session {
            conn: id,
            user_info,
            conn_tx: tx,
        };
        self.sessions.insert(id, session);

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
        /* match cmd {
            WsBoardCommand::Create => {
                let board = Board::new(
                    quader_engine::game_settings::GameSettings::default(),
                    self.wkd.clone(),
                    self.seed
                );
                self.boards.insert(conn.clone(), board);
                let (bm, handle) = WsBoardMgr::new();
                let handle = tokio::spawn(bm.run());
                handle.await.ok();
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
            WsBoardCommand::HoldPiece => {
                let board = self.boards.get_mut(&conn).unwrap();
                board.try_hold_piece();
            },
        } */

        "ok".to_string()
    }
 
    fn list_lobbies(&self) -> Vec<LobbyListing> {
        self.lobby_container.list_lobbies()
    }

    async fn join_lobby(&mut self, _conn: ConnId, lobby_id: String) {
        // TODO: Add correct username
        let username = "test".to_string();
        self.lobby_container.add_player(&lobby_id, username).ok();
    }

    pub async fn run(mut self) -> io::Result<()> {
        while let Some(cmd) = self.cmd_rx.recv().await {
            match cmd {
                Command::Connect { conn_tx, user_info, res_tx } => {
                    let conn_id = self.connect(conn_tx, user_info).await;
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
                }
                Command::StartMatch { conn, res_tx } => {
                    todo!()
                }
                Command::ListLobbies { res_tx } => {
                    let _ = res_tx.send(self.list_lobbies());
                },
                Command::JoinLobby { conn, lobby_id, res_tx } => {
                    self.join_lobby(conn, lobby_id).await;
                    let _ = res_tx.send(());
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
    pub async fn connect(&self, conn_tx: mpsc::UnboundedSender<String>, user_info: UserInfo) -> ConnId {
        let (res_tx, res_rx) = oneshot::channel();

        self.cmd_tx
            .send(Command::Connect { conn_tx, user_info, res_tx })
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
            .send(Command::BoardCommand { cmd, conn, res_tx })
            .unwrap();

        let res = res_rx.await.unwrap();

        res
    }

    pub async fn start_match(&self, conn: ConnId) {
        let (res_tx, res_rx) = oneshot::channel();

        self.cmd_tx
            .send(Command::StartMatch { conn, res_tx })
            .unwrap();

        res_rx.await.unwrap();
    }

    pub async fn list_lobbies(&self) -> Vec<LobbyListing> {
        let (res_tx, res_rx) = oneshot::channel();

        self.cmd_tx.send(Command::ListLobbies { res_tx }).unwrap();

        res_rx.await.unwrap()
    }

    pub async fn join_lobby(&self, conn: ConnId, lobby_id: impl Into<String>) {
        let (res_tx, res_rx) = oneshot::channel();

        self.cmd_tx
            .send(Command::JoinLobby {
                conn,
                lobby_id: lobby_id.into(),
                res_tx,
            })
            .unwrap();

        res_rx.await.unwrap()
    }
}

#[cfg(debug_assertions)]
fn seed_lobbies(lobby_container: &mut LobbyContainer) {
    log::debug!("Seeding lobbies");

    lobby_container.add_lobby(Lobby::from_settings_with_id(
        LobbySettings {
            name: "test1".to_owned(),
            player_limit: 128,
        },
        "admin".to_owned(),
        "0".to_owned(),
    ));

    lobby_container.add_lobby(Lobby::from_settings_with_id(
        LobbySettings {
            name: "test2".to_owned(),
            player_limit: 2,
        },
        "admin".to_owned(),
        "1".to_owned(),
    ));
}
