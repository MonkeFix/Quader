/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENSE file in the repository root for full license text.
 */

use crate::{
    auth::UserInfo,
    lobbies::{Lobby, LobbyContainer, LobbyListing, LobbySettings},
    ConnId, Msg,
};
use rand::{thread_rng, Rng as _};
use std::{
    collections::HashMap,
    io,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
};
use tokio::sync::{mpsc, oneshot};

use super::handler::WsBoardCommand;

#[derive(Debug)]
struct Session {
    pub conn: ConnId,
    pub user_info: UserInfo,
    pub conn_tx: mpsc::UnboundedSender<Msg>,
}

#[derive(Debug)]
enum Command {
    Connect {
        conn_tx: mpsc::UnboundedSender<Msg>,
        user_info: UserInfo,
        res_tx: oneshot::Sender<ConnId>,
    },
    Disconnect {
        conn: ConnId,
    },
    ListLobbies {
        res_tx: oneshot::Sender<Vec<LobbyListing>>,
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
pub struct ChatServer {
    sessions: HashMap<ConnId, Session>,
    lobby_container: LobbyContainer,
    visitor_count: Arc<AtomicUsize>,
    cmd_rx: mpsc::UnboundedReceiver<Command>,
}

impl ChatServer {
    pub fn new() -> (Self, ChatServerHandle) {
        let mut lobby_container = LobbyContainer::new();
        seed_lobbies(&mut lobby_container);

        log::debug!("created lobbies");

        let (cmd_tx, cmd_rx) = mpsc::unbounded_channel();

        log::debug!("setting up channels");

        let res = (
            Self {
                sessions: HashMap::new(),
                lobby_container,
                visitor_count: Arc::new(AtomicUsize::new(0)),
                cmd_rx,
            },
            ChatServerHandle { cmd_tx },
        );

        log::debug!("res");

        res
    }

    async fn send_system_message(&self, lobby_id: &str, skip: ConnId, msg: impl Into<String>) {
        if let Some(lobby) = self.lobby_container.lobby_map.get(lobby_id) {
            let msg = msg.into();

            for user in lobby.player_list.iter() {
                if user.conn != skip {
                    if let Some(tx) = self.sessions.get(&user.conn) {
                        let _ = tx.conn_tx.send(format!("{}", msg.clone()));
                    }
                }
            }
        }
    }

    async fn send_message(&self, conn: ConnId, msg: impl Into<String>) {
        if let Some(lobby) = self
            .lobby_container
            .lobby_map
            .iter()
            .find_map(|(lobby_id, lobby)| lobby.contains_player_conn(conn).then_some(lobby_id))
        {
            self.send_system_message(&lobby, conn, msg).await;
        };
    }

    async fn connect(&mut self, tx: mpsc::UnboundedSender<Msg>, user_info: UserInfo) -> ConnId {
        log::info!("{} joined", &user_info.username);

        let id = thread_rng().gen::<usize>();
        let session = Session {
            conn: id,
            user_info,
            conn_tx: tx,
        };
        self.sessions.insert(id, session);

        let _count = self.visitor_count.fetch_add(1, Ordering::SeqCst);
        /* self.send_system_message("main", 0, format!("Total visitors {count}"))
        .await; */

        id
    }

    async fn disconnect(&mut self, conn_id: ConnId) {
        log::info!("disconnect");

        let session = self.sessions.get(&conn_id);
        let mut lobby_ids: Vec<String> = vec![];

        let username = match session {
            Some(s) => &s.user_info.username,
            None => "Someone",
        };

        log::info!("{} disconnected", username);

        if session.is_some() {
            for (lobby_id, lobby) in &mut self.lobby_container.lobby_map {
                if lobby.remove_player(conn_id) {
                    lobby_ids.push(lobby_id.to_owned());
                }
            }
        }

        for lobby_id in lobby_ids {
            self.send_system_message(&lobby_id, 0, format!("{} disconnected", username))
                .await;
        }
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

    async fn join_lobby(&mut self, conn: ConnId, lobby_id: String) {
        if let Some(session) = self.sessions.get(&conn) {
            let res =
                self.lobby_container
                    .try_add_player(&lobby_id, conn, session.user_info.clone());
            if res.is_ok() {
                self.send_system_message(
                    &lobby_id,
                    0,
                    format!("{} connected!", session.user_info.username),
                )
                .await;
                log::info!(
                    "player {} joined lobby {}",
                    session.user_info.username,
                    lobby_id
                )
            } else {
                log::error!("error joining lobby {}", lobby_id);
                //self.send_message(conn, format!("error")).await;
            }
        }
    }

    pub async fn run(mut self) -> io::Result<()> {
        log::debug!("running server");

        while let Some(cmd) = self.cmd_rx.recv().await {
            log::debug!("got cmd: {:?}", &cmd);

            match cmd {
                Command::Connect {
                    conn_tx,
                    user_info,
                    res_tx,
                } => {
                    log::debug!("connecting user");
                    let conn_id = self.connect(conn_tx, user_info).await;
                    let _ = res_tx.send(conn_id);
                }
                Command::Disconnect { conn } => {
                    self.disconnect(conn).await;
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
                }
                Command::JoinLobby {
                    conn,
                    lobby_id,
                    res_tx,
                } => {
                    self.join_lobby(conn, lobby_id).await;
                    let _ = res_tx.send(());
                }
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
    pub async fn connect(
        &self,
        conn_tx: mpsc::UnboundedSender<String>,
        user_info: UserInfo,
    ) -> ConnId {
        let (res_tx, res_rx) = oneshot::channel();

        log::debug!("sending Command::Connect");
        self.cmd_tx
            .send(Command::Connect {
                conn_tx,
                user_info,
                res_tx,
            })
            .unwrap();

        log::debug!("awaiting result");
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
