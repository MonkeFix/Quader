/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENSE file in the repository root for full licence text.
 */

use std::{sync::{Arc}, collections::HashMap};
use std::sync::atomic::AtomicUsize;
use std::sync::Mutex;
use serde::{Deserialize, Serialize};
use tokio::{sync::{mpsc, RwLock}};
use tokio::sync::mpsc::error::SendError;
use warp::filters::ws::{Message};
use warp::http::StatusCode;
use crate::auth::UserInfo;

pub type Sessions = Arc<Mutex<HashMap<String, Session>>>;

#[derive(Debug, Clone)]
pub struct Session {
    pub client_connection: ClientConnection,
    pub user_info: UserInfo
}

impl Session {
    pub fn new(client_connection: ClientConnection, user_info: UserInfo) -> Self {
        Self {
            client_connection,
            user_info
        }
    }

    pub fn send(&self, message: Message) -> Result<(), SendError<Message>> {
        self.client_connection.sender.send(message)
    }

    pub fn ping(&self) -> Result<(), SendError<()>> {
        self.client_connection.ponger.send(())
    }
}

#[derive(Debug, Clone)]
pub struct SessionStorage {
    pub sessions: Sessions
}

impl SessionStorage {
    pub fn new() -> Self {
        Self {
            sessions: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    pub fn add(&mut self, session: Session) {
        let username = &session.user_info.username;
        self.sessions.lock().unwrap().insert(username.clone(), session);
    }
}

#[derive(Debug, Clone)]
pub struct ClientConnection {
    pub username: String,
    pub sender: mpsc::UnboundedSender<Message>,
    pub ponger: mpsc::UnboundedSender<()>,
}

impl ClientConnection {
    pub fn new(username: String, sender: mpsc::UnboundedSender<Message>, ponger: mpsc::UnboundedSender<()>) -> ClientConnection {
        ClientConnection { username, sender, ponger }
    }
}

pub type Clients = Arc<RwLock<HashMap<usize, ClientConnection>>>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum WsAction {
    Chat(String),

}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WsRequest {
    pub id: usize,
    pub action: WsAction
}

impl WsRequest {
    pub fn new(id: usize, action: WsAction) -> Self {
        Self {
            id, action,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WsResponse {
    pub id: usize,
    pub status_code: u16,
    pub status: String,
    pub message: String,
    pub action: WsAction,
}