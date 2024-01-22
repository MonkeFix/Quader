/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENSE file in the repository root for full license text.
 */

use std::time::{Duration, Instant};

use crate::auth::UserInfo;
use crate::ConnId;
use crate::{ws::server::ChatServerHandle, Msg};
use actix_ws::{CloseReason, Message};
use futures_util::StreamExt as _;
use quader_engine::board_command::BoardMoveDir;
use quader_engine::piece::RotationDirection;
use serde::{Deserialize, Serialize};
use tokio::{pin, select, sync::mpsc, time::interval};

const HEARTBEAT_INTERVAL: Duration = Duration::from_secs(5);
const CLIENT_TIMEOUT: Duration = Duration::from_secs(10);

pub async fn chat_ws(
    chat_server: ChatServerHandle,
    mut session: actix_ws::Session,
    mut msg_stream: actix_ws::MessageStream,
    user_info: UserInfo,
) {
    log::info!("user connected: {user_info:?}");

    let mut last_heartbeat = Instant::now();
    let mut interval = interval(HEARTBEAT_INTERVAL);

    let (conn_tx, mut conn_rx) = mpsc::unbounded_channel();

    log::debug!("server connect");
    let conn_id = chat_server.connect(conn_tx, user_info.clone()).await;
    log::debug!("server connected");

    let close_reason: Option<CloseReason> = loop {
        let tick = interval.tick();
        pin!(tick);

        let msg_rx = conn_rx.recv();
        pin!(msg_rx);

        select! {
            msg = msg_stream.next() => {

                match &msg {
                    // commands & messages received from client
                    Some(Ok(msg)) => {
                        log::debug!("msg: {msg:?}");

                        match msg {
                            Message::Ping(bytes) => {
                                last_heartbeat = Instant::now();
                                session.pong(&bytes).await.unwrap();
                            }
                            Message::Pong(_) => {
                                last_heartbeat = Instant::now();
                            }
                            Message::Text(text) => {
                                if let Err(err) = process_text_msg(&chat_server, &mut session, &text, conn_id, &user_info).await {
                                        log::error!("Error while processing message: {:?}", err);
                                        let _ = session.text(format!("error while processing message: {:?}", err)).await;
                                    }
                            }
                            Message::Binary(_bin) => {
                                log::warn!("unexpected binary message");
                            }
                            Message::Close(reason) => break reason.clone(),
                            _ => {
                                break None;
                            }
                        }
                    },
                    // client WebSocket stream error
                    Some(Err(err)) => {
                        log::error!("{}", err);
                        break None;
                    },
                    // client WebSocket stream ended
                    None => {
                        break None;
                    }
                }
            }
            msg = msg_rx => {
                // chat messages received from other room participants
                if let Some(msg) = &msg {
                    session.text(msg).await.unwrap();
                } else {
                    unreachable!("all connection message senders were dropped; chat server may have panicked")
                }
            }
            // heartbeat internal tick
            _ = tick => {
                // if no heartbeat ping/pong received recently, close the connection
                if Instant::now().duration_since(last_heartbeat) > CLIENT_TIMEOUT {
                    log::info!(
                        "client has not sent heartbeat in over {CLIENT_TIMEOUT:?}; disconnecting"
                    );
                    break None;
                }

                // send heartbeat ping
                let _ = session.ping(b"").await;
            }
        }
    };

    chat_server.disconnect(conn_id);
    log::info!("closed, reason: {:?}", close_reason);

    let _ = session.close(close_reason).await;
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum WsBoardCommand {
    Create,
    Destroy(ConnId),
    Move(BoardMoveDir, u32),
    Rotate(RotationDirection),
    HardDrop,
    // delta
    SoftDrop(u32),
    HoldPiece,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum WsAction {
    Chat(Msg),
    BoardCommand(WsBoardCommand),
    StartMatch,
    ListLobbies,
    JoinLobby(String),
}

async fn process_text_msg(
    chat_server: &ChatServerHandle,
    session: &mut actix_ws::Session,
    text: &str,
    conn: ConnId,
    user_info: &UserInfo
) -> Result<(), serde_json::Error> {
    let msg = text.trim();

    let action = serde_json::from_str::<WsAction>(msg)?;

    match action {
        WsAction::Chat(msg) => {
            let msg = format!("{}: {msg}", user_info.username);

            chat_server.send_message(conn, msg).await;
        }
        WsAction::BoardCommand(cmd) => {
            log::info!("conn {conn}: got a board cmd: {:?}", cmd);
            let msg = chat_server.on_board_cmd(conn, cmd).await;
            session.text(msg).await.unwrap();
        }
        WsAction::StartMatch => {
            log::info!("conn {conn}: starting match");
            chat_server.start_match(conn).await;
        }
        WsAction::ListLobbies => {
            log::info!("conn {conn}: listing lobbies");

            let lobbies = chat_server.list_lobbies().await;
            let lobbies = serde_json::to_string(&lobbies).unwrap();
            session.text(lobbies).await.unwrap();
            /* for lobby in lobbies {
                session.text(lobby).await.unwrap();
            } */
        }
        WsAction::JoinLobby(lobby) => {
            log::info!("conn {conn}: joining lobby {lobby}");

            chat_server.join_lobby(conn, &lobby).await;

            session.text(format!("joined lobby {lobby}")).await.unwrap();
        }
    }

    Ok(())
}
