/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENSE file in the repository root for full license text.
 */

use std::time::{Duration, Instant};

use crate::ws::server::ChatServerHandle;
use crate::ConnId;
use actix_ws::{CloseReason, Message};
use futures_util::StreamExt as _;
use tokio::{pin, select, sync::mpsc, time::interval};

const HEARTBEAT_INTERVAL: Duration = Duration::from_secs(5);
const CLIENT_TIMEOUT: Duration = Duration::from_secs(10);

pub async fn chat_ws(
    chat_server: ChatServerHandle,
    mut session: actix_ws::Session,
    mut msg_stream: actix_ws::MessageStream,
) {
    log::info!("connected");

    let mut name = None;
    let mut last_heartbeat = Instant::now();
    let mut interval = interval(HEARTBEAT_INTERVAL);

    let (conn_tx, mut conn_rx) = mpsc::unbounded_channel();

    let conn_id = chat_server.connect(conn_tx).await;

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
                                process_text_msg(&chat_server, &mut session, &text, conn_id, &mut name)
                                    .await;
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

    let _ = session.close(close_reason).await;
}

async fn process_text_msg(
    chat_server: &ChatServerHandle,
    session: &mut actix_ws::Session,
    text: &str,
    conn: ConnId,
    name: &mut Option<String>,
) {
    let msg = text.trim();

    if msg.starts_with('/') {
        let mut cmd_args = msg.splitn(2, ' ');

        match cmd_args.next().unwrap() {
            "/list" => {
                log::info!("conn {conn}: listing rooms");

                let rooms = chat_server.list_rooms().await;

                for room in rooms {
                    session.text(room).await.unwrap();
                }
            }

            "/join" => match cmd_args.next() {
                Some(room) => {
                    log::info!("conn {conn}: joining room {room}");

                    chat_server.join_room(conn, room).await;

                    session.text(format!("joined {room}")).await.unwrap();
                }
                None => {
                    session.text("!!! room name is required").await.unwrap();
                }
            },

            "/name" => match cmd_args.next() {
                Some(new_name) => {
                    log::info!("conn {conn}: setting name to: {new_name}");
                    name.replace(new_name.to_owned());
                }
                None => {
                    session.text("!!! name is required").await.unwrap();
                }
            },

            _ => {
                session
                    .text(format!("!!! unknown command: {msg}"))
                    .await
                    .unwrap();
            }
        }
    } else {
        let msg = match name {
            Some(ref name) => format!("{name}: {msg}"),
            None => msg.to_owned(),
        };

        chat_server.send_message(conn, msg).await;
    }
}
