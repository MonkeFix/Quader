/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENSE file in the repository root for full license text.
 */

use std::time::Duration;
use futures::stream::{SplitSink, SplitStream};
use futures::{SinkExt, StreamExt, TryFutureExt};
use log::{debug, error, info};
use tokio::select;
use tokio::sync::{mpsc, oneshot};
use tokio::time::{sleep, timeout};
use tokio_stream::wrappers::UnboundedReceiverStream;
use warp::{filters::ws::Ws, reject, reply::Reply};
use warp::http::StatusCode;
use warp::ws::{Message, WebSocket};
use crate::auth::UserInfo;

use crate::lobby::models::LobbyContainer;
use crate::Result;
use crate::ws::models::{ClientConnection, Session, SessionStorage, WsAction, WsRequest, WsResponse};

pub async fn ws(
    uuid: String,
    user: UserInfo,
    ws: Ws,
    lobby_container: LobbyContainer,
    session_storage: SessionStorage
) -> Result<impl Reply>
{
    debug!("connecting to lobby with uuid={:?}, UserInfo={:?}", uuid, user);

    if !lobby_container.contains_id(&uuid) {
        debug!("lobby with uuid={:?} not found", &uuid);
        return Err(reject::not_found());
    }

    Ok(ws.on_upgrade(move |socket| client_connected(uuid, user, socket, lobby_container, session_storage)))
}


async fn client_connected(
    lobby_id: String,
    user_info: UserInfo,
    ws: WebSocket,
    mut lobby_container: LobbyContainer,
    session_storage: SessionStorage,
) {

    // Add user to the player list
    lobby_container.add_player(&lobby_id, user_info.username.clone());

    // Drop old connection if the same user is already connected
    if let Some(session) = session_storage.sessions.lock().unwrap().get_mut(&user_info.username) {
        info!("Client {} is already logged in. Closing the old connection", &user_info.username);
        session.send(Message::text("Already logged in. Closing old connection.")).ok();
        session.send(Message::close()).ok();
    }

    info!("client {} connected", user_info.username);

    let (client_ws_sender, client_ws_rcv) = ws.split();

    let (client_sender, client_rcv) = mpsc::unbounded_channel();
    let client_rcv = UnboundedReceiverStream::new(client_rcv);
    tokio::task::spawn(sender(user_info.username.clone(), client_rcv, client_ws_sender));

    let (pong_tx, pong_rx) = mpsc::unbounded_channel();
    let (terminate_tx, _terminate_rx) = oneshot::channel();
    let client = ClientConnection::new(user_info.username.clone(), client_sender, pong_tx);

    let session = Session::new(client.clone(), user_info.clone());
    session_storage.sessions.lock().unwrap().insert(user_info.username.clone(), session);

    tokio::task::spawn(receiver(client_ws_rcv, client.clone(), terminate_tx, session_storage.clone()));




    //let (terminate_tx2, terminate_rx2) = oneshot::channel();
    //select! {
    //    _ = pinger(id, pong_rx, client, terminate_rx) => {},
    //    _ = board_updater(id, terminate_rx2) => {}
    //}
    /*info!("setting up pinger");
    .await;
     */

    info!("setting up updater");
    let (_terminate_tx, terminate_rx) = oneshot::channel();
    pinger(&user_info.username, pong_rx, client, terminate_rx).await;
}

async fn client_disconnected(username: String, session_storage: &SessionStorage) {
    info!("Client {} disconnected", &username);
}

async fn process_message(client: &ClientConnection, msg: &str, session_storage: &SessionStorage) {
    let new_msg = Message::text(format!("<{}>: {:?}", client.username, msg));

    for (&ref username, session) in session_storage.sessions.lock().unwrap().iter() {
        if &client.username != username {
            if let Err(_disconnected) = session.client_connection.sender.send(new_msg.clone()) {}
        }
    }
}

async fn sender(username: String, mut client_rcv: UnboundedReceiverStream<Message>, mut client_ws_sender: SplitSink<WebSocket, Message>) {
    while let Some(message) = client_rcv.next().await {
        debug!("Got message for user with id {}: {:?}", username, message);
        let is_close = message.is_close();
        client_ws_sender
            .send(message)
            .unwrap_or_else(|e| {
                error!("websocket send error for client with id {}: {}", username, e);
            })
            .await;
        if is_close { // nothing to do anymore, server closes the connection
        debug!("Closed the connection with user {}", username);
            break;
        }
    }
}

async fn receiver(mut client_ws_rcv: SplitStream<WebSocket>, client: ClientConnection, terminate_pinger: oneshot::Sender<()>, session_storage: SessionStorage) {
    while let Some(result) = client_ws_rcv.next().await {
        match result {
            Ok(msg) => {
                debug!("Got message from user {}: {:?}", client.username, msg);
                if msg.is_close() {
                    info!("websocket connection is closed by client {}", client.username);
                    let _ = terminate_pinger.send(()); // nothing to do with client anymore
                    break;
                } else if msg.is_pong() {
                    if let Err(_disconnected) = client.ponger.send(()) {}
                } else if let Ok(s) = msg.to_str() {
                    process_message(&client, s, &session_storage).await;
                } else { // due to format mismatch
                    if let Err(_disconnected) = client.sender.send(Message::close()) {}
                    break; // nothing to do here anymore
                }
            },
            Err(e) => {
                error!("websocket error(username={}): {}", client.username, e);
                break;
            }
        };
    }
}

#[allow(non_upper_case_globals)]
const timeout_duration: u64 = 10;

#[allow(non_upper_case_globals)]
const ping_interval: u64 = 5;

async fn pinger(username: &str, mut pong_rx: mpsc::UnboundedReceiver<()>, client: ClientConnection, terminate_rx: oneshot::Receiver<()>) {
    let ping = async move {
        loop {
            sleep(Duration::from_secs(ping_interval)).await;

            let ping_msg = Message::ping("");
            if let Err(_disconnected) = client.sender.send(ping_msg) {}
            debug!("Sent ping for user {}", username);

            let pong = pong_rx.recv();
            match timeout(Duration::from_secs(timeout_duration), pong).await {
                Ok(_) => {
                    debug!("Got pong from user {}", username);
                },
                Err(_) => {
                    debug!("Pong timeout for user {}", username);
                    let close_msg = Message::close();
                    if let Err(_disconnected) = client.sender.send(close_msg) {}
                    debug!("Sent close message for user {}", username);
                    break;
                },
            }
        }
    };
    select! {
        _ = ping => {
            debug!("Pinger for client {} is terminated due to pong timeout", username)
        }
        _ = terminate_rx => {
            debug!("Pinger for client {} is terminated due to client closure", username)
        }
    }
}

/*const MS_PER_UPDATE: f64 = 1.0 / 30.0; // 30 times per second

async fn board_updater(client_id: ID, terminate_rx: oneshot::Receiver<()>) {
    let update = async move {
        let mut prev = Instant::now();
        let mut lag = 0.0;

        loop {
            let curr = Instant::now();
            let elapsed = curr - prev;
            prev = curr;
            lag += elapsed.as_secs_f64();

            // process_input();

            while lag >= MS_PER_UPDATE {
                // board.update();
                info!("Update!");
                lag -= MS_PER_UPDATE;
            }

            // do other stuff
        }
    };
    select! {
        _ = update => {
            debug!("Updater for client {} is terminated due to timeout", client_id)
        }
        _ = terminate_rx => {
            debug!("Updater for client {} is terminated due to client closure", client_id)
        }
    }
}*/