/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENCE file in the repository root for full licence text.
 */

use std::{sync::{atomic::AtomicUsize, Arc}, collections::HashMap, time::Duration};
use std::sync::atomic::Ordering;
use std::time::SystemTime;
use futures::{stream::{SplitStream, SplitSink}, StreamExt, SinkExt, TryFutureExt};
use log::{error, debug, info};
use tokio::{sync::{mpsc, RwLock, oneshot}, time::{timeout, sleep}, select};
use tokio::time::Instant;
use tokio_stream::wrappers::UnboundedReceiverStream;
use warp::filters::ws::{Message, WebSocket};

type ID = usize;

struct IDGenerator(AtomicUsize);

impl IDGenerator {
    const fn new() -> IDGenerator {
        IDGenerator(AtomicUsize::new(1))
    }
    fn get(&self) -> ID {
        self.0.fetch_add(1, Ordering::Relaxed)
    }
}


#[derive(Clone)]
pub struct Client {
    pub id: ID,
    pub sender: mpsc::UnboundedSender<Message>,
    pub ponger: mpsc::UnboundedSender<()>,
}

impl Client {
    pub fn new(id: usize, sender: mpsc::UnboundedSender<Message>, ponger: mpsc::UnboundedSender<()>) -> Client {
        Client { id, sender, ponger }
    }
}

pub type Clients = Arc<RwLock<HashMap<usize, Client>>>;

#[allow(non_upper_case_globals)]
static id_generator: IDGenerator = IDGenerator::new();

pub async fn client_connected(ws: WebSocket) {
    let id = id_generator.get();

    info!("client with id {} connected", id);

    let (client_ws_sender, client_ws_rcv) = ws.split();

    let (client_sender, client_rcv) = mpsc::unbounded_channel();
    let client_rcv = UnboundedReceiverStream::new(client_rcv);
    tokio::task::spawn(sender(id, client_rcv, client_ws_sender));

    let (pong_tx, pong_rx) = mpsc::unbounded_channel();
    let (terminate_tx, terminate_rx) = oneshot::channel();
    let client = Client::new(id, client_sender, pong_tx);
    tokio::task::spawn(receiver(client_ws_rcv, client.clone(), terminate_tx));


    //let (terminate_tx2, terminate_rx2) = oneshot::channel();
    //select! {
    //    _ = pinger(id, pong_rx, client, terminate_rx) => {},
    //    _ = board_updater(id, terminate_rx2) => {}
    //}
    /*info!("setting up pinger");
    .await;
     */

    info!("setting up updater");
    let (terminate_tx, terminate_rx) = oneshot::channel();
    pinger(id, pong_rx, client, terminate_rx).await;
}

async fn process_message(client: &Client, msg: &str) {
    let new_msg = Message::text(format!("<User#{}>: {:?}", client.id, msg));

    if let Err(_disconnected) = client.sender.send(new_msg) {}
}

async fn sender(client_id: ID, mut client_rcv: UnboundedReceiverStream<Message>, mut client_ws_sender: SplitSink<WebSocket, Message>) {
    while let Some(message) = client_rcv.next().await {
        debug!("Got message for user with id {}: {:?}", client_id, message);
        let is_close = message.is_close();
        client_ws_sender
            .send(message)
            .unwrap_or_else(|e| {
                error!("websocket send error for client with id {}: {}", client_id, e);
            })
            .await;
        if is_close { // nothing to do anymore, server closes the connection
            debug!("Closed the connection with user {}", client_id);
            break;
        }
    }
}

async fn receiver(mut client_ws_rcv: SplitStream<WebSocket>, client: Client, terminate_pinger: oneshot::Sender<()>) {
    while let Some(result) = client_ws_rcv.next().await {
        match result {
            Ok(msg) => {
                debug!("Got message from user with id {}: {:?}", client.id, msg);
                if msg.is_close() {
                    info!("websocket connection is closed by client with id {}", client.id);
                    let _ = terminate_pinger.send(()); // nothing to do with client anymore
                    break;
                } else if msg.is_pong() {
                    if let Err(_disconnected) = client.ponger.send(()) {}
                } else if let Ok(s) = msg.to_str() {
                    process_message(&client, s).await;
                } else { // due to format mismatch
                    if let Err(_disconnected) = client.sender.send(Message::close()) {}
                    break; // nothing to do here anymore
                }
            },
            Err(e) => {
                error!("websocket error(uid={}): {}", client.id, e);
                break;
            }
        };
    }
}

#[allow(non_upper_case_globals)]
const timeout_duration: u64 = 10;

#[allow(non_upper_case_globals)]
const ping_interval: u64 = 5;

async fn pinger(client_id: ID, mut pong_rx: mpsc::UnboundedReceiver<()>, client: Client, terminate_rx: oneshot::Receiver<()>) {
    let ping = async move {
        loop {
            sleep(Duration::from_secs(ping_interval)).await;

            let ping_msg = Message::ping("");
            if let Err(_disconnected) = client.sender.send(ping_msg) {}
            debug!("Sent ping for user with id {}", client_id);

            let pong = pong_rx.recv();
            match timeout(Duration::from_secs(timeout_duration), pong).await {
                Ok(_) => {
                    debug!("Got pong from user with id {}", client_id);
                },
                Err(_) => {
                    debug!("Pong timeout for user with id {}", client_id);
                    let close_msg = Message::close();
                    if let Err(_disconnected) = client.sender.send(close_msg) {}
                    debug!("Sent close message for user with id {}", client_id);
                    break;
                },
            }
       }
    };
    select! {
        _ = ping => {
            debug!("Pinger for client {} is termitated due to pong timeout", client_id)
        }
        _ = terminate_rx => {
            debug!("Pinger for client {} is terminated due to client closure", client_id)
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