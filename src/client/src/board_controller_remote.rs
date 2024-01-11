/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENSE file in the repository root for full licence text.
 */

use std::net::TcpStream;

use tungstenite::{connect, Message, WebSocket, stream::MaybeTlsStream};
use url::Url;

pub struct BoardControllerRemote {
    socket: WebSocket<MaybeTlsStream<TcpStream>>
}

impl BoardControllerRemote {
    pub fn new(connection_uri: &str) -> Self {
        let (mut socket, response) =
            connect(Url::parse(connection_uri).unwrap()).expect("Can't connect");

        for (ref header, _value) in response.headers() {
            println!("* {}", header);
        }

        socket.send(Message::Text("Hello WS!".into())).unwrap();

        Self {
            socket
        }
    }

    pub fn close(&mut self) {
        self.socket.close(None).ok();
    }
}