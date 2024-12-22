use anyhow;
use crossbeam::channel;
use std::thread;
use std::time;
use tungstenite::{connect, Message};

pub fn establish_connection() -> anyhow::Result<channel::Receiver<Message>> {
    let (sender, receiver) = channel::unbounded();
    let server_url = "ws://localhost:8080/ws";
    let (mut socket, response) = connect(server_url)?;

    println!("Connected to the server");
    println!("Response HTTP code: {}", response.status());
    println!("Response contains the following headers:");
    for (header, _value) in response.headers() {
        println!("* {header}");
    }

    socket.send(Message::Text("Tetris".into())).unwrap();

    thread::spawn(move || {
        loop {
            socket
                .send(Message::Ping("Ping!".into()))
                .expect("Failed to send ping");
            println!("Sent ping");
            let msg = socket.read().expect("Error reading message");
            match &msg {
                Message::Pong(_) => println!("Received Pong from pinger"),
                _ => println!("Received something other"),
            }
            sender.send(msg).expect("Failed to send to channel");
            thread::sleep(time::Duration::from_secs(1));
        }
        //socket.close(None).unwrap();
    });

    Ok(receiver)
}
