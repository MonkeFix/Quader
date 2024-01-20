mod auth;
mod config;
mod lobbies;
mod ws;

use crate::config::Config;
use crate::lobbies::models::{Lobby, LobbyContainer, LobbySettings};
use crate::ws::server::{ChatServer, ChatServerHandle};
use actix_web::{get, web, App, HttpRequest, HttpResponse, HttpServer, Responder};
use serde::Serialize;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use tokio::task::spawn_local;
use tokio::{spawn, try_join};

#[derive(Serialize)]
pub struct Response {
    pub message: String,
}

pub type ConnId = usize;
pub type RoomId = String;
pub type Msg = String;

#[get("/health")]
async fn healthcheck() -> impl Responder {
    let response = Response {
        message: "Everything is fine".to_string(),
    };
    HttpResponse::Ok().json(response)
}

async fn not_found() -> impl Responder {
    let response = Response {
        message: "Not Found".to_string(),
    };
    HttpResponse::NotFound().json(response)
}

async fn chat_ws(
    req: HttpRequest,
    stream: web::Payload,
    chat_server: web::Data<ChatServerHandle>,
) -> Result<HttpResponse, actix_web::error::Error> {
    let (res, session, msg_stream) = actix_ws::handle(&req, stream)?;

    spawn_local(ws::handler::chat_ws(
        (**chat_server).clone(),
        session,
        msg_stream,
    ));

    Ok(res)
}

async fn get_count(count: web::Data<AtomicUsize>) -> impl Responder {
    let current_count = count.load(Ordering::SeqCst);
    format!("Visitors: {current_count}")
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> std::io::Result<()> {
    dotenvy::dotenv().expect("cannot load .env file");
    env_logger::init_from_env(env_logger::Env::new().default_filter_or("info"));

    let config = Config::init();

    log::info!("Using config {:?}", config);

    let mut lobby_container = LobbyContainer::new();
    seed_lobbies(&mut lobby_container);
    let lobbies = web::Data::new(lobby_container);

    let app_state = Arc::new(AtomicUsize::new(0));
    let (chat_server, server_tx) = ChatServer::new();
    let chat_server = spawn(chat_server.run());

    let http_server = HttpServer::new(move || {
        App::new()
            .app_data(lobbies.clone())
            .app_data(web::Data::new(server_tx.clone()))
            .app_data(web::Data::from(app_state.clone()))
            .configure(lobbies::config)
            .service(healthcheck)
            .route("/count", web::get().to(get_count))
            .service(web::resource("/ws").route(web::get().to(chat_ws)))
            .default_service(web::route().to(not_found))
            .wrap(actix_web::middleware::Logger::default())
    })
    .workers(4)
    .bind(("0.0.0.0", config.port))?
    .run();

    try_join!(http_server, async move { chat_server.await.unwrap() })?;

    Ok(())
}

fn seed_lobbies(lobby_container: &mut LobbyContainer) {
    let lobby1 = Lobby::from_settings_with_id(
        LobbySettings {
            name: "Super Lobby".to_string(),
            player_limit: 3,
        },
        "admin".to_string(),
        "1".to_string(),
    );
    let lobby2 = Lobby::from_settings_with_id(
        LobbySettings {
            name: "Not So Super Lobby".to_string(),
            player_limit: 6,
        },
        "user".to_string(),
        "2".to_string(),
    );

    lobby_container.add_lobby(lobby1);
    lobby_container.add_lobby(lobby2);
}
