mod auth;
mod config;
mod lobbies;
mod ws;

use crate::config::Config;
use crate::ws::server::{ChatServer, ChatServerHandle};
use actix_web::{get, web, App, HttpRequest, HttpResponse, HttpServer, Responder};
use auth::mock::ensure_auth;
use serde::Serialize;
use tokio::task::spawn_local;
use tokio::{spawn, try_join};

#[derive(Serialize)]
pub struct Response {
    pub message: String,
}

pub type ConnId = usize;
pub type RoomId = String;
pub type Msg = String;
pub type LobbyUuid = String;
pub type LobbyName = String;

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
    let ui_res = ensure_auth(req);
    if let None = ui_res {
        return Err(actix_web::error::ErrorForbidden(
            "invalid token".to_string(),
        ));
    }
    let (user_info, _token) = ui_res.unwrap();

    spawn_local(ws::handler::chat_ws(
        (**chat_server).clone(),
        session,
        msg_stream,
        user_info,
    ));

    Ok(res)
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> std::io::Result<()> {
    dotenvy::dotenv().expect("cannot load .env file");
    env_logger::init_from_env(env_logger::Env::new().default_filter_or("info"));

    let config = Config::init();

    log::info!("Using config {:?}", config);

    let (chat_server, server_tx) = ChatServer::new();
    let chat_server = spawn(chat_server.run());

    log::info!("constructing http server");

    let http_server = HttpServer::new(move || {
        App::new()
            .app_data(web::Data::new(server_tx.clone()))
            .service(healthcheck)
            .service(web::resource("/ws").route(web::get().to(chat_ws)))
            .default_service(web::route().to(not_found))
            .wrap(actix_web::middleware::Logger::default())
    })
    .workers(4)
    .bind(("0.0.0.0", config.port))?
    .run();

    log::info!("done constructing http server");

    try_join!(http_server, async move { chat_server.await.unwrap() })?;

    log::info!("bye-bye");

    Ok(())
}
