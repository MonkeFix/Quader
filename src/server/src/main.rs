mod auth;
mod config;
mod lobbies;
mod ws;

use crate::config::Config;
use crate::ws::server::{ChatServer, ChatServerHandle};
use actix_web::{get, web, App, HttpRequest, HttpResponse, HttpServer, Responder};
use auth::mock::ensure_auth;
use serde::Serialize;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
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

    let app_state = Arc::new(AtomicUsize::new(0));
    let (chat_server, server_tx) = ChatServer::new();
    let chat_server = spawn(chat_server.run());

    let http_server = HttpServer::new(move || {
        App::new()
            //.app_data(lobbies.clone())
            .app_data(web::Data::new(server_tx.clone()))
            .app_data(web::Data::from(app_state.clone()))
            .service(healthcheck)
            .route("/count", web::get().to(get_count))
            .service(web::resource("/ws").route(web::get().to(chat_ws)))
            .default_service(web::route().to(not_found))
            .wrap(actix_web::middleware::Logger::default())
    })
    .workers(4)
    .disable_signals()
    .bind(("0.0.0.0", config.port))?
    .run();

    //let server_handle = http_server.handle();
    //let task_shutdown_marker = Arc::new(AtomicBool::new(false));

    //let server_task = tokio::spawn(http_server);

    /* let shutdown = tokio::spawn(async move {
        tokio::signal::ctrl_c().await.unwrap();

        let server_stop = server_handle.stop(true);
        task_shutdown_marker.store(true, Ordering::SeqCst);
        server_stop.await;
        Ok(())
    }); */

    //try_join!(http_server, async move { chat_server.await.unwrap() })?;
    try_join!(
        http_server,
        async move { chat_server.await.unwrap() },
        //async move { shutdown.await.unwrap() }
    )?;

    Ok(())
}
