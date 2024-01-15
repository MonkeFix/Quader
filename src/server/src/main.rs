mod config;
mod lobbies;
mod auth;

use actix_web::{App, get, HttpResponse, HttpServer, Responder, web};
use serde::Serialize;
use crate::config::Config;
use crate::lobbies::models::LobbyContainer;

#[derive(Serialize)]
pub struct Response {
    pub message: String
}

#[get("/health")]
async fn healthcheck() -> impl Responder {
    let response = Response {
        message: "Everything is fine".to_string()
    };
    HttpResponse::Ok().json(response)
}

async fn not_found() -> impl Responder {
    let response = Response {
        message: "Not Found".to_string()
    };
    HttpResponse::NotFound().json(response)
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    dotenvy::dotenv().expect("cannot load .env file");
    env_logger::init_from_env(env_logger::Env::new().default_filter_or("info"));

    let config = Config::init();

    log::info!("Using config {:?}", config);

    let lobby_container = LobbyContainer::new();
    let app_data = web::Data::new(lobby_container);

    // Lobby:
    //  - create
    //  - update
    //  - get all
    //  - delete
    // WS:
    //  - connect to lobby
    HttpServer::new(move || {
        App::new()
            .app_data(app_data.clone())
            .configure(lobbies::config)
            .service(healthcheck)
            .default_service(web::route().to(not_found))
            .wrap(actix_web::middleware::Logger::default())
    })
        .bind(("0.0.0.0", config.port))?
        .run()
        .await
}
