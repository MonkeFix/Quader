use actix_web::{web, middleware::Logger, HttpServer, App, Responder, HttpResponse, get};
use dotenvy::dotenv;
use log::info;
use web_api::{config::Config, db::DBClient, app::AppState};

 #[actix_web::main]
async fn main() -> anyhow::Result<()> {
    dotenv().ok();
    env_logger::init_from_env(env_logger::Env::new().default_filter_or("info"));

    let config = Config::init();
    let db_client = DBClient::init(&config).await;
    let app_state = AppState::new(&config, db_client);

    info!("Server is running on http://localhost:{}", config.port);

    HttpServer::new(move || {
        App::new()
            .app_data(web::Data::new(app_state.clone()))
            .wrap(Logger::default())
            .service(web::scope("/api")
                        .service(health_handler)
                        .service(web_api::scope::auth()))
    })
    .bind(("0.0.0.0", config.port))?
    .run()
    .await?;

    Ok(())
}

#[get("/health")]
async fn health_handler() -> impl Responder {
    let msg = "Web API Server";
    HttpResponse::Ok().json(serde_json::json!({"status": "success", "message": msg}))
}
