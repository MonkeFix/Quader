/*
 * Copyright (c) MonkeFix. Licensed under the MIT License.
 * See the LICENCE file in the repository root for full licence text.
 */

use actix_web::{web, middleware::Logger, HttpServer, App};
use dotenvy::dotenv;
use log::info;
use utoipa::OpenApi;
use utoipa_swagger_ui::SwaggerUi;
use web_api::{config::Config, db::DBClient, app::AppState, scope, openapi::ApiDoc};

#[actix_web::main]
async fn main() -> anyhow::Result<()> {
    dotenv().ok();
    env_logger::init_from_env(env_logger::Env::new().default_filter_or("info"));

    let config = Config::init();
    let db_client = DBClient::init(&config).await;
    let app_state = AppState::new(&config, db_client);
    let openapi = ApiDoc::openapi();

    info!("Server is running on http://localhost:{}", config.port);

    HttpServer::new(move || {
        App::new()
            .app_data(web::Data::new(app_state.clone()))
            .wrap(Logger::default())
            .service(
                SwaggerUi::new("/swagger-ui/{_:.*}").url("/api-docs/openapi.json", openapi.clone()),
            )
            .service(web::scope("/api")
                        .service(scope::health_handler)
                        .service(scope::auth())
                        .service(scope::user())
            )
    })
    .bind(("0.0.0.0", config.port))?
    .run()
    .await?;

    Ok(())
}
