use actix_web::{web, Scope, get};

pub mod auth;

pub fn auth() -> Scope {
    web::scope("/auth")
        .service(self::auth::handler::register)
        .service(self::auth::handler::login)
        .service(self::auth::handler::logout)
        .service(self::auth::handler::refresh)
        .service(self::auth::handler::validate)
}

pub mod user;

pub fn user() -> Scope {
    web::scope("/user")
        .service(self::user::handler::get_me)
        .service(self::user::handler::get)
}


#[utoipa::path(
    get,
    path = "/api/health",
    tag = "Health Checker Endpoint",
    responses(
        (status = 200, body = &'static str),
    )
)]
#[get("/health")]
async fn health_handler() -> crate::http::Response<&'static str> {
    let msg = "Web API Server";
    crate::http::Response::ok(msg)
}
