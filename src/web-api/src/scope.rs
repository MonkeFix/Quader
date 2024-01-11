use actix_web::{web, Scope};

pub mod auth;

pub fn auth() -> Scope {
    web::scope("/auth")
        .service(self::auth::handler::register)
        .service(self::auth::handler::login)
        .service(self::auth::handler::logout)
}

pub mod user;

pub fn user() -> Scope {
    web::scope("/user")
        .service(self::user::handler::get_me)
        .service(self::user::handler::get)
}
