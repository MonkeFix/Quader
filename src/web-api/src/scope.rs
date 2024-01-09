use actix_web::{Scope, web};

use crate::middleware::RequireAuth;

pub mod auth;

pub fn auth() -> Scope {
    web::scope("/auth")
        .service(self::auth::handler::register)
        .service(self::auth::handler::login)
        .service(web::scope("")
                    .service(self::auth::handler::logout)
                    .wrap(RequireAuth)
        )
}
