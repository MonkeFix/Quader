use actix_web::{Scope, web};

use crate::middleware::{RequireAuth, RequireOnlyAdmin};

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

pub mod user;

pub fn user() -> Scope {
     web::scope("/user")
         .service(web::scope("")
                  .service(self::user::handler::get_me)
                  .service(self::user::handler::get)
                  .wrap(RequireAuth))
}
