/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENSE file in the repository root for full license text.
 */

use actix_web::web;

pub mod models;
mod handlers;

pub fn config(cfg: &mut web::ServiceConfig) {
    cfg.service(
        web::scope("/api")
            .service(handlers::list_lobbies)
            .service(handlers::get_lobby)
            .service(handlers::create_lobby)
            .service(handlers::update_lobby)
            .service(handlers::delete_lobby)
    );
}