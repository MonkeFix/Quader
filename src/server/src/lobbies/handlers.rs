/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENSE file in the repository root for full license text.
 */

use crate::lobbies::models::{Lobby, LobbyContainer, LobbySettings};
use actix_web::{delete, get, post, put, web, HttpRequest, HttpResponse};
use std::ops::Deref;

#[get("/lobbies")]
pub async fn list_lobbies(lobby_container: web::Data<LobbyContainer>) -> HttpResponse {
    let lobbies = lobby_container.lobby_list.read().map_err(|e| match e {
        _ => HttpResponse::InternalServerError(),
    });

    match lobbies {
        Ok(r) => HttpResponse::Ok().json(r.deref()),
        Err(err) => err.into(),
    }
}

#[get("/lobbies/{uuid}")]
pub async fn get_lobby(
    lobby_container: web::Data<LobbyContainer>,
    uuid: web::Path<String>,
) -> HttpResponse {
    if let Some(lobby) = lobby_container
        .lobby_list
        .read()
        .unwrap()
        .get(uuid.as_ref())
    {
        return HttpResponse::Ok().json(lobby);
    }

    HttpResponse::NotFound().body("Lobby Not Found")
}

#[post("/lobbies")]
pub async fn create_lobby(
    _req: HttpRequest,
    lobby_container: web::Data<LobbyContainer>,
    lobby_settings: web::Json<LobbySettings>,
) -> HttpResponse {
    //let auth = req.headers().get(actix_web::http::header::AUTHORIZATION);
    //req.extensions_mut().insert("a".to_string());

    let lobby = Lobby::from_settings(lobby_settings.0, "aaaa".to_string());

    lobby_container
        .lobby_list
        .write()
        .unwrap()
        .insert(lobby.uuid.clone(), lobby.clone());

    HttpResponse::Ok().json(lobby)
}

#[put("/lobbies/{uuid}")]
pub async fn update_lobby(
    lobby_container: web::Data<LobbyContainer>,
    uuid: web::Path<String>,
    new_settings: web::Json<LobbySettings>,
) -> HttpResponse {
    if let Some(_) = lobby_container
        .lobby_list
        .read()
        .unwrap()
        .get(uuid.as_ref())
    {
        let mut mutex = lobby_container.lobby_list.write().unwrap();
        let lobby = mutex.get_mut(uuid.as_ref()).unwrap();
        lobby.lobby_name = new_settings.name.clone();
        lobby.player_limit = new_settings.player_limit;

        return HttpResponse::Ok().json(lobby.clone());
    }

    HttpResponse::NotFound().body("Lobby Not Found")
}

#[delete("/lobbies/{uuid}")]
pub async fn delete_lobby(
    lobby_container: web::Data<LobbyContainer>,
    uuid: web::Path<String>,
) -> HttpResponse {
    lobby_container
        .lobby_list
        .write()
        .unwrap()
        .remove(uuid.as_ref())
        .unwrap();

    HttpResponse::Ok().body("Success")
}
