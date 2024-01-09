/*
 * Copyright (c) Grigory Alfyorov. Licensed under the MIT License.
 * See the LICENCE file in the repository root for full licence text.
 */

use warp::{filters::ws::Ws, reply::Reply};

use crate::client::client_connected;
use crate::Result;

pub async fn ws(ws: Ws) -> Result<impl Reply> {
    Ok(ws.on_upgrade(move |socket| client_connected(socket)))
}
