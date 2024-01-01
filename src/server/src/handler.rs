use warp::{filters::ws::Ws, reply::Reply};

use crate::client::client_connected;
use crate::Result;

pub async fn ws(ws: Ws) -> Result<impl Reply> {
    Ok(ws.on_upgrade(move |socket| client_connected(socket)))
}
