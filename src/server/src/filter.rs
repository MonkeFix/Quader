use std::convert::Infallible;
use warp::Filter;
use crate::client::Clients;

pub fn with_clients(clients: Clients) -> impl Filter<Extract=(Clients,), Error=Infallible> + Clone {
    warp::any().map(move || clients.clone())
}
