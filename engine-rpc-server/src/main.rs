use tonic::{transport::Server, Request, Response, Status};

pub mod config;

#[derive(Default)]
pub struct RpcBoard {}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    dotenvy::dotenv().expect("cannot load .env file");
    let config = config::Config::init();

    let addr = format!("[::1]:{}", config.grpc_server_port)
        .parse()
        .unwrap();

    /* Server::builder()
    //.add_service(RpcBoard::new())
    .
    .serve(addr)
    .await?; */

    Ok(())
}
