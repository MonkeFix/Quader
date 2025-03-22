pub struct Config {
    pub grpc_server_port: u16,
}

impl Config {
    pub fn init() -> Config {
        let grpc_server_port = std::env::var("GRPC_SERVER_PORT")
            .expect("GRPC_SERVER_PORT must be set")
            .parse::<u16>()
            .expect("GRPC_SERVER_PORT must be a valid integer");

        Config { grpc_server_port }
    }
}
