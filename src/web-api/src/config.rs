#[derive(Debug, Clone)]
pub struct Config {
    pub database_url: String,
    pub port: u16,
}

impl Config {
    pub fn init() -> Config {
        let database_url = std::env::var("DATABASE_URL").expect("DATABASE_URL must be set");
        let port = std::env::var("WEB_API_PORT")
            .expect("WEB_API_PORT must be set")
            .parse::<u16>()
            .expect("WEB_API_PORT must be valid integer");

        Config {
            database_url,
            port,
        }
    }
}
