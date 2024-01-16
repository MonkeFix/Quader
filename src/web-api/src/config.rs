#[derive(Debug, Clone)]
pub struct Config {
    pub database_url: String,
    pub jwt_secret: String,
    pub jwt_maxage: i64,
    pub jwt_refresh_maxage: i64,
    pub port: u16,
}

impl Config {
    pub fn init() -> Config {
        let database_url = std::env::var("DATABASE_URL").expect("DATABASE_URL must be set");
        let port = std::env::var("WEB_API_PORT")
            .expect("WEB_API_PORT must be set")
            .parse::<u16>()
            .expect("WEB_API_PORT must be valid integer");
        let jwt_secret = std::env::var("JWT_SECRET").expect("JWT_SECRET must be set");
        let jwt_maxage = std::env::var("JWT_MAXAGE")
            .expect("JWT_MAXAGE must be set")
            .parse::<u32>()
            .expect("JWT_MAXAGE must be valid integer") as i64;
        let jwt_refresh_maxage = std::env::var("JWT_REFRESH_MAXAGE")
            .expect("JWT_REFRESH_MAXAGE must be set")
            .parse::<u32>()
            .expect("JWT_REFRESH_MAXAGE must be valid integer")
            as i64;

        Config {
            database_url,
            jwt_secret,
            jwt_maxage,
            jwt_refresh_maxage,
            port,
        }
    }
}
