use sqlx::{Pool, Postgres, postgres::PgPoolOptions};

use crate::config::Config;

#[derive(Debug, Clone)]
pub struct DBClient {
    pool: Pool<Postgres>,
}

impl DBClient {
    pub async fn init(config: &Config) -> Self {
        let max_conn_var = "DB_POOL_MAX_CONNECTIONS";
        let max_connections = std::env::var(max_conn_var)
            .expect("{max_conn_var} must be set")
            .parse::<u32>()
            .expect("{max_conn_var} must be valid integer");

        let pool = PgPoolOptions::new()
            .max_connections(max_connections)
            .connect(&config.database_url)
            .await
            .expect("Failed to initialize database connection pool.");

        sqlx::migrate!().run(&pool).await.expect("Failed to execute migrations.");

        DBClient { pool }
    }
}
