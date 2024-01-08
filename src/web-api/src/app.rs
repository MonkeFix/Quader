use crate::{config::Config, db::DBClient};

#[derive(Debug, Clone)]
pub struct AppState {
    pub config: Config,
    pub db_client: DBClient,
}

impl AppState {
    pub fn new(config: &Config, db_client: DBClient) -> Self {
        AppState { config: config.clone(), db_client }
    }
}
