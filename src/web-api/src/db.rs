#![allow(async_fn_in_trait)]
use sqlx::{Pool, Postgres, postgres::PgPoolOptions};
use uuid::Uuid;

use crate::{config::Config, model, model::UserRole};

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

pub trait UserExt {
    async fn get_user(
        &self,
        user_id: Option<Uuid>,
        name: Option<&str>,
        email: Option<&str>,
    ) -> Result<Option<model::User>, sqlx::Error>;
    async fn save_user<T: Into<String> + Send>(
        &self,
        name: T,
        email: T,
        password: T,
    ) -> Result<model::User, sqlx::Error>;
    async fn save_admin_user<T: Into<String> + Send>(
        &self,
        name: T,
        email: T,
        password: T,
    ) -> Result<model::User, sqlx::Error>;
}

impl UserExt for DBClient {
    async fn get_user(
        &self,
        user_id: Option<uuid::Uuid>,
        username: Option<&str>,
        email: Option<&str>,
    ) -> Result<Option<model::User>, sqlx::Error> {
        let mut user: Option<model::User> = None;

        if let Some(user_id) = user_id {
            user = sqlx::query_as!(
                model::User,
                r#"SELECT id,
                          username,
                          email,
                          password_hash,
                          role as "role: UserRole", -- help sqlx to infer type
                          photo,
                          verified,
                          created_at,
                          updated_at
                   FROM users
                   WHERE id = $1
                "#, user_id)
                .fetch_optional(&self.pool)
                .await?;
        } else if let Some(username) = username {
            user = sqlx::query_as!(
                model::User,
                r#"SELECT id,
                          username,
                          email,
                          password_hash,
                          role as "role: UserRole",
                          photo,
                          verified,
                          created_at,
                          updated_at
                   FROM users
                   WHERE username = $1
                "#, username)
                .fetch_optional(&self.pool)
                .await?;
        } else if let Some(email) = email {
            user = sqlx::query_as!(
                model::User,
                r#"SELECT id,
                          username,
                          email,
                          password_hash,
                          role as "role: UserRole",
                          photo,
                          verified,
                          created_at,
                          updated_at
                   FROM users
                   WHERE email = $1
                "#, email)
                .fetch_optional(&self.pool)
                .await?;
        }

        Ok(user)
    }

    async fn save_user<T: Into<String> + Send>(
        &self,
        username: T,
        email: T,
        password_hash: T,
    ) -> Result<model::User, sqlx::Error> {
        let user = sqlx::query_as!(
            model::User,
            r#"INSERT INTO users (username, email, password_hash) VALUES ($1, $2, $3)
               RETURNING id, username, email, password_hash, role as "role: UserRole", photo, verified, created_at, updated_at
            "#,
            username.into(),
            email.into(),
            password_hash.into()
        )
        .fetch_one(&self.pool)
        .await?;

        Ok(user)
    }

    async fn save_admin_user<T: Into<String> + Send>(
        &self,
        username: T,
        email: T,
        password_hash: T,
    ) -> Result<model::User, sqlx::Error> {
        let user = sqlx::query_as!(
            model::User,
            r#"INSERT INTO users (username, email, password_hash, role) VALUES ($1, $2, $3, $4)
               RETURNING id, username, email, password_hash, role as "role: UserRole", photo, verified, created_at, updated_at
            "#,
            username.into(),
            email.into(),
            password_hash.into(),
            UserRole::Admin as UserRole,
        )
        .fetch_one(&self.pool)
        .await?;

        Ok(user)
    }
}
