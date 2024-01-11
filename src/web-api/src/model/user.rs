use chrono::prelude::*;
use derive_more::Display;
use serde::{Deserialize, Serialize};

#[derive(Debug, Deserialize, Serialize, Clone, Copy, sqlx::Type, PartialEq, Display, Eq, Hash)]
#[sqlx(type_name = "user_role", rename_all = "lowercase")]
pub enum UserRole {
    #[display(fmt = "admin")]
    Admin,
    #[display(fmt = "user")]
    User,
    #[display(fmt = "moderator")]
    Moderator,
    #[display(fmt = "supporter")]
    Supporter,
    #[display(fmt = "guest")]
    Guest,
}

impl UserRole {
    pub fn only_admin() -> Vec<Self> {
        vec![UserRole::Admin]
    }
    pub fn all() -> Vec<Self> {
        use UserRole::*;
        vec![Admin, User, Moderator, Supporter]
    }
}

#[derive(Debug, Deserialize, sqlx::FromRow, sqlx::Type, Serialize, Clone)]
pub struct User {
    pub id: uuid::Uuid,
    pub username: String,
    pub email: String,
    pub password_hash: String,
    pub role: UserRole,
    pub photo: String,
    pub verified: bool,
    #[serde(rename = "createdAt")]
    pub created_at: DateTime<Utc>,
    #[serde(rename = "updatedAt")]
    pub updated_at: DateTime<Utc>,
}
