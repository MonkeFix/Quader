use chrono::prelude::*;
use derive_more::Display;

use serde::{Deserialize, Serialize};

#[cfg(feature = "full")]
use utoipa::ToSchema;

#[derive(Debug, Deserialize, Serialize, Clone, Copy, PartialEq, Display, Eq, Hash)]
#[cfg_attr(feature = "full", derive(sqlx::Type, ToSchema))]
#[cfg_attr(
    feature = "full",
    sqlx(type_name = "user_role", rename_all = "lowercase")
)]
#[serde(rename_all = "snake_case")]
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

#[derive(Debug, Deserialize, Serialize, Clone)]
#[cfg_attr(feature = "full", derive(ToSchema, sqlx::FromRow, sqlx::Type))]
pub struct User {
    pub id: uuid::Uuid,
    pub username: String,
    pub email: String,
    #[serde(skip_serializing)]
    pub password_hash: String,
    pub role: UserRole,
    pub photo: String,
    pub verified: bool,
    #[serde(rename = "createdAt")]
    pub created_at: DateTime<Utc>,
    #[serde(rename = "updatedAt")]
    pub updated_at: DateTime<Utc>,
    #[serde(skip_serializing)]
    pub refresh_token: String,
}
