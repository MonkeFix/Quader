use actix_web::{error::ErrorInternalServerError, FromRequest, HttpMessage};
use chrono::prelude::*;
use derive_more::Display;
use futures_util::future::{ready, Ready};
use serde::{Deserialize, Serialize};

use crate::{http, Error};

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
    #[serde(skip_serializing)]
    pub password_hash: String,
    pub role: UserRole,
    pub photo: String,
    pub verified: bool,
    #[serde(rename = "createdAt")]
    pub created_at: DateTime<Utc>,
    #[serde(rename = "updatedAt")]
    pub updated_at: DateTime<Utc>,
}

#[derive(Serialize)]
pub struct Authenticated(User);

impl Authenticated {
    pub fn to_user(self) -> User {
        self.0
    }
}

impl FromRequest for Authenticated {
    type Error = actix_web::Error;
    type Future = Ready<Result<Self, Self::Error>>;

    fn from_request(
        req: &actix_web::HttpRequest,
        _payload: &mut actix_web::dev::Payload,
    ) -> Self::Future {
        let value = req.extensions().get::<User>().cloned();

        let result = match value {
            Some(user) => Ok(Authenticated(user)),
            None => Err(ErrorInternalServerError(http::Error::server_error(
                Error::from_str("Authentication Error"),
            ))),
        };

        ready(result)
    }
}

impl std::ops::Deref for Authenticated {
    type Target = User;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
