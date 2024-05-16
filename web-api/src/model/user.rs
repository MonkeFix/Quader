use actix_web::{error::ErrorInternalServerError, FromRequest, HttpMessage};
use futures_util::future::{ready, Ready};
use lib::model::User;
use serde::Serialize;

use crate::http;

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
                lib::Error::from_str("Authentication Error"),
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
