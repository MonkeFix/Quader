use std::marker::PhantomData;

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use validator::Validate;

use crate::{
    error::{self, Status},
    model,
};

#[derive(Validate, Debug, Default, Clone, Deserialize)]
pub struct RegisterUser {
    #[validate(length(min = 1, message = "Name is required"))]
    pub username: String,
    #[validate(
        length(min = 1, message = "Email is required"),
        email(message = "Email is invalid")
    )]
    pub email: String,
    #[validate(
        length(min = 1, message = "Password is required"),
        length(min = 6, message = "Password must be at least 6 characters")
    )]
    pub password: String,
    #[validate(
        length(min = 1, message = "Please confirm your password"),
        must_match(other = "password", message = "Passwords do not match")
    )]
    #[serde(rename = "passwordConfirm")]
    pub password_confirm: String,
}

#[derive(Validate, Debug, Default, Clone, Deserialize)]
pub struct LoginUser {
    #[validate(
        length(min = 1, message = "Email is required"),
        email(message = "Email is invalid")
    )]
    pub email: String,
    #[validate(
        length(min = 1, message = "Password is required"),
        length(min = 6, message = "Password must be at least 6 characters")
    )]
    pub password: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct User {
    pub id: String,
    pub username: String,
    pub email: String,
    pub role: String,
    pub photo: String,
    pub verified: bool,
    #[serde(rename = "createdAt")]
    pub created_at: DateTime<Utc>,
    #[serde(rename = "updatedAt")]
    pub updated_at: DateTime<Utc>,
}

impl User {
    pub fn from_model(user: &model::User) -> Self {
        User {
            id: user.id.to_string(),
            email: user.email.to_owned(),
            username: user.username.to_owned(),
            photo: user.photo.to_owned(),
            role: user.role.to_string(),
            verified: user.verified,
            created_at: user.created_at,
            updated_at: user.updated_at,
        }
    }
}

#[derive(Debug, Serialize)]
pub struct TokenData {
    pub token: String,
}

pub enum Created {}
pub enum Ok {}

#[derive(Serialize)]
pub struct Response<T, E = Ok> {
    pub status: error::Status,
    pub payload: T,
    pub http_status: PhantomData<E>,
}

impl<T, E> Response<T, E> {
    fn new(status: error::Status, payload: T) -> Response<T, E> {
        Response {
            status,
            payload,
            http_status: PhantomData,
        }
    }
}

impl<T> Response<T> {
    pub fn ok(payload: T) -> Response<T, Ok> {
        Response::new(Status::Success, payload)
    }
}

impl<T> Response<T, Created> {
    pub fn created(payload: T) -> Response<T, Created> {
        Response::new(Status::Success, payload)
    }
}

impl<T: Serialize> actix_web::Responder for Response<T, Ok> {
    type Body = actix_web::body::BoxBody;

    fn respond_to(self, _req: &actix_web::HttpRequest) -> actix_web::HttpResponse<Self::Body> {
        actix_web::HttpResponse::Ok().json(self)
    }
}

impl<T: Serialize> actix_web::Responder for Response<T, Created> {
    type Body = actix_web::body::BoxBody;

    fn respond_to(self, _req: &actix_web::HttpRequest) -> actix_web::HttpResponse<Self::Body> {
        actix_web::HttpResponse::Created().json(self)
    }
}
