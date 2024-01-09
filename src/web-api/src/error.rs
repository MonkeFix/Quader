use std::fmt;
use actix_web::{HttpResponse, ResponseError};
use log::warn;
use serde::Serialize;
use derive_more::Display;

#[derive(Debug, Display, Copy, Clone, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum Status {
    #[display(fmt = "success")]
    Success,
    #[display(fmt = "failure")]
    Failure,
    #[display(fmt = "error")]
    Error,
}

fn use_display<T, S>(value: &T, serializer: S) -> Result<S::Ok, S::Error>
where
    T: fmt::Display,
    S: serde::Serializer
{
    serializer.collect_str(value)
}

#[derive(Debug, Serialize, Clone)]
pub struct ErrorResponse {
    #[serde(serialize_with = "use_display")]
    pub status: Status,
    #[serde(serialize_with = "use_display")]
    pub message: Error,
}

impl fmt::Display for ErrorResponse {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", serde_json::to_string(&self).unwrap())
    }
}

#[derive(Debug, PartialEq, Display, Clone)]
pub enum Error {
    #[display(fmt = "Server Error. Please try again later")]
    ServerError,
    #[display(fmt = "Email or password is wrong")]
    WrongCredentials,
    #[display(fmt = "User with this email already exists")]
    EmailExist,
    #[display(fmt = "User belonging to this token no longer exists")]
    UserNoLongerExist,
    #[display(fmt = "Password cannot be empty")]
    EmptyPassword,
    #[display(fmt = "Password must not be more than {} characters", _0)]
    ExceededMaxPasswordLength(usize),
    #[display(fmt = "Error while hashing password")]
    HashingError,
    #[display(fmt = "Invalid password hash format")]
    InvalidHashFormat,
    #[display(fmt = "Authentication token is invalid or expired")]
    InvalidToken,
    #[display(fmt = "You are not logged in, please provide token")]
    TokenNotProvided,
    #[display(fmt = "You are not allowed to perform this action")]
    PermissionDenied,
    #[display(fmt = "{}", _0)]
    Message(String),
}

impl Error {
    pub fn from_str(e: impl ToString) -> Self {
        Error::Message(e.to_string())
    }
}

#[derive(Debug, Clone, Display)]
#[display(fmt = "HttpError: message: {}, status: {}", message, status)]
pub struct HttpError {
    pub message: ErrorResponse,
    pub status: u16,
}

impl HttpError {
    pub fn new(message: Error, status: u16) -> Self {
        HttpError {
            message: ErrorResponse {
                status: Status::Error,
                message
            },
            status,
        }
    }

    pub fn server_error(message: Error) -> Self {
        HttpError::new(message, 500)
    }

    pub fn bad_request(message: Error) -> Self {
        HttpError::new(message, 400)
    }

    pub fn unique_constraint_voilation(message: Error) -> Self {
        HttpError::new(message, 409)
    }

    pub fn unauthorized(message: Error) -> Self {
        HttpError::new(message, 401)
    }

    pub fn not_found(message: Error) -> Self {
        HttpError::new(message, 404)
    }
}

// for simple cases
impl From<sqlx::Error> for HttpError {
    fn from(e: sqlx::Error) -> Self {
        HttpError::server_error(crate::Error::from_str(e))
    }
}

impl ResponseError for HttpError {
    fn error_response(&self) -> HttpResponse<actix_web::body::BoxBody> {
        let err = self.clone();
        match err.status {
            400 => HttpResponse::BadRequest().json(err.message),
            401 => HttpResponse::Unauthorized().json(err.message),
            409 => HttpResponse::Conflict().json(err.message),
            500 => HttpResponse::InternalServerError().json(err.message),
            _ => {
                warn!(
                    "Missing pattern match. Converted status code {} to 500.",
                    self.status
                );

                HttpResponse::InternalServerError().json(err.message)
            }
        }
    }
    fn status_code(&self) -> actix_web::http::StatusCode {
        actix_web::http::StatusCode::from_u16(self.status).unwrap()
    }
}
