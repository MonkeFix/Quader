use actix_web::{HttpResponse, ResponseError};
use derive_more::Display;
use log::warn;
use utoipa::ToSchema;

use crate::error::{self, Status};

#[derive(Debug, Clone, Display, ToSchema)]
#[display(fmt = "Error: message: {}, status: {}", message, status)]
pub struct Error {
    pub message: error::Response,
    pub status: u16,
}

impl Error {
    pub fn new(message: error::Error, status: u16) -> Self {
        Error {
            message: error::Response {
                status: Status::Error,
                message,
            },
            status,
        }
    }

    pub fn server_error(message: error::Error) -> Self {
        Error::new(message, 500)
    }

    pub fn bad_request(message: error::Error) -> Self {
        Error::new(message, 400)
    }

    pub fn unique_constraint_voilation(message: error::Error) -> Self {
        Error::new(message, 409)
    }

    pub fn unauthorized(message: error::Error) -> Self {
        Error::new(message, 401)
    }

    pub fn not_found(message: error::Error) -> Self {
        Error::new(message, 404)
    }
}

// for simple cases
impl From<sqlx::Error> for Error {
    fn from(e: sqlx::Error) -> Self {
        Error::server_error(error::Error::from_str(e))
    }
}

impl ResponseError for Error {
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
