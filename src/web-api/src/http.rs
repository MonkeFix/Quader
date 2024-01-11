pub mod error;

use std::marker::PhantomData;

use serde::Serialize;

use crate::error::Status;

pub use self::error::Error;

pub enum Created {}
pub enum Ok {}

#[derive(Serialize)]
pub struct Response<T, E = Ok> {
    pub status: Status,
    pub payload: T,
    #[serde(skip_serializing)]
    pub http_status: PhantomData<E>,
}

impl<T, E> Response<T, E> {
    fn new(status: Status, payload: T) -> Response<T, E> {
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
