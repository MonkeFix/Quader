pub mod error;

use std::marker::PhantomData;

use actix_web::{cookie::Cookie, HttpResponseBuilder};
use serde::Serialize;

use crate::error::Status;

pub use self::error::Error;

trait HasHttpBuilder {
    fn get_builder() -> HttpResponseBuilder;
}

pub enum Created {}

impl HasHttpBuilder for Created {
    fn get_builder() -> HttpResponseBuilder {
        actix_web::HttpResponse::Created()
    }
}

pub enum Ok {}

impl HasHttpBuilder for Ok {
    fn get_builder() -> HttpResponseBuilder {
        actix_web::HttpResponse::Ok()
    }
}

#[derive(Serialize)]
pub struct Response<T, E = Ok> {
    pub status: Status,
    pub payload: T,
    #[serde(skip_serializing)]
    pub http_status: PhantomData<E>,
    #[serde(skip_serializing)]
    pub cookies: Vec<Cookie<'static>>,
}

impl<T, E> Response<T, E> {
    fn new(status: Status, payload: T) -> Self {
        Response {
            status,
            payload,
            http_status: PhantomData,
            cookies: vec![],
        }
    }
    pub fn cookie(mut self, cookie: Cookie<'static>) -> Self {
        self.cookies.push(cookie);
        self
    }
}

impl<T> Response<T, Ok> {
    pub fn ok(payload: T) -> Response<T, Ok> {
        Response::new(Status::Success, payload)
    }
}

impl<T> Response<T, Created> {
    pub fn created(payload: T) -> Response<T, Created> {
        Response::new(Status::Success, payload)
    }
}

impl<T: Serialize, E: HasHttpBuilder> actix_web::Responder for Response<T, E> {
    type Body = actix_web::body::BoxBody;

    fn respond_to(self, _req: &actix_web::HttpRequest) -> actix_web::HttpResponse<Self::Body> {
        let mut builder = E::get_builder();
        for cookie in self.cookies.clone() {
            builder.cookie(cookie);
        }
        builder.json(self)
    }
}
