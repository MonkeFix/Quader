use actix_web::dev::{Service, ServiceRequest, ServiceResponse, Transform};
use actix_web::error::{ErrorForbidden, ErrorInternalServerError, ErrorUnauthorized};
use actix_web::{web, HttpMessage};
use futures_util::future::{ready, LocalBoxFuture, Ready};
use futures_util::FutureExt;
use std::rc::Rc;
use std::task::{Context, Poll};

use crate::{Error, http};
use crate::db::UserExt;
use crate::error::{self, Status};
use crate::model::{User, UserRole};
use crate::{utils, app::AppState};

pub struct RequireAuth {
    allowed_roles: Vec<UserRole>,
}

impl RequireAuth {
    pub fn new(allowed_roles: Vec<UserRole>) -> Self {
        RequireAuth { allowed_roles }
    }
}

impl<S> Transform<S, ServiceRequest> for RequireAuth
where
    S: Service<
            ServiceRequest,
            Response = ServiceResponse<actix_web::body::BoxBody>,
            Error = actix_web::Error,
        > + 'static,
{
    type Response = ServiceResponse<actix_web::body::BoxBody>;
    type Error = actix_web::Error;
    type Transform = AuthMiddleware<S>;
    type InitError = ();
    type Future = Ready<Result<Self::Transform, Self::InitError>>;

    fn new_transform(&self, service: S) -> Self::Future {
        ready(Ok(AuthMiddleware {
            service: Rc::new(service),
            allowed_roles: self.allowed_roles.clone(),
        }))
    }
}

pub struct AuthMiddleware<S> {
    service: Rc<S>,
    allowed_roles: Vec<UserRole>,
}

impl<S> Service<ServiceRequest> for AuthMiddleware<S>
where
    S: Service<
            ServiceRequest,
            Response = ServiceResponse<actix_web::body::BoxBody>,
            Error = actix_web::Error,
        > + 'static,
{
    type Response = ServiceResponse<actix_web::body::BoxBody>;
    type Error = actix_web::Error;
    type Future = LocalBoxFuture<'static, Result<Self::Response, actix_web::Error>>;

    fn poll_ready(&self, ctx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.service.poll_ready(ctx)
    }

    fn call(&self, req: ServiceRequest) -> Self::Future {
        let token = req
            .cookie("token")
            .map(|c| c.value().to_string())
            .or_else(|| {
                req.headers()
                    .get(actix_web::http::header::AUTHORIZATION)
                    .map(|h| h.to_str().unwrap().split_at(7).1.to_string())
            });

        if token.is_none() {
            let json_error = error::Response {
                status: Status::Failure,
                message: crate::Error::TokenNotProvided,
            };
            return Box::pin(ready(Err(ErrorUnauthorized(json_error))));
        }

        // TODO make it possible to wrap endpoint several times,
        // but if user extension is included already
        // don't do any decoding work
        let app_state = req.app_data::<web::Data<AppState>>().unwrap();
        let user_id = match utils::token::decode_jwt(
            &token.unwrap(),
            app_state.config.jwt_secret.as_bytes(),
        ) {
            Ok(id) => id,
            Err(message) => {
                return Box::pin(ready(Err(ErrorUnauthorized(error::Response {
                    status: Status::Failure,
                    message,
                }))))
            }
        };

        let cloned_app_state = app_state.clone();
        let allowed_roles = self.allowed_roles.clone();
        let srv = Rc::clone(&self.service);

        async move {
            let user_id = uuid::Uuid::parse_str(user_id.as_str()).unwrap();
            let result = cloned_app_state
                .db_client
                .get_user(Some(user_id.clone()), None, None)
                .await
                .map_err(|e| ErrorInternalServerError(http::Error::server_error(Error::from_str(e))))?;

            let user = result.ok_or(ErrorUnauthorized(error::Response {
                status: Status::Failure,
                message: crate::Error::UserNoLongerExist,
            }))?;

            // Check if user's role matches the required role
            if allowed_roles.contains(&user.role) {
                req.extensions_mut().insert::<User>(user);
                let res = srv.call(req).await?;
                Ok(res)
            } else {
                let json_error = error::Response {
                    status: Status::Failure,
                    message: crate::Error::PermissionDenied,
                };
                Err(ErrorForbidden(json_error))
            }
        }
        .boxed_local()
    }
}
