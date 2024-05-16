use actix_web::dev::{Service, ServiceRequest, ServiceResponse, Transform};
use actix_web::error::{ErrorForbidden, ErrorInternalServerError, ErrorUnauthorized};
use actix_web::{web, HttpMessage};
use chrono::Utc;
use futures_util::future::{ready, LocalBoxFuture, Ready};
use futures_util::FutureExt;
use lib::error::Status;
use lib::{utils, error};
use std::rc::Rc;
use std::task::{Context, Poll};

use crate::app::AppState;
use crate::db::UserExt;
use crate::model::{User, UserRole};

pub struct RequireAuth {
    allowed_roles: Vec<UserRole>,
}

impl RequireAuth {
    pub fn filter(allowed_roles: Vec<UserRole>) -> Self {
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

fn get_user_id(
    token: Option<String>,
    jwt_secret: &[u8],
) -> Result<String, lib::Error> {
    let token = token.ok_or(lib::Error::TokenNotProvided)?;
    let claims = utils::token::decode_jwt(&token, jwt_secret)?;
    let now = Utc::now().timestamp() as usize;

    if claims.exp < now {
        Err(lib::Error::AccessTokenExpired)
    } else {
        Ok(claims.sub)
    }
}

async fn validate_token(
    user_id: String,
    db_client: &impl UserExt,
    allowed_roles: Vec<UserRole>,
) -> Result<User, error::Error> {
    let user_id = uuid::Uuid::parse_str(user_id.as_str()).unwrap();

    let result = db_client
        .get_user(Some(user_id.clone()), None, None)
        .await
        .map_err(|e| lib::Error::from_str(e))?;

    let user = result.ok_or(lib::Error::UserNoLongerExist)?;

    // Check if user's role matches the required role
    if allowed_roles.contains(&user.role) {
        Ok(user)
    } else {
        Err(lib::Error::PermissionDenied)
    }
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
            .or_else(|| req.headers()
                .get(actix_web::http::header::AUTHORIZATION)
                .map(|h| h.to_str().unwrap().split_at(7).1.to_string())
            );

        let app_state = req.app_data::<web::Data<AppState>>().unwrap();

        let jwt_secret = app_state.config.jwt_secret.as_bytes();

        match get_user_id(token, jwt_secret) {
            Err(e) => {
                let json_error = error::Response {
                    status: Status::Failure,
                    message: e,
                };
                Box::pin(ready(Err(ErrorUnauthorized(json_error))))
            },
            Ok(user_id) => {
                let allowed_roles = self.allowed_roles.clone();
                let srv = Rc::clone(&self.service);
                let app_state = app_state.clone();

                async move {
                    match validate_token(user_id, &app_state.db_client, allowed_roles).await {
                        Err(e @ lib::Error::Message(_)) => {
                            let json_error = error::Response {
                                status: Status::Error,
                                message: e,
                            };
                            Err(ErrorInternalServerError(json_error))
                        },
                        Err(e) => {
                            let json_error = error::Response {
                                status: Status::Failure,
                                message: e,
                            };
                            Err(ErrorForbidden(json_error))
                        },
                        Ok(user) => {
                            req.extensions_mut().insert::<User>(user);
                            let res = srv.call(req).await?;
                            Ok(res)
                        }
                    }
                }
                .boxed_local()
            }
        }
    }
}
