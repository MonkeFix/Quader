pub mod handler {
    use actix_web::{
        cookie::{self, Cookie},
        post, web, HttpResponse, Responder, get,
    };
    use serde_json::json;
    use validator::Validate;

    use crate::{
        app::AppState,
        db::UserExt,
        http::{self, Created, Response},
        middleware::RequireAuth,
        model::{UserRole, self},
        utils::{password, token::{self, Claims, decode_jwt}}, Error, dto,
    };

    #[utoipa::path(
        post,
        path = "/api/auth/register",
        tag = "Register Endpoint",
        request_body(content = RegisterUser),
        responses(
            (status = 201, body = User),
        )
    )]
    #[post("/register")]
    pub async fn register(
        app_state: web::Data<AppState>,
        body: web::Json<dto::RegisterUser>,
    ) -> Result<http::Response<model::User, Created>, http::Error> {
        body.validate()
            .map_err(|e| http::Error::bad_request(Error::from_str(e)))?;

        let hashed_password = password::hash(&body.password)
            .map_err(|e| http::Error::server_error(Error::from_str(e)))?;

        let result = app_state
            .db_client
            .save_user(&body.username, &body.email, &hashed_password)
            .await;

        match result {
            Ok(user) => Ok(http::Response::created(user)),
            Err(sqlx::Error::Database(db_err)) => {
                if db_err.is_unique_violation() {
                    Err(http::Error::unique_constraint_voilation(
                        Error::EmailExist,
                    ))
                } else {
                    Err(http::Error::server_error(Error::from_str(db_err)))
                }
            }
            Err(e) => Err(http::Error::server_error(Error::from_str(e))),
        }
    }

    #[utoipa::path(
        post,
        path = "/api/auth/login",
        tag = "Login Endpoint",
        request_body(
            content = LoginUser,
            example = json!({"email": "savely@krendelhoff.space", "password": "123456"})
        ),
        responses(
            (status = 200, body = TokenData),
        )
    )]
    #[post("/login")]
    pub async fn login(
        app_state: web::Data<AppState>,
        body: web::Json<dto::LoginUser>,
    ) -> Result<HttpResponse, http::Error> {
        body.validate()
            .map_err(|e| http::Error::bad_request(Error::from_str(e)))?;

        let result = app_state
            .db_client
            .get_user(None, None, Some(&body.email))
            .await
            .map_err(|e| http::Error::server_error(Error::from_str(e)))?;

        let user = result.ok_or(http::Error::unauthorized(Error::WrongCredentials))?;

        let password_matches = password::compare(&body.password, &user.password_hash)
            .map_err(|_| http::Error::unauthorized(Error::WrongCredentials))?;

        if password_matches {
            let token = token::create_jwt(
                &user.id.to_string(),
                &app_state.config.jwt_secret.as_bytes(),
                app_state.config.jwt_maxage,
            )
            .map_err(|e| http::Error::server_error(Error::from_str(e)))?;
            let cookie = Cookie::build("token", token.to_owned())
                .path("/")
                .max_age(cookie::time::Duration::new(
                    60 * &app_state.config.jwt_maxage,
                    0,
                ))
                .http_only(true)
                .finish();

            Ok(HttpResponse::Ok()
                .cookie(cookie)
                .json(Response::ok(dto::TokenData { token })))
        } else {
            Err(http::Error::unauthorized(Error::WrongCredentials))
        }
    }

    #[utoipa::path(
        post,
        path = "/api/auth/logout",
        tag = "Logout Endpoint",
        responses(
            (status = 200),
        ),
        security(
            ("token" = [])
        )
    )]
    #[post("/logout", wrap = "RequireAuth::filter(UserRole::all())")]
    pub async fn logout() -> impl Responder {
        let cookie = Cookie::build("token", "")
            .path("/")
            .max_age(cookie::time::Duration::new(-1, 0))
            .http_only(true)
            .finish();

        HttpResponse::Ok()
            .cookie(cookie)
            .json(json!({"status": "success"}))
    }

    #[utoipa::path(
        get,
        path = "/api/auth/validate/{token}",
        tag = "Validate Endpoint",
        responses(
            (status = 200, body = Claims),
        )
    )]
    #[get("/validate/{token}")]
    pub async fn validate(
        app_state: web::Data<AppState>,
        path: web::Path<(String,)>,
    ) -> Result<http::Response<Claims>, http::Error> {
        let (token,) = path.into_inner();
        decode_jwt(token, app_state.config.jwt_secret.as_bytes())
            .map(http::Response::ok)
            .map_err(http::Error::not_acceptable)
    }
}
