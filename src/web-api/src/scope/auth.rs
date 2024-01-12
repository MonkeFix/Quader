pub mod handler {
    use actix_web::{
        cookie::{self, Cookie},
        post, web, HttpResponse, Responder,
    };
    use serde_json::json;
    use validator::Validate;

    use crate::{
        app::AppState,
        db::UserExt,
        http::{self, Created, Response},
        middleware::RequireAuth,
        model::{UserRole, self},
        utils::{password, token}, Error, dto,
    };

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
                .json(Response::ok(json!({"token": token}))))
        } else {
            Err(http::Error::unauthorized(Error::WrongCredentials))
        }
    }

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
}
