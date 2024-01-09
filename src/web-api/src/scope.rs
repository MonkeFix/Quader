use actix_web::{Scope, web};

pub fn auth() -> Scope {
    web::scope("/auth")
        .service(self::handler::register)
}

mod handler {
    use actix_web::{post, web, HttpResponse};
    use validator::Validate;

    use crate::{app::AppState, dto, error::{HttpError, Status}, utils::password, db::UserExt};

    #[post("/register")]
    pub async fn register(
        app_state: web::Data<AppState>,
        body: web::Json<dto::RegisterUser>,
    ) -> Result<HttpResponse, HttpError> {

        body.validate()
            .map_err(|e| HttpError::bad_request(crate::Error::from_str(e)))?;

        let hashed_password = password::hash(&body.password)
            .map_err(|e| HttpError::server_error(crate::Error::from_str(e)))?;

        let result = app_state
            .db_client
            .save_user(&body.username, &body.email, &hashed_password)
            .await;

        match result {
            Ok(user) => Ok(HttpResponse::Created().json(dto::RegisterResponse {
                status: Status::Success,
                data: dto::UserData {
                    user: dto::User::from_model(&user),
                },
            })),
            Err(sqlx::Error::Database(db_err)) => {
                if db_err.is_unique_violation() {
                    Err(HttpError::unique_constraint_voilation(crate::Error::EmailExist))
                } else {
                    Err(HttpError::server_error(crate::Error::from_str(db_err)))
                }
            }
            Err(e) => Err(HttpError::server_error(crate::Error::from_str(e))),
        }
    }
}
