pub mod handler {
    use actix_web::{get, HttpResponse, HttpRequest, HttpMessage, web};
    use uuid::Uuid;

    use crate::{error::HttpError, model, dto, app::AppState, db::UserExt};

    #[get("/me")]
    pub async fn get_me(req: HttpRequest) -> Result<HttpResponse, HttpError> {
        let user = dto::User::from_model(req.extensions().get::<model::User>().unwrap());
        Ok(HttpResponse::Ok().json(user))
    }

    #[get("/{user_id}")]
    pub async fn get(app_state: web::Data::<AppState>, path: web::Path<(Uuid,)>) -> Result<HttpResponse, HttpError> {
        let (id,) = path.into_inner();

        let maybe_user = app_state.db_client.get_user(Some(id), None, None).await?;

        match maybe_user {
            Some(user) => Ok(HttpResponse::Ok().json(dto::User::from_model(&user))),
            None => Err(HttpError::bad_request(crate::Error::UserDoesNotExist)),
        }
    }
}
