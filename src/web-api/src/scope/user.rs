pub mod handler {
    use actix_web::{get, web, HttpResponse};
    use uuid::Uuid;

    use crate::{
        app::AppState,
        db::UserExt,
        dto,
        error::HttpError,
        middleware::RequireAuth,
        model::{Authenticated, UserRole},
    };

    #[get("/me", wrap = "RequireAuth::new(UserRole::all())")]
    pub async fn get_me(user: Authenticated) -> Result<HttpResponse, HttpError> {
        Ok(HttpResponse::Ok().json(dto::User::from_model(&user)))
    }

    #[get("/{user_id}", wrap = "RequireAuth::new(UserRole::only_admin())")]
    pub async fn get(
        app_state: web::Data<AppState>,
        path: web::Path<(Uuid,)>,
    ) -> Result<HttpResponse, HttpError> {
        let (id,) = path.into_inner();

        let maybe_user = app_state.db_client.get_user(Some(id), None, None).await?;

        match maybe_user {
            Some(user) => Ok(HttpResponse::Ok().json(dto::User::from_model(&user))),
            None => Err(HttpError::bad_request(crate::Error::UserDoesNotExist)),
        }
    }
}
