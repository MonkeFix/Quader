pub mod handler {
    use actix_web::{get, web};
    use uuid::Uuid;

    use crate::{
        app::AppState,
        db::UserExt,
        dto::{self, Response},
        error::HttpError,
        middleware::RequireAuth,
        model::{Authenticated, UserRole},
    };

    #[get("/me", wrap = "RequireAuth::new(UserRole::all())")]
    pub async fn get_me(user: Authenticated) -> Result<Response<dto::User>, HttpError> {
        Ok(Response::ok(dto::User::from_model(&user)))
    }

    #[get("/{user_id}", wrap = "RequireAuth::new(UserRole::only_admin())")]
    pub async fn get(
        app_state: web::Data<AppState>,
        path: web::Path<(Uuid,)>,
    ) -> Result<Response<dto::User>, HttpError> {
        let (id,) = path.into_inner();

        let maybe_user = app_state.db_client.get_user(Some(id), None, None).await?;

        match maybe_user {
            Some(user) => Ok(Response::ok(dto::User::from_model(&user))),
            None => Err(HttpError::bad_request(crate::Error::UserDoesNotExist)),
        }
    }
}
