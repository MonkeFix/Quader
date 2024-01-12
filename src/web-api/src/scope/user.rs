pub mod handler {
    use actix_web::{get, web};
    use uuid::Uuid;

    use crate::{
        app::AppState,
        db::UserExt,
        error::Error,
        middleware::RequireAuth,
        model::{Authenticated, UserRole, self}, http,
    };

    #[get("/me", wrap = "RequireAuth::filter(UserRole::all())")]
    pub async fn get_me(user: Authenticated) -> Result<http::Response<model::User>, http::Error> {
        Ok(http::Response::ok(user.to_user()))
    }

    #[get("/{user_id}", wrap = "RequireAuth::filter(UserRole::only_admin())")]
    pub async fn get(
        app_state: web::Data<AppState>,
        path: web::Path<(Uuid,)>,
    ) -> Result<http::Response<model::User>, http::Error> {
        let (id,) = path.into_inner();

        let maybe_user = app_state.db_client.get_user(Some(id), None, None).await?;

        match maybe_user {
            Some(user) => Ok(http::Response::ok(user)),
            None => Err(http::Error::bad_request(Error::UserDoesNotExist)),
        }
    }
}
