pub mod handler {
    use actix_web::{get, web};
    use uuid::Uuid;

    use crate::{
        app::AppState,
        db::UserExt,
        dto,
        error::Error,
        middleware::RequireAuth,
        model::{Authenticated, UserRole}, http,
    };

    #[get("/me", wrap = "RequireAuth::new(UserRole::all())")]
    pub async fn get_me(user: Authenticated) -> Result<http::Response<dto::User>, http::Error> {
        Ok(http::Response::ok(dto::User::from_model(&user)))
    }

    #[get("/{user_id}", wrap = "RequireAuth::new(UserRole::only_admin())")]
    pub async fn get(
        app_state: web::Data<AppState>,
        path: web::Path<(Uuid,)>,
    ) -> Result<http::Response<dto::User>, http::Error> {
        let (id,) = path.into_inner();

        let maybe_user = app_state.db_client.get_user(Some(id), None, None).await?;

        match maybe_user {
            Some(user) => Ok(http::Response::ok(dto::User::from_model(&user))),
            None => Err(http::Error::bad_request(Error::UserDoesNotExist)),
        }
    }
}
