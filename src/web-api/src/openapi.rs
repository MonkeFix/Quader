use utoipa::OpenApi;

use crate::{dto, error, http, model, scope};

#[derive(OpenApi)]
#[openapi(
    paths(scope::health_handler),
    components(schemas(
        model::User,
        model::UserRole,
        dto::RegisterUser,
        dto::LoginUser,
        dto::TokenData,
        error::Response,
        error::Status,
        error::Error,
    ))
)]
pub struct ApiDoc;
