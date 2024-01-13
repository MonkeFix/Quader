use utoipa::{
    openapi::security::{HttpAuthScheme, HttpBuilder, SecurityScheme},
    Modify, OpenApi,
};

use crate::{dto, error, model, scope};

#[derive(OpenApi)]
#[openapi(
    paths(
        scope::health_handler,
        scope::auth::handler::register,
        scope::auth::handler::login,
        scope::auth::handler::logout,
        scope::user::handler::get_me,
        scope::user::handler::get,
    ),
    components(schemas(
        model::User,
        model::UserRole,
        dto::RegisterUser,
        dto::LoginUser,
        dto::TokenData,
        error::Response,
        error::Status,
        error::Error,
    )),
    modifiers(&SecurityAddon)
)]
pub struct ApiDoc;

struct SecurityAddon;

impl Modify for SecurityAddon {
    fn modify(&self, openapi: &mut utoipa::openapi::OpenApi) {
        let components = openapi.components.as_mut().unwrap();
        components.add_security_scheme(
            "token",
            SecurityScheme::Http(
                HttpBuilder::new()
                    .scheme(HttpAuthScheme::Bearer)
                    .bearer_format("JWT")
                    .build(),
            ),
        )
    }
}
