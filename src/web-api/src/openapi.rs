use lib::{error, utils};
use utoipa::{
    openapi::security::{ApiKey, ApiKeyValue, SecurityScheme},
    Modify, OpenApi,
};

use crate::{dto, model, scope};

#[derive(OpenApi)]
#[openapi(
    paths(
        scope::health_handler,
        scope::auth::handler::register,
        scope::auth::handler::login,
        scope::auth::handler::refresh,
        scope::auth::handler::validate,
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
        lib::Error,
        utils::token::Claims,
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
            SecurityScheme::ApiKey(ApiKey::Cookie(ApiKeyValue::new("token"))),
        );
        components.add_security_scheme(
            "refresh_token",
            SecurityScheme::ApiKey(ApiKey::Cookie(ApiKeyValue::new("refresh_token"))),
        )
    }
}
